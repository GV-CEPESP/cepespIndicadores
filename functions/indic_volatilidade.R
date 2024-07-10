
## Função para calcular os indicadores de 'Volatilidade'

indic_volat <- function(data,
                        agregacao = c("BR", "UF", 
                                      "PF_MUN", "VR_MUN")){
  
  ## Desabilitando as mensagens do 'dplyr'
  
  options(dplyr.summarise.inform = FALSE)
  
  ## Lista temporária onde os dados serão armazenados
  
  indicadores_final <- list()
  
################################# BR ######################################
  
  if(agregacao == "BR"){
    
    ## For loop que calcula os indicadores de 'Volatilidade'
    ## para cada ano
    
    for(ano in seq(2002, 2022, by = 4)){
      
      cat("Lendo", ano, "\n")
      
      ## Cria um data frame com as informações da
      ## eleição corrente
      
      ano_t0 <- data %>% 
        filter(ANO_ELEICAO == ano) %>%
        filter(QTDE_VAGAS == INFORMACAO_DISPONIVEL) %>% 
        mutate(ANO_ELEICAO = as.character(ANO_ELEICAO)) %>% 
        select(-SIGLA_PARTIDO) %>% 
        arrange(ANO_ELEICAO,
                NUMERO_PARTIDO)
      
      ## Cria um data frame com as informações da próxima
      ## eleição em relação ao ano corrente
      
      ano_t4 <- data %>% 
        filter(ANO_ELEICAO == ano - 4) %>% 
        filter(QTDE_VAGAS == INFORMACAO_DISPONIVEL) %>% 
        mutate(ANO_ELEICAO = as.character(ANO_ELEICAO)) %>% 
        select(-SIGLA_PARTIDO) %>% 
        rename("PERC_VOTOS2" = "PERC_VOTOS",
               "PERC_CADEIRAS2" = "PERC_CADEIRAS") %>% 
        arrange(ANO_ELEICAO,
                NUMERO_PARTIDO)
      
      ## Alterando o número do PSL e do DEM em 2018 quando t = 2022
      
      if(ano == 2022){
        
        ano_t4 <- ano_t4 %>% 
          mutate(NUMERO_PARTIDO = ifelse(NUMERO_PARTIDO %in% c("17", "25"),
                                         "44",
                                         NUMERO_PARTIDO)) %>% 
          group_by(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   QTDE_VAGAS,
                   INFORMACAO_DISPONIVEL,
                   QTDE_VOTOS_VALIDOS_BR,
                   NUMERO_PARTIDO) %>% 
          summarise(VOT_PART_BR = sum(VOT_PART_BR,
                                       na.rm = TRUE),
                    TOT_CADEIRAS = sum(TOT_CADEIRAS,
                                       na.rm = TRUE),
                    PERC_VOTOS2 = sum(PERC_VOTOS2,
                                       na.rm = TRUE),
                    PERC_CADEIRAS2 = sum(PERC_CADEIRAS2,
                                       na.rm = TRUE))
          
        
      }
      
      ## Condição para que os indicadores sejam calculados
      
      if(nrow(ano_t0) > 0 &
         nrow(ano_t4) > 0){
        
        ## Verificando se existem partidos ausentes em uma das eleições
        
        partido_falt_ano_t0 <- anti_join(ano_t4,
                                         ano_t0, 
                                       by = "NUMERO_PARTIDO")
        
        partido_falt_ano_t4 <- anti_join(ano_t0,
                                         ano_t4, 
                                         by = "NUMERO_PARTIDO")
        
        ## Equipara o número de partidos entre as eleições e atribui 0
        ## quando não existir em um dos anos de referência
        
        ano_t0 <- partido_falt_ano_t0 %>% 
          select(-PERC_VOTOS2,
                 -PERC_CADEIRAS2) %>% 
          mutate(ANO_ELEICAO = as.character(ano),
                 QTDE_VAGAS = ano_t0[1, "QTDE_VAGAS"][[1]],
                 QTDE_VOTOS_VALIDOS_BR = ano_t0[1, "QTDE_VOTOS_VALIDOS_BR"][[1]],
                 VOT_PART_BR = 0,
                 TOT_CADEIRAS = 0,
                 PERC_VOTOS = 0,
                 PERC_CADEIRAS = 0) %>% 
          rbind(ano_t0) %>% 
          arrange(ANO_ELEICAO,
                  NUMERO_PARTIDO)
        
        ano_t4 <- partido_falt_ano_t4 %>% 
          mutate(ANO_ELEICAO = as.character(ano),
                 PERC_VOTOS2 = 0,
                 PERC_CADEIRAS2 = 0) %>% 
          rbind(ano_t4) %>% 
          ungroup() %>% 
          select(NUMERO_PARTIDO,
                 PERC_VOTOS2,
                 PERC_CADEIRAS2) %>% 
          arrange(NUMERO_PARTIDO)
        
        ## Juntando ambos os bancos
        
        suppressMessages(
          indicadores1 <- left_join(ano_t0,
                                    ano_t4))
        
        ## Cálculo dos indicadores de 'Volatilidade'
        
        indicadores1 <- indicadores1 %>% 
          mutate(DIFERENCA_ELEITORAL = volatilidade(PERC_VOTOS,
                                                    PERC_VOTOS2),
                 DIFERENCA_PARLAMENTAR = volatilidade(PERC_CADEIRAS,
                                                      PERC_CADEIRAS2)) %>% 
          group_by(ANO_ELEICAO) %>% 
          mutate(VOLATILIDADE_ELEITORAL = sum(DIFERENCA_ELEITORAL,
                                              na.rm = TRUE)/2,
                 VOLATILIDADE_PARLAMENTAR = sum(DIFERENCA_PARLAMENTAR,
                                                na.rm = TRUE)/2)
        
        ## Empilha os indicadores calculados em um único banco
        
        indicadores_final <- bind_rows(indicadores_final,
                                       indicadores1)
        
      } else if(nrow(ano_t0) > 0 &
                nrow(ano_t4) == 0) {
        
        ## Atribuindo NA aos indicadores de "Volatilidade" porque a 
        ## informação não existe em um dos anos de referência
        
        indicadores1 <- ano_t0 %>% 
          mutate(PERC_VOTOS2 = NA,
                 PERC_CADEIRAS2 = NA,
                 VOLATILIDADE_ELEITORAL = NA,
                 VOLATILIDADE_PARLAMENTAR = NA)
        
        ## Empilha os indicadores calculados em um único banco
        
        indicadores_final <- bind_rows(indicadores_final,
                                       indicadores1)
        
      } else if(nrow(ano_t0) == 0 &
                nrow(ano_t4) > 0) {
        
        ## Atribuindo NA aos indicadores de "Volatilidade" porque a 
        ## informação não existe em um dos anos de referência
        
        indicadores1 <- ano_t4 %>% 
          mutate(ANO_ELEICAO = ano) %>% 
          mutate(PERC_VOTOS = NA,
                 PERC_CADEIRAS = NA,
                 VOLATILIDADE_ELEITORAL = NA,
                 VOLATILIDADE_PARLAMENTAR = NA)
        
        ## Empilha os indicadores calculados em um único banco
        
        indicadores_final <- bind_rows(indicadores_final,
                                       indicadores1)
        
      }
      
    }
    
##################################### UF ##################################
    
  } else if(agregacao == "UF"){
    
    ## Lista dos estados brasileiros
    
    ufs <- c("AC", "AL", "AP", "AM", "BA", 
             "CE", "DF", "ES", "GO", "MA", 
             "MT", "MS", "MG", "PA", "PB", 
             "PR", "PE", "PI", "RJ", "RN", 
             "RS", "RO", "RR", "SC", "SP", 
             "SE", "TO")
    
    ## For loop que calcula os indicadores de 'Volatilidade'
    ## para cada ano
    
    for(ano in seq(2002, 2022, by = 4)){
      
      for(uf in ufs){
        
        cat("Lendo", ano, uf, "\n")
        
        ## Cria um data frame com as informações da
        ## eleição corrente
        
        ano_t0 <- data %>% 
          filter(ANO_ELEICAO == ano &
                 SIGLA_UF == uf) %>% 
          filter(QTDE_VAGAS == INFORMACAO_DISPONIVEL) %>% 
          mutate(ANO_ELEICAO = as.character(ANO_ELEICAO)) %>% 
          select(-SIGLA_PARTIDO) %>% 
          arrange(ANO_ELEICAO,
                  SIGLA_UF,
                  NUMERO_PARTIDO)
        
        ## Cria um data frame com as informações da 
        ## eleição t-4
        
        ano_t4 <- data %>% 
          filter(ANO_ELEICAO == ano - 4 &
                   SIGLA_UF == uf) %>% 
          filter(QTDE_VAGAS == INFORMACAO_DISPONIVEL) %>%
          mutate(ANO_ELEICAO = as.character(ANO_ELEICAO)) %>% 
          select(-SIGLA_PARTIDO) %>% 
          rename("PERC_VOTOS2" = "PERC_VOTOS",
                 "PERC_CADEIRAS2" = "PERC_CADEIRAS") %>% 
          arrange(ANO_ELEICAO,
                  SIGLA_UF,
                  NUMERO_PARTIDO)
        
        ## Alterando o número do PSL e do DEM em 2018 quando t = 2022
        
        if(ano == 2022){
          
          ano_t4 <- ano_t4 %>% 
            mutate(NUMERO_PARTIDO = ifelse(NUMERO_PARTIDO %in% c("17", "25"),
                                           "44",
                                           NUMERO_PARTIDO)) %>% 
            group_by(ANO_ELEICAO,
                     SIGLA_UF,
                     DESCRICAO_CARGO,
                     QTDE_VAGAS,
                     INFORMACAO_DISPONIVEL,
                     QTDE_VOTOS_VALIDOS,
                     NUMERO_PARTIDO) %>% 
            summarise(VOT_PART_UF = sum(VOT_PART_UF,
                                        na.rm = TRUE),
                      TOT_CADEIRAS = sum(TOT_CADEIRAS,
                                         na.rm = TRUE),
                      PERC_VOTOS2 = sum(PERC_VOTOS2,
                                        na.rm = TRUE),
                      PERC_CADEIRAS2 = sum(PERC_CADEIRAS2,
                                           na.rm = TRUE))
          
          
        }
        
        ## Condição para os indicadores sejam calculados
        
        if(nrow(ano_t0) > 0 &
           nrow(ano_t4) > 0){
          
          ## Verificando se existem partidos ausentes em uma das eleições
          
          partido_falt_ano_t0 <- anti_join(ano_t4,
                                           ano_t0, 
                                           by = "NUMERO_PARTIDO")
          
          partido_falt_ano_t4 <- anti_join(ano_t0,
                                           ano_t4, 
                                           by = "NUMERO_PARTIDO")
          
          ## Equipara o número de partidos entre as eleições e atribui 0
          ## quando não existir em um dos anos de referência
          
          ano_t0 <- partido_falt_ano_t0 %>%  
            select(-PERC_VOTOS2,
                   -PERC_CADEIRAS2) %>% 
            mutate(ANO_ELEICAO = as.character(ano),
                   SIGLA_UF = uf,
                   QTDE_VAGAS = ano_t0[1, "QTDE_VAGAS"][[1]],
                   QTDE_VOTOS_VALIDOS = ano_t0[1, "QTDE_VOTOS_VALIDOS"][[1]],
                   VOT_PART_UF = 0,
                   TOT_CADEIRAS = 0,
                   PERC_VOTOS = 0,
                   PERC_CADEIRAS = 0) %>% 
            rbind(ano_t0) %>% 
            arrange(ANO_ELEICAO,
                    SIGLA_UF,
                    NUMERO_PARTIDO)
          
          ano_t4 <- partido_falt_ano_t4 %>% 
            mutate(PERC_VOTOS2 = 0,
                   PERC_CADEIRAS2 = 0) %>% 
            rbind(ano_t4) %>% 
            ungroup() %>% 
            select(NUMERO_PARTIDO,
                   PERC_VOTOS2,
                   PERC_CADEIRAS2) %>% 
            arrange(NUMERO_PARTIDO)
          
          ## Juntando ambos os bancos
          
          suppressMessages(
            indicadores1 <- left_join(ano_t0,
                                      ano_t4))
          
          ## Cálculo dos indicadores de 'Volatilidade'
          
          indicadores1 <- indicadores1 %>% 
            mutate(DIFERENCA_ELEITORAL = volatilidade(PERC_VOTOS,
                                                      PERC_VOTOS2),
                   DIFERENCA_PARLAMENTAR = volatilidade(PERC_CADEIRAS,
                                                        PERC_CADEIRAS2)) %>% 
            group_by(ANO_ELEICAO) %>% 
            mutate(VOLATILIDADE_ELEITORAL = sum(DIFERENCA_ELEITORAL,
                                                na.rm = TRUE)/2,
                   VOLATILIDADE_PARLAMENTAR = sum(DIFERENCA_PARLAMENTAR,
                                                  na.rm = TRUE)/2)
          
          ## Empilha os indicadores calculados em um único banco
          
          indicadores_final <- bind_rows(indicadores_final,
                            indicadores1)
          
        } else if(nrow(ano_t0) > 0 &
                  nrow(ano_t4) == 0) {
          
          ## Atribuindo NA aos indicadores de "Volatilidade" porque a 
          ## informação não existe em um dos anos de referência
          
          indicadores1 <- ano_t0 %>% 
            mutate(PERC_VOTOS2 = NA,
                   PERC_CADEIRAS2 = NA,
                   VOLATILIDADE_ELEITORAL = NA,
                   VOLATILIDADE_PARLAMENTAR = NA)
          
          ## Empilha os indicadores calculados em um único banco
          
          indicadores_final <- bind_rows(indicadores_final,
                                         indicadores1)
          
        } else if(nrow(ano_t0) == 0 &
                  nrow(ano_t4) > 0) {
          
          ## Atribuindo NA aos indicadores de "Volatilidade" porque a 
          ## informação não existe em um dos anos de referência
          
          indicadores1 <- ano_t4 %>% 
            mutate(ANO_ELEICAO = ano) %>% 
            mutate(PERC_VOTOS = NA,
                   PERC_CADEIRAS = NA,
                   VOLATILIDADE_ELEITORAL = NA,
                   VOLATILIDADE_PARLAMENTAR = NA)
          
          ## Empilha os indicadores calculados em um único banco
          
          indicadores_final <- bind_rows(indicadores_final,
                                         indicadores1)
          
        }
        
      }
    }
    
################################### PF_MUN ###################################    
    
  } else if(agregacao == "PF_MUN"){
    
    ## For loop que calcula os indicadores de 'Volatilidade'
    ## para cada ano
    
    for(ano in seq(2004, 2020, by = 4)){
      
      ## Lista de municípios brasileiros
      
      num_municipios <- vagas_ver %>% 
        ungroup() %>% 
        filter(ANO_ELEICAO == ano) %>% 
        filter(!is.na(QTDE_VAGAS)) %>% 
        select(SIGLA_UF,
               COD_MUN_TSE,
               COD_MUN_IBGE,
               NOME_MUNICIPIO) %>% 
        unique() %>% 
        arrange(SIGLA_UF,
                NOME_MUNICIPIO)
      
      for(municipio in 1:nrow(num_municipios)){
        
        cat("Lendo", ano, "município", municipio, "de", nrow(num_municipios), "\n")
        
        ## Cria um data frame com as informações da
        ## eleição corrente
        
        ano_t0 <- data %>% 
          filter(NUM_TURNO == 1) %>% 
          filter(ANO_ELEICAO == ano &
                 COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>%
          filter(QTDE_VAGAS == INFORMACAO_DISPONIVEL) %>% 
          mutate(ANO_ELEICAO = as.character(ANO_ELEICAO)) %>% 
          select(-SIGLA_PARTIDO) %>% 
          arrange(ANO_ELEICAO,
                  SIGLA_UF,
                  NOME_MUNICIPIO,
                  NUMERO_PARTIDO) 
        
        ## Cria um data frame com as informações da eleição
        ## em t-4
        
        ano_t4 <- data %>% 
          filter(NUM_TURNO == 1) %>% 
          filter(ANO_ELEICAO == ano - 4 &
                 COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
          filter(QTDE_VAGAS == INFORMACAO_DISPONIVEL) %>% 
          mutate(ANO_ELEICAO = as.character(ANO_ELEICAO)) %>% 
          select(-SIGLA_PARTIDO) %>% 
          rename("PERC_VOTOS2" = "PERC_VOTOS") %>% 
          arrange(ANO_ELEICAO,
                  SIGLA_UF,
                  NOME_MUNICIPIO,
                  NUMERO_PARTIDO)
        
        ## Condição para os indicadores sejam calculados
        
        if(nrow(ano_t0) > 0 &
           nrow(ano_t4) > 0){
          
          ## Verificando se existem partidos ausentes em uma das eleições
          
          partido_falt_ano_t0 <- anti_join(ano_t4,
                                           ano_t0, 
                                           by = "NUMERO_PARTIDO")
          
          partido_falt_ano_t4 <- anti_join(ano_t0,
                                           ano_t4, 
                                           by = "NUMERO_PARTIDO")
          
          ## Equipara o número de partidos entre as eleições e atribui 0
          ## quando não existir em um dos anos de referência
          
          ano_t0 <- partido_falt_ano_t0 %>% 
            select(-PERC_VOTOS2) %>% 
            mutate(ANO_ELEICAO = as.character(ano),
                   NUM_TURNO = ano_t0[1, "NUM_TURNO"][[1]],
                   SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                   COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                   COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                   QTDE_VAGAS = ano_t0[1, "QTDE_VAGAS"][[1]],
                   QTDE_VOTOS_VALIDOS = ano_t0[1, "QTDE_VOTOS_VALIDOS"][[1]],
                   VOT_PART_MUN = 0,
                   TOT_CADEIRAS = 0,
                   PERC_VOTOS = 0) %>% 
            rbind(ano_t0) %>% 
            arrange(ANO_ELEICAO,
                    NUM_TURNO,
                    SIGLA_UF,
                    NOME_MUNICIPIO,
                    NUMERO_PARTIDO)
          
          ano_t4 <- partido_falt_ano_t4 %>% 
            mutate(PERC_VOTOS2 = 0) %>% 
            rbind(ano_t4) %>% 
            ungroup() %>% 
            select(NUM_TURNO,
                   NUMERO_PARTIDO,
                   PERC_VOTOS2) %>% 
            arrange(NUMERO_PARTIDO)
          
          ## Juntando ambos os bancos
          
          suppressMessages(
            indicadores1 <- left_join(ano_t0,
                                      ano_t4))
          
          ## Cálculo dos indicadores de 'Volatilidade'
          
          indicadores1 <- indicadores1 %>% 
            mutate(DIFERENCA_ELEITORAL = volatilidade(PERC_VOTOS,
                                                      PERC_VOTOS2)) %>% 
            group_by(ANO_ELEICAO) %>% 
            mutate(VOLATILIDADE_ELEITORAL = sum(DIFERENCA_ELEITORAL,
                                                na.rm = TRUE)/2)
          
          ## Empilha os indicadores calculados em um único banco
          
          indicadores_final <- bind_rows(indicadores_final,
                                         indicadores1)
          
        } else if(nrow(ano_t0) > 0 &
                  nrow(ano_t4) == 0) {
          
          ## Atribuindo 0 aos indicadores de "Volatilidade" porque a 
          ## informação não existe em um dos anos de referência
          
          indicadores1 <- ano_t0 %>% 
            mutate(PERC_VOTOS2 = NA,
                   VOLATILIDADE_ELEITORAL = NA)
          
          ## Empilha os indicadores calculados em um único banco
          
          indicadores_final <- bind_rows(indicadores_final,
                                         indicadores1)
          
        } else if(nrow(ano_t0) == 0 &
                  nrow(ano_t4) > 0) {
          
          ## Atribuindo NA aos indicadores de "Volatilidade" porque a 
          ## informação não existe em um dos anos de referência
          
          indicadores1 <- ano_t4 %>% 
            mutate(ANO_ELEICAO = ano) %>% 
            mutate(PERC_VOTOS = NA,
                   VOLATILIDADE_ELEITORAL = NA)
          
          ## Empilha os indicadores calculados em um único banco
          
          indicadores_final <- bind_rows(indicadores_final,
                                         indicadores1)
          
        }
        
        ## Salvando uma versão temporária para conferência posterior
        
        saveRDS(indicadores_final,
                "data/output/volatilidade_prefeitos_mun_temp.rds")
        
      }
    }
    
##################################### VR_MUN ###################################    
    
  } else if(agregacao == "VR_MUN"){
    
    ## For loop que calcula os indicadores de 'Volatilidade'
    ## para cada ano
    
    for(ano in seq(2004, 2020, by = 4)){
      
      ## Lista de municípios brasileiros
      
      num_municipios <- vagas_ver %>% 
        ungroup() %>% 
        filter(ANO_ELEICAO == ano) %>% 
        filter(!is.na(QTDE_VAGAS)) %>% 
        select(SIGLA_UF,
               COD_MUN_TSE,
               COD_MUN_IBGE,
               NOME_MUNICIPIO) %>% 
        unique() %>% 
        arrange(SIGLA_UF,
                NOME_MUNICIPIO)
      
      for(municipio in 1:nrow(num_municipios)){
        
        cat("Lendo", ano, "município", municipio, "de", nrow(num_municipios), "\n")
        
        ## Cria um data frame com as informações da
        ## eleição corrente
        
        ano_t0 <- data %>% 
          filter(NUM_TURNO == 1) %>% 
          filter(ANO_ELEICAO == ano &
                 COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>%
          filter(QTDE_VAGAS == INFORMACAO_DISPONIVEL) %>% 
          mutate(ANO_ELEICAO = as.character(ANO_ELEICAO)) %>% 
          select(-SIGLA_PARTIDO) %>% 
          arrange(ANO_ELEICAO,
                  SIGLA_UF,
                  NOME_MUNICIPIO,
                  NUMERO_PARTIDO)
        
        ## Cria um data frame com as informações da eleição
        ## em t-4
        
        ano_t4 <- data %>% 
          filter(NUM_TURNO == 1) %>% 
          filter(ANO_ELEICAO == ano - 4 &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
          filter(QTDE_VAGAS == INFORMACAO_DISPONIVEL) %>% 
          mutate(ANO_ELEICAO = as.character(ANO_ELEICAO)) %>% 
          select(-SIGLA_PARTIDO) %>% 
          rename("PERC_VOTOS2" = "PERC_VOTOS",
                 "PERC_CADEIRAS2" = "PERC_CADEIRAS") %>% 
          arrange(ANO_ELEICAO,
                  SIGLA_UF,
                  NOME_MUNICIPIO,
                  NUMERO_PARTIDO)
        
        ## Condição para os indicadores sejam calculados
        
        if(nrow(ano_t0) > 0 &
           nrow(ano_t4) > 0){
          
          ## Verificando se existem partidos ausentes em uma das eleições
          
          partido_falt_ano_t0 <- anti_join(ano_t4,
                                           ano_t0, 
                                           by = "NUMERO_PARTIDO")
          
          partido_falt_ano_t4 <- anti_join(ano_t0,
                                           ano_t4, 
                                           by = "NUMERO_PARTIDO")
          
          ## Equipara o número de partidos entre as eleições e atribui 0
          ## quando não existir em um dos anos de referência
          
          ano_t0 <- partido_falt_ano_t0 %>% 
            select(-PERC_VOTOS2,
                   -PERC_CADEIRAS2) %>% 
            mutate(ANO_ELEICAO = as.character(ano),
                   NUM_TURNO = ano_t0[1, "NUM_TURNO"][[1]],
                   SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                   COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                   COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                   QTDE_VAGAS = ano_t0[1, "QTDE_VAGAS"][[1]],
                   QTDE_VOTOS_VALIDOS = ano_t0[1, "QTDE_VOTOS_VALIDOS"][[1]],
                   VOT_PART_MUN = 0,
                   TOT_CADEIRAS = 0,
                   PERC_VOTOS = 0,
                   PERC_CADEIRAS = 0) %>% 
            rbind(ano_t0) %>% 
            arrange(ANO_ELEICAO,
                    NUM_TURNO,
                    SIGLA_UF,
                    NOME_MUNICIPIO,
                    NUMERO_PARTIDO)
          
          ano_t4 <- partido_falt_ano_t4 %>% 
            mutate(PERC_VOTOS2 = 0,
                   PERC_CADEIRAS2 = 0) %>% 
            rbind(ano_t4) %>% 
            ungroup() %>% 
            select(NUM_TURNO,
                   NUMERO_PARTIDO,
                   PERC_VOTOS2,
                   PERC_CADEIRAS2) %>% 
            arrange(NUMERO_PARTIDO)
          
          ## Juntando ambos os bancos
          
          suppressMessages(
            indicadores1 <- left_join(ano_t0,
                                      ano_t4))
          
          ## Cálculo dos indicadores de 'Volatilidade'
          
          indicadores1 <- indicadores1 %>% 
            mutate(DIFERENCA_ELEITORAL = volatilidade(PERC_VOTOS,
                                                      PERC_VOTOS2),
                   DIFERENCA_PARLAMENTAR = volatilidade(PERC_CADEIRAS,
                                                        PERC_CADEIRAS2)) %>% 
            group_by(ANO_ELEICAO) %>% 
            mutate(VOLATILIDADE_ELEITORAL = sum(DIFERENCA_ELEITORAL,
                                                na.rm = TRUE)/2,
                   VOLATILIDADE_PARLAMENTAR = sum(DIFERENCA_PARLAMENTAR,
                                                  na.rm = TRUE)/2)
          
          ## Empilha os indicadores calculados em um único banco
          
          indicadores_final <- bind_rows(indicadores_final,
                                         indicadores1)
          
        } else if(nrow(ano_t0) > 0 &
                  nrow(ano_t4) == 0) {
          
          ## Atribuindo NA aos indicadores de "Volatilidade" porque a 
          ## informação não existe em um dos anos de referência
          
          indicadores1 <- ano_t0 %>% 
            mutate(PERC_VOTOS2 = NA,
                   PERC_CADEIRAS2 = NA,
                   VOLATILIDADE_ELEITORAL = NA,
                   VOLATILIDADE_PARLAMENTAR = NA)
          
          ## Empilha os indicadores calculados em um único banco
          
          indicadores_final <- bind_rows(indicadores_final,
                                         indicadores1)
          
        } else if(nrow(ano_t0) == 0 &
                  nrow(ano_t4) > 0) {
          
          ## Atribuindo 0 aos indicadores de "Volatilidade" porque a 
          ## informação não existe em um dos anos de referência
          
          indicadores1 <- ano_t4 %>% 
            mutate(ANO_ELEICAO = ano) %>% 
            mutate(PERC_VOTOS = NA,
                   PERC_CADEIRAS = NA,
                   VOLATILIDADE_ELEITORAL = NA,
                   VOLATILIDADE_PARLAMENTAR = NA)
          
          ## Empilha os indicadores calculados em um único banco
          
          indicadores_final <- bind_rows(indicadores_final,
                                         indicadores1)
          
        }
        
        ## Salvando uma versão temporária para conferência posterior
        
        saveRDS(indicadores_final,
                "data/output/volatilidade_vereadores_mun_temp.rds")
        
      }
      
    }
  }
  
  return(indicadores_final)
  
}

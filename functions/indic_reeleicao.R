
## Função para calcular os indicadores de 'Reeleição'

indic_reel <- function(candidatos,
                       eleitos,
                       agregacao = c("BR", "UF", "PF_UF", 
                                     "PF_ELEIT_APT", "MUN")){
  
  ## Desabilitando as mensagens do 'dplyr'
  
  options(dplyr.summarise.inform = FALSE)
  
  ## Lista temporária onde os dados serão armazenados
  
  temp <- list()
  
  temp2 <- list()
  
  com_erro <- list()
  
  indicadores_final <- list()
  
  ################################### BR #########################################    
  
  if(agregacao == "BR"){
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(1998, 2018, by = 4)){
      
      cat("Lendo",ano,"\n")
      
      ## Banco com os candidatos da próxima eleição em 
      ## relação ao ano corrente
      
      candidatos_ano2 <- candidatos %>% 
        filter(ANO_ELEICAO == ano + 4) 
      
      ## Banco com os candidatos eleitos na primeira
      ## eleição de referência
      
      eleitos_ano1 <- eleitos %>% 
        filter(ANO_ELEICAO == ano) 
      
      ## Banco com os candidatos eleitos na segunda
      ## eleição de referência
      
      eleitos_ano2 <- eleitos %>% 
        filter(ANO_ELEICAO == ano + 4) 
      
      ## Filtra os candidatos que se reapresentaram na eleição
      ## seguinte ao ano corrente e que foram reeleitos
      
      eleitos_ano2 <- eleitos_ano2 %>% 
        filter(ID_CEPESP %in% 
               eleitos_ano1$ID_CEPESP)
      
      ## Verifica quantos candidatos foram reeleitos e salva o resultado
      
      indicadores2 <- eleitos_ano2 %>% 
        group_by(ANO_ELEICAO) %>% 
        summarise(REELEITOS = n()) 
      
      ## Calcula o total de candidatos que se reapresentaram na 
      ## eleição seguinte ao ano corrente e cruza o resultado com o número 
      ## de reeleitos
      
      suppressMessages(
        indicadores1 <- candidatos_ano2 %>% 
          filter(ID_CEPESP %in% 
                 eleitos_ano1$ID_CEPESP) %>% 
          group_by(ANO_ELEICAO,
                   DESCRICAO_CARGO) %>% 
          mutate(REAPRESENTACAO = n()) %>% 
          select(ANO_ELEICAO,
                 SIGLA_UF,
                 DESCRICAO_CARGO,
                 QTDE_VAGAS,
                 REAPRESENTACAO) %>% 
          unique() %>% 
          group_by(ANO_ELEICAO,
                   DESCRICAO_CARGO) %>% 
          reframe(QTDE_VAGAS = sum(QTDE_VAGAS,
                                   na.rm = TRUE),
                  REAPRESENTACAO = REAPRESENTACAO) %>% 
          unique() %>% 
          left_join(indicadores2))
      
      ## Calcula os indicadores de 'Reeleição'
      
      indicadores1 <- indicadores1 %>% 
        mutate(DERROTADOS = REAPRESENTACAO - REELEITOS,
               DESISTENCIA = QTDE_VAGAS - REAPRESENTACAO,
               REELEICAO_BRUTA = reeleicao(REELEITOS, QTDE_VAGAS),
               REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS, 
                                                 DERROTADOS),
               RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
               RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
               RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                               QTDE_VAGAS))
      
      ## Empilha os indicadores calculados no banco criado
      
      indicadores_final <- bind_rows(indicadores_final, 
                                     indicadores1)
    } 
    
    ###################################### UF ######################################   
    
  } else if(agregacao == "UF"){
    
    ## Lista dos estados brasileiros
    
    ufs <- c("AC", "AL", "AP", "AM", "BA", 
             "CE", "DF", "ES", "GO", "MA", 
             "MT", "MS", "MG", "PA", "PB", 
             "PR", "PE", "PI", "RJ", "RN", 
             "RS", "RO", "RR", "SC", "SP", 
             "SE", "TO")
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(1998, 2018, by = 4)){
      
      for(uf in ufs){
        
        cat("Lendo", ano, uf, "\n")
        
        ## Banco com os candidatos da próxima eleição em 
        ## relação ao ano corrente
        
        candidatos_ano2 <- candidatos %>% 
          filter(ANO_ELEICAO == ano + 4 &
                 SIGLA_UF == SIGLA_UF)
        
        ## Banco com os candidatos eleitos na primeira
        ## eleição de referência
        
        eleitos_ano1 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                 SIGLA_UF == uf)
        
        ## Banco com os candidatos eleitos na segunda
        ## eleição de referência
        
        eleitos_ano2 <- eleitos %>% 
          filter(ANO_ELEICAO == ano + 4 &
                 SIGLA_UF == uf)
        
        ## Filtra os candidatos que se reapresentaram na eleição
        ## seguinte ao ano corrente e que foram reeleitos
        
        eleitos_ano2 <- eleitos_ano2 %>% 
          filter(ID_CEPESP %in% 
                 eleitos_ano1$ID_CEPESP)
        
        ## Verifica o total de candidatos reeleitos e salva o resultado 
        
        indicadores2 <- eleitos_ano2 %>% 
          group_by(ANO_ELEICAO,
                   SIGLA_UF) %>% 
          summarise(REELEITOS = n()) 
        
        ## Calcula o total de candidatos que se reapresentaram na 
        ## eleição seguinte ao ano corrente e junta com o número 
        ## de reeleitos
        
        suppressMessages(
          indicadores1 <- candidatos_ano2 %>% 
            filter(ID_CEPESP %in% 
                   eleitos_ano1$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF) %>%
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   SIGLA_UF,
                   QTDE_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF) %>% 
            reframe(QTDE_VAGAS = sum(QTDE_VAGAS,
                                     na.rm = TRUE),
                    REAPRESENTACAO = REAPRESENTACAO) %>% 
            unique() %>% 
            left_join(indicadores2))
        
        ## Calcula os indicadores de 'Reeleição'
        
        indicadores1 <- indicadores1 %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS,
                 DESISTENCIA = QTDE_VAGAS - REAPRESENTACAO,
                 REELEICAO_BRUTA = reeleicao(REELEITOS, QTDE_VAGAS),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS, 
                                                   DERROTADOS),
                 RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
                 RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                 RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                 QTDE_VAGAS))
        
        ## Empilha os indicadores calculados no banco criado
        
        indicadores_final <- bind_rows(indicadores_final, 
                                       indicadores1)
        
      }
    } 
    
    ################################# PF_UF ########################################    
    
  } else if(agregacao == "PF_UF"){
    
    ## Lista dos estados brasileiros
    
    ufs <- c("AC", "AL", "AP", "AM", "BA", 
             "CE", "ES", "GO", "MA", 
             "MT", "MS", "MG", "PA", "PB", 
             "PR", "PE", "PI", "RJ", "RN", 
             "RS", "RO", "RR", "SC", "SP", 
             "SE", "TO")
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(2008, 2020, by = 4)){
      
      for(uf in ufs){
        
        ## Verificando quantos municípios existem no estado
        
        num_municipios <- candidatos %>% 
          filter(ANO_ELEICAO == ano & 
                   SIGLA_UF == uf) %>% 
          ungroup() %>% 
          select(COD_MUN_TSE,
                 COD_MUN_IBGE,
                 NOME_MUNICIPIO) %>% 
          unique() %>% 
          arrange(NOME_MUNICIPIO)
        
        ## Salvando o número de municípios na agregação 
        
        municipios_agreg <- nrow(num_municipios)
        
        for(municipio in 1:nrow(num_municipios)){
          
          cat("Lendo", ano, uf, "município", municipio, "de", nrow(num_municipios), "\n")
          
          ## Verificando qual candidato foi eleito em t-8
          
          eleitos_t8 <- eleitos %>% 
            filter(ANO_ELEICAO == ano - 8 &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
            mutate(ELEITO_T8 = 1) %>% 
            ungroup() %>% 
            select(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   ID_CEPESP,
                   ELEITO_T8)
          
          ## Verificando qual candidato foi eleito em t-4
          
          suppressMessages(
            eleitos_t4 <- eleitos %>% 
              filter(ANO_ELEICAO == ano - 4 &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
              mutate(ELEITO_T4 = 1) %>% 
              left_join(eleitos_t8) %>% 
              mutate(ELEITO_T8 = ifelse(is.na(ELEITO_T8),
                                        0,
                                        ELEITO_T8),
                     PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                          ELEITO_T8 == 1,
                                          0,
                                          1)) %>% 
              ungroup() %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     ID_CEPESP,
                     ELEITO_T4,
                     ELEITO_T8,
                     PERMIT_CAND))
          
          ## Verificando qual candidato foi eleito em t0
          
          eleitos_t0 <- eleitos %>% 
            filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
          
          if(nrow(eleitos_t0) == 1){
            
            ## Verifica se o candidato eleito em t-4 se reelegeu em t0
            
            suppressMessages(
              indicadores2 <- eleitos_t0 %>% 
                filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
                group_by(ANO_ELEICAO,
                         SIGLA_UF,
                         COD_MUN_TSE,
                         COD_MUN_IBGE) %>% 
                summarise(REELEITOS_AGREG = n()))
            } 
          
          ## Atribui valor 0 caso o prefeito eleito em t-4 não tenha 
          ## se reelegido (desde que a informação exista p/ todos os anos)
          
          if(nrow(indicadores2) == 0 &
             ((nrow(eleitos_t8) > 0) &
              (nrow(eleitos_t4) > 0) &
              (nrow(eleitos_t0) > 0))) {
              
              indicadores2 <- indicadores2 %>% 
                ungroup() %>% 
                add_row(ANO_ELEICAO = as.character(ano),
                        SIGLA_UF = uf,
                        COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                        COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                        REELEITOS_AGREG = 0)
              
              ## Atribui valor NA caso a informação não exista p/ todos os anos
              
            } else if(nrow(indicadores2) == 0 &
                      ((nrow(eleitos_t8) == 0) |
                       (nrow(eleitos_t4) == 0) |
                       (nrow(eleitos_t0) == 0))) {
              
              indicadores2 <- indicadores2 %>% 
                ungroup() %>% 
                add_row(ANO_ELEICAO = as.character(ano),
                        SIGLA_UF = uf,
                        COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                        COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                        REELEITOS_AGREG = NA)
              
            }
            
            ## Verifica se o prefeito eleito em t-4 se recandidatou em t0
            
            suppressMessages(
              indicadores1 <- candidatos %>% 
                filter(ANO_ELEICAO == ano &
                       SIGLA_UF == uf &
                       COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
                filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
                ungroup() %>% 
                select(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE,
                       NOME_MUNICIPIO,
                       DESCRICAO_CARGO) %>% 
                unique()) 
            
            ## Salva se o prefeito eleito em t-4 se candidatou em t0
            
            if(nrow(indicadores1) == 1){
              
              indicadores1 <- indicadores1 %>% 
                group_by(ANO_ELEICAO,
                         SIGLA_UF,
                         COD_MUN_TSE,
                         COD_MUN_IBGE,
                         NOME_MUNICIPIO,
                         DESCRICAO_CARGO) %>% 
                mutate(REAPRESENTACAO = n())
              
              ## Atribuindo 0 nos casos em que o prefeito eleito em t-4 não se 
              ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
              ## informação exista p/ todos os anos)
              
            } else if(nrow(indicadores1) == 0 &
                      ((nrow(eleitos_t8) > 0) &
                       (nrow(eleitos_t4) > 0) &
                       (nrow(eleitos_t0) > 0))){
              
              indicadores1 <- indicadores1 %>% 
                ungroup() %>% 
                add_row(ANO_ELEICAO = as.character(ano),
                        SIGLA_UF = uf,
                        COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                        COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                        NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                        DESCRICAO_CARGO = "PREFEITO") %>% 
                mutate(REAPRESENTACAO = 0)
              
            } else if(nrow(indicadores1) == 0 &
                      ((nrow(eleitos_t8) == 0) |
                       (nrow(eleitos_t4) == 0) |
                       (nrow(eleitos_t0) == 0))) {
              
              indicadores1 <- indicadores1 %>% 
                ungroup() %>% 
                add_row(ANO_ELEICAO = as.character(ano),
                        SIGLA_UF = uf,
                        COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                        COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                        NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                        DESCRICAO_CARGO = "PREFEITO") %>% 
                mutate(REAPRESENTACAO = NA)
              
            } 
            
            ## Removendo as colunas que não serão mais utilizadas
            
            eleitos_t4 <- eleitos_t4 %>% 
              select(SIGLA_UF,
                     COD_MUN_TSE,
                     PERMIT_CAND)
            
            ## Juntando com as informações dos prefeitos que podiam se 
            ## recandidatar em t0, bem como dos que foram reeleitos em t0
            
            suppressMessages(
              indicadores1 <- indicadores1 %>% 
                left_join(eleitos_t4) %>%
                left_join(eleitores_aptos) %>% 
                left_join(indicadores2) %>% 
                mutate(QTDE_VAGAS = NA,
                       QTDE_MUNICIPIOS_AGREG = NA) %>% 
                select(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE,
                       NOME_MUNICIPIO,
                       DESCRICAO_CARGO,
                       AGREG_ELEITORES_APTOS,
                       QTDE_MUNICIPIOS_AGREG,
                       QTDE_VAGAS,
                       PERMIT_CAND,
                       REAPRESENTACAO,
                       REELEITOS_AGREG) %>% 
                unique())
            
          if(is.na(indicadores1$REAPRESENTACAO) |
             is.na(indicadores1$REELEITOS_AGREG)){
            
            ## Atualizando o número de municípios com informação disponível
            ## na agregação
            
            municipios_agreg <- municipios_agreg - 1
            
            ## Empilhando o município com erro em um banco adicional
            
            com_erro <- bind_rows(com_erro,
                                  indicadores1)
            
            ## Salvando os municípios com problemas
            
            saveRDS(com_erro,
                    "data/output/reeleicao_prefeitos_municipos_com_erro.rds")
            
          }
          
          ## Empilha os indicadores calculados no banco criado
          
          temp <- bind_rows(temp, 
                            indicadores1)
          
          ## Atualiza a quantidade de municípios na agregação
          
          temp <- temp %>% 
            mutate(QTDE_MUNICIPIOS_AGREG = municipios_agreg)
          
        }
        
        ## Calcula os indicadores de 'Reeleição' na agregação de referência
        
        suppressMessages(
        temp2 <- temp %>% 
          group_by(ANO_ELEICAO,
                   SIGLA_UF,
                   DESCRICAO_CARGO,
                   QTDE_MUNICIPIOS_AGREG,
                   QTDE_VAGAS) %>% 
          summarise(PERMIT_CAND = sum(PERMIT_CAND, 
                                      na.rm = TRUE),
                    REAPRESENTACAO = sum(REAPRESENTACAO,
                                         na.rm = TRUE),
                    REELEITOS_AGREG = sum(REELEITOS_AGREG,
                                          na.rm = TRUE)) %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                 DESISTENCIA = QTDE_MUNICIPIOS_AGREG - REAPRESENTACAO,
                 TX_PREFEITOS_PERMIT_REEL = PERMIT_CAND/QTDE_MUNICIPIOS_AGREG,
                 TX_PREFEITOS_RECAND = REAPRESENTACAO/PERMIT_CAND,
                 REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, QTDE_MUNICIPIOS_AGREG),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                   DERROTADOS)) %>% 
          mutate(across(DERROTADOS:REELEICAO_LIQUIDA, ~ifelse(is.nan(.),
                                                              0,
                                                              .))))
        
        ## Empilhando os resultados agregados por uf no arquivo final
        
        indicadores_final <- bind_rows(indicadores_final,
                                       temp2)
        
        ## Salvando o progresso para conferência
        
        saveRDS(indicadores_final,
                "data/output/reel_pf_uf_temp.rds")
        
      }
    }
    
    ############################# PF_ELEITO_APT ####################################    
    
  } else if(agregacao == "PF_ELEIT_APT"){
    
    ## Lista das faixas de eleitores aptos
    
    agreg_eleitores <- unique(eleitores_aptos$AGREG_ELEITORES_APTOS)
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(2008, 2020, by = 4)){
      
      for(agreg in agreg_eleitores){
        
        ## Verificando quantos municípios existem na agregação
        
        suppressMessages(
        num_municipios <- candidatos %>% 
          left_join(eleitores_aptos) %>% 
          filter(ANO_ELEICAO == ano & 
                 AGREG_ELEITORES_APTOS == agreg) %>% 
          ungroup() %>% 
          select(AGREG_ELEITORES_APTOS,
                 SIGLA_UF,
                 COD_MUN_TSE,
                 COD_MUN_IBGE,
                 NOME_MUNICIPIO) %>% 
          unique() %>% 
          arrange(AGREG_ELEITORES_APTOS,
                  SIGLA_UF,
                  NOME_MUNICIPIO))
        
        ## Salvando o número de municípios na agregação 
        
        municipios_agreg <- nrow(num_municipios)
        
        for(municipio in 1:nrow(num_municipios)){
          
          cat("Lendo", ano, agreg, "município", municipio, "de", nrow(num_municipios), "\n")
          
          
          ## Verificando qual candidato foi eleito em t-8
          
          eleitos_t8 <- eleitos %>% 
            filter(ANO_ELEICAO == ano - 8 &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
            mutate(ELEITO_T8 = 1) %>% 
            ungroup() %>% 
            select(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   ID_CEPESP,
                   ELEITO_T8)
          
          ## Verificando qual candidato foi eleito em t-4
          
          suppressMessages(
            eleitos_t4 <- eleitos %>% 
              filter(ANO_ELEICAO == ano - 4 &
                       COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
              mutate(ELEITO_T4 = 1) %>% 
              left_join(eleitos_t8) %>% 
              mutate(ELEITO_T8 = ifelse(is.na(ELEITO_T8),
                                        0,
                                        ELEITO_T8),
                     PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                            ELEITO_T8 == 1,
                                          0,
                                          1)) %>% 
              ungroup() %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     ID_CEPESP,
                     ELEITO_T4,
                     ELEITO_T8,
                     PERMIT_CAND))
          
          ## Verificando qual candidato foi eleito em t0
          
          eleitos_t0 <- eleitos %>% 
            filter(ANO_ELEICAO == ano &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
          
          if(nrow(eleitos_t0) == 1){
            
            ## Verifica se o candidato eleito em t-4 se reelegeu em t0
            
            suppressMessages(
              indicadores2 <- eleitos_t0 %>% 
                filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
                group_by(ANO_ELEICAO,
                         SIGLA_UF,
                         COD_MUN_TSE,
                         COD_MUN_IBGE) %>% 
                summarise(REELEITOS_AGREG = n()))
          } 
          
          ## Atribui valor 0 caso o prefeito eleito em t-4 não tenha 
          ## se reelegido (desde que a informação exista p/ todos os anos)
          
          if(nrow(indicadores2) == 0 &
             ((nrow(eleitos_t8) > 0) &
              (nrow(eleitos_t4) > 0) &
              (nrow(eleitos_t0) > 0))) {
            
            indicadores2 <- indicadores2 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      REELEITOS_AGREG = 0)
            
            ## Atribui valor NA caso a informação não exista p/ todos os anos
            
          } else if(nrow(indicadores2) == 0 &
                    ((nrow(eleitos_t8) == 0) |
                     (nrow(eleitos_t4) == 0) |
                     (nrow(eleitos_t0) == 0))) {
            
            indicadores2 <- indicadores2 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      REELEITOS_AGREG = NA)
            
          }
          
          ## Verifica se o prefeito eleito em t-4 se recandidatou em t0
          
          suppressMessages(
            indicadores1 <- candidatos %>% 
              filter(ANO_ELEICAO == ano &
                       SIGLA_UF == num_municipios$SIGLA_UF[municipio] &
                       COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
              filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
              ungroup() %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     DESCRICAO_CARGO) %>% 
              unique()) 
          
          ## Salva se o prefeito eleito em t-4 se candidatou em t0
          
          if(nrow(indicadores1) == 1){
            
            indicadores1 <- indicadores1 %>% 
              group_by(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE,
                       NOME_MUNICIPIO,
                       DESCRICAO_CARGO) %>% 
              mutate(REAPRESENTACAO = n())
            
            ## Atribuindo 0 nos casos em que o prefeito eleito em t-4 não se 
            ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
            ## informação exista p/ todos os anos)
            
          } else if(nrow(indicadores1) == 0 &
                    ((nrow(eleitos_t8) > 0) &
                     (nrow(eleitos_t4) > 0) &
                     (nrow(eleitos_t0) > 0))){
            
            indicadores1 <- indicadores1 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                      DESCRICAO_CARGO = "PREFEITO") %>% 
              mutate(REAPRESENTACAO = 0)
            
          } else if(nrow(indicadores1) == 0 &
                    ((nrow(eleitos_t8) == 0) |
                     (nrow(eleitos_t4) == 0) |
                     (nrow(eleitos_t0) == 0))) {
            
            indicadores1 <- indicadores1 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                      DESCRICAO_CARGO = "PREFEITO") %>% 
              mutate(REAPRESENTACAO = NA)
            
          } 
          
          ## Removendo as colunas que não serão mais utilizadas
          
          eleitos_t4 <- eleitos_t4 %>% 
            select(SIGLA_UF,
                   COD_MUN_TSE,
                   PERMIT_CAND)
          
          ## Juntando com as informações dos prefeitos que podiam se 
          ## recandidatar em t0, bem como dos que foram reeleitos em t0
          
          suppressMessages(
            indicadores1 <- indicadores1 %>% 
              left_join(eleitos_t4) %>%
              left_join(eleitores_aptos) %>% 
              left_join(indicadores2) %>% 
              mutate(QTDE_VAGAS = NA,
                     QTDE_MUNICIPIOS_AGREG = NA) %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     DESCRICAO_CARGO,
                     AGREG_ELEITORES_APTOS,
                     QTDE_MUNICIPIOS_AGREG,
                     QTDE_VAGAS,
                     PERMIT_CAND,
                     REAPRESENTACAO,
                     REELEITOS_AGREG) %>% 
              unique())
          
          if(is.na(indicadores1$REAPRESENTACAO) |
             is.na(indicadores1$REELEITOS_AGREG)){
            
            ## Atualizando o número de municípios com informação disponível
            ## na agregação
            
            municipios_agreg <- municipios_agreg - 1
            
            ## Empilhando o município com erro em um banco adicional
            
            com_erro <- bind_rows(com_erro,
                                  indicadores1)
            
            ## Salvando os municípios com problemas
            
            saveRDS(com_erro,
                    "data/output/reeleicao_prefeitos_municipos_com_erro_v2.rds")
            
          }
          
          ## Empilha os indicadores calculados no banco criado
          
          temp <- bind_rows(temp, 
                            indicadores1)
          
          ## Atualiza a quantidade de municípios na agregação
          
          temp <- temp %>% 
            mutate(QTDE_MUNICIPIOS_AGREG = municipios_agreg)
          
        }
        
        ## Calcula os indicadores de 'Reeleição' na agregação de referência
        
        suppressMessages(
        temp2 <- temp %>% 
          group_by(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   AGREG_ELEITORES_APTOS,
                   QTDE_MUNICIPIOS_AGREG,
                   QTDE_VAGAS) %>% 
          summarise(PERMIT_CAND = sum(PERMIT_CAND, 
                                      na.rm = TRUE),
                    REAPRESENTACAO = sum(REAPRESENTACAO,
                                         na.rm = TRUE),
                    REELEITOS_AGREG = sum(REELEITOS_AGREG,
                                          na.rm = TRUE)) %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                 DESISTENCIA = QTDE_MUNICIPIOS_AGREG - REAPRESENTACAO,
                 TX_PREFEITOS_PERMIT_REEL = PERMIT_CAND/QTDE_MUNICIPIOS_AGREG,
                 TX_PREFEITOS_RECAND = REAPRESENTACAO/PERMIT_CAND,
                 REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, QTDE_MUNICIPIOS_AGREG),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                   DERROTADOS)) %>% 
          mutate(across(DERROTADOS:REELEICAO_LIQUIDA, ~ifelse(is.nan(.),
                                                              0,
                                                              .))))
        
        ## Empilhando os resultados agregados por uf no arquivo final
        
        indicadores_final <- bind_rows(indicadores_final,
                                       temp2)
        
        ## Salvando o progresso para conferência
        
        saveRDS(indicadores_final,
                "data/output/reel_pf_elt_apt_temp.rds")
      }
    }
    
    ################################### MUN ########################################    
    
  } else if(agregacao == "MUN"){
    
    ## Lista de municípios brasileiros
    
    municipios <- unique(pf_mun_cand$COD_MUN_TSE)
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(2000, 2016, by = 4)){
    
      for(municipio in seq_along(municipios)){
        
        cat("Lendo", ano, municipio, "\n")
        
        ## Banco com os candidatos da próxima eleição em 
        ## relação ao ano corrente
        
        candidatos_ano2 <- candidatos %>% 
          filter(ANO_ELEICAO == ano + 4 &
                 COD_MUN_TSE == municipios[municipio]) 
        
        ## Banco com os candidatos eleitos na primeira
        ## eleição de referência
        
        eleitos_ano1 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                 COD_MUN_TSE == municipios[municipio])
        
        ## Banco com os candidatos eleitos na segunda
        ## eleição de referência
        
        eleitos_ano2 <- eleitos %>% 
          filter(ANO_ELEICAO == ano + 4 &
                 COD_MUN_TSE == municipios[municipio])
        
        ## Filtra os candidatos que se reapresentaram na eleição
        ## seguinte ao ano corrente e que foram reeleitos
        
        eleitos_ano2 <- eleitos_ano2 %>% 
          filter(ID_CEPESP %in% 
                 eleitos_ano1$ID_CEPESP)
        
        ## Verifica quantos candidatos foram reeleitos e salva o resultado
        
        indicadores2 <- eleitos_ano2 %>% 
          group_by(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE) %>% 
          summarise(REELEITOS = n()) 
        
        ## Calcula o total de candidatos que se reapresentaram na 
        ## eleição seguinte ao ano corrente e junta com o número 
        ## de reeleitos
        
        suppressMessages(
          indicadores1 <- candidatos_ano2 %>% 
            filter(ID_CEPESP %in% 
                   eleitos_ano1$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO) %>%
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   QTDE_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO) %>% 
            reframe(QTDE_VAGAS = sum(QTDE_VAGAS,
                                     na.rm = TRUE),
                    REAPRESENTACAO = REAPRESENTACAO) %>% 
            unique() %>% 
            left_join(indicadores2))
        
        ## Calcula os indicadores de 'Reeleição'
        
        indicadores1 <- indicadores1 %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS,
                 DESISTENCIA = QTDE_VAGAS - REAPRESENTACAO,
                 REELEICAO_BRUTA = reeleicao(REELEITOS, QTDE_VAGAS),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS, 
                                                   DERROTADOS),
                 RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
                 RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                 RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                 QTDE_VAGAS))
        
        ## Empilha os indicadores calculados no banco criado
        
        indicadores_final <- bind_rows(indicadores_final, 
                                       indicadores1)
        
      }
      
    }
  } 
  
  
  return(indicadores_final)
  
} 

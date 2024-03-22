
## Função para calcular os indicadores de 'Volatilidade'

indic_volat <- function(data,
                        agregacao = c("BR", "UF", "MUN")){
  
  ## Desabilitando as mensagens do 'dplyr'
  
  options(dplyr.summarise.inform = FALSE)
  
  ## Lista temporária onde os dados serão armazenados
  
  temp <- list()
  
  if(agregacao == "BR"){
    
    ## For loop que calcula os indicadores de 'Volatilidade'
    ## para cada ano
    
    for(ano in seq(1998, 2018, by = 4)){
      
      cat("Lendo", ano, "\n")
      
      ## Cria um data frame com as informações da
      ## eleição corrente
      
      indicadores_ano1 <- data %>% 
        filter(ANO_ELEICAO == ano) %>% 
        select(-SIGLA_PARTIDO) %>% 
        arrange(ANO_ELEICAO,
                NUMERO_PARTIDO)
      
      ## Cria um data frame com as informações da próxima
      ## eleição em relação ao ano corrente
      
      indicadores_ano2 <- data %>% 
        filter(ANO_ELEICAO == ano + 4) %>% 
        select(-SIGLA_PARTIDO) %>% 
        rename("PERC_VOTOS2" = "PERC_VOTOS",
               "PERC_CADEIRAS2" = "PERC_CADEIRAS") %>% 
        arrange(ANO_ELEICAO,
                NUMERO_PARTIDO)
      
      ## Condição para que se comece a calcular os indicadores
      
      if(nrow(indicadores_ano1) > 0 |
         nrow(indicadores_ano2) > 0){
        
        ## Verificando se existem partidos ausentes em uma das eleições
        
        partido_falt_ano2 <- anti_join(indicadores_ano1,
                                       indicadores_ano2, 
                                       by = "NUMERO_PARTIDO")
        
        partido_falt_ano1 <- anti_join(indicadores_ano2,
                                       indicadores_ano1, 
                                       by = "NUMERO_PARTIDO")
        
        ## Equipara o número de partidos entre as eleições e atribui 0
        ## quando não existir em um dos anos de referência
        
        indicadores_ano1 <- partido_falt_ano1 %>% 
          select(-PERC_VOTOS2,
                 -PERC_CADEIRAS2) %>% 
          mutate(ANO_ELEICAO = ano,
                 QT_VAGAS = indicadores_ano1[1, "QT_VAGAS"][[1]],
                 QT_VOTOS_VALIDOS_BR = indicadores_ano1[1, "QT_VOTOS_VALIDOS_BR"][[1]],
                 VOT_PART_BR = 0,
                 TOT_CADEIRAS = 0,
                 PERC_VOTOS = 0,
                 PERC_CADEIRAS = 0,
                 NUM_PART_PARLAMENT = indicadores_ano1[1, "NUM_PART_PARLAMENT"][[1]]) %>% 
          rbind(indicadores_ano1) %>% 
          arrange(ANO_ELEICAO,
                  NUMERO_PARTIDO)
        
        indicadores_ano2 <- partido_falt_ano2 %>% 
          mutate(ANO_ELEICAO = ano,
                 PERC_VOTOS2 = 0,
                 PERC_CADEIRAS2 = 0) %>% 
          rbind(indicadores_ano2) %>% 
          ungroup() %>% 
          select(NUMERO_PARTIDO,
                 PERC_VOTOS2,
                 PERC_CADEIRAS2) %>% 
          arrange(NUMERO_PARTIDO)
        
        ## Juntando ambos os bancos
        
        suppressMessages(
          indicadores_ano1 <- left_join(indicadores_ano1,
                                        indicadores_ano2))
        
        ## Cálculo dos indicadores de 'Volatilidade'
        
        indicadores_ano1 <- indicadores_ano1 %>% 
          mutate(ANO_ELEICAO = ANO_ELEICAO + 4,
                 VOLATILIDADE_ELEITORAL = volatilidade(PERC_VOTOS,
                                                       PERC_VOTOS2),
                 VOLATILIDADE_PARLAMENTAR = volatilidade(PERC_CADEIRAS,
                                                         PERC_CADEIRAS2))
        
        ## Empilha os indicadores calculados em um único banco
        
        temp <- bind_rows(temp,
                          indicadores_ano1)
        
      }
      
    }
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
    
    for(ano in seq(1998, 2018, by = 4)){
      for(uf in ufs){
        
        cat("Lendo", ano, uf, "\n")
        
        ## Cria um data frame com as informações da
        ## eleição corrente
        
        indicadores_ano1 <- data %>% 
          filter(ANO_ELEICAO == ano &
                   UF == uf) %>% 
          select(-SIGLA_PARTIDO) %>% 
          arrange(ANO_ELEICAO,
                  UF,
                  NUMERO_PARTIDO)
        
        ## Cria um data frame com as informações da próxima
        ## eleição em relação ao ano corrente
        
        indicadores_ano2 <- data %>% 
          filter(ANO_ELEICAO == ano + 4 &
                   UF == uf) %>% 
          select(-SIGLA_PARTIDO) %>% 
          rename("PERC_VOTOS2" = "PERC_VOTOS",
                 "PERC_CADEIRAS2" = "PERC_CADEIRAS") %>% 
          arrange(ANO_ELEICAO,
                  UF,
                  NUMERO_PARTIDO)
        
        ## Condição para que se comece a calcular os indicadores
        
        if(nrow(indicadores_ano1) > 0 |
           nrow(indicadores_ano2) > 0){
          
          ## Verificando se existem partidos ausentes em uma das eleições
          
          partido_falt_ano2 <- anti_join(indicadores_ano1,
                                         indicadores_ano2, 
                                         by = "NUMERO_PARTIDO")
          
          partido_falt_ano1 <- anti_join(indicadores_ano2,
                                         indicadores_ano1, 
                                         by = "NUMERO_PARTIDO")
          
          ## Equipara o número de partidos entre as eleições e atribui 0
          ## quando não existir em um dos anos de referência
          
          indicadores_ano1 <- partido_falt_ano1 %>% 
            select(-PERC_VOTOS2,
                   -PERC_CADEIRAS2) %>% 
            mutate(ANO_ELEICAO = ano,
                   UF = uf,
                   QT_VAGAS = indicadores_ano1[1, "QT_VAGAS"][[1]],
                   QT_VOTOS_VALIDOS = indicadores_ano1[1, "QT_VOTOS_VALIDOS"][[1]],
                   VOT_PART_UF = 0,
                   TOT_CADEIRAS = 0,
                   PERC_VOTOS = 0,
                   PERC_CADEIRAS = 0,
                   NUM_PART_PARLAMENT = indicadores_ano1[1, "NUM_PART_PARLAMENT"][[1]]) %>% 
            rbind(indicadores_ano1) %>% 
            arrange(ANO_ELEICAO,
                    UF,
                    NUMERO_PARTIDO)
          
          indicadores_ano2 <- partido_falt_ano2 %>% 
            mutate(PERC_VOTOS2 = 0,
                   PERC_CADEIRAS2 = 0) %>% 
            rbind(indicadores_ano2) %>% 
            ungroup() %>% 
            select(NUMERO_PARTIDO,
                   PERC_VOTOS2,
                   PERC_CADEIRAS2) %>% 
            arrange(NUMERO_PARTIDO)
          
          ## Juntando ambos os bancos
          
          suppressMessages(
            indicadores_ano1 <- left_join(indicadores_ano1,
                                          indicadores_ano2))
          
          ## Cálculo dos indicadores de 'Volatilidade'
          
          indicadores_ano1 <- indicadores_ano1 %>% 
            mutate(ANO_ELEICAO = ANO_ELEICAO + 4,
                   VOLATILIDADE_ELEITORAL = volatilidade(PERC_VOTOS,
                                                         PERC_VOTOS2),
                   VOLATILIDADE_PARLAMENTAR = volatilidade(PERC_CADEIRAS,
                                                           PERC_CADEIRAS2))
          
          ## Empilha os indicadores calculados em um único banco
          
          temp <- bind_rows(temp,
                            indicadores_ano1)
          
        }
        
      }
    }
  } else if(agregacao == "MUN"){
    
    ## Lista de municípios brasileiros
    
    municipios <- unique(pf_mun_cand$COD_MUN_TSE)
    
    ## For loop que calcula os indicadores de 'Volatilidade'
    ## para cada ano
    
    for(ano in seq(2000, 2016, by = 4)){
      for(municipio in municipios){
        
        cat("Lendo", ano, municipio, "\n")
        
        ## Cria um data frame com as informações da
        ## eleição corrente
        
        indicadores_ano1 <- data %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == municipio) %>% 
          select(-SIGLA_PARTIDO) %>% 
          arrange(ANO_ELEICAO,
                  UF,
                  NOME_MUNICIPIO,
                  NUMERO_PARTIDO)
        
        ## Cria um data frame com as informações da próxima
        ## eleição em relação ao ano corrente
        
        indicadores_ano2 <- data %>% 
          filter(ANO_ELEICAO == ano + 4 &
                   COD_MUN_TSE == municipio) %>% 
          select(-SIGLA_PARTIDO) %>% 
          rename("PERC_VOTOS2" = "PERC_VOTOS",
                 "PERC_CADEIRAS2" = "PERC_CADEIRAS") %>% 
          arrange(ANO_ELEICAO,
                  UF,
                  NOME_MUNICIPIO,
                  NUMERO_PARTIDO)
        
        ## Condição para que se comece a calcular os indicadores
        
        if(nrow(indicadores_ano1) > 0 |
           nrow(indicadores_ano2) > 0){
          
          ## Verificando se existem partidos ausentes em uma das eleições
          
          partido_falt_ano2 <- anti_join(indicadores_ano1,
                                         indicadores_ano2, 
                                         by = "NUMERO_PARTIDO")
          
          partido_falt_ano1 <- anti_join(indicadores_ano2,
                                         indicadores_ano1, 
                                         by = "NUMERO_PARTIDO")
          
          ## Equipara o número de partidos entre as eleições e atribui 0
          ## quando não existir em um dos anos de referência
          
          indicadores_ano1 <- partido_falt_ano1 %>% 
            select(-PERC_VOTOS2,
                   -PERC_CADEIRAS2) %>% 
            mutate(ANO_ELEICAO = ano,
                   UF = indicadores_ano1[1, "UF"][[1]],
                   COD_MUN_TSE = municipio,
                   QT_VAGAS = indicadores_ano1[1, "QT_VAGAS"][[1]],
                   QT_VOTOS_VALIDOS = indicadores_ano1[1, "QT_VOTOS_VALIDOS"][[1]],
                   VOT_PART_MUN = 0,
                   TOT_CADEIRAS = 0,
                   PERC_VOTOS = 0,
                   PERC_CADEIRAS = 0,
                   NUM_PART_PARLAMENT = indicadores_ano1[1, "NUM_PART_PARLAMENT"][[1]]) %>% 
            rbind(indicadores_ano1) %>% 
            arrange(ANO_ELEICAO,
                    UF,
                    NOME_MUNICIPIO,
                    NUMERO_PARTIDO)
          
          indicadores_ano2 <- partido_falt_ano2 %>% 
            mutate(PERC_VOTOS2 = 0,
                   PERC_CADEIRAS2 = 0) %>% 
            rbind(indicadores_ano2) %>% 
            ungroup() %>% 
            select(NUMERO_PARTIDO,
                   PERC_VOTOS2,
                   PERC_CADEIRAS2) %>% 
            arrange(NUMERO_PARTIDO)
          
          ## Juntando ambos os bancos
          
          suppressMessages(  
            indicadores_ano1 <- left_join(indicadores_ano1,
                                          indicadores_ano2))
          
          ## Cálculo dos indicadores de 'Volatilidade'
          
          indicadores_ano1 <- indicadores_ano1 %>% 
            mutate(ANO_ELEICAO = ANO_ELEICAO + 4,
                   VOLATILIDADE_ELEITORAL = volatilidade(PERC_VOTOS,
                                                         PERC_VOTOS2),
                   VOLATILIDADE_PARLAMENTAR = volatilidade(PERC_CADEIRAS,
                                                           PERC_CADEIRAS2))
          
          ## Empilha os indicadores calculados em um único banco
          
          temp <- bind_rows(temp,
                            indicadores_ano1)
          
        }
        
      }
    }
  }
  
  return(temp)
  
}

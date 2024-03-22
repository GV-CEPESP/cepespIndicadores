
## Função para calcular os indicadores de 'Reeleição'

indic_reel <- function(candidatos,
                       eleitos,
                       agregacao = c("BR", "UF", "MUN")){
  
  ## Desabilitando as mensagens do 'dplyr'
  
  options(dplyr.summarise.inform = FALSE)
  
  ## Lista temporária onde os dados serão armazenados
  
  temp <- list()
  
  if(agregacao == "BR"){
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(1998, 2018, by = 4)){
      
      cat("Lendo",ano,"\n")
      
      ## Banco com os candidatos da próxima eleição em 
      ## relação ao ano corrente
      
      candidatos_ano2 <- candidatos %>% 
        filter(ANO_ELEICAO == ano + 4) %>% 
        mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                        "DEPUTADO ESTADUAL",
                                        DESCRICAO_CARGO))
      
      ## Bancos com os candidatos eleitos na primeira
      ## eleição de referência
      
      eleitos_ano1 <- eleitos %>% 
        filter(ANO_ELEICAO == ano) %>% 
        mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                        "DEPUTADO ESTADUAL",
                                        DESCRICAO_CARGO))
      
      ## Bancos com os candidatos eleitos na segunda
      ## eleição de referencia
      
      eleitos_ano2 <- eleitos %>% 
        filter(ANO_ELEICAO == ano + 4) %>% 
        mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                        "DEPUTADO ESTADUAL",
                                        DESCRICAO_CARGO))
      
      ## Filtra os candidatos que se reapresentaram na eleição
      ## seguinte ao ano corrente e os que foram reeleitos
      
      eleitos_ano2 <- eleitos_ano2 %>% 
        filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                 eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
      
      ## Dos candidatos que se reapresentaram na eleição seguinte ao
      ## ano de referência, filtra-se somente os eleitos 
      
      indicadores2 <- eleitos_ano2 %>% 
        filter(DESC_SIT_TOT_TURNO == "ELEITO"|
                 DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
                 DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
                 DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
                 DESC_SIT_TOT_TURNO == "MÉDIA" |
                 DESC_SIT_TOT_TURNO == "MEDIA") %>% 
        group_by(ANO_ELEICAO) %>% 
        summarise(REELEITOS = n()) 
      
      ## Calcula o total de candidatos que se reapresentaram na 
      ## eleição seguinte ao ano corrente e junta com o número 
      ## de reeleitos
      
      suppressMessages(
        indicadores1 <- candidatos_ano2 %>% 
          filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                   eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
          mutate(REAPRESENTACAO = n()) %>% 
          select(ANO_ELEICAO,
                 UF,
                 DESCRICAO_CARGO,
                 QT_VAGAS,
                 REAPRESENTACAO) %>% 
          unique() %>% 
          group_by(ANO_ELEICAO,
                   DESCRICAO_CARGO) %>% 
          reframe(QT_VAGAS = sum(QT_VAGAS,
                                 na.rm = TRUE),
                  REAPRESENTACAO = REAPRESENTACAO) %>% 
          unique() %>% 
          left_join(indicadores2))
      
      ## Calcula os indicadores de 'Renovação'
      
      indicadores1 <- indicadores1 %>% 
        mutate(DERROTADOS = REAPRESENTACAO - REELEITOS,
               DESISTENCIA = QT_VAGAS - REAPRESENTACAO,
               REELEICAO = reeleicao(REELEITOS, QT_VAGAS),
               REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS, 
                                                 DERROTADOS),
               RENOVACAO = renovacao(REELEICAO),
               RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
               RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                               QT_VAGAS))
      
      ## Empilha os indicadores calculados no banco criado
      
      temp <- bind_rows(temp, 
                        indicadores1)
    } 
    
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
                   UF == uf)
        
        ## Bancos com os candidatos eleitos na primeira
        ## eleição de referência
        
        eleitos_ano1 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                   UF == uf)
        
        ## Bancos com os candidatos eleitos na segunda
        ## eleição de referencia
        
        eleitos_ano2 <- eleitos %>% 
          filter(ANO_ELEICAO == ano + 4 &
                   UF == uf)
        
        ## Filtra os candidatos que se reapresentaram na eleição
        ## seguinte ao ano corrente e os que foram reeleitos
        
        eleitos_ano2 <- eleitos_ano2 %>% 
          filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                   eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
        
        ## Dos candidatos que se reapresentaram na eleição seguinte ao
        ## ano de referência, filtra-se somente os eleitos 
        
        indicadores2 <- eleitos_ano2 %>% 
          filter(DESC_SIT_TOT_TURNO == "ELEITO"|
                   DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
                   DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
                   DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
                   DESC_SIT_TOT_TURNO == "MÉDIA" |
                   DESC_SIT_TOT_TURNO == "MEDIA") %>% 
          group_by(ANO_ELEICAO,
                   UF) %>% 
          summarise(REELEITOS = n()) 
        
        ## Calcula o total de candidatos que se reapresentaram na 
        ## eleição seguinte ao ano corrente e junta com o número 
        ## de reeleitos
        
        suppressMessages(
          indicadores1 <- candidatos_ano2 %>% 
            filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                     eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   UF,
                   QT_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     UF) %>% 
            reframe(QT_VAGAS = sum(QT_VAGAS,
                                   na.rm = TRUE),
                    REAPRESENTACAO = REAPRESENTACAO) %>% 
            unique() %>% 
            left_join(indicadores2))
        
        ## Calcula os indicadores de 'Renovação'
        
        indicadores1 <- indicadores1 %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS,
                 DESISTENCIA = QT_VAGAS - REAPRESENTACAO,
                 REELEICAO = reeleicao(REELEITOS, QT_VAGAS),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS, 
                                                   DERROTADOS),
                 RENOVACAO = renovacao(REELEICAO),
                 RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                 RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                 QT_VAGAS))
        
        ## Empilha os indicadores calculados no banco criado
        
        temp <- bind_rows(temp, 
                          indicadores1)
        
      }
    } 
    
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
        
        ## Bancos com os candidatos eleitos na primeira
        ## eleição de referência
        
        eleitos_ano1 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == municipios[municipio])
        
        ## Bancos com os candidatos eleitos na segunda
        ## eleição de referencia
        
        eleitos_ano2 <- eleitos %>% 
          filter(ANO_ELEICAO == ano + 4 &
                   COD_MUN_TSE == municipios[municipio])
        
        ## Filtra os candidatos que se reapresentaram na eleição
        ## seguinte ao ano corrente e os que foram reeleitos
        
        eleitos_ano2 <- eleitos_ano2 %>% 
          filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                   eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
        
        ## Dos candidatos que se reapresentaram na eleição seguinte ao
        ## ano de referência, filtra-se somente os eleitos 
        
        indicadores2 <- eleitos_ano2 %>% 
          filter(DESC_SIT_TOT_TURNO == "ELEITO"|
                   DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
                   DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
                   DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
                   DESC_SIT_TOT_TURNO == "MÉDIA" |
                   DESC_SIT_TOT_TURNO == "MEDIA") %>% 
          group_by(ANO_ELEICAO,
                   UF,
                   COD_MUN_TSE) %>% 
          summarise(REELEITOS = n()) 
        
        ## Calcula o total de candidatos que se reapresentaram na 
        ## eleição seguinte ao ano corrente e junta com o número 
        ## de reeleitos
        
        suppressMessages(
          indicadores1 <- candidatos_ano2 %>% 
            filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                     eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   UF,
                   COD_MUN_TSE,
                   NOME_MUNICIPIO,
                   QT_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     UF,
                     COD_MUN_TSE,
                     NOME_MUNICIPIO) %>% 
            reframe(QT_VAGAS = sum(QT_VAGAS,
                                   na.rm = TRUE),
                    REAPRESENTACAO = REAPRESENTACAO) %>% 
            unique() %>% 
            left_join(indicadores2))
        
        ## Calcula os indicadores de 'Renovação'
        
        indicadores1 <- indicadores1 %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS,
                 DESISTENCIA = QT_VAGAS - REAPRESENTACAO,
                 REELEICAO = reeleicao(REELEITOS, QT_VAGAS),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS, 
                                                   DERROTADOS),
                 RENOVACAO = renovacao(REELEICAO),
                 RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                 RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                 QT_VAGAS))
        
        ## Empilha os indicadores calculados no banco criado
        
        temp <- bind_rows(temp, 
                          indicadores1)
        
      }
      
    }
  } 
  
  
  return(temp)
  
} 

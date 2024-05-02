
## Função para calcular os indicadores de 'Reeleição'

indic_reel <- function(candidatos,
                       eleitos,
                       agregacao = c("BR", "UF", "PF_UF", 
                                     "PF_ELEIT_APT", "MUN")){
  
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
                 SIGLA_UF,
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
    
    SIGLA_UFs <- c("AC", "AL", "AP", "AM", "BA", 
             "CE", "DF", "ES", "GO", "MA", 
             "MT", "MS", "MG", "PA", "PB", 
             "PR", "PE", "PI", "RJ", "RN", 
             "RS", "RO", "RR", "SC", "SP", 
             "SE", "TO")
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(1998, 2018, by = 4)){
      for(SIGLA_UF in SIGLA_UFs){
        
        cat("Lendo", ano, SIGLA_UF, "\n")
        
        ## Banco com os candidatos da próxima eleição em 
        ## relação ao ano corrente
        
        candidatos_ano2 <- candidatos %>% 
          filter(ANO_ELEICAO == ano + 4 &
                   SIGLA_UF == SIGLA_UF)
        
        ## Bancos com os candidatos eleitos na primeira
        ## eleição de referência
        
        eleitos_ano1 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                   SIGLA_UF == SIGLA_UF)
        
        ## Bancos com os candidatos eleitos na segunda
        ## eleição de referencia
        
        eleitos_ano2 <- eleitos %>% 
          filter(ANO_ELEICAO == ano + 4 &
                   SIGLA_UF == SIGLA_UF)
        
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
                   SIGLA_UF) %>% 
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
                   SIGLA_UF,
                   QT_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF) %>% 
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
    
  } else if(agregacao == "PF_UF"){
    
    ## Lista de municípios brasileiros
    
    municipios <- unique(pf_mun_cand$COD_MUN_TSE)
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(2008, 2020, by = 4)){
      for(municipio in seq_along(municipios)){
        
        cat("Lendo", ano, municipio, "\n")
        
        ## Banco com os candidatos da eleição em t0
        
        candidatos_t0 <- candidatos %>% 
          filter(ANO_ELEICAO == ano &
                 COD_MUN_TSE == municipios[municipio]) 
        
        # Banco com os candidatos da eleição em t-4
        
        candidatos_t4 <- candidatos %>% 
          filter(ANO_ELEICAO == ano - 4 &
                 COD_MUN_TSE == municipios[municipio]) 
        
        ## Verificando qual candidato foi eleito em t-8
        
        eleitos_t8 <- candidatos %>% 
          filter(ANO_ELEICAO == ano - 8 &
                 COD_MUN_TSE == municipios[municipio]) %>% 
          filter(DESC_SIT_TOT_TURNO == "ELEITO") %>% 
          mutate(ELEITO_T8 = 1) %>% 
          ungroup() %>% 
          select(ID_CEPESP,
                 ELEITO_T8)
        
        ## Verificando qual candidato foi eleito em t-4
        
        eleitos_t4 <- pf_mun_cand %>% 
          filter(ANO_ELEICAO == ano - 4 &
                 COD_MUN_TSE == municipios[municipio]) %>% 
          filter(DESC_SIT_TOT_TURNO == "ELEITO") %>% 
          mutate(ELEITO_T4 = 1) %>% 
          left_join(eleitos_t8) %>% 
          mutate(ELEITO_T8 = ifelse(is.na(ELEITO_T8),
                                    0,
                                    ELEITO_T8),
                 PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                      ELEITO_T8 == 1,
                                      0,
                                      1)) %>% 
          filter(PERMIT_CAND == 1) %>% 
          ungroup() %>% 
          select(ID_CEPESP,
                 ELEITO_T4)
        
        ## Verificando qual candidato foi eleito em t0
        
        eleitos_t0 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                 COD_MUN_TSE == municipios[municipio]) 
        
        ## Filtra os candidatos que podiam e se candidataram na eleição
        ## em t0 e que foram reeleitos 
        
        eleitos_t0 <- eleitos_t0 %>% 
          filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP)
        
        ## Dos candidatos que se reapresentaram na eleição seguinte ao
        ## ano de referência, filtra-se somente os eleitos 
        
        indicadores2 <- eleitos_t0 %>% 
          group_by(ANO_ELEICAO,
                   SIGLA_UF) %>% 
          summarise(REELEITOS = n())
        
        ## Calcula o total de candidatos que se reapresentaram em t0 e 
        ## junta com o número de reeleitos
        
        suppressMessages(
          indicadores1 <- candidatos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     SIGLA_UF) %>% 
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   SIGLA_UF,
                   QT_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF) %>% 
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
    
  } else if(agregacao == "PF_ELEIT_APT"){
    
    ## Lista de municípios brasileiros
    
    municipios <- unique(pf_mun_cand$COD_MUN_TSE)
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(2008, 2020, by = 4)){
      for(municipio in seq_along(municipios)){
        
        cat("Lendo", ano, municipio, "\n")
        
        ## Banco com os candidatos da eleição em t0
        
        candidatos_t0 <- candidatos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == municipios[municipio]) 
        
        # Banco com os candidatos da eleição em t-4
        
        candidatos_t4 <- candidatos %>% 
          filter(ANO_ELEICAO == ano - 4 &
                   COD_MUN_TSE == municipios[municipio]) 
        
        ## Verificando quantos candidatos foram eleitos em t-8
        
        eleitos_t8 <- pf_mun_cand %>% 
          filter(ANO_ELEICAO == ano - 8 &
                   COD_MUN_TSE == municipios[municipio])
        filter(DESC_SIT_TOT_TURNO == "ELEITO") %>% 
          mutate(ELEITO_T8 = 1) %>% 
          select(ID_CEPESP,
                 ELEITO_T8)
        
        ## Verificando quantos candidatos foram eleitos em t-4
        
        eleitos_t4 <- pf_mun_cand %>% 
          filter(ANO_ELEICAO == ano - 4 &
                   COD_MUN_TSE == municipios[municipio])
        filter(DESC_SIT_TOT_TURNO == "ELEITO") %>% 
          mutate(ELEITO_T4 = 1) %>% 
          left_join(eleitos_t8) %>% 
          mutate(PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                        ELEITO_T8 == 1,
                                      0,
                                      1)) %>% 
          filter(PERMIT_CAND == 1) %>% 
          select(ID_CEPESP,
                 ELEITO_T4)
        
        ## Banco com os candidatos eleitos em t0
        
        eleitos_t0 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == municipios[municipio]) 
        
        ## Verificando quantos candidatos podiam se recandidatar
        ## à prefeitura em t0
        
        candidatos_t0 <- candidatos_t0
        
        ## Filtra os candidatos que podiam e se candidataram na eleição
        ## em t0 e que foram reeleitos reeleitos
        
        eleitos_t0 <- eleitos_t0 %>% 
          filter(ID_CEPESP %in% eleitos_t4)
        
        ## Dos candidatos que se reapresentaram na eleição seguinte ao
        ## ano de referência, filtra-se somente os eleitos 
        
        indicadores2 <- eleitos_t0 %>% 
          group_by(ANO_ELEICAO,
                   ELEITORES_APTOS_AGREG) %>% 
          summarise(REELEITOS = n())
        
        ## Calcula o total de candidatos que se reapresentaram em t0 e 
        ## junta com o número de reeleitos
        
        suppressMessages(
          indicadores1 <- candidatos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     ELEITORES_APTOS_AGREG) %>% 
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   ELEITORES_APTOS_AGREG,
                   QT_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     ELEITORES_APTOS_AGREG) %>% 
            reframe(QT_VAGAS = sum(QT_VAGAS,
                                   na.rm = TRUE),
                    REAPRESENTACAO = REAPRESENTACAO) %>% 
            unique() %>% 
            left_join(indicadores2))
        
        ## Calcula os indicadores de 'Reeleição'
        
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
                   SIGLA_UF,
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
                   SIGLA_UF,
                   COD_MUN_TSE,
                   NOME_MUNICIPIO,
                   QT_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
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

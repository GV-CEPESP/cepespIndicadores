
## Calcula o percentual de votos e cadeiras conquistadas pelos partidos 
## em diferentes agregações regionais

indic_perc_votos_cadeiras <- function(data, 
                                agregacao = c("BR", "UF", 
                                              "PF_MUN", "VR_MUN")){
  
  ################################# BR ######################################  
  
  if(agregacao == "BR"){
    
    ## Calculando a quantidade de vagas e a disponibilidade de informação,
    ## bem como o total de cadeiras conquistadas por cada partido
    
    temp <- data %>% 
      filter((DESC_SIT_TOT_TURNO == "ELEITO"|
                DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
                DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
                DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
                DESC_SIT_TOT_TURNO == "MÉDIA" |
                DESC_SIT_TOT_TURNO == "MEDIA") |
               (NOME_CANDIDATO == "SELMA ROSANE SANTOS ARRUDA" &
                  DES_SITUACAO_CANDIDATURA == "CASSADO")) %>% 
      filter(!(ANO_ELEICAO == 1998 &
                 NOME_CANDIDATO %in% c("ARLINDO MELLO",
                                       "MAURÍCIO ROSLINDO FRUET",
                                       "MAURO CLAUDIO TEMOCHKO"))) %>% 
      filter(!(ANO_ELEICAO == 1998 &
                 NOME_CANDIDATO %in% c("ANA CARLA DE CASTRO MENEZES",
                                       "ALFREDO JOSÉ PENHA"))) %>% 
      group_by(ANO_ELEICAO,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO,
               SIGLA_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>%
      ungroup() %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             QTDE_VOTOS_VALIDOS_BR,
             NUMERO_PARTIDO,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(TOT_CADEIRAS,
                                         na.rm = TRUE),
             QTDE_VAGAS = sum(TOT_CADEIRAS,
                              na.rm = TRUE))
    
    ## Verifica todos os partidos que receberam votos na eleição
    
    suppressMessages(
    data <- data %>% 
      ungroup() %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_BR) %>%
      unique() %>% 
      left_join(temp) %>% 
      group_by(ANO_ELEICAO) %>% 
      fill(INFORMACAO_DISPONIVEL,
           .direction = "downup") %>% 
      fill(QTDE_VAGAS,
           .direction = "downup") %>% 
      fill(QTDE_VOTOS_VALIDOS_BR,
           .direction = "downup") %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             QTDE_VAGAS,
             INFORMACAO_DISPONIVEL,
             QTDE_VOTOS_VALIDOS_BR,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_BR,
             TOT_CADEIRAS) %>% 
      unique() %>% 
      mutate(TOT_CADEIRAS = ifelse(is.na(TOT_CADEIRAS),
                                   0,
                                   TOT_CADEIRAS)))
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada 
    ## partido em cada eleição
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_BR/QTDE_VOTOS_VALIDOS_BR,
             PERC_CADEIRAS = TOT_CADEIRAS/INFORMACAO_DISPONIVEL)
    
    #################################### UF ###################################    
    
  } else if(agregacao == "UF"){
    
    ## Calculando a quantidade de vagas e a disponibilidade de informação,
    ## bem como o total de cadeiras conquistadas por cada partido
    
    temp <- data %>% 
      filter((DESC_SIT_TOT_TURNO == "ELEITO"|
                DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
                DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
                DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
                DESC_SIT_TOT_TURNO == "MÉDIA" |
                DESC_SIT_TOT_TURNO == "MEDIA") |
               (NOME_CANDIDATO == "SELMA ROSANE SANTOS ARRUDA" &
                  DES_SITUACAO_CANDIDATURA == "CASSADO")) %>% 
      filter(!(ANO_ELEICAO == 1998 &
                 NOME_CANDIDATO %in% c("ARLINDO MELLO",
                                       "MAURÍCIO ROSLINDO FRUET",
                                       "MAURO CLAUDIO TEMOCHKO"))) %>% 
      filter(!(ANO_ELEICAO == 1998 &
                 NOME_CANDIDATO %in% c("ANA CARLA DE CASTRO MENEZES",
                                       "ALFREDO JOSÉ PENHA"))) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>% 
      select(ANO_ELEICAO,
             SIGLA_UF,
             DESCRICAO_CARGO,
             QTDE_VAGAS,
             QTDE_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(TOT_CADEIRAS,
                                         na.rm = TRUE)) 
    
    ## Verifica todos os partidos que receberam votos na eleição
    
  suppressMessages(
    data <- data %>% 
      ungroup() %>% 
      select(ANO_ELEICAO,
             SIGLA_UF,
             DESCRICAO_CARGO,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_UF) %>%
      unique() %>% 
      left_join(temp) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF) %>% 
      fill(INFORMACAO_DISPONIVEL,
           .direction = "downup") %>% 
      fill(QTDE_VAGAS,
           .direction = "downup") %>% 
      fill(QTDE_VOTOS_VALIDOS,
           .direction = "downup") %>% 
      select(ANO_ELEICAO,
             SIGLA_UF,
             DESCRICAO_CARGO,
             QTDE_VAGAS,
             INFORMACAO_DISPONIVEL,
             QTDE_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_UF,
             TOT_CADEIRAS) %>% 
      unique() %>% 
      mutate(TOT_CADEIRAS = ifelse(is.na(TOT_CADEIRAS),
                                   0,
                                   TOT_CADEIRAS)))
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada 
    ## partido em cada eleição e uf
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_UF/QTDE_VOTOS_VALIDOS,
             PERC_CADEIRAS = TOT_CADEIRAS/INFORMACAO_DISPONIVEL) 
    
    ################################### PF_MUN #################################    
    
  } else if(agregacao == "PF_MUN"){
    
    ## Calculando a quantidade de vagas e a disponibilidade de informação,
    ## bem como o total de cadeiras conquistadas por cada partido
    
    temp <- data %>% 
      mutate(TOT_CADEIRAS = ifelse(DESC_SIT_TOT_TURNO == "ELEITO"|
                                     DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
                                     DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
                                     DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
                                     DESC_SIT_TOT_TURNO == "MÉDIA" |
                                     DESC_SIT_TOT_TURNO == "MEDIA",
                                     1,
                                     NA)) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO) %>% 
      fill(TOT_CADEIRAS, .direction = "downup") %>% 
      filter(NUM_TURNO == 1) %>% 
      ungroup() %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             DESCRICAO_CARGO,
             QTDE_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(TOT_CADEIRAS,
                                         na.rm = TRUE),
             QTDE_VAGAS = sum(TOT_CADEIRAS,
                              na.rm = TRUE)) 
    
    ## Verifica todos os partidos que receberam votos na eleição
    
    suppressMessages(
    data <- data %>% 
      ungroup() %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_MUN) %>%
      unique() %>% 
      left_join(temp) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE) %>% 
      fill(INFORMACAO_DISPONIVEL,
           .direction = "downup") %>% 
      fill(QTDE_VAGAS,
           .direction = "downup") %>% 
      fill(QTDE_VOTOS_VALIDOS,
           .direction = "downup") %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             QTDE_VAGAS,
             INFORMACAO_DISPONIVEL,
             QTDE_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             TOT_CADEIRAS) %>% 
      unique() %>% 
      mutate(TOT_CADEIRAS = ifelse(is.na(TOT_CADEIRAS),
                                   0,
                                   TOT_CADEIRAS)) %>% 
      filter(NUM_TURNO == 1))
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada partido em cada
    ## eleição e município
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_MUN/QTDE_VOTOS_VALIDOS) 
    
    ################################## VR_MUN #################################
    
  } else if(agregacao == "VR_MUN"){
    
    ## Calculando a quantidade de vagas e a disponibilidade de informação,
    ## bem como o total de cadeiras conquistadas por cada partido
    
    temp <- data %>% 
      filter((DESC_SIT_TOT_TURNO == "ELEITO"|
                DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
                DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
                DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
                DESC_SIT_TOT_TURNO == "MÉDIA" |
                DESC_SIT_TOT_TURNO == "MEDIA")) %>% 
      group_by(ANO_ELEICAO,
               NUM_TURNO,
               SIGLA_UF,
               COD_MUN_TSE,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             DESCRICAO_CARGO,
             QTDE_VAGAS,
             QTDE_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(TOT_CADEIRAS,
                                         na.rm = TRUE)) 
    
    ## Verifica todos os partidos que receberam votos na eleição
    
    suppressMessages(
    data <- data %>% 
      ungroup() %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_MUN) %>%
      unique() %>% 
      left_join(temp) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE) %>% 
      fill(INFORMACAO_DISPONIVEL,
           .direction = "downup") %>% 
      fill(QTDE_VAGAS,
           .direction = "downup") %>% 
      fill(QTDE_VOTOS_VALIDOS,
           .direction = "downup") %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             QTDE_VAGAS,
             INFORMACAO_DISPONIVEL,
             QTDE_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             TOT_CADEIRAS) %>% 
      unique() %>% 
      mutate(TOT_CADEIRAS = ifelse(is.na(TOT_CADEIRAS),
                                   0,
                                   TOT_CADEIRAS)))
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada partido em 
    ## cada eleição e município
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_MUN/QTDE_VOTOS_VALIDOS,
             PERC_CADEIRAS = TOT_CADEIRAS/INFORMACAO_DISPONIVEL)
    
  }
}

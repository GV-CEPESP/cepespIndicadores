
## Calcula o percentual de votos e cadeiras conquistadas pelos partidos 
## em diferentes agregações regionais

indic_cadeiras_conq <- function(data, 
                                agregacao = c("BR", "UF", 
                                              "PF_MUN", "VR_MUN")){
  
################################# BR ######################################  
  
  if(agregacao == "BR"){
    
    ## Filtra os candidatos que foram eleitos
    
    data <- data %>% 
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
                                       "ALFREDO JOSÉ PENHA")))
    
    ## Soma o total de cadeiras conquistadas pelos partidos em cada eleição e
    ## organiza os dados
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO,
               SIGLA_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             QTDE_VOTOS_VALIDOS_BR,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_BR,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(TOT_CADEIRAS,
                                         na.rm = TRUE),
             QTDE_VAGAS = sum(TOT_CADEIRAS,
                            na.rm = TRUE)) %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             QTDE_VAGAS,
             INFORMACAO_DISPONIVEL,
             QTDE_VOTOS_VALIDOS_BR,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_BR,
             TOT_CADEIRAS) %>% 
      unique()
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e
    ## o número de partidos parlamentares em cada eleição
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_BR/QTDE_VOTOS_VALIDOS_BR,
             PERC_CADEIRAS = TOT_CADEIRAS/INFORMACAO_DISPONIVEL) %>% 
      group_by(ANO_ELEICAO) %>% 
      mutate(NUM_PART_PARLAMENT = n())
    
#################################### UF ###################################    
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
               DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
               DESC_SIT_TOT_TURNO == "MÉDIA" |
               DESC_SIT_TOT_TURNO == "MEDIA") %>% 
      filter(!(ANO_ELEICAO == 1998 &
                 NOME_CANDIDATO %in% c("ARLINDO MELLO",
                                       "MAURÍCIO ROSLINDO FRUET",
                                       "MAURO CLAUDIO TEMOCHKO"))) %>% 
      filter(!(ANO_ELEICAO == 1998 &
                 NOME_CANDIDATO %in% c("ANA CARLA DE CASTRO MENEZES",
                                       "ALFREDO JOSÉ PENHA")))
    
    ## Soma o total de cadeiras conquistadas pelos partidos em cada ano e uf e
    ## organiza os dados
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO,
               SIGLA_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>% 
      select(ANO_ELEICAO,
             SIGLA_UF,
             DESCRICAO_CARGO,
             QTDE_VAGAS,
             QTDE_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_UF,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(TOT_CADEIRAS,
                                         na.rm = TRUE)) %>% 
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
      unique()
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e uf, bem como o
    ## número de partidos parlamentares em cada eleição
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_UF/QTDE_VOTOS_VALIDOS,
             PERC_CADEIRAS = TOT_CADEIRAS/INFORMACAO_DISPONIVEL) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF) %>% 
      mutate(NUM_PART_PARLAMENT = n())
    
################################### PF_MUN #################################    
    
  } else if(agregacao == "PF_MUN"){
    
    ## Filtra os candidatos que foram eleitos
    
    data <- data %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
               DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
               DESC_SIT_TOT_TURNO == "MÉDIA" |
               DESC_SIT_TOT_TURNO == "MEDIA")
    
    ## Soma o total de cadeiras conquistadas pelos partidos em cada ano e uf e
    ## organiza os dados
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               NUM_TURNO,
               SIGLA_UF,
               COD_MUN_TSE,
               COD_MUN_IBGE,
               NOME_MUNICIPIO,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO,
               SIGLA_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             QTDE_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(TOT_CADEIRAS,
                                         na.rm = TRUE),
             QTDE_VAGAS = sum(TOT_CADEIRAS,
                              na.rm = TRUE)) %>% 
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
      unique()
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e uf, bem como o
    ## número de partidos parlamentares em cada eleição
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_MUN/QTDE_VOTOS_VALIDOS,
             PERC_CADEIRAS = TOT_CADEIRAS/INFORMACAO_DISPONIVEL) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE) %>% 
      mutate(NUM_PART_PARLAMENT = n())
    
################################## VR_MUN #################################
    
  } else if(agregacao == "VR_MUN"){
    
    ## Filtra os candidatos que foram eleitos
    
    data <- data %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
               DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
               DESC_SIT_TOT_TURNO == "MÉDIA" |
               DESC_SIT_TOT_TURNO == "MEDIA")
    
    ## Soma o total de cadeiras conquistadas pelos partidos em cada ano e uf e
    ## organiza os dados
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               NUM_TURNO,
               SIGLA_UF,
               COD_MUN_TSE,
               COD_MUN_IBGE,
               NOME_MUNICIPIO,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO,
               SIGLA_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             QTDE_VAGAS,
             QTDE_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(TOT_CADEIRAS,
                                         na.rm = TRUE)) %>% 
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
      unique()
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e uf, bem como o
    ## número de partidos parlamentares em cada eleição
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_MUN/QTDE_VOTOS_VALIDOS,
             PERC_CADEIRAS = TOT_CADEIRAS/INFORMACAO_DISPONIVEL) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE) %>% 
      mutate(NUM_PART_PARLAMENT = n())
    
  }
}

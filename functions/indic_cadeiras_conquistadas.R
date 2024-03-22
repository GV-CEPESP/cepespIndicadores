
## Calcula o percentual de votos e cadeiras conquistadas pelos partidos 
## em diferentes agregações regionais

indic_cadeiras_conq <- function(data, 
                                agregacao = c("BR", "UF", "MUN")){
  
  if(agregacao == "BR"){
    
    ## Filtra os candidatos que foram eleitos
    
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
             QT_VOTOS_VALIDOS_BR,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_BR,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO) %>% 
      mutate(QT_VAGAS = sum(TOT_CADEIRAS,
                            na.rm = TRUE)) %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             QT_VAGAS,
             QT_VOTOS_VALIDOS_BR,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_BR,
             TOT_CADEIRAS) %>% 
      unique()
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e
    ## o número de partidos parlamentares em cada eleição
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_BR/QT_VOTOS_VALIDOS_BR,
             PERC_CADEIRAS = TOT_CADEIRAS/QT_VAGAS) %>% 
      group_by(ANO_ELEICAO) %>% 
      mutate(NUM_PART_PARLAMENT = n())
    
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
               UF,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO,
               SIGLA_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>% 
      select(ANO_ELEICAO,
             UF,
             DESCRICAO_CARGO,
             QT_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_UF,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO,
               UF) %>% 
      mutate(QT_VAGAS = sum(TOT_CADEIRAS,
                            na.rm = TRUE)) %>% 
      select(ANO_ELEICAO,
             UF,
             DESCRICAO_CARGO,
             QT_VAGAS,
             QT_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_UF,
             TOT_CADEIRAS) %>% 
      unique()
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e uf, bem como o
    ## número de partidos parlamentares em cada eleição
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_UF/QT_VOTOS_VALIDOS,
             PERC_CADEIRAS = TOT_CADEIRAS/QT_VAGAS) %>% 
      group_by(ANO_ELEICAO,
               UF) %>% 
      mutate(NUM_PART_PARLAMENT = n())
    
  } else if(agregacao == "MUN"){
    
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
               UF,
               COD_MUN_TSE,
               NOME_MUNICIPIO,
               DESCRICAO_CARGO,
               NUMERO_PARTIDO,
               SIGLA_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             UF,
             COD_MUN_TSE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             QT_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO,
               UF,
               NOME_MUNICIPIO) %>% 
      mutate(QT_VAGAS = sum(TOT_CADEIRAS,
                            na.rm = TRUE)) %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             UF,
             COD_MUN_TSE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             QT_VAGAS,
             QT_VOTOS_VALIDOS,
             NUMERO_PARTIDO,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             TOT_CADEIRAS) %>% 
      unique()
    
    ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e uf, bem como o
    ## número de partidos parlamentares em cada eleição
    
    data <- data %>% 
      mutate(PERC_VOTOS = VOT_PART_MUN/QT_VOTOS_VALIDOS,
             PERC_CADEIRAS = TOT_CADEIRAS/QT_VAGAS) %>% 
      group_by(ANO_ELEICAO,
               UF,
               NOME_MUNICIPIO) %>% 
      mutate(NUM_PART_PARLAMENT = n())
    
  }
}

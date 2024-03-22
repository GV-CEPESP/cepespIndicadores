
## Calcula os quocientes eleitoral e partidário para cada um dos
## cargos proporcionais em disputa

indic_distcad <- function(data,
                          agregacao = c("BR", "UF", "MUN")){
  
  
  if(agregacao == "BR"){
    
    ## Calcula o quociente eleitoral e partidário para os 
    ## Deputados Federais a nível federal
    
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
                                       "ALFREDO JOSÉ PENHA"))) %>% 
      mutate(QT_VAGAS = 513,
             QUOCIENTE_ELEITORAL = quoc_eleitoral(QT_VOTOS_VALIDOS_BR,
                                                  QT_VAGAS)) %>% 
      group_by(ANO_ELEICAO,
               DESCRICAO_CARGO,
               SIGLA_PARTIDO) %>% 
      mutate(QUOCIENTE_PARTIDARIO = n()) %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             QT_VOTOS_VALIDOS_BR,
             QT_VAGAS,
             SIGLA_PARTIDO,
             VOT_PART_BR,
             QUOCIENTE_ELEITORAL,
             QUOCIENTE_PARTIDARIO) %>% 
      unique()
  
  
  } else if(agregacao == "UF"){
    
    ## Calcula o quociente eleitoral e partidário para os 
    ## Deputados Federais e Deputados Estaduais anível estadual
    
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
                                       "ALFREDO JOSÉ PENHA"))) %>% 
      mutate(QUOCIENTE_ELEITORAL = quoc_eleitoral(QT_VOTOS_VALIDOS,
                                                  QT_VAGAS)) %>% 
      group_by(ANO_ELEICAO,
               UF,
               DESCRICAO_CARGO,
               SIGLA_PARTIDO) %>% 
      mutate(QUOCIENTE_PARTIDARIO = n()) %>% 
      select(ANO_ELEICAO,
             UF,
             DESCRICAO_CARGO,
             QT_VOTOS_VALIDOS,
             QT_VAGAS,
             SIGLA_PARTIDO,
             VOT_PART_UF,
             QUOCIENTE_ELEITORAL,
             QUOCIENTE_PARTIDARIO) %>% 
      unique()
    
  } else if(agregacao == "MUN"){
    
    ## Calcula o quociente eleitoral e partidário para os 
    ## Vereadores a nível municipal
    
    data <- data %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
               DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
               DESC_SIT_TOT_TURNO == "MÉDIA" |
               DESC_SIT_TOT_TURNO == "MEDIA") %>% 
      mutate(QUOCIENTE_ELEITORAL = quoc_eleitoral(QT_VOTOS_VALIDOS,
                                                  QT_VAGAS)) %>% 
      group_by(ANO_ELEICAO,
               UF,
               COD_MUN_TSE,
               NOME_MUNICIPIO,
               DESCRICAO_CARGO,
               SIGLA_PARTIDO) %>% 
      mutate(QUOCIENTE_PARTIDARIO = n()) %>% 
      select(ANO_ELEICAO,
             UF,
             COD_MUN_TSE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             QT_VOTOS_VALIDOS,
             QT_VAGAS,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             QUOCIENTE_ELEITORAL,
             QUOCIENTE_PARTIDARIO) %>% 
      unique()
    
  }
  
  return(data)
  
}

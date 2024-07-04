
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
      mutate(QTDE_VAGAS = 513,
             QUOCIENTE_ELEITORAL = quoc_eleitoral(QTDE_VOTOS_VALIDOS_BR,
                                                  QTDE_VAGAS)) %>% 
      group_by(ANO_ELEICAO,
               DESCRICAO_CARGO,
               SIGLA_PARTIDO) %>% 
      mutate(QUOCIENTE_PARTIDARIO = n()) %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             QTDE_VOTOS_VALIDOS_BR,
             QTDE_VAGAS,
             SIGLA_PARTIDO,
             VOT_PART_BR,
             QUOCIENTE_ELEITORAL,
             QUOCIENTE_PARTIDARIO) %>% 
      unique() %>% 
      group_by(ANO_ELEICAO) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(QUOCIENTE_PARTIDARIO,
                                         na.rm = TRUE))
  
  
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
      mutate(QUOCIENTE_ELEITORAL = quoc_eleitoral(QTDE_VOTOS_VALIDOS,
                                                  QTDE_VAGAS)) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               DESCRICAO_CARGO,
               SIGLA_PARTIDO) %>% 
      mutate(QUOCIENTE_PARTIDARIO = n()) %>% 
      select(ANO_ELEICAO,
             SIGLA_UF,
             DESCRICAO_CARGO,
             QTDE_VOTOS_VALIDOS,
             QTDE_VAGAS,
             SIGLA_PARTIDO,
             VOT_PART_UF,
             QUOCIENTE_ELEITORAL,
             QUOCIENTE_PARTIDARIO) %>% 
      unique() %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(QUOCIENTE_PARTIDARIO,
                                         na.rm = TRUE))
    
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
      mutate(QUOCIENTE_ELEITORAL = quoc_eleitoral(QTDE_VOTOS_VALIDOS,
                                                  QTDE_VAGAS)) %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE,
               COD_MUN_IBGE,
               NOME_MUNICIPIO,
               DESCRICAO_CARGO,
               SIGLA_PARTIDO) %>% 
      mutate(QUOCIENTE_PARTIDARIO = n()) %>% 
      select(ANO_ELEICAO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             QTDE_VOTOS_VALIDOS,
             QTDE_VAGAS,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             QUOCIENTE_ELEITORAL,
             QUOCIENTE_PARTIDARIO) %>% 
      unique() %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF,
               COD_MUN_TSE) %>% 
      mutate(INFORMACAO_DISPONIVEL = sum(QUOCIENTE_PARTIDARIO,
                                         na.rm = TRUE))
    
    ## Salva os municípios com informação incompleta em um novo data frame
    
    com_erro <- data %>% 
      filter(INFORMACAO_DISPONIVEL != QTDE_VAGAS)
    
    ## Exporta o resultado para conferência posterior
    
    saveRDS(com_erro,
            "data/output/quoc_eleitoral_partidario_vereadores_municipos_com_erro.rds")
    
    ## Remove esses casos da base final
    
    data <- data %>% 
      filter(INFORMACAO_DISPONIVEL == QTDE_VAGAS)
    
    
  }
  
  return(data)
  
}

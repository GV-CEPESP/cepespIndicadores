
## Calcula o total de eleitos em cargos proporcionais por
## ano e agregação regional

eleitos <- function(data,
                    agregacao = c("UF", 
                                  "MUN_PF",
                                  "MUN_VR")){
  
  ## Calcula o total de eleitos em cargos proporcionais por
  ## ano e UF
  
  if(agregacao == "UF"){
    
    data <- data %>% 
      select(ANO_ELEICAO, 
             NUM_TURNO,
             SIGLA_UF,
             CODIGO_CARGO,
             DESCRICAO_CARGO,
             ID_CEPESP,
             NOME_CANDIDATO,
             DATA_NASCIMENTO,
             CPF_CANDIDATO,
             NUM_TITULO_ELEITORAL_CANDIDATO,
             SIGLA_PARTIDO,
             DESC_SIT_TOT_TURNO) %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
               DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
               DESC_SIT_TOT_TURNO == "MÉDIA" |
               DESC_SIT_TOT_TURNO == "MEDIA") %>% 
      filter(!(ANO_ELEICAO == 1998 &
               NOME_CANDIDATO %in% c("ARLINDO MELLO",
                                     "MAURÍCIO ROSLINDO FRUET",
                                     "MAURO CLAUDIO TEMOCHKO")))
    
    ## Verifica quais candidatos foram eleitos nas eleições majoritárias
    ## municipais por ano e município
    
  } else if(agregacao == "MUN_PF"){
    
    data <- data %>% 
      select(ANO_ELEICAO, 
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             NOME_MUNICIPIO,
             CODIGO_CARGO,
             DESCRICAO_CARGO,
             ID_CEPESP,
             NOME_CANDIDATO,
             DATA_NASCIMENTO,
             CPF_CANDIDATO,
             NUM_TITULO_ELEITORAL_CANDIDATO,
             SIGLA_PARTIDO,
             DESC_SIT_TOT_TURNO) %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO")
    
    ## Calcula o total de eleitos em cargos proporcionais por
    ## ano e município
    
  } else if(agregacao == "MUN_VR"){
    
    data <- data %>% 
      select(ANO_ELEICAO, 
             NUM_TURNO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             NOME_MUNICIPIO,
             CODIGO_CARGO,
             DESCRICAO_CARGO,
             ID_CEPESP,
             NOME_CANDIDATO,
             DATA_NASCIMENTO,
             CPF_CANDIDATO,
             NUM_TITULO_ELEITORAL_CANDIDATO,
             SIGLA_PARTIDO,
             DESC_SIT_TOT_TURNO) %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
             DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
             DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
             DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
             DESC_SIT_TOT_TURNO == "MÉDIA" |
             DESC_SIT_TOT_TURNO == "MEDIA") 
    
  }
}

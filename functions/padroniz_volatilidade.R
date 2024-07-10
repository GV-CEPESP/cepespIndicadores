
## Função para padronização dos indicadores de 'Volatilidade'

padroniz_volat <- function(data,
                           agregacao = c("BR", "UF", 
                                         "PF_MUN", "VR_MUN")){
  
####################################### BR #####################################
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      mutate(INFORMACAO_DISPONIVEL = ifelse(VOLATILIDADE_ELEITORAL == 0 &
                                              VOLATILIDADE_PARLAMENTAR == 0 &
                                              PERC_VOTOS2 == 0 &
                                              PERC_CADEIRAS2 == 0,
                                            0,
                                            INFORMACAO_DISPONIVEL),
             VOLATILIDADE_ELEITORAL = ifelse(INFORMACAO_DISPONIVEL == 0,
                                             NA,
                                             VOLATILIDADE_ELEITORAL),
             VOLATILIDADE_PARLAMENTAR = ifelse(INFORMACAO_DISPONIVEL == 0,
                                               NA,
                                               VOLATILIDADE_PARLAMENTAR)) %>% 
      mutate(VOLATILIDADE_ELEITORAL = round(VOLATILIDADE_ELEITORAL,
                                            digits = 2),
             VOLATILIDADE_PARLAMENTAR = round(VOLATILIDADE_PARLAMENTAR,
                                              digits = 2),
             `Agregação regional` = "Brasil",
             UF = "BR",
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)` = NA,
             `Município` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO),
             Turno = "1") %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Volatilidade eleitoral" = "VOLATILIDADE_ELEITORAL",
             "Volatilidade parlamentar" = "VOLATILIDADE_PARLAMENTAR") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Volatilidade eleitoral`,
             `Volatilidade parlamentar`) %>% 
      arrange(`Ano da eleição`,
              Cargo) %>% 
      unique()
    
######################################### UF ###################################
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      mutate(INFORMACAO_DISPONIVEL = ifelse(VOLATILIDADE_ELEITORAL == 0 &
                                              VOLATILIDADE_PARLAMENTAR == 0 &
                                              PERC_VOTOS2 == 0 &
                                              PERC_CADEIRAS2 == 0,
                                            0,
                                            INFORMACAO_DISPONIVEL),
             VOLATILIDADE_ELEITORAL = ifelse(INFORMACAO_DISPONIVEL == 0,
                                             NA,
                                             VOLATILIDADE_ELEITORAL),
             VOLATILIDADE_PARLAMENTAR = ifelse(INFORMACAO_DISPONIVEL == 0,
                                               NA,
                                               VOLATILIDADE_PARLAMENTAR)) %>% 
      mutate(VOLATILIDADE_ELEITORAL = round(VOLATILIDADE_ELEITORAL,
                                            digits = 2),
             VOLATILIDADE_PARLAMENTAR = round(VOLATILIDADE_PARLAMENTAR,
                                              digits = 2),
             `Agregação regional` = "UF",
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)` = NA,
             `Município` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO),
             Turno = "1") %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Volatilidade eleitoral" = "VOLATILIDADE_ELEITORAL",
             "Volatilidade parlamentar" = "VOLATILIDADE_PARLAMENTAR") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Volatilidade eleitoral`,
             `Volatilidade parlamentar`) %>%
      arrange(`Ano da eleição`,
              UF) %>% 
      unique()
    
##################################### PF_MUN ###################################
    
  } else if(agregacao == "PF_MUN"){
    
    data <- data %>% 
      mutate(INFORMACAO_DISPONIVEL = ifelse(VOLATILIDADE_ELEITORAL == 0 &
                                            PERC_VOTOS2 == 0,
                                            0,
                                            INFORMACAO_DISPONIVEL),
             VOLATILIDADE_ELEITORAL = ifelse(INFORMACAO_DISPONIVEL == 0,
                                             NA,
                                             VOLATILIDADE_ELEITORAL),
             VOLATILIDADE_PARLAMENTAR = NA) %>% 
      mutate(VOLATILIDADE_ELEITORAL = round(VOLATILIDADE_ELEITORAL,
                                            digits = 2),
             `Agregação regional` = "Município",
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Código do município (TSE)" = "COD_MUN_TSE",
             "Código do município (IBGE)" = "COD_MUN_IBGE",
             "Município" = "NOME_MUNICIPIO",
             "Volatilidade eleitoral" = "VOLATILIDADE_ELEITORAL",
             "Volatilidade parlamentar" = "VOLATILIDADE_PARLAMENTAR") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Volatilidade eleitoral`,
             `Volatilidade parlamentar`) %>%
      arrange(`Ano da eleição`,
              UF,
              `Município`) %>% 
      unique()
    
#################################### VR_MUN ############################    
    
  } else if(agregacao == "VR_MUN"){
    
    data <- data %>% 
      mutate(INFORMACAO_DISPONIVEL = ifelse(VOLATILIDADE_ELEITORAL == 0 &
                                              VOLATILIDADE_PARLAMENTAR == 0 &
                                              PERC_VOTOS2 == 0 &
                                              PERC_CADEIRAS2 == 0,
                                            0,
                                            INFORMACAO_DISPONIVEL),
             VOLATILIDADE_ELEITORAL = ifelse(INFORMACAO_DISPONIVEL == 0,
                                             NA,
                                             VOLATILIDADE_ELEITORAL),
             VOLATILIDADE_PARLAMENTAR = ifelse(INFORMACAO_DISPONIVEL == 0,
                                               NA,
                                               VOLATILIDADE_PARLAMENTAR)) %>% 
      mutate(VOLATILIDADE_ELEITORAL = round(VOLATILIDADE_ELEITORAL,
                                            digits = 2),
             VOLATILIDADE_PARLAMENTAR = round(VOLATILIDADE_PARLAMENTAR,
                                              digits = 2),
             `Agregação regional` = "Município",
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Código do município (TSE)" = "COD_MUN_TSE",
             "Código do município (IBGE)" = "COD_MUN_IBGE",
             "Município" = "NOME_MUNICIPIO",
             "Volatilidade eleitoral" = "VOLATILIDADE_ELEITORAL",
             "Volatilidade parlamentar" = "VOLATILIDADE_PARLAMENTAR") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Volatilidade eleitoral`,
             `Volatilidade parlamentar`) %>%
      arrange(`Ano da eleição`,
              UF,
              `Município`) %>% 
      unique()
    
  }
  
}

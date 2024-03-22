
## Função para padronização dos indicadores de 'Volatilidade'

padroniz_volat <- function(data,
                           agregacao = c("BR", "UF", "MUN")){
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      mutate(VOLATILIDADE_ELEITORAL = round(VOLATILIDADE_ELEITORAL,
                                            digits = 2),
             VOLATILIDADE_PARLAMENTAR = round(VOLATILIDADE_PARLAMENTAR,
                                              digits = 2),
             `Agregação regional` = "Brasil",
             UF = "BR",
             `Código do município` = NA,
             `Nome do município` = NA,
             `Quantidade agregada de eleitores aptos` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO),
             Turno = 1) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Volatilidade eleitoral" = "VOLATILIDADE_ELEITORAL",
             "Volatilidade parlamentar" = "VOLATILIDADE_PARLAMENTAR") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Quantidade agregada de eleitores aptos`,
             `Volatilidade eleitoral`,
             `Volatilidade parlamentar`) %>% 
      arrange(`Ano da eleição`,
              Cargo) %>% 
      unique()
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      mutate(VOLATILIDADE_ELEITORAL = round(VOLATILIDADE_ELEITORAL,
                                            digits = 2),
             VOLATILIDADE_PARLAMENTAR = round(VOLATILIDADE_PARLAMENTAR,
                                              digits = 2),
             `Agregação regional` = "UF",
             `Código do município` = NA,
             `Nome do município` = NA,
             `Quantidade agregada de eleitores aptos` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO),
             Turno = 1) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Volatilidade eleitoral" = "VOLATILIDADE_ELEITORAL",
             "Volatilidade parlamentar" = "VOLATILIDADE_PARLAMENTAR") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Quantidade agregada de eleitores aptos`,
             `Volatilidade eleitoral`,
             `Volatilidade parlamentar`) %>%
      arrange(`Ano da eleição`,
              UF) %>% 
      unique()
    
  } else if(agregacao == "MUN"){
    
    data <- data %>% 
      left_join(eleitores_aptos) %>% 
      mutate(VOLATILIDADE_ELEITORAL = round(VOLATILIDADE_ELEITORAL,
                                            digits = 2),
             VOLATILIDADE_PARLAMENTAR = round(VOLATILIDADE_PARLAMENTAR,
                                              digits = 2),
             `Agregação regional` = "Município",
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "Cargo" = "DESCRICAO_CARGO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Quantidade agregada de eleitores aptos" = "AGREG_ELEITORES_APTOS",
             "Volatilidade eleitoral" = "VOLATILIDADE_ELEITORAL",
             "Volatilidade parlamentar" = "VOLATILIDADE_PARLAMENTAR") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Quantidade agregada de eleitores aptos`,
             `Volatilidade eleitoral`,
             `Volatilidade parlamentar`) %>%
      arrange(`Ano da eleição`,
              UF,
              `Nome do município`) %>% 
      unique()
    
  }
}

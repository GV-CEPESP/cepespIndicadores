
## Função para padronização dos indicadores de 'Reeleição'

padroniz_reel <- function(data,
                          agregacao = c("BR", "UF", "MUN")){
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      mutate(RECANDIDATURAS = round(RECANDIDATURAS,
                                    digits = 2),
             REELEICAO = round(REELEICAO,
                               digits = 2),
             REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                       digits = 2),
             RENOVACAO = round(RENOVACAO,
                               digits = 2),
             RENOVACAO_LIQUIDA = round(RENOVACAO_LIQUIDA,
                                       digits = 2),
             `Agregação regional` = "Brasil",
             UF = "BR",
             `Código do município` = NA,
             `Nome do município` = NA,
             `Quantidade agregada de eleitores aptos` = NA,
             DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                      "DEPUTADO ESTADUAL",
                                      DESCRICAO_CARGO),
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Reapresentação" = "REAPRESENTACAO",
             "Reeleitos" = "REELEITOS",
             "Recandidaturas" = "RECANDIDATURAS",
             "Reeleição" = "REELEICAO",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação" = "RENOVACAO",
             "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Quantidade agregada de eleitores aptos`,
             `Reapresentação`,
             `Reeleitos`,
             Recandidaturas,
             `Reeleição`,
             `Reeleição líquida`,
             `Renovação`,
             `Renovação líquida`) %>% 
      arrange(`Ano da eleição`)
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      mutate(REELEICAO = round(REELEICAO,
                               digits = 2),
             REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                       digits = 2),
             RENOVACAO = round(RENOVACAO,
                               digits = 2),
             RENOVACAO_LIQUIDA = round(RENOVACAO_LIQUIDA,
                                       digits = 2),
             RECANDIDATURAS = round(RECANDIDATURAS,
                                    digits = 2),
             `Agregação regional` = "UF",
             `Código do município` = NA,
             `Nome do município` = NA,
             `Quantidade agregada de eleitores aptos` = NA,
             DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                      "DEPUTADO ESTADUAL",
                                      DESCRICAO_CARGO),
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Reapresentação" = "REAPRESENTACAO",
             "Reeleitos" = "REELEITOS",
             "Recandidaturas" = "RECANDIDATURAS",
             "Reeleição" = "REELEICAO",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação" = "RENOVACAO",
             "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Quantidade agregada de eleitores aptos`,
             `Reapresentação`,
             `Reeleitos`,
             Recandidaturas,
             `Reeleição`,
             `Reeleição líquida`,
             `Renovação`,
             `Renovação líquida`) %>%
      arrange(`Ano da eleição`,
              UF)
    
  } else if(agregacao == "MUN"){
    
    data <- data %>% 
      ungroup() %>% 
      left_join(eleitores_aptos) %>% 
      unique() %>% 
      mutate(REELEICAO = round(REELEICAO,
                               digits = 2),
             REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                       digits = 2),
             RENOVACAO = round(RENOVACAO,
                               digits = 2),
             RENOVACAO_LIQUIDA = round(RENOVACAO_LIQUIDA,
                                       digits = 2),
             RECANDIDATURAS = round(RECANDIDATURAS,
                                    digits = 2),
             `Agregação regional` = "Município",
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Quantidade agregada de eleitores aptos" = "AGREG_ELEITORES_APTOS",
             "Reapresentação" = "REAPRESENTACAO",
             "Reeleitos" = "REELEITOS",
             "Recandidaturas" = "RECANDIDATURAS",
             "Reeleição" = "REELEICAO",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação" = "RENOVACAO",
             "Renovação líquida" = "RENOVACAO_LIQUIDA",
             "Recandidaturas" = "RECANDIDATURAS") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Quantidade agregada de eleitores aptos`,
             `Reapresentação`,
             `Reeleitos`,
             Recandidaturas,
             `Reeleição`,
             `Reeleição líquida`,
             `Renovação`,
             `Renovação líquida`) %>%
      arrange(`Ano da eleição`,
              UF,
              `Nome do município`)
    
  }
  
}

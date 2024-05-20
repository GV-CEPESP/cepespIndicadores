
## Função para padronização dos indicadores de 'Reeleição bruta'

padroniz_reel <- function(data,
                          agregacao = c("BR", 
                                        "UF", 
                                        "PF_UF",
                                        "PF_ELEIT_APT",
                                        "MUN")){

#################################### BR ########################################
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      mutate(RECANDIDATURAS = round(RECANDIDATURAS,
                                    digits = 2),
             REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                     digits = 2),
             REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                       digits = 2),
             RENOVACAO_BRUTA = round(RENOVACAO_BRUTA,
                                     digits = 2),
             RENOVACAO_LIQUIDA = round(RENOVACAO_LIQUIDA,
                                       digits = 2),
             `Agregação regional` = "Brasil",
              UF = "BR",
             `Código TSE do município` = NA,
             `Código IBGE do município`= NA,
             `Nome do município` = NA,
             `Faixa de eleitores aptos` = NA,
             `Quantidade de municípios na agregação` = NA,
             `Municípios elegíveis para reapresentação` = NA,
             `Taxa de prefeitos elegíveis à reeleição` = NA,
             `Taxa de prefeitos que se recandidataram` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Reapresentação" = "REAPRESENTACAO",
             "Reeleitos" = "REELEITOS",
             "Recandidaturas" = "RECANDIDATURAS",
             "Reeleição bruta" = "REELEICAO_BRUTA",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação bruta" = "RENOVACAO_BRUTA",
             "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
              UF,
             `Código TSE do município`,
             `Código IBGE do município`,
             `Nome do município`,
             Cargo,
             `Faixa de eleitores aptos`,
             `Quantidade de municípios na agregação`,
             `Cadeiras disponíveis`,
             `Municípios elegíveis para reapresentação`,
             `Reapresentação`,
             `Reeleitos`,
             `Taxa de prefeitos elegíveis à reeleição`,
             `Taxa de prefeitos que se recandidataram`,
             `Reeleição bruta`,
             `Reeleição líquida`,
             `Renovação bruta`,
             `Renovação líquida`,
             Recandidaturas) %>% 
      arrange(`Ano da eleição`)
    
#################################### UF ########################################
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      mutate(REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                     digits = 2),
             REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                       digits = 2),
             RENOVACAO_BRUTA = round(RENOVACAO_BRUTA,
                                     digits = 2),
             RENOVACAO_LIQUIDA = round(RENOVACAO_LIQUIDA,
                                       digits = 2),
             RECANDIDATURAS = round(RECANDIDATURAS,
                                    digits = 2),
             `Agregação regional` = "UF",
             `Código TSE do município` = NA,
             `Código IBGE do município`= NA,
             `Nome do município` = NA,
             `Faixa de eleitores aptos` = NA,
             `Faixa de eleitores aptos` = NA,
             `Quantidade de municípios na agregação` = NA,
             `Municípios elegíveis para reapresentação` = NA,
             `Taxa de prefeitos elegíveis à reeleição` = NA,
             `Taxa de prefeitos que se recandidataram` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Reapresentação" = "REAPRESENTACAO",
             "Reeleitos" = "REELEITOS",
             "Recandidaturas" = "RECANDIDATURAS",
             "Reeleição bruta" = "REELEICAO_BRUTA",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação bruta" = "RENOVACAO_BRUTA",
             "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código TSE do município`,
             `Código IBGE do município`,
             `Nome do município`,
             Cargo,
             `Faixa de eleitores aptos`,
             `Quantidade de municípios na agregação`,
             `Cadeiras disponíveis`,
             `Municípios elegíveis para reapresentação`,
             `Reapresentação`,
             `Reeleitos`,
             `Taxa de prefeitos elegíveis à reeleição`,
             `Taxa de prefeitos que se recandidataram`,
             `Reeleição bruta`,
             `Reeleição líquida`,
             `Renovação bruta`,
             `Renovação líquida`,
             Recandidaturas) %>%
      arrange(`Ano da eleição`,
              UF)
    
#################################### PF_UF #####################################

  } else if(agregacao == "PF_UF"){
    
    data <- data %>% 
      ungroup() %>% 
      unique() %>% 
      mutate(TX_PREFEITOS_PERMIT_REEL = round(TX_PREFEITOS_PERMIT_REEL,
                                              digits = 2) * 100,
             TX_PREFEITOS_RECAND = round(TX_PREFEITOS_RECAND,
                                         digits = 2) * 100,
             REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                     digits = 2),
             REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                       digits = 2),
             `Agregação regional` = "UF",
             `Código TSE do município` = NA,
             `Código IBGE do município`= NA,
             `Nome do município` = NA,
             `Faixa de eleitores aptos` = NA,
             `Renovação bruta` = NA,
             `Renovação líquida` = NA,
             Recandidaturas = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Quantidade de municípios na agregação" = "QTDE_MUNICIPIOS_AGREG",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Municípios elegíveis para reapresentação" = "PERMIT_CAND",  
             "Reapresentação" = "REAPRESENTACAO",
             "Taxa de prefeitos elegíveis à reeleição" = "TX_PREFEITOS_PERMIT_REEL",
             "Taxa de prefeitos que se recandidataram" = "TX_PREFEITOS_RECAND",
             "Reeleitos" = "REELEITOS_AGREG",
             "Reeleição bruta" = "REELEICAO_BRUTA",
             "Reeleição líquida" = "REELEICAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
              UF,
             `Código TSE do município`,
             `Código IBGE do município`,
             `Nome do município`,
             Cargo,
             `Faixa de eleitores aptos`,
             `Quantidade de municípios na agregação`,
             `Cadeiras disponíveis`,
             `Municípios elegíveis para reapresentação`,
             `Reapresentação`,
             `Reeleitos`,
             `Taxa de prefeitos elegíveis à reeleição`,
             `Taxa de prefeitos que se recandidataram`,
             `Reeleição bruta`,
             `Reeleição líquida`,
             `Renovação bruta`,
             `Renovação líquida`,
             Recandidaturas) %>%
      arrange(`Ano da eleição`,
              UF)
    
################################## PF_ELEIT_APT ################################    
    
   } else if(agregacao == "PF_ELEIT_APT"){
      
      data <- data %>% 
        ungroup() %>% 
        unique() %>% 
        mutate(TX_PREFEITOS_PERMIT_REEL = round(TX_PREFEITOS_PERMIT_REEL,
                                                digits = 2) * 100,
               TX_PREFEITOS_RECAND = round(TX_PREFEITOS_RECAND,
                                           digits = 2) * 100,
               REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                       digits = 2),
               REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                         digits = 2),
               `Agregação regional` = "Faixa de eleitores aptos",
                UF = NA,
               `Código TSE do município` = NA,
               `Código IBGE do município`= NA,
               `Nome do município` = NA,
               `Renovação bruta` = NA,
               `Renovação líquida` = NA,
               Recandidaturas = NA,
               DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
        rename("Ano da eleição" = "ANO_ELEICAO",
               "Cargo" = "DESCRICAO_CARGO",
               "Faixa de eleitores aptos" = "AGREG_ELEITORES_APTOS",
               "Quantidade de municípios na agregação" = "QTDE_MUNICIPIOS_AGREG",
               "Cadeiras disponíveis" = "QTDE_VAGAS",
               "Municípios elegíveis para reapresentação" = "PERMIT_CAND",  
               "Reapresentação" = "REAPRESENTACAO",
               "Taxa de prefeitos elegíveis à Reeleição bruta" = "TX_PREFEITOS_PERMIT_REEL",
               "Taxa de prefeitos que se recandidataram" = "TX_PREFEITOS_RECAND",
               "Reeleitos" = "REELEITOS_AGREG",
               "Reeleição bruta" = "REELEICAO_BRUTA",
               "Reeleição líquida" = "REELEICAO_LIQUIDA") %>% 
        select(`Ano da eleição`,
               `Agregação regional`,
                UF,
               `Código TSE do município`,
               `Código IBGE do município`,
               `Nome do município`,
               Cargo,
               `Faixa de eleitores aptos`,
               `Quantidade de municípios na agregação`,
               `Cadeiras disponíveis`,
               `Municípios elegíveis para reapresentação`,
               `Reapresentação`,
               `Reeleitos`,
               `Taxa de prefeitos elegíveis à Reeleição bruta`,
               `Taxa de prefeitos que se recandidataram`,
               `Reeleição bruta`,
               `Reeleição líquida`,
               `Renovação bruta`,
               `Renovação líquida`,
               Recandidaturas) %>%
        arrange(`Ano da eleição`,
                `Faixa de eleitores aptos`)
    
#################################### MUN_VR ####################################
    
  } else if(agregacao == "MUN"){
    
    ## Verificando o número de municípios em cada agregação de eleitores aptos
    
    eleitores_aptos <- eleitores_aptos %>% 
      group_by(ANO_ELEICAO,
               AGREG_ELEITORES_APTOS) %>% 
      mutate(QTDE_MUNICIPIOS_AGREG = n())
    
    ## Padronizando o formato dos dados
    
    suppressMessages(
    data <- data %>% 
      ungroup() %>% 
      left_join(eleitores_aptos) %>% 
      unique() %>% 
      mutate(REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                     digits = 2),
             REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                       digits = 2),
             RENOVACAO_BRUTA = round(RENOVACAO_BRUTA,
                                     digits = 2),
             RENOVACAO_LIQUIDA = round(RENOVACAO_LIQUIDA,
                                       digits = 2),
             RECANDIDATURAS = round(RECANDIDATURAS,
                                    digits = 2),
             `Agregação regional` = "Município",
             `Municípios elegíveis para reapresentação` = NA,
             `Taxa de prefeitos elegíveis à reeleição` = NA,
             `Taxa de prefeitos que se recandidataram` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Código TSE do município" = "COD_MUN_TSE",
             "Código IBGE do município" = "COD_MUN_IBGE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Faixa de eleitores aptos" = "AGREG_ELEITORES_APTOS",
             "Quantidade de municípios na agregação" = "QTDE_MUNICIPIOS_AGREG",
             "Reapresentação" = "REAPRESENTACAO",
             "Reeleitos" = "REELEITOS",
             "Recandidaturas" = "RECANDIDATURAS",
             "Reeleição bruta" = "REELEICAO_BRUTA",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação bruta" = "RENOVACAO_BRUTA",
             "Renovação líquida" = "RENOVACAO_LIQUIDA",
             "Recandidaturas" = "RECANDIDATURAS") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
              UF,
             `Código TSE do município`,
             `Código IBGE do município`,
             `Nome do município`,
             Cargo,
             `Faixa de eleitores aptos`,
             `Quantidade de municípios na agregação`,
             `Cadeiras disponíveis`,
             `Municípios elegíveis para reapresentação`,
             `Reapresentação`,
             `Reeleitos`,
             `Taxa de prefeitos elegíveis à reeleição`,
             `Taxa de prefeitos que se recandidataram`,
             `Reeleição bruta`,
             `Reeleição líquida`,
             `Renovação bruta`,
             `Renovação líquida`,
             Recandidaturas) %>%
      arrange(`Ano da eleição`,
              UF,
              `Nome do município`))
    
  }
  
}

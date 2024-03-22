
## Função para padronização dos indicadores de 'Participação e Alienação'

padroniz_particip_alien <- function(data,
                                    agregacao = c("BR", "UF", "MUN")){
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      ungroup() %>% 
      mutate(ABSTENCAO_PERCENTUAL = round(ABSTENCAO_PERCENTUAL,
                                          digits = 2),
             VOTOS_BRANCOS_PERCENTUAIS = round(VOTOS_BRANCOS_PERCENTUAIS,
                                               digits = 2),
             VOTOS_NULOS_PERCENTUAIS = round(VOTOS_NULOS_PERCENTUAIS,
                                             digits = 2),
             ALIENACAO_PERCENTUAL = round(ALIENACAO_PERCENTUAL,
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
             "Turno" = "NUM_TURNO",
             "Cargo" = "DESCRICAO_CARGO",
             "Quantidade de eleitores aptos" = "QT_APTOS",
             "Quantidade de comparecimento" = "QT_COMPARECIMENTO",
             "Quantidade de abstenções" = "QT_ABSTENCOES",
             "Percentual de abstenções" = "ABSTENCAO_PERCENTUAL",
             "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS",
             "Percentual de votos brancos" = "VOTOS_BRANCOS_PERCENTUAIS",
             "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
             "Percentual de votos nulos" = "VOTOS_NULOS_PERCENTUAIS",
             "Alienação absoluta" = "ALIENACAO_ABSOLUTA",
             "Alienação percentual" = "ALIENACAO_PERCENTUAL") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Quantidade agregada de eleitores aptos`,
             `Quantidade de eleitores aptos`,
             `Quantidade de comparecimento`,
             `Quantidade de abstenções`,
             `Percentual de abstenções`,
             `Quantidade de votos brancos`,
             `Percentual de votos brancos`,
             `Quantidade de votos nulos`, 
             `Percentual de votos nulos`,
             `Alienação absoluta`,
             `Alienação percentual`) %>% 
      arrange(`Ano da eleição`,
              Cargo,
              Turno)
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      ungroup() %>% 
      mutate(ABSTENCAO_PERCENTUAL = round(ABSTENCAO_PERCENTUAL,
                                          digits = 2),
             VOTOS_BRANCOS_PERCENTUAIS = round(VOTOS_BRANCOS_PERCENTUAIS,
                                               digits = 2),
             VOTOS_NULOS_PERCENTUAIS = round(VOTOS_NULOS_PERCENTUAIS,
                                             digits = 2),
             ALIENACAO_PERCENTUAL = round(ALIENACAO_PERCENTUAL,
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
             "Turno" = "NUM_TURNO",
             "Cargo" = "DESCRICAO_CARGO",
             "Quantidade de eleitores aptos" = "QT_APTOS",
             "Quantidade de comparecimento" = "QT_COMPARECIMENTO",
             "Quantidade de abstenções" = "QT_ABSTENCOES",
             "Percentual de abstenções" = "ABSTENCAO_PERCENTUAL",
             "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS",
             "Percentual de votos brancos" = "VOTOS_BRANCOS_PERCENTUAIS",
             "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
             "Percentual de votos nulos" = "VOTOS_NULOS_PERCENTUAIS",
             "Alienação absoluta" = "ALIENACAO_ABSOLUTA",
             "Alienação percentual" = "ALIENACAO_PERCENTUAL") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Quantidade agregada de eleitores aptos`,
             `Quantidade de eleitores aptos`,
             `Quantidade de comparecimento`,
             `Quantidade de abstenções`,
             `Percentual de abstenções`,
             `Quantidade de votos brancos`,
             `Percentual de votos brancos`,
             `Quantidade de votos nulos`, 
             `Percentual de votos nulos`,
             `Alienação absoluta`,
             `Alienação percentual`) %>% 
      arrange(`Ano da eleição`,
              UF,
              Cargo,
              Turno)
    
  } else if(agregacao == "MUN"){
    
    data <- data %>%
      ungroup() %>% 
      left_join(eleitores_aptos) %>% 
      mutate(ABSTENCAO_PERCENTUAL = round(ABSTENCAO_PERCENTUAL,
                                          digits = 2),
             VOTOS_BRANCOS_PERCENTUAIS = round(VOTOS_BRANCOS_PERCENTUAIS,
                                               digits = 2),
             VOTOS_NULOS_PERCENTUAIS = round(VOTOS_NULOS_PERCENTUAIS,
                                             digits = 2),
             ALIENACAO_PERCENTUAL = round(ALIENACAO_PERCENTUAL,
                                          digits = 2),
             `Agregação regional` = "Município",
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "Cargo" = "DESCRICAO_CARGO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Quantidade de eleitores aptos" = "QT_APTOS",
             "Quantidade agregada de eleitores aptos" = "AGREG_ELEITORES_APTOS",
             "Quantidade de comparecimento" = "QT_COMPARECIMENTO",
             "Quantidade de abstenções" = "QT_ABSTENCOES",
             "Percentual de abstenções" = "ABSTENCAO_PERCENTUAL",
             "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS",
             "Percentual de votos brancos" = "VOTOS_BRANCOS_PERCENTUAIS",
             "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
             "Percentual de votos nulos" = "VOTOS_NULOS_PERCENTUAIS",
             "Alienação absoluta" = "ALIENACAO_ABSOLUTA",
             "Alienação percentual" = "ALIENACAO_PERCENTUAL") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Quantidade agregada de eleitores aptos`,
             `Quantidade de eleitores aptos`,
             `Quantidade de comparecimento`,
             `Quantidade de abstenções`,
             `Percentual de abstenções`,
             `Quantidade de votos brancos`,
             `Percentual de votos brancos`,
             `Quantidade de votos nulos`, 
             `Percentual de votos nulos`,
             `Alienação absoluta`,
             `Alienação percentual`) %>% 
      arrange(`Ano da eleição`,
              UF,
              `Nome do município`,
              Cargo,
              Turno)
    
  }
  
}

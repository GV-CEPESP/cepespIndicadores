
## Função para padronização dos indicadores de 'Fragmentação'

padroniz_frag <- function(data, 
                          agregacao = c("BR", "UF", "MUN_PF", "MUN_VR")){
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Votos válidos" = "QT_VOTOS_VALIDOS_BR") %>% 
      mutate(`Número efetivo de partidos eleitoral` = round(`Número efetivo de partidos eleitoral`,
                                                            digits = 2),
             `Número efetivo de partidos legislativo` = round(`Número efetivo de partidos legislativo`,
                                                              digits = 2),
             `Fracionalização` = round(`Fracionalização`,
                                       digits = 4) * 100,
             `Fracionalização máxima` = round(`Fracionalização máxima`,
                                              digits = 4) * 100,
             `Fragmentação` = round(`Fragmentação`,
                                    digits = 4) * 100,
             `Desproporcionalidade` = round(`Desproporcionalidade`,
                                            digits = 2),
             `Agregação regional` = "Brasil",
             UF = "BR",
             `Código do município` = NA,
             `Nome do município` = NA,
             Cargo = str_to_title(Cargo),
             `Quantidade agregada de eleitores aptos` = NA,
             Turno = 1) %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Quantidade agregada de eleitores aptos`,
             `Número efetivo de partidos eleitoral`,
             `Número efetivo de partidos legislativo`,
             Fracionalização,
             `Fracionalização máxima`,
             Fragmentação,
             `Desproporcionalidade`) %>% 
      unique() %>% 
      arrange(`Ano da eleição`)
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Votos válidos" = "QT_VOTOS_VALIDOS") %>% 
      mutate(`Número efetivo de partidos eleitoral` = round(`Número efetivo de partidos eleitoral`,
                                                            digits = 2),
             `Número efetivo de partidos legislativo` = round(`Número efetivo de partidos legislativo`,
                                                              digits = 2),
             `Fracionalização` = round(`Fracionalização`,
                                       digits = 4) * 100,
             `Fracionalização máxima` = round(`Fracionalização máxima`,
                                              digits = 4) * 100,
             `Fragmentação` = round(`Fragmentação`,
                                    digits = 4) * 100,
             `Desproporcionalidade` = round(`Desproporcionalidade`,
                                            digits = 2),
             `Agregação regional` = "UF",
             `Código do município` = NA,
             `Nome do município` = NA,
             Cargo = str_to_title(Cargo),
             `Quantidade agregada de eleitores aptos` = NA,
             Turno = 1) %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Quantidade agregada de eleitores aptos`,
             `Número efetivo de partidos eleitoral`,
             `Número efetivo de partidos legislativo`,
             Fracionalização,
             `Fracionalização máxima`,
             Fragmentação,
             `Desproporcionalidade`) %>% 
      unique() %>% 
      arrange(`Ano da eleição`,
              UF,
              Turno)
    
  } else if(agregacao == "MUN_PF"){
    
    data <- data %>% 
      ungroup() %>% 
      left_join(eleitores_aptos) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Votos válidos" = "QT_VOTOS_VALIDOS",
             "Quantidade agregada de eleitores aptos" = "AGREG_ELEITORES_APTOS") %>% 
      mutate(`Número efetivo de partidos eleitoral` = round(`Número efetivo de partidos eleitoral`,
                                                            digits = 2),
             `Número efetivo de partidos legislativo` = NA,
             `Fracionalização` = NA,
             `Fracionalização máxima` = NA,
             `Fragmentação` = NA,
             `Desproporcionalidade` = NA,
             `Agregação regional` = "Município",
             Cargo = str_to_title(Cargo)) %>%
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Quantidade agregada de eleitores aptos`,
             `Número efetivo de partidos eleitoral`,
             `Número efetivo de partidos legislativo`,
             Fracionalização,
             `Fracionalização máxima`,
             Fragmentação,
             `Desproporcionalidade`) %>% 
      unique() %>% 
      arrange(`Ano da eleição`,
              UF,
              `Nome do município`,
              Turno)
    
    
  } else if(agregacao == "MUN_VR"){
    
    data <- data %>% 
      ungroup() %>% 
      left_join(eleitores_aptos) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Votos válidos" = "QT_VOTOS_VALIDOS",
             "Sigla do partido" = "SIGLA_PARTIDO",
             "Quantidade agregada de eleitores aptos" = "AGREG_ELEITORES_APTOS") %>% 
      mutate(`Número efetivo de partidos eleitoral` = round(`Número efetivo de partidos eleitoral`,
                                                            digits = 2),
             `Número efetivo de partidos legislativo` = round(`Número efetivo de partidos legislativo`,
                                                              digits = 2),
             `Fracionalização` = round(`Fracionalização`,
                                       digits = 4) * 100,
             `Fracionalização máxima` = round(`Fracionalização máxima`,
                                              digits = 4) * 100,
             `Fragmentação` = round(`Fragmentação`,
                                    digits = 4) * 100,
             `Desproporcionalidade` = round(`Desproporcionalidade`,
                                            digits = 2),
             `Agregação regional` = "Município",
             Cargo = str_to_title(Cargo)) %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Quantidade agregada de eleitores aptos`,
             `Número efetivo de partidos eleitoral`,
             `Número efetivo de partidos legislativo`,
             Fracionalização,
             `Fracionalização máxima`,
             Fragmentação,
             `Desproporcionalidade`) %>% 
      unique() %>% 
      arrange(`Ano da eleição`,
              UF,
              `Nome do município`,
              Turno)
    
  }
  
}

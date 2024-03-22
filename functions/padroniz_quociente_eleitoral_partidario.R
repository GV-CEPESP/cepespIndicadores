
## Função para padronização dos indicadores de 'Quociente Eleitoral e 
##                                              Quociente Partidário'

padroniz_quoc <- function(data, 
                          agregacao = c("BR", "UF", "MUN")){
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Votos válidos" = "QT_VOTOS_VALIDOS_BR",
             "Sigla do partido" = "SIGLA_PARTIDO",
             "Total de votos conquistados" = "VOT_PART_BR",
             "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
             "Quociente partidário" = "QUOCIENTE_PARTIDARIO") %>% 
      mutate(`Quociente eleitoral` = round(`Quociente eleitoral`, 
                                           digits = 0),
             `Quociente partidário` = round(`Quociente partidário`, 
                                            digits = 0),
             `Agregação regional` = "Brasil",
             UF = "BR",
             `Código do município` = NA,
             `Nome do município` = NA,
             Cargo = str_to_title(Cargo),
             `Quantidade agregada de eleitores aptos` = NA) %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Quantidade agregada de eleitores aptos`,
             `Sigla do partido`,
             `Quociente eleitoral`,
             `Quociente partidário`) 
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Votos válidos" = "QT_VOTOS_VALIDOS",
             "Sigla do partido" = "SIGLA_PARTIDO",
             "Total de votos conquistados" = "VOT_PART_UF",
             "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
             "Quociente partidário" = "QUOCIENTE_PARTIDARIO") %>% 
      mutate(`Quociente eleitoral` = round(`Quociente eleitoral`, 
                                           digits = 0),
             `Quociente partidário` = round(`Quociente partidário`, 
                                            digits = 0),
             `Agregação regional` = "UF",
             `Código do município` = NA,
             `Nome do município` = NA,
             Cargo = str_to_title(Cargo),
             `Quantidade agregada de eleitores aptos` = NA) %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Quantidade agregada de eleitores aptos`,
             `Sigla do partido`,
             `Quociente eleitoral`,
             `Quociente partidário`) 
    
  } else if(agregacao == "MUN"){
    
    data <- data %>% 
      ungroup() %>% 
      left_join(eleitores_aptos) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Votos válidos" = "QT_VOTOS_VALIDOS",
             "Quantidade agregada de eleitores aptos" = "AGREG_ELEITORES_APTOS",
             "Sigla do partido" = "SIGLA_PARTIDO",
             "Total de votos conquistados" = "VOT_PART_MUN",
             "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
             "Quociente partidário" = "QUOCIENTE_PARTIDARIO") %>% 
      mutate(`Quociente eleitoral` = round(`Quociente eleitoral`, 
                                           digits = 0),
             `Quociente partidário` = round(`Quociente partidário`, 
                                            digits = 0),
             `Agregação regional` = "Município",
             Cargo = str_to_title(Cargo)) %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município`,
             `Nome do município`,
             Cargo,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Quantidade agregada de eleitores aptos`,
             `Sigla do partido`,
             `Quociente eleitoral`,
             `Quociente partidário`) 
    
  }
  
}

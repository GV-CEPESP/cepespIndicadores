
## Função para padronização dos indicadores de 'Participação e Alienação'

padroniz_particip_alien <- function(data,
                                    agregacao = c("BR", "UF", "MUN")){
  
################################### BR ####################################
  
  if(agregacao == "BR"){
    
    ## Organizando os dados
    
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
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)` = NA,
             `Município` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO),
             NUM_TURNO = ifelse(is.na(NUM_TURNO),
                                1,
                                NUM_TURNO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "Cargo" = "DESCRICAO_CARGO",
             "Eleitores aptos" = "QTDE_APTOS",
             "Comparecimento" = "QTDE_COMPARECIMENTO",
             "Abstenção absoluta " = "QTDE_ABSTENCOES",
             "Abstenção percentual" = "ABSTENCAO_PERCENTUAL",
             "Votos brancos absolutos " = "QTDE_VOTOS_BRANCOS",
             "Votos brancos percentuais " = "VOTOS_BRANCOS_PERCENTUAIS",
             "Votos nulos absolutos " = "QTDE_VOTOS_NULOS", 
             "Votos nulos percentuais " = "VOTOS_NULOS_PERCENTUAIS",
             "Alienação absoluta" = "ALIENACAO_ABSOLUTA",
             "Alienação percentual" = "ALIENACAO_PERCENTUAL") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Eleitores aptos`,
             `Comparecimento`,
             `Abstenção absoluta `,
             `Abstenção percentual`,
             `Votos brancos absolutos `,
             `Votos brancos percentuais `,
             `Votos nulos absolutos `, 
             `Votos nulos percentuais `,
             `Alienação absoluta`,
             `Alienação percentual`) %>% 
      arrange(`Ano da eleição`,
              Turno,
              Cargo)
    
####################################### UF ################################
    
  } else if(agregacao == "UF"){
    
    ## Organizando os dados
    
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
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)` = NA,
             `Município` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO),
             NUM_TURNO = ifelse(is.na(NUM_TURNO),
                                   1,
                                NUM_TURNO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Eleitores aptos" = "QTDE_APTOS",
             "Comparecimento" = "QTDE_COMPARECIMENTO",
             "Abstenção absoluta " = "QTDE_ABSTENCOES",
             "Abstenção percentual" = "ABSTENCAO_PERCENTUAL",
             "Votos brancos absolutos " = "QTDE_VOTOS_BRANCOS",
             "Votos brancos percentuais " = "VOTOS_BRANCOS_PERCENTUAIS",
             "Votos nulos absolutos " = "QTDE_VOTOS_NULOS", 
             "Votos nulos percentuais " = "VOTOS_NULOS_PERCENTUAIS",
             "Alienação absoluta" = "ALIENACAO_ABSOLUTA",
             "Alienação percentual" = "ALIENACAO_PERCENTUAL") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Eleitores aptos`,
             `Comparecimento`,
             `Abstenção absoluta `,
             `Abstenção percentual`,
             `Votos brancos absolutos `,
             `Votos brancos percentuais `,
             `Votos nulos absolutos `, 
             `Votos nulos percentuais `,
             `Alienação absoluta`,
             `Alienação percentual`) %>% 
      arrange(`Ano da eleição`,
              Turno,
              UF,
              Cargo)
    
####################################### MUN ################################
    
  } else if(agregacao == "MUN"){
    
    ## Preparando base de vagas p/ o join
    
    vagas <- vagas_ver %>% 
      mutate(MUN_EXISTE = ifelse(is.na(QTDE_VAGAS),
                                 0,
                                 1)) %>% 
      select(ANO_ELEICAO,
             SIGLA_UF,
             COD_MUN_TSE,
             COD_MUN_IBGE,
             MUN_EXISTE)
    
    ## Organizando os dados
    
    suppressMessages(
    data <- data %>%
      right_join(vagas) %>%
      filter(MUN_EXISTE != 0) %>% 
      group_by(SIGLA_UF,
               COD_MUN_TSE) %>% 
      fill(NOME_MUNICIPIO,
           .direction = "downup") %>% 
      group_by(SIGLA_UF,
               COD_MUN_TSE) %>% 
      fill(DESCRICAO_CARGO,
           .direction = "downup") %>% 
      ungroup() %>% 
      mutate(ABSTENCAO_PERCENTUAL = round(ABSTENCAO_PERCENTUAL,
                                          digits = 2),
             VOTOS_BRANCOS_PERCENTUAIS = round(VOTOS_BRANCOS_PERCENTUAIS,
                                               digits = 2),
             VOTOS_NULOS_PERCENTUAIS = round(VOTOS_NULOS_PERCENTUAIS,
                                             digits = 2),
             ALIENACAO_PERCENTUAL = round(ALIENACAO_PERCENTUAL,
                                          digits = 2),
             `Agregação regional` = "Município",
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO),
             NUM_TURNO = ifelse(is.na(NUM_TURNO),
                                1,
                                NUM_TURNO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Código do município (TSE)" = "COD_MUN_TSE",
             "Código do município (IBGE)" = "COD_MUN_IBGE",
             "Município" = "NOME_MUNICIPIO",
             "Eleitores aptos" = "QTDE_APTOS",
             "Comparecimento" = "QTDE_COMPARECIMENTO",
             "Abstenção absoluta " = "QTDE_ABSTENCOES",
             "Abstenção percentual" = "ABSTENCAO_PERCENTUAL",
             "Votos brancos absolutos " = "QTDE_VOTOS_BRANCOS",
             "Votos brancos percentuais " = "VOTOS_BRANCOS_PERCENTUAIS",
             "Votos nulos absolutos " = "QTDE_VOTOS_NULOS", 
             "Votos nulos percentuais " = "VOTOS_NULOS_PERCENTUAIS",
             "Alienação absoluta" = "ALIENACAO_ABSOLUTA",
             "Alienação percentual" = "ALIENACAO_PERCENTUAL") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Eleitores aptos`,
             `Comparecimento`,
             `Abstenção absoluta `,
             `Abstenção percentual`,
             `Votos brancos absolutos `,
             `Votos brancos percentuais `,
             `Votos nulos absolutos `, 
             `Votos nulos percentuais `,
             `Alienação absoluta`,
             `Alienação percentual`) %>% 
      arrange(`Ano da eleição`,
              Turno,
              UF,
              `Município`,
              Cargo))
    
  }
  
}

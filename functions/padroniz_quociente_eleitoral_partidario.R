
## Função para padronização dos indicadores de 'Quociente Eleitoral e 
##                                              Quociente Partidário'

padroniz_quoc <- function(data, 
                          agregacao = c("BR", "UF", "MUN")){
  
################################## BR #####################################
  
  if(agregacao == "BR"){
    
    ## Organizando os dados
    
    data <- data %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Votos válidos" = "QTDE_VOTOS_VALIDOS_BR",
             "Sigla do partido" = "SIGLA_PARTIDO",
             "Total de votos conquistados" = "VOT_PART_BR",
             "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
             "Quociente partidário" = "QUOCIENTE_PARTIDARIO",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL") %>% 
      mutate(`Quociente eleitoral` = round(`Quociente eleitoral`, 
                                           digits = 0),
             `Quociente partidário` = round(`Quociente partidário`, 
                                            digits = 0),
             `Agregação regional` = "Brasil",
             UF = "BR",
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)` = NA,
             `Município` = NA,
              Cargo = str_to_title(Cargo)) %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Sigla do partido`,
             `Quociente eleitoral`,
             `Quociente partidário`) 
  
##################################### UF ##################################
    
  } else if(agregacao == "UF"){
    
    ## Organizando os dados
    
    data <- data %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Votos válidos" = "QTDE_VOTOS_VALIDOS",
             "Sigla do partido" = "SIGLA_PARTIDO",
             "Total de votos conquistados" = "VOT_PART_UF",
             "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
             "Quociente partidário" = "QUOCIENTE_PARTIDARIO",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL") %>% 
      mutate(`Quociente eleitoral` = round(`Quociente eleitoral`, 
                                           digits = 0),
             `Quociente partidário` = round(`Quociente partidário`, 
                                            digits = 0),
             `Agregação regional` = "UF",
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)` = NA,
             `Município` = NA,
             Cargo = str_to_title(Cargo)) %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Sigla do partido`,
             `Quociente eleitoral`,
             `Quociente partidário`) 
    
################################# MUN #####################################
    
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
             QTDE_VAGAS,
             MUN_EXISTE) %>% 
      rename("QTDE_VAGAS2" = "QTDE_VAGAS")
    
    ## Organizando os dados
    
    suppressMessages(
    data <- data %>% 
      right_join(vagas) %>%
      filter(MUN_EXISTE != 0) %>% 
      mutate(QTDE_VAGAS = ifelse(is.na(QTDE_VAGAS),
                                 QTDE_VAGAS2,
                                 QTDE_VAGAS)) %>%
      group_by(SIGLA_UF,
               COD_MUN_TSE) %>% 
      fill(NOME_MUNICIPIO,
           .direction = "downup") %>% 
      group_by(SIGLA_UF,
               COD_MUN_TSE) %>% 
      fill(DESCRICAO_CARGO,
           .direction = "downup") %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Código do município (TSE)" = "COD_MUN_TSE",
             "Código do município (IBGE)" = "COD_MUN_IBGE",
             "Município" = "NOME_MUNICIPIO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Votos válidos" = "QTDE_VOTOS_VALIDOS",
             "Sigla do partido" = "SIGLA_PARTIDO",
             "Total de votos conquistados" = "VOT_PART_MUN",
             "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
             "Quociente partidário" = "QUOCIENTE_PARTIDARIO",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL") %>% 
      mutate(`Quociente eleitoral` = round(`Quociente eleitoral`, 
                                           digits = 0),
             `Quociente partidário` = round(`Quociente partidário`, 
                                            digits = 0),
             `Agregação regional` = "Município",
             Cargo = str_to_title(Cargo),
             `Municípios-Vagas com informação disponíveis` = ifelse(is.na(`Municípios-Vagas com informação disponíveis`),
                                                                    0,
                                                                    `Municípios-Vagas com informação disponíveis`)) %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Votos válidos`,
             `Sigla do partido`,
             `Quociente eleitoral`,
             `Quociente partidário`)) 
    
  }
  
}

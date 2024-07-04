
## Função para padronização dos indicadores de 'Fragmentação'

padroniz_frag <- function(data, 
                          agregacao = c("BR", "UF", 
                                        "PF_MUN", "VR_MUN")){
  
################################# BR ######################################  
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Votos válidos" = "QTDE_VOTOS_VALIDOS_BR",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL") %>% 
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
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)` = NA,
             `Município` = NA,
             Cargo = str_to_title(Cargo),
             Turno = "1") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Número efetivo de partidos eleitoral`,
             `Número efetivo de partidos legislativo`,
             Fracionalização,
             `Fracionalização máxima`,
             Fragmentação,
             `Desproporcionalidade`) %>% 
      unique() %>% 
      arrange(`Ano da eleição`)

#################################### UF ###################################
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Votos válidos" = "QTDE_VOTOS_VALIDOS",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL") %>% 
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
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)` = NA,
             `Município` = NA,
             Cargo = str_to_title(Cargo),
             Turno = "1") %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
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
    
##################################### PF_MUN ###############################    
    
  } else if(agregacao == "PF_MUN"){
    
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
      mutate(QTDE_VAGAS = ifelse(is.na(QTDE_VAGAS),
                                       1,
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
             "Turno" = "NUM_TURNO",
             "UF" = "SIGLA_UF",
             "Código do município (TSE)" = "COD_MUN_TSE",
             "Código do município (IBGE)" = "COD_MUN_IBGE",
             "Município" = "NOME_MUNICIPIO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Votos válidos" = "QTDE_VOTOS_VALIDOS",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL") %>% 
      mutate(`Número efetivo de partidos eleitoral` = round(`Número efetivo de partidos eleitoral`,
                                                            digits = 2),
             across(everything(), ~ifelse(is.nan(.),
                                          0.00,
                                          .)),
             `Número efetivo de partidos legislativo` = NA,
             `Fracionalização` = NA,
             `Fracionalização máxima` = NA,
             `Fragmentação` = NA,
             `Desproporcionalidade` = NA,
             `Agregação regional` = "Município",
             Cargo = str_to_title(Cargo),
             Turno = ifelse(is.na(Turno),
                            1,
                            Turno),
             `Municípios-Vagas com informação disponíveis` = ifelse(is.na(`Municípios-Vagas com informação disponíveis`),
                                                                    0,
                                                                    `Municípios-Vagas com informação disponíveis`)) %>%
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Número efetivo de partidos eleitoral`,
             `Número efetivo de partidos legislativo`,
             Fracionalização,
             `Fracionalização máxima`,
             Fragmentação,
             `Desproporcionalidade`) %>% 
      unique() %>% 
      arrange(`Ano da eleição`,
              UF,
              `Município`,
              Turno))
    
################################ VR_MUN ###################################    
    
  } else if(agregacao == "VR_MUN"){
    
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
             "Turno" = "NUM_TURNO",
             "UF" = "SIGLA_UF",
             "Código do município (TSE)" = "COD_MUN_TSE",
             "Código do município (IBGE)" = "COD_MUN_IBGE",
             "Município" = "NOME_MUNICIPIO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Votos válidos" = "QTDE_VOTOS_VALIDOS",
             "Sigla do partido" = "SIGLA_PARTIDO",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL") %>% 
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
             across(everything(), ~ifelse(is.nan(.),
                                          0.00,
                                          .)),
             `Agregação regional` = "Município",
             Cargo = str_to_title(Cargo),
             Turno = "1",
             `Municípios-Vagas com informação disponíveis` = ifelse(is.na(`Municípios-Vagas com informação disponíveis`),
                                                                    0,
                                                                    `Municípios-Vagas com informação disponíveis`)) %>% 
      select(`Ano da eleição`,
             Turno,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Número efetivo de partidos eleitoral`,
             `Número efetivo de partidos legislativo`,
             Fracionalização,
             `Fracionalização máxima`,
             Fragmentação,
             `Desproporcionalidade`) %>% 
      unique() %>% 
      arrange(`Ano da eleição`,
              UF,
              `Município`,
              Turno))
    
  }
  
}

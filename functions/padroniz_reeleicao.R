
## Função para padronização dos indicadores de 'Reeleição'

padroniz_reel <- function(data,
                          agregacao = c("BR", 
                                        "UF", 
                                        "PF_BR", "PF_UF",
                                        "PF_ELEIT_APT",
                                        "VR_BR", "VR_UF",
                                        "PF_MUN",
                                        "VR_ELEIT_APT")){

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
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)`= NA,
             `Município` = NA,
             `Eleitores aptos` = NA,
             #INFORMACAO_DISPONIVEL = NA,
             `Municípios na agregação regional ` = NA,
             `Municípios aptos para a reeleição de prefeito` = NA,
             `Taxa de prefeitos aptos a disputar a reeleição` = NA,
             `Taxa de prefeitos que tentaram a reeleição` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Candidatos tentando a reeleição" = "REAPRESENTACAO",
             "Reeleitos" = "REELEITOS_AGREG",
             "Recandidaturas" = "RECANDIDATURAS",
             "Reeleição bruta" = "REELEICAO_BRUTA",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação bruta" = "RENOVACAO_BRUTA",
             "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
              UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Eleitores aptos`,
             `Municípios na agregação regional `,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Municípios aptos para a reeleição de prefeito`,
             `Candidatos tentando a reeleição`,
             `Reeleitos`,
             `Taxa de prefeitos aptos a disputar a reeleição`,
             `Taxa de prefeitos que tentaram a reeleição`,
             `Reeleição bruta`,
             `Reeleição líquida`,
             `Renovação bruta`,
             `Renovação líquida`,
             `Recandidaturas`) %>% 
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
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)`= NA,
             `Município` = NA,
             `Eleitores aptos` = NA,
             `Eleitores aptos` = NA,
             #INFORMACAO_DISPONIVEL = NA,
             `Municípios na agregação regional ` = NA,
             `Municípios aptos para a reeleição de prefeito` = NA,
             `Taxa de prefeitos aptos a disputar a reeleição` = NA,
             `Taxa de prefeitos que tentaram a reeleição` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Candidatos tentando a reeleição" = "REAPRESENTACAO",
             "Reeleitos" = "REELEITOS_AGREG",
             "Recandidaturas" = "RECANDIDATURAS",
             "Reeleição bruta" = "REELEICAO_BRUTA",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação bruta" = "RENOVACAO_BRUTA",
             "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Eleitores aptos`,
             `Municípios na agregação regional `,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Municípios aptos para a reeleição de prefeito`,
             `Candidatos tentando a reeleição`,
             `Reeleitos`,
             `Taxa de prefeitos aptos a disputar a reeleição`,
             `Taxa de prefeitos que tentaram a reeleição`,
             `Reeleição bruta`,
             `Reeleição líquida`,
             `Renovação bruta`,
             `Renovação líquida`,
             `Recandidaturas`) %>%
      arrange(`Ano da eleição`,
              UF)
    
  #################################### PF_BR #####################################

  } else if(agregacao == "PF_BR"){
    
    data <- data %>% 
      ungroup() %>% 
      unique() %>% 
      mutate(TX_PREFEITOS_PERMIT_REEL = round(TX_PREFEITOS_PERMIT_REEL,
                                              digits = 4) * 100,
             TX_PREFEITOS_RECAND = round(TX_PREFEITOS_RECAND,
                                         digits = 4) * 100,
             REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                     digits = 2),
             REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                       digits = 2),
             `Agregação regional` = "Brasil",
             UF = "BR",
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)`= NA,
             `Município` = NA,
             `Eleitores aptos` = NA,
             `Renovação bruta` = NA,
             `Renovação líquida` = NA,
             `Recandidaturas` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL",
             "Municípios na agregação regional " = "QTDE_MUNICIPIOS_AGREG",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Municípios aptos para a reeleição de prefeito" = "PERMIT_CAND",  
             "Candidatos tentando a reeleição" = "REAPRESENTACAO",
             "Taxa de prefeitos aptos a disputar a reeleição" = "TX_PREFEITOS_PERMIT_REEL",
             "Taxa de prefeitos que tentaram a reeleição" = "TX_PREFEITOS_RECAND",
             "Reeleitos" = "REELEITOS_AGREG",
             "Reeleição bruta" = "REELEICAO_BRUTA",
             "Reeleição líquida" = "REELEICAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
             UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Eleitores aptos`,
             `Municípios na agregação regional `,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Municípios aptos para a reeleição de prefeito`,
             `Candidatos tentando a reeleição`,
             `Reeleitos`,
             `Taxa de prefeitos aptos a disputar a reeleição`,
             `Taxa de prefeitos que tentaram a reeleição`,
             `Reeleição bruta`,
             `Reeleição líquida`,
             `Renovação bruta`,
             `Renovação líquida`,
             `Recandidaturas`) %>%
      arrange(`Ano da eleição`)
  
  #################################### PF_UF #####################################
  
  } else if(agregacao == "PF_UF"){
    
    data <- data %>% 
      ungroup() %>% 
      unique() %>% 
      mutate(TX_PREFEITOS_PERMIT_REEL = round(TX_PREFEITOS_PERMIT_REEL,
                                              digits = 4) * 100,
             TX_PREFEITOS_RECAND = round(TX_PREFEITOS_RECAND,
                                         digits = 4) * 100,
             REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                     digits = 2),
             REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                       digits = 2),
             `Agregação regional` = "UF",
             `Código do município (TSE)` = NA,
             `Código do município (IBGE)`= NA,
             `Município` = NA,
             `Eleitores aptos` = NA,
             `Renovação bruta` = NA,
             `Renovação líquida` = NA,
             `Recandidaturas` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL",
             "Municípios na agregação regional " = "QTDE_MUNICIPIOS_AGREG",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Municípios aptos para a reeleição de prefeito" = "PERMIT_CAND",  
             "Candidatos tentando a reeleição" = "REAPRESENTACAO",
             "Taxa de prefeitos aptos a disputar a reeleição" = "TX_PREFEITOS_PERMIT_REEL",
             "Taxa de prefeitos que tentaram a reeleição" = "TX_PREFEITOS_RECAND",
             "Reeleitos" = "REELEITOS_AGREG",
             "Reeleição bruta" = "REELEICAO_BRUTA",
             "Reeleição líquida" = "REELEICAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
              UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Eleitores aptos`,
             `Municípios na agregação regional `,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Municípios aptos para a reeleição de prefeito`,
             `Candidatos tentando a reeleição`,
             `Reeleitos`,
             `Taxa de prefeitos aptos a disputar a reeleição`,
             `Taxa de prefeitos que tentaram a reeleição`,
             `Reeleição bruta`,
             `Reeleição líquida`,
             `Renovação bruta`,
             `Renovação líquida`,
             `Recandidaturas`) %>%
      arrange(`Ano da eleição`,
              UF)
    
################################## PF_ELEIT_APT ################################    
    
   } else if(agregacao == "PF_ELEIT_APT"){
      
      data <- data %>% 
        ungroup() %>% 
        unique() %>% 
        mutate(TX_PREFEITOS_PERMIT_REEL = round(TX_PREFEITOS_PERMIT_REEL,
                                                digits = 4) * 100,
               TX_PREFEITOS_RECAND = round(TX_PREFEITOS_RECAND,
                                           digits = 4) * 100,
               REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                       digits = 2),
               REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                         digits = 2),
               `Agregação regional` = "Eleitores aptos",
                UF = NA,
               `Código do município (TSE)` = NA,
               `Código do município (IBGE)`= NA,
               `Município` = NA,
               `Renovação bruta` = NA,
               `Renovação líquida` = NA,
               `Recandidaturas` = NA,
               DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
        rename("Ano da eleição" = "ANO_ELEICAO",
               "Cargo" = "DESCRICAO_CARGO",
               "Eleitores aptos" = "AGREG_ELEITORES_APTOS",
               "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL",
               "Municípios na agregação regional " = "QTDE_MUNICIPIOS_AGREG",
               "Cadeiras disponíveis" = "QTDE_VAGAS",
               "Municípios aptos para a reeleição de prefeito" = "PERMIT_CAND",  
               "Candidatos tentando a reeleição" = "REAPRESENTACAO",
               "Taxa de prefeitos aptos a disputar a reeleição" = "TX_PREFEITOS_PERMIT_REEL",
               "Taxa de prefeitos que tentaram a reeleição" = "TX_PREFEITOS_RECAND",
               "Reeleitos" = "REELEITOS_AGREG",
               "Reeleição bruta" = "REELEICAO_BRUTA",
               "Reeleição líquida" = "REELEICAO_LIQUIDA") %>% 
        select(`Ano da eleição`,
               `Agregação regional`,
                UF,
               `Código do município (TSE)`,
               `Código do município (IBGE)`,
               `Município`,
               Cargo,
               `Eleitores aptos`,
               `Municípios na agregação regional `,
               `Municípios-Vagas com informação disponíveis`,
               `Cadeiras disponíveis`,
               `Municípios aptos para a reeleição de prefeito`,
               `Candidatos tentando a reeleição`,
               `Reeleitos`,
               `Taxa de prefeitos aptos a disputar a reeleição`,
               `Taxa de prefeitos que tentaram a reeleição`,
               `Reeleição bruta`,
               `Reeleição líquida`,
               `Renovação bruta`,
               `Renovação líquida`,
               `Recandidaturas`) %>%
        arrange(`Ano da eleição`,
                `Eleitores aptos`)
    
  #################################### VR_BR ####################################
    
   } else if(agregacao == "VR_BR"){
     
     ## Padronizando o formato dos dados
     
     suppressMessages(
       data <- data %>% 
         ungroup() %>% 
         mutate(QTDE_VAGAS = round(QTDE_VAGAS,
                                   digits = 2),
                REAPRESENTACAO = round(REAPRESENTACAO,
                                   digits = 2),
                REELEITOS_AGREG = round(REELEITOS_AGREG,
                                   digits = 2),
                REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                        digits = 2),
                REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                          digits = 2),
                RENOVACAO_BRUTA = round(RENOVACAO_BRUTA,
                                        digits = 2),
                RENOVACAO_LIQUIDA = round(RENOVACAO_LIQUIDA,
                                          digits = 2),
                RECANDIDATURAS = round(RECANDIDATURAS,
                                       digits = 2),
                `Agregação regional` = "Brasil",
                UF = "BR",
                `Código do município (TSE)` = NA,
                `Código do município (IBGE)`= NA,
                `Município` = NA,
                `Eleitores aptos` = NA,
                `Municípios aptos para a reeleição de prefeito` = NA,
                `Taxa de prefeitos aptos a disputar a reeleição` = NA,
                `Taxa de prefeitos que tentaram a reeleição` = NA,
                DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
         rename("Ano da eleição" = "ANO_ELEICAO",
                "Cargo" = "DESCRICAO_CARGO",
                "Cadeiras disponíveis" = "QTDE_VAGAS",
                "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL",
                "Municípios na agregação regional " = "QTDE_MUNICIPIOS_AGREG",
                "Candidatos tentando a reeleição" = "REAPRESENTACAO",
                "Reeleitos" = "REELEITOS_AGREG",
                "Recandidaturas" = "RECANDIDATURAS",
                "Reeleição bruta" = "REELEICAO_BRUTA",
                "Reeleição líquida" = "REELEICAO_LIQUIDA",
                "Renovação bruta" = "RENOVACAO_BRUTA",
                "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
         select(`Ano da eleição`,
                `Agregação regional`,
                UF,
                `Código do município (TSE)`,
                `Código do município (IBGE)`,
                `Município`,
                Cargo,
                `Eleitores aptos`,
                `Municípios na agregação regional `,
                `Municípios-Vagas com informação disponíveis`,
                `Cadeiras disponíveis`,
                `Municípios aptos para a reeleição de prefeito`,
                `Candidatos tentando a reeleição`,
                `Reeleitos`,
                `Taxa de prefeitos aptos a disputar a reeleição`,
                `Taxa de prefeitos que tentaram a reeleição`,
                `Reeleição bruta`,
                `Reeleição líquida`,
                `Renovação bruta`,
                `Renovação líquida`,
                `Recandidaturas`) %>%
         arrange(`Ano da eleição`))
     
  ################################### VR_UF #####################################
     
   }  else if(agregacao == "VR_UF"){
       
       ## Padronizando o formato dos dados
       
       suppressMessages(
         data <- data %>% 
           ungroup() %>% 
           mutate(QTDE_VAGAS = round(QTDE_VAGAS,
                                     digits = 2),
                  REAPRESENTACAO = round(REAPRESENTACAO,
                                         digits = 2),
                  REELEITOS_AGREG = round(REELEITOS_AGREG,
                                          digits = 2),
                  REELEICAO_BRUTA = round(REELEICAO_BRUTA,
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
                  `Código do município (TSE)` = NA,
                  `Código do município (IBGE)`= NA,
                  `Município` = NA,
                  `Eleitores aptos` = NA,
                  `Municípios aptos para a reeleição de prefeito` = NA,
                  `Taxa de prefeitos aptos a disputar a reeleição` = NA,
                  `Taxa de prefeitos que tentaram a reeleição` = NA,
                  DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
           rename("Ano da eleição" = "ANO_ELEICAO",
                  "UF" = "SIGLA_UF",
                  "Cargo" = "DESCRICAO_CARGO",
                  "Cadeiras disponíveis" = "QTDE_VAGAS",
                  "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL",
                  "Municípios na agregação regional " = "QTDE_MUNICIPIOS_AGREG",
                  "Candidatos tentando a reeleição" = "REAPRESENTACAO",
                  "Reeleitos" = "REELEITOS_AGREG",
                  "Recandidaturas" = "RECANDIDATURAS",
                  "Reeleição bruta" = "REELEICAO_BRUTA",
                  "Reeleição líquida" = "REELEICAO_LIQUIDA",
                  "Renovação bruta" = "RENOVACAO_BRUTA",
                  "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
           select(`Ano da eleição`,
                  `Agregação regional`,
                  UF,
                  `Código do município (TSE)`,
                  `Código do município (IBGE)`,
                  `Município`,
                  Cargo,
                  `Eleitores aptos`,
                  `Municípios na agregação regional `,
                  `Municípios-Vagas com informação disponíveis`,
                  `Cadeiras disponíveis`,
                  `Municípios aptos para a reeleição de prefeito`,
                  `Candidatos tentando a reeleição`,
                  `Reeleitos`,
                  `Taxa de prefeitos aptos a disputar a reeleição`,
                  `Taxa de prefeitos que tentaram a reeleição`,
                  `Reeleição bruta`,
                  `Reeleição líquida`,
                  `Renovação bruta`,
                  `Renovação líquida`,
                  `Recandidaturas`) %>%
           arrange(`Ano da eleição`))
     
  
  #################################### VR_MUN ####################################
  
 } else if(agregacao == "VR_MUN"){
    
    ## Padronizando o formato dos dados
    
    suppressMessages(
    data <- data %>% 
      ungroup() %>% 
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
             `Eleitores aptos` = NA,
             `Municípios na agregação regional ` = NA,
             INFORMACAO_DISPONIVEL = ifelse(is.na(INFORMACAO_DISPONIVEL),
                                            0,
                                            INFORMACAO_DISPONIVEL),
             `Municípios aptos para a reeleição de prefeito` = NA,
             `Taxa de prefeitos aptos a disputar a reeleição` = NA,
             `Taxa de prefeitos que tentaram a reeleição` = NA,
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "UF" = "SIGLA_UF",
             "Cargo" = "DESCRICAO_CARGO",
             "Código do município (TSE)" = "COD_MUN_TSE",
             "Código do município (IBGE)" = "COD_MUN_IBGE",
             "Município" = "NOME_MUNICIPIO",
             "Cadeiras disponíveis" = "QTDE_VAGAS",
             "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL",
             "Candidatos tentando a reeleição" = "REAPRESENTACAO",
             "Reeleitos" = "REELEITOS_AGREG",
             "Recandidaturas" = "RECANDIDATURAS",
             "Reeleição bruta" = "REELEICAO_BRUTA",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação bruta" = "RENOVACAO_BRUTA",
             "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
      select(`Ano da eleição`,
             `Agregação regional`,
              UF,
             `Código do município (TSE)`,
             `Código do município (IBGE)`,
             `Município`,
             Cargo,
             `Eleitores aptos`,
             `Municípios na agregação regional `,
             `Municípios-Vagas com informação disponíveis`,
             `Cadeiras disponíveis`,
             `Municípios aptos para a reeleição de prefeito`,
             `Candidatos tentando a reeleição`,
             `Reeleitos`,
             `Taxa de prefeitos aptos a disputar a reeleição`,
             `Taxa de prefeitos que tentaram a reeleição`,
             `Reeleição bruta`,
             `Reeleição líquida`,
             `Renovação bruta`,
             `Renovação líquida`,
             `Recandidaturas`) %>%
      arrange(`Ano da eleição`,
              UF,
              `Município`))
   
   #################################### VR_ELEIT_APT ###############################
    
 } else if(agregacao == "VR_ELEIT_APT"){
   
   ## Padronizando o formato dos dados
   
   suppressMessages(
     data <- data %>% 
       ungroup() %>% 
       mutate(QTDE_VAGAS = round(QTDE_VAGAS,
                                 digits = 2),
              REAPRESENTACAO = round(REAPRESENTACAO,
                                     digits = 2),
              REELEITOS_AGREG = round(REELEITOS_AGREG,
                                      digits = 2),
              REELEICAO_BRUTA = round(REELEICAO_BRUTA,
                                      digits = 2),
              REELEICAO_LIQUIDA = round(REELEICAO_LIQUIDA,
                                        digits = 2),
              RENOVACAO_BRUTA = round(RENOVACAO_BRUTA,
                                      digits = 2),
              RENOVACAO_LIQUIDA = round(RENOVACAO_LIQUIDA,
                                        digits = 2),
              RECANDIDATURAS = round(RECANDIDATURAS,
                                     digits = 2),
              `Agregação regional` = "Eleitores aptos",
              UF = NA,
              `Código do município (TSE)` = NA,
              `Código do município (IBGE)` = NA,
              `Município` = NA,
              `Municípios aptos para a reeleição de prefeito` = NA,
              `Taxa de prefeitos aptos a disputar a reeleição` = NA,
              `Taxa de prefeitos que tentaram a reeleição` = NA,
              DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
       rename("Ano da eleição" = "ANO_ELEICAO",
              "Cargo" = "DESCRICAO_CARGO",
              "Cadeiras disponíveis" = "QTDE_VAGAS",
              "Eleitores aptos" = "AGREG_ELEITORES_APTOS",
              "Municípios-Vagas com informação disponíveis" = "INFORMACAO_DISPONIVEL",
              "Municípios na agregação regional " = "QTDE_MUNICIPIOS_AGREG",
              "Candidatos tentando a reeleição" = "REAPRESENTACAO",
              "Reeleitos" = "REELEITOS_AGREG",
              "Recandidaturas" = "RECANDIDATURAS",
              "Reeleição bruta" = "REELEICAO_BRUTA",
              "Reeleição líquida" = "REELEICAO_LIQUIDA",
              "Renovação bruta" = "RENOVACAO_BRUTA",
              "Renovação líquida" = "RENOVACAO_LIQUIDA") %>% 
       select(`Ano da eleição`,
              `Agregação regional`,
              UF,
              `Código do município (TSE)`,
              `Código do município (IBGE)`,
              `Município`,
              Cargo,
              `Eleitores aptos`,
              `Municípios na agregação regional `,
              `Municípios-Vagas com informação disponíveis`,
              `Cadeiras disponíveis`,
              `Municípios aptos para a reeleição de prefeito`,
              `Candidatos tentando a reeleição`,
              `Reeleitos`,
              `Taxa de prefeitos aptos a disputar a reeleição`,
              `Taxa de prefeitos que tentaram a reeleição`,
              `Reeleição bruta`,
              `Reeleição líquida`,
              `Renovação bruta`,
              `Renovação líquida`,
              `Recandidaturas`) %>%
       arrange(`Ano da eleição`,
               `Eleitores aptos`))
 
   }
  
}

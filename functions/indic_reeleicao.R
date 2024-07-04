
## Função para calcular os indicadores de 'Reeleição'

indic_reel <- function(candidatos,
                       eleitos,
                       agregacao = c("BR", "UF", 
                                     "PF_BR", "PF_UF", 
                                     "PF_ELEIT_APT", 
                                     "VR_BR", "VR_UF",
                                     "VR_MUN", "VR_ELEIT_APT")){
  
  ## Desabilitando as mensagens do 'dplyr'
  
  options(dplyr.summarise.inform = FALSE)
  
  indicadores_final <- list()
  
  ################################### BR #########################################    
  
  if(agregacao == "BR"){
    
    ## Lista temporária onde os dados com erro serão armazenados
    
    com_erro <- list()
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(2002, 2022, by = 4)){
      
      cat("Lendo", ano, "\n")
      
      ## Salvando o cargo de referência
      
      cargo <- unique(candidatos$DESCRICAO_CARGO)
      
      ## Salvando a quantidade de vagas disponíveis em t0
      
      vagas_t0 <- vagas_dep %>% 
        filter(ANO_ELEICAO == ano &
               DESCRICAO_CARGO == cargo) %>% 
        ungroup() %>% 
        select(SIGLA_UF,
               QTDE_VAGAS) %>% 
        unique() %>% 
        summarise(QTDE_VAGAS = sum(QTDE_VAGAS,
                                   na.rm = TRUE)) %>% 
        pull(QTDE_VAGAS)
      
      ## Banco com os candidatos em t0 
      
      candidatos_t0 <- candidatos %>% 
        filter(ANO_ELEICAO == ano) 
      
      ## Banco com os candidatos eleitos em t0
      
      eleitos_t0 <- eleitos %>% 
        filter(ANO_ELEICAO == ano) 
      
      ## Banco com os candidatos eleitos em t-4
      
      eleitos_t4 <- eleitos %>% 
        filter(ANO_ELEICAO == ano - 4) 
      
      ## Verifica quantos candidatos foram reeleitos e salva o resultado
      
      indicadores2 <- eleitos_t0 %>% 
        filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
        group_by(ANO_ELEICAO) %>% 
        summarise(REELEITOS_AGREG = n()) 
      
      ## Atribui valor 0 caso nenhum dos deputados eleitos em t-4 tenha 
      ## se reelegido em t0
      
      if(nrow(indicadores2) == 0 &
         ((nrow(eleitos_t4) > 0) &
          (nrow(eleitos_t0) > 0))) {
        
        indicadores2 <- indicadores2 %>% 
          ungroup() %>% 
          add_row(ANO_ELEICAO = as.character(ano),
                  REELEITOS_AGREG = 0)
        
        ## Atribui valor NA caso a informação não exista p/ todos os anos de 
        ## referência
        
      } else if(nrow(indicadores2) == 0 &
                ((nrow(eleitos_t4) > 0) |
                 (nrow(eleitos_t0) > 0))) {
        
        indicadores2 <- indicadores2 %>% 
          ungroup() %>% 
          add_row(ANO_ELEICAO = as.character(ano),
                  REELEITOS_AGREG = NA)
        
      }
      
      ## Calcula o total de candidatos eleitos em t-4 que se reapresentaram 
      ## em t0
      
      suppressMessages(
        indicadores1 <- candidatos_t0 %>% 
          filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
          group_by(ANO_ELEICAO,
                   DESCRICAO_CARGO) %>% 
          mutate(REAPRESENTACAO = n()) %>% 
          select(ANO_ELEICAO,
                 DESCRICAO_CARGO,
                 QTDE_VAGAS,
                 REAPRESENTACAO) %>% 
          mutate(QTDE_VAGAS = vagas_t0) %>% 
          unique())
      
      ## Atribuindo 0 nos casos em que nenhum dos deputados eleitos em t-4 se 
      ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
      ## informação exista p/ todos os anos)
      
      if(nrow(indicadores1) == 0 &
         ((nrow(eleitos_t4) > 0) &
          (nrow(eleitos_t0) > 0))){
        
        indicadores1 <- indicadores1 %>% 
          ungroup() %>% 
          add_row(ANO_ELEICAO = as.character(ano),
                  DESCRICAO_CARGO = cargo,
                  QTDE_VAGAS = vagas_t0,
                  REAPRESENTACAO = 0)
        
        ## Atribui valor NA caso a informação não exista p/ todos os anos de 
        ## referência
        
      } else if(nrow(indicadores1) == 0 &
                ((nrow(eleitos_t4) == 0) |
                 (nrow(eleitos_t0) == 0))){
        
        indicadores1 <- indicadores1 %>% 
          ungroup() %>% 
          add_row(ANO_ELEICAO = as.character(ano),
                  DESCRICAO_CARGO = cargo,
                  QTDE_VAGAS = vagas_t0,
                  REAPRESENTACAO = NA)
        
      }
      
      ## Juntando as informações dos deputados que se recandidataram
      ## em t0, bem como dos que foram reeleitos em t0
      
      suppressMessages(
        indicadores1 <- indicadores1 %>%
          left_join(indicadores2) %>% 
          mutate(INFORMACAO_DISPONIVEL = nrow(eleitos_t0)) %>% 
          select(ANO_ELEICAO,
                 DESCRICAO_CARGO,
                 INFORMACAO_DISPONIVEL,
                 QTDE_VAGAS,
                 REAPRESENTACAO,
                 REELEITOS_AGREG) %>% 
          unique())
      
      ## Se a informação não está completa para alguma das variáveis-chave,
      ## torna todas elas NA
      
      if(is.na(indicadores1$REAPRESENTACAO) |
         is.na(indicadores1$REELEITOS_AGREG)){
        
        indicadores1 <- indicadores1 %>% 
          mutate(REAPRESENTACAO = NA,
                 REELEITOS_AGREG = NA,
                 INFORMACAO_DISPONIVEL = NA)
        
        ## Empilhando o ano com erro em um banco adicional
        
        com_erro <- bind_rows(com_erro,
                              indicadores1)
        
        ## Salvando os municípios com problemas
        
        saveRDS(com_erro,
                "data/output/reeleicao_deputados_anos_com_erro.rds")
        
      }
      
      ## Calcula os indicadores de 'Reeleição'
      
      indicadores1 <- indicadores1 %>% 
        mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
               DESISTENCIA = INFORMACAO_DISPONIVEL - REAPRESENTACAO,
               REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, INFORMACAO_DISPONIVEL),
               REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                 DERROTADOS),
               RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
               RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
               RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                               INFORMACAO_DISPONIVEL))
      
      ## Empilha os indicadores calculados no banco criado
      
      indicadores_final <- bind_rows(indicadores_final, 
                                     indicadores1)
    } 
    
    ###################################### UF ######################################   
    
  } else if(agregacao == "UF"){
    
    ## Lista dos estados brasileiros
    
    ufs <- c("AC", "AL", "AP", "AM", "BA", 
             "CE", "DF", "ES", "GO", "MA", 
             "MT", "MS", "MG", "PA", "PB", 
             "PR", "PE", "PI", "RJ", "RN", 
             "RS", "RO", "RR", "SC", "SP", 
             "SE", "TO")
    
    ## Lista temporária onde os dados com erro e p/ conferência serão armazenados
    
    com_erro <- list()
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(2002, 2022, by = 4)){
      
      for(uf in ufs){
        
        cat("Lendo", ano, uf, "\n")
        
        ## Salvando o cargo de referência
        
        cargo <- unique(candidatos$DESCRICAO_CARGO)
        
        ## Salvando a quantidade de vagas disponíveis em t0
        
        vagas_t0 <- vagas_dep %>% 
          filter(ANO_ELEICAO == ano &
                 SIGLA_UF == uf &
                 DESCRICAO_CARGO == cargo) %>% 
          ungroup() %>% 
          select(SIGLA_UF,
                 QTDE_VAGAS) %>% 
          unique() %>% 
          pull(QTDE_VAGAS)
        
        ## Banco com os candidatos em t0 
        
        candidatos_t0 <- candidatos %>% 
          filter(ANO_ELEICAO == ano &
                   SIGLA_UF == uf) 
        
        ## Banco com os candidatos eleitos em t0
        
        eleitos_t0 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                   SIGLA_UF == uf) 
        
        ## Banco com os candidatos eleitos em t-4
        
        eleitos_t4 <- eleitos %>% 
          filter(ANO_ELEICAO == ano - 4 &
                   SIGLA_UF == uf) 
        
        ## Verifica quantos candidatos foram reeleitos e salva o resultado
        
        suppressMessages(
          indicadores2 <- eleitos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     SIGLA_UF) %>% 
            summarise(REELEITOS_AGREG = n())) 
        
        ## Atribui valor 0 caso nenhum dos deputados eleitos em t-4 tenha 
        ## se reelegido em t0
        
        if(nrow(indicadores2) == 0 &
           ((nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = uf,
                    REELEITOS_AGREG = 0)
          
          ## Atribui valor NA caso a informação não exista p/ todos os anos de 
          ## referência
          
        } else if(nrow(indicadores2) == 0 &
                  ((nrow(eleitos_t4) > 0) |
                   (nrow(eleitos_t0) > 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = uf,
                    REELEITOS_AGREG = NA)
          
        }
        
        ## Calcula o total de candidatos eleitos em t-4 que se reapresentaram 
        ## em t0
        
        suppressMessages(
          indicadores1 <- candidatos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF) %>% 
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   SIGLA_UF,
                   QTDE_VAGAS,
                   REAPRESENTACAO) %>% 
            mutate(QTDE_VAGAS = vagas_t0) %>% 
            unique())
        
        ## Atribuindo 0 nos casos em que nenhum dos deputados eleitos em t-4 se 
        ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
        ## informação exista p/ todos os anos)
        
        if(nrow(indicadores1) == 0 &
           ((nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))){
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    DESCRICAO_CARGO = cargo,
                    SIGLA_UF = uf,
                    QTDE_VAGAS = vagas_t0,
                    REAPRESENTACAO = 0)
          
          ## Atribui valor NA caso a informação não exista p/ todos os anos de 
          ## referência
          
        } else if(nrow(indicadores1) == 0 &
                  ((nrow(eleitos_t4) == 0) |
                   (nrow(eleitos_t0) == 0))){
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    DESCRICAO_CARGO = cargo,
                    SIGLA_UF = uf,
                    QTDE_VAGAS = vagas_t0,
                    REAPRESENTACAO = NA)
          
        }
        
        ## Juntando as informações dos deputados que se recandidataram
        ## em t0, bem como dos que foram reeleitos em t0
        
        suppressMessages(
          indicadores1 <- indicadores1 %>%
            left_join(indicadores2) %>% 
            mutate(INFORMACAO_DISPONIVEL = nrow(eleitos_t0)) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   SIGLA_UF,
                   INFORMACAO_DISPONIVEL,
                   QTDE_VAGAS,
                   REAPRESENTACAO,
                   REELEITOS_AGREG) %>% 
            unique())
        
        ## Se a informação não está completa para alguma das variáveis-chave,
        ## torna todas elas NA
        
        if(is.na(indicadores1$REAPRESENTACAO) |
           is.na(indicadores1$REELEITOS_AGREG)){
          
          indicadores1 <- indicadores1 %>% 
            mutate(REAPRESENTACAO = NA,
                   REELEITOS_AGREG = NA,
                   INFORMACAO_DISPONIVEL = NA)
          
          ## Empilhando o ano com erro em um banco adicional
          
          com_erro <- bind_rows(com_erro,
                                indicadores1)
          
          ## Salvando os municípios com problemas
          
          saveRDS(com_erro,
                  "data/output/reeleicao_deputados_anos_com_erro.rds")
          
        }
        
        ## Calcula os indicadores de 'Reeleição'
        
        indicadores1 <- indicadores1 %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                 DESISTENCIA = INFORMACAO_DISPONIVEL - REAPRESENTACAO,
                 REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, INFORMACAO_DISPONIVEL),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                   DERROTADOS),
                 RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
                 RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                 RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                 INFORMACAO_DISPONIVEL))
        
        ## Empilha os indicadores calculados no banco criado
        
        indicadores_final <- bind_rows(indicadores_final, 
                                       indicadores1)
        
      }
    } 
    
    ################################### PF_BR ####################################      
    
  } else if(agregacao == "PF_BR"){
    
    ## Lista temporária onde os dados com erro e p/ conferência serão armazenados
    
    com_erro <- list()
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano, uf e agregação regional
    
    for(ano in seq(2008, 2020, by = 4)){
      
      ## Lista temporária onde os dados serão armazenados
      
      temp <- list()
      
      temp2 <- list()
      
      ## Lista de municípios brasileiros
      
      suppressMessages(
        num_municipios <- vagas_ver %>%
          filter(ANO_ELEICAO == ano &
                 !is.na(QTDE_VAGAS)) %>% 
          ungroup() %>% 
          select(SIGLA_UF,
                 COD_MUN_TSE,
                 COD_MUN_IBGE,
                 NOME_MUNICIPIO,
                 QTDE_VAGAS) %>% 
          unique() %>% 
          arrange(SIGLA_UF,
                  NOME_MUNICIPIO))
      
      ## Salvando o número de municípios na agregação 
      
      municipios_agreg <- nrow(num_municipios)
      
      for(municipio in 1:nrow(num_municipios)){
        
        cat("Lendo", ano, "município", municipio, "de", nrow(num_municipios), "\n")
        
        ## Verificando qual candidato foi eleito em t-8
        
        eleitos_t8 <- eleitos %>% 
          filter(ANO_ELEICAO == ano - 8 &
                 COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
          mutate(ELEITO_T8 = 1) %>% 
          ungroup() %>% 
          select(SIGLA_UF,
                 COD_MUN_TSE,
                 COD_MUN_IBGE,
                 ID_CEPESP,
                 ELEITO_T8)
        
        ## Verificando qual candidato foi eleito em t-4
        
        suppressMessages(
          eleitos_t4 <- eleitos %>% 
            filter(ANO_ELEICAO == ano - 4 &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
            mutate(ELEITO_T4 = 1))
        
        ## Condição p/ calcular o indicador de candidatos passíveis de se recandidatar
        ## em t0
        
        if(nrow(eleitos_t8) > 0){
          
          suppressMessages(
            eleitos_t4 <- eleitos_t4 %>% 
              left_join(eleitos_t8) %>% 
              mutate(ELEITO_T8 = ifelse(is.na(ELEITO_T8),
                                        0,
                                        ELEITO_T8),
                     PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                            ELEITO_T8 == 1,
                                          0,
                                          1)) %>% 
              ungroup() %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     ID_CEPESP,
                     ELEITO_T4,
                     ELEITO_T8,
                     PERMIT_CAND))
          
        } else if(nrow(eleitos_t8) == 0) {
          
          suppressMessages(
            eleitos_t4 <- eleitos_t4 %>% 
              left_join(eleitos_t8) %>% 
              mutate(PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                            ELEITO_T8 == 1,
                                          0,
                                          1)) %>% 
              ungroup() %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     ID_CEPESP,
                     ELEITO_T4,
                     ELEITO_T8,
                     PERMIT_CAND))
          
        }
        
        ## Verificando qual candidato foi eleito em t0
        
        eleitos_t0 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
        
        ## Verifica se o candidato eleito em t-4 se reelegeu em t0
        
        suppressMessages(
          indicadores2 <- eleitos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE) %>% 
            summarise(REELEITOS_AGREG = n()))
        
        ## Atribui valor 0 caso o prefeito eleito em t-4 não tenha 
        ## se reelegido em t0 (desde que a informação exista p/ todos os anos)
        
        if(nrow(indicadores2) == 0 &
           ((nrow(eleitos_t8) > 0) &
            (nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    REELEITOS_AGREG = 0)
          
          ## Atribui valor NA caso a informação não exista p/ todos os anos
          
        } else if(nrow(indicadores2) == 0 &
                  ((nrow(eleitos_t8) == 0) |
                   (nrow(eleitos_t4) == 0) |
                   (nrow(eleitos_t0) == 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    REELEITOS_AGREG = NA)
          
        }
        
        ## Verifica se o prefeito eleito em t-4 se recandidatou em t0
        
        suppressMessages(
          indicadores1 <- candidatos %>% 
            filter(ANO_ELEICAO == ano &
                   SIGLA_UF == num_municipios$SIGLA_UF[municipio] &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            ungroup() %>% 
            select(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   DESCRICAO_CARGO,
                   ID_CEPESP) %>% 
            unique()) 
        
        ## Salva se o prefeito eleito em t-4 se candidatou em t0
        
        indicadores1 <- indicadores1 %>% 
          group_by(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   DESCRICAO_CARGO) %>% 
          summarise(REAPRESENTACAO = n())
        
        ## Atribuindo 0 nos casos em que o prefeito eleito em t-4 não se 
        ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
        ## informação exista p/ todos os anos)
        
        if(nrow(indicadores1) == 0 &
           ((nrow(eleitos_t8) > 0) &
            (nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))){
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                    DESCRICAO_CARGO = "PREFEITO") %>% 
            mutate(REAPRESENTACAO = 0)
          
        } else if(nrow(indicadores1) == 0 &
                  ((nrow(eleitos_t8) == 0) |
                   (nrow(eleitos_t4) == 0) |
                   (nrow(eleitos_t0) == 0))) {
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                    DESCRICAO_CARGO = "PREFEITO") %>% 
            mutate(REAPRESENTACAO = NA)
          
        } 
        
        ## Removendo as colunas que não serão mais utilizadas
        
        eleitos_t4 <- eleitos_t4 %>% 
          select(SIGLA_UF,
                 COD_MUN_TSE,
                 PERMIT_CAND)
        
        ## Juntando com as informações dos prefeitos que podiam se 
        ## recandidatar em t0, bem como dos que foram reeleitos em t0
        
        suppressMessages(
          indicadores1 <- indicadores1 %>% 
            left_join(eleitos_t4) %>%
            left_join(indicadores2) %>% 
            mutate(QTDE_VAGAS = NA,
                   QTDE_MUNICIPIOS_AGREG = municipios_agreg,
                   INFORMACAO_DISPONIVEL = 1) %>% 
            select(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   DESCRICAO_CARGO,
                   INFORMACAO_DISPONIVEL,
                   QTDE_MUNICIPIOS_AGREG,
                   QTDE_VAGAS,
                   PERMIT_CAND,
                   REAPRESENTACAO,
                   REELEITOS_AGREG) %>% 
            unique())
        
        ## Se a informação não está completa para alguma das variáveis-chave,
        ## torna todas elas NA
        
        if(is.na(indicadores1$PERMIT_CAND) |
           is.na(indicadores1$REAPRESENTACAO) |
           is.na(indicadores1$REELEITOS_AGREG)){
          
          indicadores1 <- indicadores1 %>% 
            mutate(PERMIT_CAND = NA,
                   REAPRESENTACAO = NA,
                   REELEITOS_AGREG = NA,
                   INFORMACAO_DISPONIVEL = NA)
          
          ## Empilhando o município com erro em um banco adicional
          
          com_erro <- bind_rows(com_erro,
                                indicadores1)
          
          ## Salvando os municípios com problemas
          
          saveRDS(com_erro,
                  "data/output/reeleicao_prefeitos_municipos_br_com_erro.rds")
          
        }
        
        ## Empilha os indicadores calculados no banco criado
        
        temp <- bind_rows(temp, 
                          indicadores1)
        
      }
      
      ## Calcula os indicadores de 'Reeleição' na agregação de referência
      
      suppressMessages(
        temp2 <- temp %>% 
          group_by(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   QTDE_MUNICIPIOS_AGREG,
                   QTDE_VAGAS) %>% 
          summarise(INFORMACAO_DISPONIVEL = sum(INFORMACAO_DISPONIVEL, 
                                                na.rm = TRUE),
                    PERMIT_CAND = sum(PERMIT_CAND, 
                                      na.rm = TRUE),
                    REAPRESENTACAO = sum(REAPRESENTACAO,
                                         na.rm = TRUE),
                    REELEITOS_AGREG = sum(REELEITOS_AGREG,
                                          na.rm = TRUE)) %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                 DESISTENCIA = INFORMACAO_DISPONIVEL - REAPRESENTACAO,
                 TX_PREFEITOS_PERMIT_REEL = PERMIT_CAND/INFORMACAO_DISPONIVEL,
                 TX_PREFEITOS_RECAND = REAPRESENTACAO/PERMIT_CAND,
                 REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, INFORMACAO_DISPONIVEL),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                   DERROTADOS)) %>% 
          mutate(across(DERROTADOS:REELEICAO_LIQUIDA, ~ifelse(is.nan(.),
                                                              0,
                                                              .))))
      
      ## Empilhando os resultados agregados por uf no arquivo final
      
      indicadores_final <- bind_rows(indicadores_final,
                                     temp2)
      
      ## Salvando o progresso para conferência
      
      saveRDS(indicadores_final,
              "data/output/reel_pf_br_temp.rds")
      
      
    }
    
    ################################# PF_UF ########################################
    
  } else if(agregacao == "PF_UF"){
    
    ## Lista dos estados brasileiros
    
    ufs <- c("AC", "AL", "AP", "AM", "BA", 
             "CE", "ES", "GO", "MA", 
             "MT", "MS", "MG", "PA", "PB", 
             "PR", "PE", "PI", "RJ", "RN", 
             "RS", "RO", "RR", "SC", "SP", 
             "SE", "TO")
    
    ## Lista temporária onde os dados com erro e p/ conferência serão armazenados
    
    com_erro <- list()
    
    desagreg <- list()
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano, uf e agregação regional
    
    for(ano in seq(2008, 2020, by = 4)){
      
      for(uf in ufs){
        
        ## Lista temporária onde os dados serão armazenados
        
        temp <- list()
        
        temp2 <- list()
        
        ## Verificando quantos municípios existem no estado
        
        num_municipios <- vagas_ver %>% 
          filter(ANO_ELEICAO == ano & 
                 SIGLA_UF == uf) %>% 
          filter(!is.na(QTDE_VAGAS)) %>% 
          ungroup() %>% 
          select(SIGLA_UF,
                 COD_MUN_TSE,
                 COD_MUN_IBGE,
                 NOME_MUNICIPIO) %>% 
          unique() %>% 
          arrange(SIGLA_UF,
                  NOME_MUNICIPIO)
        
        ## Salvando o número de municípios na agregação 
        
        municipios_agreg <- nrow(num_municipios)
        
        for(municipio in 1:nrow(num_municipios)){
          
          cat("Lendo", ano, uf, "município", municipio, "de", nrow(num_municipios), "\n")
          
          ## Verificando qual candidato foi eleito em t-8
          
          eleitos_t8 <- eleitos %>% 
            filter(ANO_ELEICAO == ano - 8 &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
            mutate(ELEITO_T8 = 1) %>% 
            ungroup() %>% 
            select(SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   ID_CEPESP,
                   ELEITO_T8)
          
          ## Verificando qual candidato foi eleito em t-4
          
          suppressMessages(
            eleitos_t4 <- eleitos %>% 
              filter(ANO_ELEICAO == ano - 4 &
                       COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
              mutate(ELEITO_T4 = 1))
          
          ## Condição p/ calcular o indicador de candidatos passíveis de se recandidatar
          ## em t0
          
          if(nrow(eleitos_t8) > 0){
            
            suppressMessages(
              eleitos_t4 <- eleitos_t4 %>% 
                left_join(eleitos_t8) %>% 
                mutate(ELEITO_T8 = ifelse(is.na(ELEITO_T8),
                                          0,
                                          ELEITO_T8),
                       PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                              ELEITO_T8 == 1,
                                            0,
                                            1)) %>% 
                ungroup() %>% 
                select(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE,
                       ID_CEPESP,
                       ELEITO_T4,
                       ELEITO_T8,
                       PERMIT_CAND))
            
          } else if(nrow(eleitos_t8) == 0) {
            
            suppressMessages(
              eleitos_t4 <- eleitos_t4 %>% 
                left_join(eleitos_t8) %>% 
                mutate(PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                              ELEITO_T8 == 1,
                                            0,
                                            1)) %>% 
                ungroup() %>% 
                select(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE,
                       ID_CEPESP,
                       ELEITO_T4,
                       ELEITO_T8,
                       PERMIT_CAND))
            
          }
          
          ## Verificando qual candidato foi eleito em t0
          
          eleitos_t0 <- eleitos %>% 
            filter(ANO_ELEICAO == ano &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
          
          ## Verifica se o candidato eleito em t-4 se reelegeu em t0
          
          suppressMessages(
            indicadores2 <- eleitos_t0 %>% 
              filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
              group_by(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE) %>% 
              summarise(REELEITOS_AGREG = n()))
          
          ## Atribui valor 0 caso o prefeito eleito em t-4 não tenha 
          ## se reelegido em t0 (desde que a informação exista p/ todos os anos)
          
          if(nrow(indicadores2) == 0 &
             ((nrow(eleitos_t8) > 0) &
              (nrow(eleitos_t4) > 0) &
              (nrow(eleitos_t0) > 0))) {
            
            indicadores2 <- indicadores2 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = uf,
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      REELEITOS_AGREG = 0)
            
            ## Atribui valor NA caso a informação não exista p/ todos os anos
            
          } else if(nrow(indicadores2) == 0 &
                    ((nrow(eleitos_t8) == 0) |
                     (nrow(eleitos_t4) == 0) |
                     (nrow(eleitos_t0) == 0))) {
            
            indicadores2 <- indicadores2 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = uf,
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      REELEITOS_AGREG = NA)
            
          }
          
          ## Verifica se o prefeito eleito em t-4 se recandidatou em t0
          
          suppressMessages(
            indicadores1 <- candidatos %>% 
              filter(ANO_ELEICAO == ano &
                       SIGLA_UF == uf &
                       COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
              filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
              ungroup() %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     DESCRICAO_CARGO,
                     ID_CEPESP) %>% 
              unique()) 
          
          ## Salva se o prefeito eleito em t-4 se candidatou em t0
          
          indicadores1 <- indicadores1 %>% 
            group_by(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     DESCRICAO_CARGO) %>% 
            summarise(REAPRESENTACAO = n())
          
          ## Atribuindo 0 nos casos em que o prefeito eleito em t-4 não se 
          ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
          ## informação exista p/ todos os anos)
          
          if(nrow(indicadores1) == 0 &
             ((nrow(eleitos_t8) > 0) &
              (nrow(eleitos_t4) > 0) &
              (nrow(eleitos_t0) > 0))){
            
            indicadores1 <- indicadores1 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = uf,
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                      DESCRICAO_CARGO = "PREFEITO") %>% 
              mutate(REAPRESENTACAO = 0)
            
          } else if(nrow(indicadores1) == 0 &
                    ((nrow(eleitos_t8) == 0) |
                     (nrow(eleitos_t4) == 0) |
                     (nrow(eleitos_t0) == 0))) {
            
            indicadores1 <- indicadores1 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = uf,
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                      DESCRICAO_CARGO = "PREFEITO") %>% 
              mutate(REAPRESENTACAO = NA)
            
          } 
          
          ## Removendo as colunas que não serão mais utilizadas
          
          eleitos_t4 <- eleitos_t4 %>% 
            select(SIGLA_UF,
                   COD_MUN_TSE,
                   PERMIT_CAND)
          
          ## Juntando com as informações dos prefeitos que podiam se 
          ## recandidatar em t0, bem como dos que foram reeleitos em t0
          
          suppressMessages(
            indicadores1 <- indicadores1 %>% 
              left_join(eleitos_t4) %>%
              left_join(eleitores_aptos) %>% 
              left_join(indicadores2) %>% 
              mutate(QTDE_VAGAS = NA,
                     QTDE_MUNICIPIOS_AGREG = municipios_agreg,
                     INFORMACAO_DISPONIVEL = 1) %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     DESCRICAO_CARGO,
                     AGREG_ELEITORES_APTOS,
                     INFORMACAO_DISPONIVEL,
                     QTDE_MUNICIPIOS_AGREG,
                     QTDE_VAGAS,
                     PERMIT_CAND,
                     REAPRESENTACAO,
                     REELEITOS_AGREG) %>% 
              unique())
          
          ## Se a informação não está completa para alguma das variáveis-chave,
          ## torna todas elas NA
          
          if(is.na(indicadores1$PERMIT_CAND) |
             is.na(indicadores1$REAPRESENTACAO) |
             is.na(indicadores1$REELEITOS_AGREG)){
            
            indicadores1 <- indicadores1 %>% 
              mutate(PERMIT_CAND = NA,
                     REAPRESENTACAO = NA,
                     REELEITOS_AGREG = NA,
                     INFORMACAO_DISPONIVEL = NA)
            
            ## Empilhando o município com erro em um banco adicional
            
            com_erro <- bind_rows(com_erro,
                                  indicadores1)
            
            ## Salvando os municípios com problemas
            
            saveRDS(com_erro,
                    "data/output/reeleicao_prefeitos_municipos_com_erro.rds")
            
          }
          
          ## Empilha os indicadores calculados no banco criado
          
          temp <- bind_rows(temp, 
                            indicadores1)
          
        }
        
        ## Calcula os indicadores de 'Reeleição' na agregação de referência
        
        suppressMessages(
          temp2 <- temp %>% 
            group_by(ANO_ELEICAO,
                     SIGLA_UF,
                     DESCRICAO_CARGO,
                     QTDE_MUNICIPIOS_AGREG,
                     QTDE_VAGAS) %>% 
            summarise(INFORMACAO_DISPONIVEL = sum(INFORMACAO_DISPONIVEL, 
                                                  na.rm = TRUE),
                      PERMIT_CAND = sum(PERMIT_CAND, 
                                        na.rm = TRUE),
                      REAPRESENTACAO = sum(REAPRESENTACAO,
                                           na.rm = TRUE),
                      REELEITOS_AGREG = sum(REELEITOS_AGREG,
                                            na.rm = TRUE)) %>% 
            mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                   DESISTENCIA = INFORMACAO_DISPONIVEL - REAPRESENTACAO,
                   TX_PREFEITOS_PERMIT_REEL = PERMIT_CAND/INFORMACAO_DISPONIVEL,
                   TX_PREFEITOS_RECAND = REAPRESENTACAO/PERMIT_CAND,
                   REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, INFORMACAO_DISPONIVEL),
                   REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                     DERROTADOS)) %>% 
            mutate(across(DERROTADOS:REELEICAO_LIQUIDA, ~ifelse(is.nan(.),
                                                                0,
                                                                .))))
        
        ## Empilhando os resultados agregados por uf no arquivo final
        
        indicadores_final <- bind_rows(indicadores_final,
                                       temp2)
        
        ## Empilha os dados desagregados em um arquivo p/ conferência
        
        desagreg <- bind_rows(desagreg, 
                              temp)
        
        ## Salvando o banco desagregado para conferência
        
        saveRDS(desagreg,
                "data/output/reel_pf_uf_desagreg_temp.rds")
        
        ## Salvando o progresso para conferência
        
        saveRDS(indicadores_final,
                "data/output/reel_pf_uf_temp.rds")
        
      }
    }
    
    ############################# PF_ELEITO_APT ####################################    
    
  } else if(agregacao == "PF_ELEIT_APT"){
    
    ## Lista das faixas de eleitores aptos
    
    agreg_eleitores <- as.character(sort(unique(eleitores_aptos$AGREG_ELEITORES_APTOS)))
    
    ## Lista temporária onde os dados com erro e p/ conferência serão armazenados
    
    com_erro <- list()
    
    desagreg <- list()
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano e agregação regional
    
    for(ano in seq(2008, 2020, by = 4)){
      
      for(agreg in agreg_eleitores){
        
        ## Lista temporária onde os dados serão armazenados
        
        temp <- list()
        
        temp2 <- list()
        
        ## Verificando quantos municípios existem na agregação
        
        suppressMessages(
          num_municipios <- vagas_ver %>% 
            left_join(eleitores_aptos) %>% 
            filter(ANO_ELEICAO == ano & 
                   AGREG_ELEITORES_APTOS == agreg) %>% 
            filter(!is.na(QTDE_VAGAS)) %>% 
            ungroup() %>% 
            select(AGREG_ELEITORES_APTOS,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO) %>% 
            unique() %>% 
            arrange(AGREG_ELEITORES_APTOS,
                    SIGLA_UF,
                    NOME_MUNICIPIO))
        
        ## Salvando o número de municípios na agregação 
        
        municipios_agreg <- nrow(num_municipios)
        
        for(municipio in 1:nrow(num_municipios)){
          
          cat("Lendo", ano, agreg, "município", municipio, "de", nrow(num_municipios), "\n")
          
          
          ## Verificando qual candidato foi eleito em t-8
          
          eleitos_t8 <- eleitos %>% 
            filter(ANO_ELEICAO == ano - 8 &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
            mutate(ELEITO_T8 = 1) %>% 
            ungroup() %>% 
            select(SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   ID_CEPESP,
                   ELEITO_T8)
          
          ## Verificando qual candidato foi eleito em t-4
          
          suppressMessages(
            eleitos_t4 <- eleitos %>% 
              filter(ANO_ELEICAO == ano - 4 &
                       COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
              mutate(ELEITO_T4 = 1))
          
          ## Condição p/ calcular o indicador de candidatos passíveis de se recandidatar
          ## em t0
          
          if(nrow(eleitos_t8) > 0){
            
            suppressMessages(
              eleitos_t4 <- eleitos_t4 %>% 
                left_join(eleitos_t8) %>% 
                mutate(ELEITO_T8 = ifelse(is.na(ELEITO_T8),
                                          0,
                                          ELEITO_T8),
                       PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                              ELEITO_T8 == 1,
                                            0,
                                            1)) %>% 
                ungroup() %>% 
                select(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE,
                       ID_CEPESP,
                       ELEITO_T4,
                       ELEITO_T8,
                       PERMIT_CAND))
            
          } else if(nrow(eleitos_t8) == 0) {
            
            suppressMessages(
              eleitos_t4 <- eleitos_t4 %>% 
                left_join(eleitos_t8) %>% 
                mutate(PERMIT_CAND = ifelse(ELEITO_T4 == 1 &
                                              ELEITO_T8 == 1,
                                            0,
                                            1)) %>% 
                ungroup() %>% 
                select(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE,
                       ID_CEPESP,
                       ELEITO_T4,
                       ELEITO_T8,
                       PERMIT_CAND))
            
          }
          
          ## Verificando qual candidato foi eleito em t0
          
          eleitos_t0 <- eleitos %>% 
            filter(ANO_ELEICAO == ano &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
          
          ## Verifica se o candidato eleito em t-4 se reelegeu em t0
          
          suppressMessages(
            indicadores2 <- eleitos_t0 %>% 
              filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
              group_by(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE) %>% 
              summarise(REELEITOS_AGREG = n()))
          
          ## Atribui valor 0 caso o prefeito eleito em t-4 não tenha 
          ## se reelegido em t0 (desde que a informação exista p/ todos os anos)
          
          if(nrow(indicadores2) == 0 &
             ((nrow(eleitos_t8) > 0) &
              (nrow(eleitos_t4) > 0) &
              (nrow(eleitos_t0) > 0))) {
            
            indicadores2 <- indicadores2 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      REELEITOS_AGREG = 0)
            
            ## Atribui valor NA caso a informação não exista p/ todos os anos
            
          } else if(nrow(indicadores2) == 0 &
                    ((nrow(eleitos_t8) == 0) |
                     (nrow(eleitos_t4) == 0) |
                     (nrow(eleitos_t0) == 0))) {
            
            indicadores2 <- indicadores2 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      REELEITOS_AGREG = NA)
            
          }
          
          ## Verifica se o prefeito eleito em t-4 se recandidatou em t0
          
          suppressMessages(
            indicadores1 <- candidatos %>% 
              filter(ANO_ELEICAO == ano &
                       SIGLA_UF == num_municipios$SIGLA_UF[municipio] &
                       COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>% 
              filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
              ungroup() %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     DESCRICAO_CARGO) %>% 
              unique()) 
          
          ## Salva se o prefeito eleito em t-4 se candidatou em t0
          
          indicadores1 <- indicadores1 %>% 
            group_by(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     DESCRICAO_CARGO) %>% 
            mutate(REAPRESENTACAO = n())
          
          ## Atribuindo 0 nos casos em que o prefeito eleito em t-4 não se 
          ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
          ## informação exista p/ todos os anos)
          
          if(nrow(indicadores1) == 0 &
             ((nrow(eleitos_t8) > 0) &
              (nrow(eleitos_t4) > 0) &
              (nrow(eleitos_t0) > 0))){
            
            indicadores1 <- indicadores1 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                      DESCRICAO_CARGO = "PREFEITO") %>% 
              mutate(REAPRESENTACAO = 0)
            
          } else if(nrow(indicadores1) == 0 &
                    ((nrow(eleitos_t8) == 0) |
                     (nrow(eleitos_t4) == 0) |
                     (nrow(eleitos_t0) == 0))) {
            
            indicadores1 <- indicadores1 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                      DESCRICAO_CARGO = "PREFEITO") %>% 
              mutate(REAPRESENTACAO = NA)
            
          } 
          
          ## Removendo as colunas que não serão mais utilizadas
          
          eleitos_t4 <- eleitos_t4 %>% 
            select(SIGLA_UF,
                   COD_MUN_TSE,
                   PERMIT_CAND)
          
          ## Juntando com as informações dos prefeitos que podiam se 
          ## recandidatar em t0, bem como dos que foram reeleitos em t0
          
          suppressMessages(
            indicadores1 <- indicadores1 %>% 
              left_join(eleitos_t4) %>%
              left_join(eleitores_aptos) %>% 
              left_join(indicadores2) %>% 
              mutate(QTDE_VAGAS = NA,
                     QTDE_MUNICIPIOS_AGREG = municipios_agreg,
                     INFORMACAO_DISPONIVEL = 1) %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     DESCRICAO_CARGO,
                     AGREG_ELEITORES_APTOS,
                     INFORMACAO_DISPONIVEL,
                     QTDE_MUNICIPIOS_AGREG,
                     QTDE_VAGAS,
                     PERMIT_CAND,
                     REAPRESENTACAO,
                     REELEITOS_AGREG) %>% 
              unique())
          
          if(is.na(indicadores1$PERMIT_CAND) |
             is.na(indicadores1$REAPRESENTACAO) |
             is.na(indicadores1$REELEITOS_AGREG)){
            
            ## Se a informação não está completa para alguma das variáveis-chave,
            ## torna todas elas NA
            
            indicadores1 <- indicadores1 %>% 
              mutate(PERMIT_CAND = NA,
                     REAPRESENTACAO = NA,
                     REELEITOS_AGREG = NA,
                     INFORMACAO_DISPONIVEL = NA)
            
            ## Empilhando o município com erro em um banco adicional
            
            com_erro <- bind_rows(com_erro,
                                  indicadores1)
            
            ## Salvando os municípios com problemas
            
            saveRDS(com_erro,
                    "data/output/reeleicao_prefeitos_municipos_elt_apt_com_erro.rds")
            
          }
          
          ## Empilha os indicadores calculados no banco criado
          
          temp <- bind_rows(temp, 
                            indicadores1)
          
        }
        
        ## Empilha os dados desagregados
        
        desagreg <- bind_rows(desagreg, 
                              temp)
        
        ## Salvando o banco desagregado para conferência
        
        saveRDS(desagreg,
                "data/output/reel_pf_elt_apt_desagreg_temp.rds")
        
        ## Calcula os indicadores de 'Reeleição' na agregação de referência
        
        suppressMessages(
          temp2 <- temp %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     AGREG_ELEITORES_APTOS,
                     QTDE_MUNICIPIOS_AGREG,
                     QTDE_VAGAS) %>% 
            summarise(INFORMACAO_DISPONIVEL = sum(INFORMACAO_DISPONIVEL, 
                                                  na.rm = TRUE),
                      PERMIT_CAND = sum(PERMIT_CAND, 
                                        na.rm = TRUE),
                      REAPRESENTACAO = sum(REAPRESENTACAO,
                                           na.rm = TRUE),
                      REELEITOS_AGREG = sum(REELEITOS_AGREG,
                                            na.rm = TRUE)) %>% 
            mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                   DESISTENCIA = INFORMACAO_DISPONIVEL - REAPRESENTACAO,
                   TX_PREFEITOS_PERMIT_REEL = PERMIT_CAND/INFORMACAO_DISPONIVEL,
                   TX_PREFEITOS_RECAND = REAPRESENTACAO/PERMIT_CAND,
                   REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, INFORMACAO_DISPONIVEL),
                   REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                     DERROTADOS)) %>% 
            mutate(across(DERROTADOS:REELEICAO_LIQUIDA, ~ifelse(is.nan(.),
                                                                0,
                                                                .))))
        
        ## Empilhando os resultados agregados por uf no arquivo final
        
        indicadores_final <- bind_rows(indicadores_final,
                                       temp2)
        
        ## Salvando o progresso para conferência
        
        saveRDS(indicadores_final,
                "data/output/reel_pf_elt_apt_temp.rds")
      }
    }
    
    ################################## VR_BR ##########################################  
    
  } else if(agregacao == "VR_BR"){
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano e agregação regional
    
    for(ano in seq(2004, 2020, by = 4)){
      
      ## Lista temporária onde os dados serão armazenados
      
      temp <- list()
      
      temp2 <- list()
      
      ## Verificando quantos municípios existem na agregação
      
      suppressMessages(
        num_municipios <- vagas_ver %>%
          filter(ANO_ELEICAO == ano &
                 !is.na(QTDE_VAGAS)) %>% 
          ungroup() %>% 
          select(SIGLA_UF,
                 COD_MUN_TSE,
                 COD_MUN_IBGE,
                 NOME_MUNICIPIO,
                 QTDE_VAGAS) %>% 
          unique() %>% 
          arrange(SIGLA_UF,
                  NOME_MUNICIPIO))
      
      ## Salvando o número de municípios na agregação 
      
      municipios_agreg <- nrow(num_municipios)
      
      ## Salvando o total de vagas na agregação
      
      total_vagas <- sum(num_municipios$QTDE_VAGAS)
      
      for(municipio in 1:nrow(num_municipios)){
        
        cat("Lendo", ano, "município", municipio, "de", nrow(num_municipios), "\n")
        
        ## Salvando a quantidade de vagas disponíveis em t0
        
        vagas_t0 <- vagas_ver %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>%  
          pull(QTDE_VAGAS) %>% 
          unique()
        
        ## Banco com os candidatos em t0
        
        candidatos_t0 <- candidatos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) 
        
        ## Banco com os candidatos eleitos na primeira
        ## eleição de referência
        
        eleitos_t0 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
        
        ## Banco com os candidatos eleitos em t-4
        
        eleitos_t4 <- eleitos %>% 
          filter(ANO_ELEICAO == ano - 4 &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
        
        ## Verifica quantos candidatos foram reeleitos e salva o resultado
        
        suppressMessages(
          indicadores2 <- eleitos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE) %>% 
            summarise(REELEITOS_AGREG = n())) 
        
        ## Atribui valor 0 caso nenhum dos vereadores eleitos em t-4 tenha 
        ## se reelegido em t0
        
        if(nrow(indicadores2) == 0 &
           ((nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    REELEITOS_AGREG = 0)
          
          ## Atribui valor NA caso a informação não exista p/ todos os anos de 
          ## referência
          
        } else if(nrow(indicadores2) == 0 &
                  ((nrow(eleitos_t4) == 0) |
                   (nrow(eleitos_t0) == 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    REELEITOS_AGREG = NA)
          
        }
        
        ## Calcula o total de candidatos eleitos em t-4 que se reapresentaram 
        ## em t0
        
        suppressMessages(
          indicadores1 <- candidatos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO) %>%
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   QTDE_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO) %>% 
            reframe(QTDE_VAGAS = sum(QTDE_VAGAS,
                                     na.rm = TRUE),
                    REAPRESENTACAO = REAPRESENTACAO) %>% 
            unique())
        
        ## Atribuindo 0 nos casos em que nenhum dos vereadores eleitos em t-4 se 
        ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
        ## informação exista p/ todos os anos)
        
        if(nrow(indicadores1) == 0 &
           ((nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))){
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    DESCRICAO_CARGO = "VEREADOR",
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                    QTDE_VAGAS = vagas_t0,
                    REAPRESENTACAO = 0)
          
          ## Atribui valor NA caso a informação não exista p/ todos os anos de 
          ## referência
          
        } else if(nrow(indicadores1) == 0 &
                  ((nrow(eleitos_t4) == 0) |
                   (nrow(eleitos_t0) == 0))){
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    DESCRICAO_CARGO = "VEREADOR",
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                    QTDE_VAGAS = vagas_t0,
                    REAPRESENTACAO = NA)
          
        }
        
        ## Juntando as informações dos vereadores que se recandidataram
        ## em t0, bem como dos que foram reeleitos em t0
        
        suppressMessages(
          indicadores1 <- indicadores1 %>%
            left_join(indicadores2) %>% 
            mutate(INFORMACAO_DISPONIVEL = nrow(eleitos_t0)) %>% 
            select(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   DESCRICAO_CARGO,
                   INFORMACAO_DISPONIVEL,
                   QTDE_VAGAS,
                   REAPRESENTACAO,
                   REELEITOS_AGREG) %>% 
            unique())
        
        if(is.na(indicadores1$REAPRESENTACAO) |
           is.na(indicadores1$REELEITOS_AGREG)){
          
          ## Se a informação não está completa para alguma das variáveis-chave,
          ## torna todas elas NA
          
          indicadores1 <- indicadores1 %>% 
            mutate(REAPRESENTACAO = NA,
                   REELEITOS_AGREG = NA,
                   INFORMACAO_DISPONIVEL = 0)
          
        }
        
        ## Calcula os indicadores de 'Reeleição'
        
        indicadores1 <- indicadores1 %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                 DESISTENCIA = INFORMACAO_DISPONIVEL - REAPRESENTACAO,
                 REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, INFORMACAO_DISPONIVEL),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                   DERROTADOS),
                 RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
                 RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                 RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                 INFORMACAO_DISPONIVEL))
        
        ## Empilha os indicadores calculados no banco criado
        
        temp <- bind_rows(temp, 
                          indicadores1)
        
      }
      
      ## Calcula os indicadores de 'Reeleição' na agregação de referência
      
      suppressMessages(
      temp2 <- temp %>%
        mutate(QTDE_MUNICIPIOS_AGREG = municipios_agreg,
               INFORMACAO_DISPONIVEL = ifelse(INFORMACAO_DISPONIVEL != 0,
                                                         1,
                                                         INFORMACAO_DISPONIVEL)) %>%
        group_by(ANO_ELEICAO,
                 DESCRICAO_CARGO,
                 QTDE_MUNICIPIOS_AGREG) %>%
        summarise(INFORMACAO_DISPONIVEL = sum(INFORMACAO_DISPONIVEL,
                                              na.rm = TRUE),
                  across(QTDE_VAGAS:RECANDIDATURAS, ~mean(.,na.rm = TRUE))))
      
      ## Versão alternativa para o cálculo dos indicadores
      
      # suppressMessages(
      #   temp2 <- temp %>%
      #     mutate(QTDE_MUNICIPIOS_AGREG = municipios_agreg,
      #            QTDE_MUNICIPIOS_AGREG_DISPONIVEL = ifelse(INFORMACAO_DISPONIVEL != 0,
      #                                                      1,
      #                                                      INFORMACAO_DISPONIVEL),
      #            QTDE_VAGAS = total_vagas) %>%
      #     group_by(ANO_ELEICAO,
      #              DESCRICAO_CARGO,
      #              QTDE_MUNICIPIOS_AGREG,
      #              QTDE_VAGAS) %>%
      #     summarise(QTDE_MUNICIPIOS_AGREG_DISPONIVEL = sum(QTDE_MUNICIPIOS_AGREG_DISPONIVEL,
      #                                                      na.rm = TRUE),
      #               QTDE_VAGAS_DISPONIVEL = sum(INFORMACAO_DISPONIVEL,
      #                                           na.rm = TRUE),
      #               REAPRESENTACAO = sum(REAPRESENTACAO,
      #                                    na.rm = TRUE),
      #               REELEITOS_AGREG = sum(REELEITOS_AGREG,
      #                                     na.rm = TRUE)) %>%
      #     mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
      #            DESISTENCIA = QTDE_VAGAS_DISPONIVEL - REAPRESENTACAO,
      #            REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, QTDE_VAGAS_DISPONIVEL),
      #            REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG,
      #                                              DERROTADOS),
      #            RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
      #            RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
      #            RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
      #                                            QTDE_VAGAS_DISPONIVEL)) %>%
      #     mutate(across(DERROTADOS:REELEICAO_LIQUIDA, ~ifelse(is.nan(.),
      #                                                         0,
      #                                                         .))) %>%
      #     select(ANO_ELEICAO:QTDE_MUNICIPIOS_AGREG,
      #            QTDE_MUNICIPIOS_AGREG_DISPONIVEL,
      #            QTDE_VAGAS,
      #            QTDE_VAGAS_DISPONIVEL,
      #            REAPRESENTACAO:RECANDIDATURAS))
      
      ## Empilhando os resultados agregados por uf no arquivo final
      
      indicadores_final <- bind_rows(indicadores_final,
                                     temp2)
      
      ## Salvando o progresso para conferência
      
      saveRDS(indicadores_final,
              "data/output/reel_vr_br_temp.rds")
      
    }
    
    ################################## VR_UF #######################################
    
  } else if(agregacao == "VR_UF"){
    
    ## Lista dos estados brasileiros
    
    ufs <- c("AC", "AL", "AP", "AM", "BA", 
             "CE", "ES", "GO", "MA", 
             "MT", "MS", "MG", "PA", "PB", 
             "PR", "PE", "PI", "RJ", "RN", 
             "RS", "RO", "RR", "SC", "SP", 
             "SE", "TO")
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano e agregação regional
    
    for(ano in seq(2004, 2020, by = 4)){
      
      for(uf in ufs){
      
      ## Lista temporária onde os dados serão armazenados
      
      temp <- list()
      
      temp2 <- list()
      
      ## Verificando quantos municípios existem na agregação
      
      suppressMessages(
        num_municipios <- vagas_ver %>%
          filter(ANO_ELEICAO == ano &
                 SIGLA_UF == uf &
                !is.na(QTDE_VAGAS)) %>% 
          ungroup() %>% 
          select(SIGLA_UF,
                 COD_MUN_TSE,
                 COD_MUN_IBGE,
                 NOME_MUNICIPIO,
                 QTDE_VAGAS) %>% 
          unique() %>% 
          arrange(SIGLA_UF,
                  NOME_MUNICIPIO))
      
      ## Salvando o número de municípios na agregação 
      
      municipios_agreg <- nrow(num_municipios)
      
      ## Salvando o total de vagas na agregação
      
      total_vagas <- sum(num_municipios$QTDE_VAGAS)
      
      for(municipio in 1:nrow(num_municipios)){
        
        cat("Lendo", ano, uf, "município", municipio, "de", nrow(num_municipios), "\n")
        
        ## Salvando a quantidade de vagas disponíveis em t0
        
        vagas_t0 <- vagas_ver %>% 
          filter(ANO_ELEICAO == ano &
                 COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>%  
          pull(QTDE_VAGAS) %>% 
          unique()
        
        ## Banco com os candidatos em t0
        
        candidatos_t0 <- candidatos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) 
        
        ## Banco com os candidatos eleitos na primeira
        ## eleição de referência
        
        eleitos_t0 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
        
        ## Banco com os candidatos eleitos em t-4
        
        eleitos_t4 <- eleitos %>% 
          filter(ANO_ELEICAO == ano - 4 &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
        
        ## Verifica quantos candidatos foram reeleitos e salva o resultado
        
        suppressMessages(
          indicadores2 <- eleitos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE) %>% 
            summarise(REELEITOS_AGREG = n())) 
        
        ## Atribui valor 0 caso nenhum dos vereadores eleitos em t-4 tenha 
        ## se reelegido em t0
        
        if(nrow(indicadores2) == 0 &
           ((nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    REELEITOS_AGREG = 0)
          
          ## Atribui valor NA caso a informação não exista p/ todos os anos de 
          ## referência
          
        } else if(nrow(indicadores2) == 0 &
                  ((nrow(eleitos_t4) == 0) |
                   (nrow(eleitos_t0) == 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    REELEITOS_AGREG = NA)
          
        }
        
        ## Calcula o total de candidatos eleitos em t-4 que se reapresentaram 
        ## em t0
        
        suppressMessages(
          indicadores1 <- candidatos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO) %>%
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   QTDE_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO) %>% 
            reframe(QTDE_VAGAS = sum(QTDE_VAGAS,
                                     na.rm = TRUE),
                    REAPRESENTACAO = REAPRESENTACAO) %>% 
            unique())
        
        ## Atribuindo 0 nos casos em que nenhum dos vereadores eleitos em t-4 se 
        ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
        ## informação exista p/ todos os anos)
        
        if(nrow(indicadores1) == 0 &
           ((nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))){
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    DESCRICAO_CARGO = "VEREADOR",
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                    QTDE_VAGAS = vagas_t0,
                    REAPRESENTACAO = 0)
          
          ## Atribui valor NA caso a informação não exista p/ todos os anos de 
          ## referência
          
        } else if(nrow(indicadores1) == 0 &
                  ((nrow(eleitos_t4) == 0) |
                   (nrow(eleitos_t0) == 0))){
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    DESCRICAO_CARGO = "VEREADOR",
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                    QTDE_VAGAS = vagas_t0,
                    REAPRESENTACAO = NA)
          
        }
        
        ## Juntando as informações dos vereadores que se recandidataram
        ## em t0, bem como dos que foram reeleitos em t0
        
        suppressMessages(
          indicadores1 <- indicadores1 %>%
            left_join(indicadores2) %>% 
            mutate(INFORMACAO_DISPONIVEL = nrow(eleitos_t0)) %>% 
            select(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   DESCRICAO_CARGO,
                   INFORMACAO_DISPONIVEL,
                   QTDE_VAGAS,
                   REAPRESENTACAO,
                   REELEITOS_AGREG) %>% 
            unique())
        
        if(is.na(indicadores1$REAPRESENTACAO) |
           is.na(indicadores1$REELEITOS_AGREG)){
          
          ## Se a informação não está completa para alguma das variáveis-chave,
          ## torna todas elas NA
          
          indicadores1 <- indicadores1 %>% 
            mutate(REAPRESENTACAO = NA,
                   REELEITOS_AGREG = NA,
                   INFORMACAO_DISPONIVEL = 0)
          
        }
        
        ## Calcula os indicadores de 'Reeleição'
        
        indicadores1 <- indicadores1 %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                 DESISTENCIA = INFORMACAO_DISPONIVEL - REAPRESENTACAO,
                 REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, INFORMACAO_DISPONIVEL),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                   DERROTADOS),
                 RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
                 RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                 RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                 INFORMACAO_DISPONIVEL))
        
        ## Empilha os indicadores calculados no banco criado
        
        temp <- bind_rows(temp, 
                          indicadores1)
        
      }
      
      ## Calcula os indicadores de 'Reeleição' na agregação de referência
      
      suppressMessages(
      temp2 <- temp %>%
        mutate(QTDE_MUNICIPIOS_AGREG = municipios_agreg,
               INFORMACAO_DISPONIVEL = ifelse(INFORMACAO_DISPONIVEL != 0,
                                                         1,
                                                         INFORMACAO_DISPONIVEL)) %>%
        group_by(ANO_ELEICAO,
                 SIGLA_UF,
                 DESCRICAO_CARGO,
                 QTDE_MUNICIPIOS_AGREG) %>%
        summarise(INFORMACAO_DISPONIVEL = sum(INFORMACAO_DISPONIVEL,
                                              na.rm = TRUE),
                  across(QTDE_VAGAS:RECANDIDATURAS, ~mean(.,na.rm = TRUE))))
      
      ## Versão alternativa para o cálculo dos indicadores
      
      # suppressMessages(
      #   temp2 <- temp %>%
      #     mutate(QTDE_MUNICIPIOS_AGREG = municipios_agreg,
      #            QTDE_MUNICIPIOS_AGREG_DISPONIVEL = ifelse(INFORMACAO_DISPONIVEL != 0,
      #                                                      1,
      #                                                      INFORMACAO_DISPONIVEL),
      #            QTDE_VAGAS = total_vagas) %>%
      #     group_by(ANO_ELEICAO,
      #              SIGLA_UF,
      #              DESCRICAO_CARGO,
      #              QTDE_MUNICIPIOS_AGREG,
      #              QTDE_VAGAS) %>%
      #     summarise(QTDE_MUNICIPIOS_AGREG_DISPONIVEL = sum(QTDE_MUNICIPIOS_AGREG_DISPONIVEL,
      #                                                      na.rm = TRUE),
      #               QTDE_VAGAS_DISPONIVEL = sum(INFORMACAO_DISPONIVEL,
      #                                           na.rm = TRUE),
      #               REAPRESENTACAO = sum(REAPRESENTACAO,
      #                                    na.rm = TRUE),
      #               REELEITOS_AGREG = sum(REELEITOS_AGREG,
      #                                     na.rm = TRUE)) %>%
      #     mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
      #            DESISTENCIA = QTDE_VAGAS_DISPONIVEL - REAPRESENTACAO,
      #            REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, QTDE_VAGAS_DISPONIVEL),
      #            REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG,
      #                                              DERROTADOS),
      #            RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
      #            RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
      #            RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
      #                                            QTDE_VAGAS_DISPONIVEL)) %>%
      #     mutate(across(DERROTADOS:REELEICAO_LIQUIDA, ~ifelse(is.nan(.),
      #                                                         0,
      #                                                         .))) %>%
      #     select(ANO_ELEICAO:QTDE_MUNICIPIOS_AGREG,
      #            QTDE_MUNICIPIOS_AGREG_DISPONIVEL,
      #            QTDE_VAGAS,
      #            QTDE_VAGAS_DISPONIVEL,
      #            REAPRESENTACAO:RECANDIDATURAS))
      
      ## Empilhando os resultados agregados por uf no arquivo final
      
      indicadores_final <- bind_rows(indicadores_final,
                                     temp2)
      
      ## Salvando o progresso para conferência
      
      saveRDS(indicadores_final,
              "data/output/reel_vr_uf_temp.rds")
      
    }
  }
    
    ################################### VR_MUN ########################################
    
  } else if(agregacao == "VR_MUN"){
    
    ## Lista temporária onde os dados com erro serão armazenados
    
    com_erro <- list()
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano e agregação regional
    
    for(ano in seq(2004, 2020, by = 4)){
      
      ## Lista de municípios brasileiros
      
      num_municipios <- vagas_ver %>% 
        ungroup() %>% 
        filter(ANO_ELEICAO == ano) %>% 
        filter(!is.na(QTDE_VAGAS)) %>% 
        select(SIGLA_UF,
               COD_MUN_TSE,
               COD_MUN_IBGE,
               NOME_MUNICIPIO) %>% 
        unique() %>% 
        arrange(SIGLA_UF,
                NOME_MUNICIPIO)
      
      for(municipio in 1:nrow(num_municipios)){
        
        cat("Lendo", ano, "município", municipio, "de", nrow(num_municipios), "\n")
        
        ## Salvando a quantidade de vagas disponíveis em t0
        
        vagas_t0 <- vagas_ver %>% 
          filter(ANO_ELEICAO == ano &
                 COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>%  
          pull(QTDE_VAGAS) %>% 
          unique()
        
        ## Banco com os candidatos em t0
        
        candidatos_t0 <- candidatos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) 
        
        ## Banco com os candidatos eleitos na primeira
        ## eleição de referência
        
        eleitos_t0 <- eleitos %>% 
          filter(ANO_ELEICAO == ano &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
        
        ## Banco com os candidatos eleitos em t-4
        
        eleitos_t4 <- eleitos %>% 
          filter(ANO_ELEICAO == ano - 4 &
                   COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
        
        ## Verifica quantos candidatos foram reeleitos e salva o resultado
        
        indicadores2 <- eleitos_t0 %>% 
          filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
          group_by(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE) %>% 
          summarise(REELEITOS_AGREG = n()) 
        
        ## Atribui valor 0 caso nenhum dos vereadores eleitos em t-4 tenha 
        ## se reelegido em t0
        
        if(nrow(indicadores2) == 0 &
           ((nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    REELEITOS_AGREG = 0)
          
          ## Atribui valor NA caso a informação não exista p/ todos os anos de 
          ## referência
          
        } else if(nrow(indicadores2) == 0 &
                  ((nrow(eleitos_t4) == 0) |
                   (nrow(eleitos_t0) == 0))) {
          
          indicadores2 <- indicadores2 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    REELEITOS_AGREG = NA)
          
        }
        
        ## Calcula o total de candidatos eleitos em t-4 que se reapresentaram 
        ## em t0
        
        suppressMessages(
          indicadores1 <- candidatos_t0 %>% 
            filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO) %>%
            mutate(REAPRESENTACAO = n()) %>% 
            select(ANO_ELEICAO,
                   DESCRICAO_CARGO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   QTDE_VAGAS,
                   REAPRESENTACAO) %>% 
            unique() %>% 
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO) %>% 
            reframe(QTDE_VAGAS = sum(QTDE_VAGAS,
                                     na.rm = TRUE),
                    REAPRESENTACAO = REAPRESENTACAO) %>% 
            unique())
        
        ## Atribuindo 0 nos casos em que nenhum dos vereadores eleitos em t-4 se 
        ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
        ## informação exista p/ todos os anos)
        
        if(nrow(indicadores1) == 0 &
           ((nrow(eleitos_t4) > 0) &
            (nrow(eleitos_t0) > 0))){
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    DESCRICAO_CARGO = "VEREADOR",
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                    QTDE_VAGAS = vagas_t0,
                    REAPRESENTACAO = 0)
          
          ## Atribui valor NA caso a informação não exista p/ todos os anos de 
          ## referência
          
        } else if(nrow(indicadores1) == 0 &
                  ((nrow(eleitos_t4) == 0) |
                   (nrow(eleitos_t0) == 0))){
          
          indicadores1 <- indicadores1 %>% 
            ungroup() %>% 
            add_row(ANO_ELEICAO = as.character(ano),
                    DESCRICAO_CARGO = "VEREADOR",
                    SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                    COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                    COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                    NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                    QTDE_VAGAS = vagas_t0,
                    REAPRESENTACAO = NA)
          
        }
        
        ## Juntando as informações dos vereadores que se recandidataram
        ## em t0, bem como dos que foram reeleitos em t0
        
        suppressMessages(
          indicadores1 <- indicadores1 %>%
            left_join(indicadores2) %>% 
            mutate(INFORMACAO_DISPONIVEL = nrow(eleitos_t0)) %>% 
            select(ANO_ELEICAO,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO,
                   DESCRICAO_CARGO,
                   INFORMACAO_DISPONIVEL,
                   QTDE_VAGAS,
                   REAPRESENTACAO,
                   REELEITOS_AGREG) %>% 
            unique())
        
        if(is.na(indicadores1$REAPRESENTACAO) |
           is.na(indicadores1$REELEITOS_AGREG)){
          
          ## Se a informação não está completa para alguma das variáveis-chave,
          ## torna todas elas NA
          
          indicadores1 <- indicadores1 %>% 
            mutate(REAPRESENTACAO = NA,
                   REELEITOS_AGREG = NA,
                   INFORMACAO_DISPONIVEL = 0)
          
          ## Empilhando o município com erro em um banco adicional
          
          com_erro <- bind_rows(com_erro,
                                indicadores1)
          
          ## Salvando os municípios com problemas
          
          saveRDS(com_erro,
                  "data/output/reeleicao_vereadores_municipos_com_erro.rds")
          
        }
        
        ## Calcula os indicadores de 'Reeleição'
        
        indicadores1 <- indicadores1 %>% 
          mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                 DESISTENCIA = INFORMACAO_DISPONIVEL - REAPRESENTACAO,
                 REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, INFORMACAO_DISPONIVEL),
                 REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                   DERROTADOS),
                 RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
                 RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                 RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                 INFORMACAO_DISPONIVEL))
        
        ## Empilha os indicadores calculados no banco criado
        
        indicadores_final <- bind_rows(indicadores_final, 
                                       indicadores1)
        
        ## Salvando o progresso para conferência
        
        saveRDS(indicadores_final,
                "data/output/reel_vr_mun_temp.rds")
        
      }
      
    }
    
    ############################### VR_ELEIT_APT ###############################
    
  } else if(agregacao == "VR_ELEIT_APT"){
    
    ## Lista das faixas de eleitores aptos
    
    agreg_eleitores <- as.character(sort(unique(eleitores_aptos$AGREG_ELEITORES_APTOS)))
    
    ## Lista temporária onde os dados com erro e p/ conferência serão armazenados
    
    com_erro <- list()
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano e agregação regional
    
    for(ano in seq(2004, 2020, by = 4)){
      
      for(agreg in agreg_eleitores){
        
        ## Lista temporária onde os dados serão armazenados
        
        temp <- list()
        
        temp2 <- list()
        
        ## Verificando quantos municípios existem na agregação
        
        suppressMessages(
          num_municipios <- vagas_ver %>% 
            left_join(eleitores_aptos) %>% 
            filter(ANO_ELEICAO == ano & 
                   AGREG_ELEITORES_APTOS == agreg) %>% 
            filter(!is.na(QTDE_VAGAS)) %>% 
            ungroup() %>% 
            select(AGREG_ELEITORES_APTOS,
                   SIGLA_UF,
                   COD_MUN_TSE,
                   COD_MUN_IBGE,
                   NOME_MUNICIPIO) %>% 
            unique() %>% 
            arrange(AGREG_ELEITORES_APTOS,
                    SIGLA_UF,
                    NOME_MUNICIPIO))
        
        ## Salvando o número de municípios na agregação 
        
        municipios_agreg <- nrow(num_municipios)
        
        for(municipio in 1:nrow(num_municipios)){
          
          cat("Lendo", ano, agreg, "município", municipio, "de", nrow(num_municipios), "\n")
          
          ## Salvando a quantidade de vagas disponíveis em t0
          
          vagas_t0 <- vagas_ver %>% 
            filter(ANO_ELEICAO == ano &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) %>%  
            pull(QTDE_VAGAS) %>% 
            unique()
          
          ## Banco com os candidatos em t0
          
          candidatos_t0 <- candidatos %>% 
            filter(ANO_ELEICAO == ano &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio]) 
          
          ## Banco com os candidatos eleitos na primeira
          ## eleição de referência
          
          eleitos_t0 <- eleitos %>% 
            filter(ANO_ELEICAO == ano &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
          
          ## Banco com os candidatos eleitos em t-4
          
          eleitos_t4 <- eleitos %>% 
            filter(ANO_ELEICAO == ano - 4 &
                     COD_MUN_TSE == num_municipios$COD_MUN_TSE[municipio])
          
          ## Verifica quantos candidatos foram reeleitos e salva o resultado
          
          suppressMessages(
            indicadores2 <- eleitos_t0 %>% 
              filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
              group_by(ANO_ELEICAO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE) %>% 
              summarise(REELEITOS_AGREG = n())) 
          
          ## Atribui valor 0 caso nenhum dos vereadores eleitos em t-4 tenha 
          ## se reelegido em t0
          
          if(nrow(indicadores2) == 0 &
             ((nrow(eleitos_t4) > 0) &
              (nrow(eleitos_t0) > 0))) {
            
            indicadores2 <- indicadores2 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      REELEITOS_AGREG = 0)
            
            ## Atribui valor NA caso a informação não exista p/ todos os anos de 
            ## referência
            
          } else if(nrow(indicadores2) == 0 &
                    ((nrow(eleitos_t4) == 0) |
                     (nrow(eleitos_t0) == 0))) {
            
            indicadores2 <- indicadores2 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      REELEITOS_AGREG = NA)
            
          }
          
          ## Calcula o total de candidatos eleitos em t-4 que se reapresentaram 
          ## em t0
          
          suppressMessages(
            indicadores1 <- candidatos_t0 %>% 
              filter(ID_CEPESP %in% eleitos_t4$ID_CEPESP) %>% 
              group_by(ANO_ELEICAO,
                       DESCRICAO_CARGO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE,
                       NOME_MUNICIPIO) %>%
              mutate(REAPRESENTACAO = n()) %>% 
              select(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     QTDE_VAGAS,
                     REAPRESENTACAO) %>% 
              unique() %>% 
              group_by(ANO_ELEICAO,
                       DESCRICAO_CARGO,
                       SIGLA_UF,
                       COD_MUN_TSE,
                       COD_MUN_IBGE,
                       NOME_MUNICIPIO) %>% 
              reframe(QTDE_VAGAS = sum(QTDE_VAGAS,
                                       na.rm = TRUE),
                      REAPRESENTACAO = REAPRESENTACAO) %>% 
              unique())
          
          ## Atribuindo 0 nos casos em que nenhum dos vereadores eleitos em t-4 se 
          ## recandidatou em t0 e ajustando p/ os demais casos (desde que a
          ## informação exista p/ todos os anos)
          
          if(nrow(indicadores1) == 0 &
             ((nrow(eleitos_t4) > 0) &
              (nrow(eleitos_t0) > 0))){
            
            indicadores1 <- indicadores1 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      DESCRICAO_CARGO = "VEREADOR",
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                      QTDE_VAGAS = vagas_t0,
                      REAPRESENTACAO = 0)
            
            ## Atribui valor NA caso a informação não exista p/ todos os anos de 
            ## referência
            
          } else if(nrow(indicadores1) == 0 &
                    ((nrow(eleitos_t4) == 0) |
                     (nrow(eleitos_t0) == 0))){
            
            indicadores1 <- indicadores1 %>% 
              ungroup() %>% 
              add_row(ANO_ELEICAO = as.character(ano),
                      DESCRICAO_CARGO = "VEREADOR",
                      SIGLA_UF = num_municipios$SIGLA_UF[municipio],
                      COD_MUN_TSE = num_municipios$COD_MUN_TSE[municipio],
                      COD_MUN_IBGE = num_municipios$COD_MUN_IBGE[municipio],
                      NOME_MUNICIPIO = num_municipios$NOME_MUNICIPIO[municipio],
                      QTDE_VAGAS = vagas_t0,
                      REAPRESENTACAO = NA)
            
          }
          
          ## Juntando as informações dos vereadores que se recandidataram
          ## em t0, bem como dos que foram reeleitos em t0
          
          suppressMessages(
            indicadores1 <- indicadores1 %>%
              left_join(eleitores_aptos) %>% 
              left_join(indicadores2) %>% 
              mutate(INFORMACAO_DISPONIVEL = nrow(eleitos_t0)) %>% 
              select(ANO_ELEICAO,
                     SIGLA_UF,
                     COD_MUN_TSE,
                     COD_MUN_IBGE,
                     NOME_MUNICIPIO,
                     DESCRICAO_CARGO,
                     AGREG_ELEITORES_APTOS,
                     INFORMACAO_DISPONIVEL,
                     QTDE_VAGAS,
                     REAPRESENTACAO,
                     REELEITOS_AGREG) %>% 
              unique())
          
          if(is.na(indicadores1$REAPRESENTACAO) |
             is.na(indicadores1$REELEITOS_AGREG)){
            
            ## Se a informação não está completa para alguma das variáveis-chave,
            ## torna todas elas NA
            
            indicadores1 <- indicadores1 %>% 
              mutate(REAPRESENTACAO = NA,
                     REELEITOS_AGREG = NA,
                     INFORMACAO_DISPONIVEL = 0)
            
            ## Empilhando o município com erro em um banco adicional
            
            com_erro <- bind_rows(com_erro,
                                  indicadores1)
            
            ## Salvando os municípios com problemas
            
            # saveRDS(com_erro,
            #         "data/output/reeleicao_vereadores_municipos_elt_apt_com_erro.rds")
            
          }
          
          ## Calcula os indicadores de 'Reeleição'
          
          indicadores1 <- indicadores1 %>% 
            mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                   DESISTENCIA = INFORMACAO_DISPONIVEL - REAPRESENTACAO,
                   REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, INFORMACAO_DISPONIVEL),
                   REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG, 
                                                     DERROTADOS),
                   RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
                   RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                   RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                   INFORMACAO_DISPONIVEL))
          
          ## Empilha os indicadores calculados no banco criado
          
          temp <- bind_rows(temp, 
                            indicadores1)
          
        }
        
        ## Calcula os indicadores de 'Reeleição' na agregação de referência
        
        # suppressMessages(
        # temp2 <- temp %>% 
        #   mutate(QTDE_MUNICIPIOS_AGREG = municipios_agreg,
        #          INFORMACAO_DISPONIVEL = ifelse(INFORMACAO_DISPONIVEL != 0,
        #                                                    1,
        #                                                    INFORMACAO_DISPONIVEL)) %>% 
        #   group_by(ANO_ELEICAO,
        #            DESCRICAO_CARGO,
        #            AGREG_ELEITORES_APTOS,
        #            QTDE_MUNICIPIOS_AGREG) %>% 
        #   summarise(INFORMACAO_DISPONIVEL = sum(INFORMACAO_DISPONIVEL,
        #                                         na.rm = TRUE),
        #             across(QTDE_VAGAS:RECANDIDATURAS, ~mean(.,na.rm = TRUE))))
        
        ## Versão alternativa para o cálculo dos indicadores
        
        suppressMessages(
          temp2 <- temp %>%
            mutate(QTDE_MUNICIPIOS_AGREG = municipios_agreg,
                   QTDE_MUNICIPIOS_AGREG_DISPONIVEL = ifelse(INFORMACAO_DISPONIVEL != 0,
                                                             1,
                                                             INFORMACAO_DISPONIVEL)
            ) %>%
            group_by(ANO_ELEICAO,
                     DESCRICAO_CARGO,
                     AGREG_ELEITORES_APTOS,
                     QTDE_MUNICIPIOS_AGREG,
                     QTDE_VAGAS) %>%
            summarise(QTDE_VAGAS = sum(QTDE_VAGAS,
                                       na.rm = TRUE),
                      QTDE_MUNICIPIOS_AGREG_DISPONIVEL = sum(QTDE_MUNICIPIOS_AGREG_DISPONIVEL,
                                                             na.rm = TRUE),
                      QTDE_VAGAS_DISPONIVEL = sum(INFORMACAO_DISPONIVEL,
                                                  na.rm = TRUE),
                      REAPRESENTACAO = sum(REAPRESENTACAO,
                                           na.rm = TRUE),
                      REELEITOS_AGREG = sum(REELEITOS_AGREG,
                                            na.rm = TRUE)) %>%
            mutate(DERROTADOS = REAPRESENTACAO - REELEITOS_AGREG,
                   DESISTENCIA = QTDE_VAGAS_DISPONIVEL - REAPRESENTACAO,
                   REELEICAO_BRUTA = reeleicao(REELEITOS_AGREG, QTDE_VAGAS_DISPONIVEL),
                   REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS_AGREG,
                                                     DERROTADOS),
                   RENOVACAO_BRUTA = renovacao(REELEICAO_BRUTA),
                   RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
                   RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                                   QTDE_VAGAS_DISPONIVEL)) %>%
            mutate(across(DERROTADOS:REELEICAO_LIQUIDA, ~ifelse(is.nan(.),
                                                                0,
                                                                .))) %>%
            select(ANO_ELEICAO:QTDE_MUNICIPIOS_AGREG,
                   QTDE_MUNICIPIOS_AGREG_DISPONIVEL,
                   QTDE_VAGAS,
                   QTDE_VAGAS_DISPONIVEL,
                   REAPRESENTACAO:RECANDIDATURAS))
        
        ## Empilhando os resultados agregados por uf no arquivo final
        
        indicadores_final <- bind_rows(indicadores_final,
                                       temp2)
        
        ## Salvando o progresso para conferência
        
        saveRDS(indicadores_final,
                "data/output/reel_vr_elt_apt_temp_v2.rds")
        
      }
    }
  }
  
  return(indicadores_final)
  
}  

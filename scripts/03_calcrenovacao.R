

# Objetivo
#'        - Calcular os indicadores de renovacao das bancadas:
#'        - Conservacao, Renovacao bruta, Renovacao liquida;
#'        - Limpeza e padronizacao dos dados.



# 1. Formulas -------------------------------------------------------------


# 1.1. Conservacao --------------------------------------------------------

## Formula para o calculo da conservacao parlamentar

conserv <- function(reel, derr) {
  reel/(derr + reel) * 100
}


# 1.2. Renovacao bruta ----------------------------------------------------

## Funcao para o calculo da renovacao bruta

renov_br <- function(desi,derr, vag){
  
  (desi + derr)/(vag) * 100
}


# 1.3. Renovacao liquida --------------------------------------------------

## Funcao para o calculo da renovacao liquida

renov_liq <- function(derr, reel){
  
  derr/(reel + derr) * 100
}



# 1.4. Recandidaturas -----------------------------------------------------

## Função para o cálculo das recandidaturas

recand <- function(recand, eleit){
  
  recand/eleit * 100
  
}


# 2. Preparacao inicial dos dados -----------------------------------------


# 2.1. Deputado Federal ---------------------------------------------------

## Descarta as colunas desnecessarias

df_uf_cand <- df_uf_cand %>% 
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)%>% 
  mutate_all(na_if,"")

## Transforma as observacoes vazias em NA

df_uf_cand[df_uf_cand ==""]<-NA

## Omite os valores NA

df_uf_cand <- na.omit(df_uf_cand) 

## Cria um banco com somente os candidatos eleitos em cada eleicao

df_uf_eleitos <- df_uf_cand %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>%
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)

# 2.2. Deputado Estadual --------------------------------------------------

## Descarta as colunas desnecessarias

de_uf_cand <- de_uf_cand %>% 
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)%>% 
  mutate_all(na_if,"")

## Transforma as observacoes vazias em NA

de_uf_cand[de_uf_cand ==""]<-NA

## Omite os valores NA

de_uf_cand <- na.omit(de_uf_cand) 

## Cria um banco com somente os candidatos eleitos em cada eleicao

de_uf_eleitos <- de_uf_cand %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>%
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)


# 2.3. Prefeito -----------------------------------------------------------

## Descarta as colunas desnecessarias

pf_mun_cand <- pf_mun_cand %>% 
  select(ANO_ELEICAO, 
         UF,
         COD_MUN_TSE,
         NOME_MUNICIPIO,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)%>% 
  mutate_all(na_if,"")

## Transforma as observacoes vazias em NA

pf_mun_cand[pf_mun_cand ==""]<-NA

## Omite os valores NA

pf_mun_cand <- na.omit(pf_mun_cand) 

## Cria um banco com somente os candidatos eleitos em cada eleicao

pf_mun_eleitos <- pf_mun_cand %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO") %>%
  select(ANO_ELEICAO, 
         UF,
         COD_MUN_TSE,
         NOME_MUNICIPIO,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)

# 2.4. Vereador -----------------------------------------------------------

## Descarta as colunas desnecessarias

vr_mun_cand <- vr_mun_cand %>% 
  select(ANO_ELEICAO, 
         UF,
         COD_MUN_TSE,
         NOME_MUNICIPIO,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)%>% 
  mutate_all(na_if,"")


## Transforma as observacoes vazias em NA

vr_mun_cand[vr_mun_cand ==""]<-NA

## Omite os valores NA

vr_mun_cand <- na.omit(vr_mun_cand) 

## Cria um banco com somente os candidatos eleitos em cada eleicao

cand_vr <- vr_mun_cand %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>%
  select(ANO_ELEICAO, 
         UF,
         COD_MUN_TSE,
         NOME_MUNICIPIO,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)


# 3. Calculo dos indicadores ----------------------------------------------


# 3.1. Deputado Federal ---------------------------------------------------


# 3.1.1. Brasil -----------------------------------------------------------

## For loop que calcula os indicadores de renovacao parlamentar

## Cria uma lista vazia onde os dados serao armazenados

ind_eleicoes_fed_br <- list()

## For loop que calcula os indicadores de renovacao
## para cada ano

for(ano in sort(unique(df_uf_cand$ANO_ELEICAO))){
  
  cat("Lendo",ano,"\n")
  
  ## Banco com os candidatos da proxima eleicao em 
  ## relacao ao ano corrente
  
  candidatos_ano2 <- filter(df_uf_cand,
                            ANO_ELEICAO == ano + 4)
  
  ## Bancos com os candidatos eleitos na primeira
  ## eleicao de referencia
  
  eleitos_ano1 <- filter(df_uf_eleitos,
                         ANO_ELEICAO == ano)
  
  ## Bancos com os candidatos eleitos na segunda
  ## eleicao de referencia
  
  eleitos_ano2 <- filter(df_uf_cand,
                         ANO_ELEICAO == ano + 4)
  
  ## Filtra os candidatos que se reapresentaram na eleicao
  ## seguinte ao ano corrente e os que foram reeleitos
  
  eleitos_ano2 <- filter(eleitos_ano2, NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                           eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
  
  ## Calcula os candidatos que se reapresentaram na eleicao seguinte
  ## ao ano corrente
  
  indicadores1 <- filter(candidatos_ano2,
                         NUM_TITULO_ELEITORAL_CANDIDATO %in%
                           eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
    summarise(
      `Reapresentação` = n()
    )
  
  ## Atribui a variavel 'Ano da eleicao' o ano da proxima eleicao
  ## em relacao ao ano corrente
  
  indicadores1$`Ano da eleição` <- ano + 4
  
  ## Dos candidatos que se reapresentaram na eleicao seguinte a
  ## eleicao de referencia, filtra-se somente os eleitos
  
  indicadores2 <- eleitos_ano2 %>% 
    filter(DESC_SIT_TOT_TURNO == "ELEITO"|
             DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
             DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>% 
    summarise(
      Reeleitos = n()
    )
  
  ## Remove os bancos que nao serao mais utilizados
  
  rm(eleitos_ano1, eleitos_ano2)
  
  ## Filtra no banco referente as estatiscas gerais da
  ## eleicao do ano corrente, somente o ano 
  ## que esta sendo utilizado no momento  
  
  estatisticas_ano1 <- filter(df_br,
                              `Ano da eleição` == ano)
  
  ## Filtra no banco referente as estatiscas gerais da
  ## eleicao seguinte ao ano corrente, somente o ano
  ## que esta sendo utilizado no momento  
  
  estatisticas_ano2 <- filter(df_br,
                              `Ano da eleição` == ano + 4)
  
  ## Atribui a variavel 'Ano da eleicao' o ano da proxima eleicao
  ## em relacao ao ano corrente
  
  indicadores2$`Ano da eleição` <- ano + 4
  
  ## Junta os bancos em um unico  
  
  indicadores1 <- left_join(indicadores1,
                            indicadores2)
  
  ## Calcula a variavel 'Derrotados' 
  
  indicadores1$Derrotados <- indicadores1$Reapresentação - indicadores1$Reeleitos
  
  ## Calcula a variavel 'Desistencia' 
  
  indicadores1$Desistência <- 513 - indicadores1$Reapresentação
  
  ## Condicao para que se calcule os indicadores
  
  if(indicadores1$Reapresentação > 0){
    
    ## Calcula o indicador 'Conservacao' 
    
    indicadores1$`Conservação` <- conserv(indicadores1$Reeleitos, 
                                          indicadores1$Derrotados)
    
    ## Calcula o indicador 'Renovacao bruta' 
    
    indicadores1$`Renovação bruta` <- renov_br(indicadores1$Desistência,
                                               indicadores1$Derrotados, 513)
    
    ## Calcula o indicador 'Renovacao liquida' 
    
    indicadores1$`Renovação líquida` <- renov_liq(indicadores1$Derrotados, 
                                                  indicadores1$Reeleitos)
    
    ## Calcula o indicador 'Recandidaturas'
    
    indicadores1$Recandidaturas <- recand(indicadores1$Reapresentação,
                                          513)
    
  }
  
  ## Empilha os indicadores calculados no banco criado
  
  ind_eleicoes_fed_br <- bind_rows(ind_eleicoes_fed_br, indicadores1)
}



# 3.1.2. Estado -----------------------------------------------------------

## For loop que calcula os indicadores de renovacao parlamentar

## Cria uma lista vazia onde os dados serao armazenados

ind_eleicoes_fed_uf <- list()

## For loop que calcula os indicadores de renovacao
## para cada ano e uf

for(ano in sort(unique(df_uf_cand$ANO_ELEICAO))){
  for(uf in sort(unique(df_uf_cand$UF))){
    
    cat("Lendo",ano,uf,"\n")
    
    ## Banco com os candidatos da proxima eleicao em 
    ## relacao ao ano corrente
    
    candidatos_ano2 <- filter(df_uf_cand,
                              ANO_ELEICAO == ano + 4,
                              UF == uf)
    
    ## Bancos com os candidatos eleitos na primeira
    ## eleicao de referencia
    
    eleitos_ano1 <- filter(df_uf_eleitos,
                           ANO_ELEICAO == ano,
                           UF == uf)
    
    ## Bancos com os candidatos eleitos na segunda
    ## eleicao de referencia
    
    eleitos_ano2 <- filter(df_uf_cand,
                           ANO_ELEICAO == ano+4,
                           UF == uf)
    
    ## Filtra os candidatos que se reapresentaram na eleicao
    ## seguinte ao ano corrente e os que foram reeleitos
    
    eleitos_ano2 <- filter(eleitos_ano2, NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                             eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
    
    ## Calcula os candidatos que se reapresentaram na eleicao seguinte
    ## ao ano corrente
    
    indicadores1 <- filter(candidatos_ano2,
                           NUM_TITULO_ELEITORAL_CANDIDATO %in%
                             eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
      summarise(
        `Reapresentação` = n()
      )
    
    ## Dos candidatos que se reapresentaram na eleicao seguinte a
    ## eleicao de referencia, filtra-se somente os eleitos

    indicadores2 <- eleitos_ano2 %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>% 
      summarise(
        Reeleitos = n()
      )
    
    ## Remove os bancos que nao serao mais utilizados
    
    rm(eleitos_ano1,eleitos_ano2)
    
    ## Filtra no banco referente as estatiscas gerais da
    ## eleicao do ano corrente, somente o ano 
    ## que esta sendo utilizado no momento  
    
    estatisticas_ano1 <- filter(df_uf,
                                `Ano da eleição` == ano,
                                UF == uf)
    
    ## Filtra no banco referente as estatiscas gerais da
    ## eleicao seguinte ao ano corrente, somente o ano
    ## que esta sendo utilizado no momento  
    
    estatisticas_ano2 <- filter(df_uf,
                                `Ano da eleição` == ano + 4,
                                UF == uf)
    
    ## Atribui a variavel 'Ano da eleicao' o ano da proxima eleicao
    ## em relacao ao ano corrente
    
    indicadores1$`Ano da eleição` <- ano + 4
    indicadores2$`Ano da eleição` <- ano + 4
    
    ## Atribui a variavel 'UF' a uf corrente
    
    indicadores1$UF <- uf
    indicadores2$UF <- uf
    
    ## Junta os bancos em um unico  
    
    indicadores1 <- left_join(indicadores1,
                              indicadores2, 
                              by = c("Ano da eleição",
                                     "UF"))
    
    ## Calcula a variavel 'Derrotados'
    
    indicadores1$Derrotados <- indicadores1$Reapresentação - indicadores1$Reeleitos
    
    ## Calcula a variavel 'Desistencia'
    
    indicadores1$Desistência <- unique(estatisticas_ano1$Vagas) - indicadores1$Reapresentação
    
    ## Condicao para que se calcule os indicadores
    
    if(indicadores1$Reapresentação > 0){
      
      ## Calcula o indicador 'Conservacao'
      
      indicadores1$`Conservação` <- conserv(indicadores1$Reeleitos, 
                                            indicadores1$Derrotados)
      
      ## Calcula o indicador 'Renovacao bruta'
      
      indicadores1$`Renovação bruta` <- renov_br(indicadores1$Desistência,
                                                 indicadores1$Derrotados, 
                                                 unique(estatisticas_ano1$Vagas))
      
      ## Calcula o indicador 'Renovacao liquida'
      
      indicadores1$`Renovação líquida` <- renov_liq(indicadores1$Derrotados, 
                                                    indicadores1$Reeleitos)
      
      ## Calcula o indicador 'Recandidaturas'
      
      indicadores1$Recandidaturas <- recand(indicadores1$Reapresentação,
                                            unique(estatisticas_ano1$Vagas))
    }
    
    ## Empilha os indicadores calculados no banco criado  
    
    ind_eleicoes_fed_uf <- bind_rows(ind_eleicoes_fed_uf, indicadores1)
  }
}


# 3.2. Deputado Estadual --------------------------------------------------


# 3.2.1. Brasil -----------------------------------------------------------


## For loop que calcula os indicadores de renovacao parlamentar

## Cria uma lista vazia onde os dados serao armazenados

ind_eleicoes_est_br <- list()


## For loop que calcula os indicadores de renovacao
## para cada ano

for(ano in sort(unique(de_uf_cand$ANO_ELEICAO))){
  
  cat("Lendo",ano,"\n")
  
  ## Banco com os candidatos da proxima eleicao em 
  ## relacao ao ano corrente
  
  candidatos_ano2 <- dplyr::filter(de_uf_cand,
                            ANO_ELEICAO == ano + 4)
  
  ## Bancos com os candidatos eleitos na primeira
  ## eleicao de referencia
  
  eleitos_ano1 <- dplyr::filter(de_uf_eleitos,
                         ANO_ELEICAO == ano)
  
  ## Bancos com os candidatos eleitos na segunda
  ## eleicao de referencia
  
  eleitos_ano2 <- dplyr::filter(de_uf_cand,
                         ANO_ELEICAO == ano + 4)
  
  ## Filtra os candidatos que se reapresentaram na eleicao
  ## seguinte ao ano corrente e os que foram reeleitos
  
  eleitos_ano2 <- dplyr::filter(eleitos_ano2, NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                         eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
  
  ## Calcula os candidatos que se reapresentaram na eleicao seguinte
  ## ao ano corrente
  
  indicadores1 <- dplyr::filter(candidatos_ano2,
                         NUM_TITULO_ELEITORAL_CANDIDATO %in%
                           eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
    dplyr::summarise(
      `Reapresentação` = n()
    )
  
  ## Atribui a variavel 'Ano da eleicao' o ano da proxima eleicao
  ## em relacao ao ano corrente
  
  indicadores1$`Ano da eleição` <- ano + 4
  
  ## Dos candidatos que se reapresentaram na eleicao seguinte a
  ## eleicao de referencia, filtra-se somente os eleitos
  
  indicadores2 <- eleitos_ano2 %>% 
    dplyr::filter(DESC_SIT_TOT_TURNO == "ELEITO"|
             DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
             DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>% 
    dplyr::summarise(
      Reeleitos = n()
    )
  
  ## Remove os bancos que nao serao mais utilizados
  
  rm(eleitos_ano1, eleitos_ano2)
  
  ## Filtra no banco referente as estatiscas gerais da
  ## eleicao do ano corrente, somente o ano 
  ## que esta sendo utilizado no momento  
  
  estatisticas_ano1 <- dplyr::filter(de_br,
                              `Ano da eleição` == ano)
  
  ## Filtra no banco referente as estatiscas gerais da
  ## eleicao seguinte ao ano corrente, somente o ano
  ## que esta sendo utilizado no momento  
  
  estatisticas_ano2 <- dplyr::filter(de_br,
                              `Ano da eleição` == ano + 4)
  
  ## Atribui a variavel 'Ano da eleicao' o ano da proxima eleicao
  ## em relacao ao ano corrente
  
  indicadores2$`Ano da eleição` <- ano + 4
  
  ## Junta os bancos em um unico  
  
  indicadores1 <- left_join(indicadores1,
                            indicadores2)
  
  ## Calcula a variavel 'Derrotados' 
  
  indicadores1$Derrotados <- indicadores1$Reapresentação - indicadores1$Reeleitos
  
  ## Calcula a variavel 'Desistencia' 
  
  indicadores1$Desistência <- 1059 - indicadores1$Reapresentação
  
  ## Condicao para que se calcule os indicadores
  
  if(indicadores1$Reapresentação > 0){
    
    ## Calcula o indicador 'Conservacao' 
    
    indicadores1$`Conservação` <- conserv(indicadores1$Reeleitos, 
                                          indicadores1$Derrotados)
    
    ## Calcula o indicador 'Renovacao bruta' 
    
    indicadores1$`Renovação bruta` <- renov_br(indicadores1$Desistência,
                                               indicadores1$Derrotados, 1059)
    
    ## Calcula o indicador 'Renovacao liquida' 
    
    indicadores1$`Renovação líquida` <- renov_liq(indicadores1$Derrotados, 
                                                  indicadores1$Reeleitos)
    
    ## Calcula o indicador 'Recandidaturas'
    
    indicadores1$Recandidaturas <- recand(indicadores1$Reapresentação,
                                          1059)
    
  }
  
  ## Empilha os indicadores calculados no banco criado
  
  ind_eleicoes_est_br <- bind_rows(ind_eleicoes_est_br, indicadores1)
}


# 3.2.2. Estado -----------------------------------------------------------

## For loop que calcula os indicadores de renovacao parlamentar

## Cria uma lista vazia onde os dados serao armazenados

ind_eleicoes_est_uf <- list()

## For loop que calcula os indicadores de renovacao
## para cada ano e uf

for(ano in sort(unique(de_uf_cand$ANO_ELEICAO))){
  for(uf in sort(unique(de_uf_cand$UF))){
    
    cat("Lendo",ano,uf,"\n")
    
    ## Banco com os candidatos da proxima eleicao em 
    ## relacao ao ano corrente
    
    candidatos <- filter(de_uf_cand,
                         ANO_ELEICAO == ano + 4,
                         UF == uf)
    
    ## Bancos com os candidatos eleitos na primeira
    ## eleicao de referencia
    
    eleitos_ano1 <- filter(de_uf_eleitos,
                           ANO_ELEICAO == ano,
                           UF == uf)
    
    ## Bancos com os candidatos eleitos na segunda
    ## eleicao de referencia
    
    eleitos_ano2 <- filter(de_uf_eleitos,
                           ANO_ELEICAO == ano+4,
                           UF == uf)
    
    ## OBSERVACAO: O estado do Rio de Janeiro possui dados incompletos
    ## e, por isso, foi necessario um tratamento especifico para ele
    
    if(uf =="RJ" & ano == 1998){
      
      ## Filtra os candidatos que se reapresentaram na eleicao
      ## seguinte ao ano corrente e os que foram reeleitos
      
      eleitos_ano2 <- filter(eleitos_ano2,
                             NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                               eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO &
                               DATA_NASCIMENTO %in% eleitos_ano1$DATA_NASCIMENTO)
      
      ## Calcula os candidatos que se reapresentaram na eleicao seguinte
      ## ao ano corrente
      
      indicadores1 <- dplyr::filter(candidatos,
                             NUM_TITULO_ELEITORAL_CANDIDATO %in%
                               eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO &
                               DATA_NASCIMENTO %in% eleitos_ano1$DATA_NASCIMENTO) %>% 
        dplyr::summarise(
          `Reapresentação` = n())
    }else {
      
      ## Filtra os candidatos que se reapresentaram na eleicao
      ## seguinte ao ano corrente e os que foram reeleitos
      
      eleitos_ano2 <- dplyr::filter(eleitos_ano2,
                             NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                               eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
      
      ## Calcula os candidatos que se reapresentaram na eleicao seguinte
      ## ao ano corrente
      
      indicadores1 <- dplyr::filter(candidatos,
                             NUM_TITULO_ELEITORAL_CANDIDATO %in%
                               eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
        dplyr::summarise(
          `Reapresentação` = n())
    }
    
    ## Atribui a variavel 'Ano da eleicao' o ano da proxima eleicao
    ## em relacao ao ano corrente
    
    indicadores1$`Ano da eleição` <- ano + 4
    
    ## Atribui a variavel 'UF' a uf corrente
    
    indicadores1$UF <- uf
    
    ## Dos candidatos que se reapresentaram na eleicao seguinte a
    ## eleicao de referencia, filtra-se somente os eleitos
    
    indicadores2 <- eleitos_ano2 %>% 
      dplyr::filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>% 
      dplyr::summarise(
        Reeleitos = n()
      )
    
    ## Remove os bancos que nao serao mais utilizados
    
    rm(eleitos_ano1,eleitos_ano2)
    
    ## Filtra no banco referente as estatiscas gerais da
    ## eleicao do ano corrente, somente o ano 
    ## que esta sendo utilizado no momento  
    
    estatisticas_ano1 <- dplyr::filter(de_uf,
                                `Ano da eleição` == ano,
                                UF == uf)
    
    ## Filtra no banco referente as estatiscas gerais da
    ## eleicao seguinte ao ano corrente, somente o ano
    ## que esta sendo utilizado no momento  
    
    estatisticas_ano2 <- dplyr::filter(de_uf,
                                `Ano da eleição` == ano+4,
                                UF == uf)
    ## Acrescenta as colunas `Ano de eleição` e UF aos bancos gerados
    

    indicadores2$`Ano da eleição` <- ano + 4
    indicadores2$UF <- uf
    
    ## Junta os bancos em um unico  
    
    indicadores1 <- left_join(indicadores1,
                              indicadores2, 
                              by = c("Ano da eleição",
                                     "UF"))
    
    indicadores1 <- left_join(indicadores1,
                              vags_de, 
                              by = c("Ano da eleição",
                                     "UF"))
    
    ## Calcula a variavel 'Derrotados'
    
    indicadores1$Derrotados <- indicadores1$Reapresentação - indicadores1$Reeleitos
    
    ## Calcula a variavel 'Desistencia'
    
    indicadores1$Desistência <- unique(estatisticas_ano1$Vagas) - indicadores1$Reapresentação
    
    ## Condicao para que se calcule os indicadores
    
    if(length(indicadores1$Reapresentação) > 0){
      
      ## Calcula o indicador 'Conservacao'
      
      indicadores1$`Conservação` <- conserv(indicadores1$Reeleitos, 
                                            indicadores1$Derrotados)
      
      ## Calcula o indicador 'Renovacao bruta'
      
      indicadores1$`Renovação bruta` <- renov_br(indicadores1$Desistência,
                                                 indicadores1$Derrotados, 
                                                 indicadores1$`Cadeiras oferecidas`)
      
      ## Calcula o indicador 'Renovacao liquida'
      
      indicadores1$`Renovação líquida` <- renov_liq(indicadores1$Derrotados, 
                                                    indicadores1$Reeleitos)
      
      ## Calcula o indicador 'Recandidaturas'
      
      indicadores1$Recandidaturas <- recand(indicadores1$Reapresentação,
                                            indicadores1$`Cadeiras oferecidas`)
    }
    
    ## Empilha todos os indicadores no banco criado 
    
    ind_eleicoes_est_uf <- bind_rows(ind_eleicoes_est_uf,indicadores1)
  }
}


# 3.3. Prefeito -----------------------------------------------------------

## For loop que calcula os indicadores de renovacao parlamentar

## Cria uma lista vazia onde os dados serao armazenados

ind_eleicoes_pf <- list()

## For loop que calcula os indicadores de renovacao
## para cada ano, municipio e turno

for(ano in sort(unique(pf_mun_cand$ANO_ELEICAO))){
  for(municipio in sort(unique(pf_mun_cand$COD_MUN_TSE))){
    for(turno in sort(unique(pf_mun_cand$NUM_TURNO))){
    
    cat("Lendo",ano, municipio, turno, "\n")
    
    ## Banco com os candidatos da proxima eleicao em 
    ## relacao ao ano corrente
    
    candidatos_ano2 <- filter(pf_mun_cand,
                              ANO_ELEICAO == ano + 4,
                              COD_MUN_TSE == municipio,
                              NUM_TURNO == turno)
    
    ## Bancos com os candidatos eleitos na primeira
    ## eleicao de referencia
    
    eleitos_ano1 <- filter(pf_mun_eleitos,
                           ANO_ELEICAO == ano,
                           COD_MUN_TSE == municipio,
                           NUM_TURNO == turno)
    
    ## Bancos com os candidatos eleitos na segunda
    ## eleicao de referencia
    
    eleitos_ano2 <- filter(pf_mun_eleitos,
                           ANO_ELEICAO == ano+4,
                           COD_MUN_TSE == municipio,
                           NUM_TURNO == turno)
    
    ## Filtra os candidatos que se reapresentaram na eleicao
    ## seguinte ao ano corrente e os que foram reeleitos
    
    eleitos_ano2 <- filter(eleitos_ano2, NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                           eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
    
    ## Calcula os candidatos que se reapresentaram na eleicao seguinte
    ## ao ano corrente
    
    indicadores1 <- filter(candidatos_ano2,
                           NUM_TITULO_ELEITORAL_CANDIDATO %in%
                           eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
      summarise(
        `Reapresentação` = n()
      )
    
    ## Dos candidatos que se reapresentaram na eleicao seguinte a
    ## eleicao de referencia, filtra-se somente os eleitos
    
    indicadores2 <- eleitos_ano2 %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO") %>% 
      summarise(
        Reeleitos = n()
      )
    
    ## Remove os bancos que nao serao mais utilizados
    
    rm(eleitos_ano1,eleitos_ano2)
    
    ## Filtra no banco referente as estatiscas gerais da
    ## eleicao do ano corrente, somente o ano 
    ## que esta sendo utilizado no momento  
    
    estatisticas_ano1 <- filter(pf_mun,
                                `Ano da eleição` == ano,
                                `Código do município` == municipio,
                                `Número do turno` == turno)
    
    ## Filtra no banco referente as estatiscas gerais da
    ## eleicao seguinte ao ano corrente, somente o ano
    ## que esta sendo utilizado no momento  
    
    estatisticas_ano2 <- filter(pf_mun,
                                `Ano da eleição` == ano + 4,
                                `Código do município` == municipio,
                                `Número do turno` == turno)
    
    ## Condicao para que se comece a calcular os indicadores
    
    if(nrow(estatisticas_ano1) > 0){
      
      
      ## Atribui a variavel 'Ano da eleicao' o ano da proxima eleicao
      ## em relacao ao ano corrente
      
      indicadores1$`Ano da eleição` <- ano + 4
      indicadores2$`Ano da eleição` <- ano + 4
      
      ## Atribui a variavel 'Codigo do municipio' o codigo tse 
      ## do municipio corrente
      
      indicadores1$`Código do município` <- municipio
      indicadores2$`Código do município` <- municipio
      
      ## Atribui a variavel 'Turno' o turno corrente
      
      indicadores1$`Turno` <- turno
      indicadores2$`Turno` <- turno
      
      ## Junta os bancos em um unico  
      
      indicadores1 <- left_join(indicadores1,
                                indicadores2)
      
      ## Calcula a variavel 'Derrotados'
      
      indicadores1$Derrotados <- indicadores1$Reapresentação - indicadores1$Reeleitos
      
      ## Calcula a variavel 'Desistencia'
      
      indicadores1$Desistência <- 1 - indicadores1$Reapresentação
      
      ## Calcula o indicador 'Conservacao'
      
      indicadores1$`Conservação` <- conserv(indicadores1$Reeleitos, 
                                            indicadores1$Derrotados)
      
      ## Calcula o indicador 'Renovacao bruta'
      
      indicadores1$`Renovação bruta` <- renov_br(indicadores1$Desistência,
                                                 indicadores1$Derrotados, 
                                                 1)
      
      ## Calcula o indicador 'Renovacao liquida'
      
      indicadores1$`Renovação líquida` <- renov_liq(indicadores1$Derrotados, 
                                                    indicadores1$Reeleitos)
      
      ## Calcula o indicador 'Recandidaturas'
      
      indicadores1$Recandidaturas <- recand(indicadores1$Reapresentação,
                                            1)
      
      ## Condicao para que se atribua novos valores aos indicadores com
      ## valor NA
      
      if(indicadores1$Reapresentação == 0){
        
        indicadores1$`Conservação` <- 0
        indicadores1$`Renovação líquida` <- 0
        
      }
      
      ## Empilha todos os indicadores calculados no banco criado
      
      ind_eleicoes_pf <- bind_rows(ind_eleicoes_pf, indicadores1)
    }
  }
  }
}
 

# 3.4. Vereador -----------------------------------------------------------

## For loop que calcula os indicadores de renovacao parlamentar

## Cria uma lista vazia onde os dados serao armazenados

ind_eleicoes_vr <- list()

## For loop que calcula os indicadores de renovacao
## para cada ano e municipio

for(ano in sort(unique(vr_mun_cand$ANO_ELEICAO))){
  for(municipio in sort(unique(vr_mun_cand$COD_MUN_TSE))){
    
    cat("Lendo",ano,municipio,"\n")
    
    ## Banco com os candidatos da proxima eleicao em 
    ## relacao ao ano corrente
    
    candidatos_ano2 <- filter(vr_mun_cand,
                              ANO_ELEICAO == ano + 4,
                              COD_MUN_TSE == municipio)
    
    ## Bancos com os candidatos eleitos na primeira
    ## eleicao de referencia
    
    eleitos_ano1 <- filter(cand_vr,
                           ANO_ELEICAO == ano,
                           COD_MUN_TSE == municipio)
    
    ## Bancos com os candidatos eleitos na segunda
    ## eleicao de referencia
    
    eleitos_ano2 <- filter(cand_vr,
                           ANO_ELEICAO == ano+4,
                           COD_MUN_TSE == municipio)
    
    ## Filtra os candidatos que se reapresentaram na eleicao
    ## seguinte ao ano corrente e os que foram reeleitos
    
    eleitos_ano2 <- filter(eleitos_ano2, NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                             eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
    
    ## Calcula os candidatos que se reapresentaram na eleicao seguinte
    ## ao ano corrente
    
    indicadores1 <- filter(candidatos_ano2,
                           NUM_TITULO_ELEITORAL_CANDIDATO %in%
                             eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
      summarise(
        `Reapresentação` = n()
      )
    
    ## Dos candidatos que se reapresentaram na eleicao seguinte a
    ## eleicao de referencia, filtra-se somente os eleitos
    
    indicadores2 <- eleitos_ano2 %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>% 
      summarise(
        Reeleitos = n()
      )
    
    ## Remove os bancos que nao serao mais utilizados
    
    rm(eleitos_ano1,eleitos_ano2)
    
    ## Filtra no banco referente as estatiscas gerais da
    ## eleicao do ano corrente, somente o ano 
    ## que esta sendo utilizado no momento  
    
    estatisticas_ano1 <- filter(vr_mun,
                                `Ano da eleição` == ano,
                                `Código do município` == municipio)
    
    ## Filtra no banco referente as estatiscas gerais da
    ## eleicao seguinte ao ano corrente, somente o ano
    ## que esta sendo utilizado no momento  
    
    estatisticas_ano2 <- filter(vr_mun,
                                `Ano da eleição` == ano + 4,
                                `Código do município` == municipio)
    
    ## Condicao para que se comece a calcular os indicadores
    
    if(nrow(estatisticas_ano1) > 0){
      
      
      ## Atribui a variavel 'Ano da eleicao' o ano da proxima eleicao
      ## em relacao ao ano corrente
      
      indicadores1$`Ano da eleição` <- ano + 4
      indicadores2$`Ano da eleição` <- ano + 4
      
      ## Atribui a variavel 'Codigo do municipio' o codigo tse 
      ## do municipio corrente
      
      indicadores1$`Código do município` <- municipio
      indicadores2$`Código do município` <- municipio
      
      ## Junta os bancos em um unico  
      
      indicadores1 <- left_join(indicadores1,
                                indicadores2)
      
      ## Calcula a variavel 'Derrotados'
      
      indicadores1$Derrotados <- indicadores1$Reapresentação - indicadores1$Reeleitos
      
      ## Calcula a variavel 'Desistencia'
      
      indicadores1$Desistência <- unique(estatisticas_ano1$Vagas) - indicadores1$Reapresentação
      
      ## Condicao para que se calcule os indicadores
      
      if(indicadores1$Reapresentação > 0){
        
        ## Calcula o indicador 'Conservacao'
        
        indicadores1$`Conservação` <- conserv(indicadores1$Reeleitos, 
                                              indicadores1$Derrotados)
        
        ## Calcula o indicador 'Renovacao bruta'
        
        indicadores1$`Renovação bruta` <- renov_br(indicadores1$Desistência,
                                                   indicadores1$Derrotados, 
                                                   unique(estatisticas_ano1$Vagas))
        
        ## Calcula o indicador 'Renovacao liquida'
        
        indicadores1$`Renovação líquida` <- renov_liq(indicadores1$Derrotados, 
                                                      indicadores1$Reeleitos)
        
        ## Calcula o indicador 'Recandidaturas'
        
        indicadores1$Recandidaturas <- recand(indicadores1$Reapresentação,
                                              unique(estatisticas_ano1$Vagas))
        
      }
      
      ## Empilha todos os indicadores calculados no banco criado
      
      ind_eleicoes_vr <- bind_rows(ind_eleicoes_vr, indicadores1)
    }
  }
}


# 4. Padronizacao dos dados -----------------------------------------------


# 4.1. Deputado Federal ---------------------------------------------------


# 4.1.1. Brasil -----------------------------------------------------------

## Remove as linhas desnecessarias

ind_eleicoes_fed_br <- ind_eleicoes_fed_br[-c(6),]

## Adiciona o valor 'Deputado Federal' a variavel 'Cargo'

ind_eleicoes_fed_br$Cargo <- "Deputado Federal"

## Adiciona o valor '513' a variavel 'Cadeiras disponiveis'

ind_eleicoes_fed_br$`Cadeiras disponíveis` <- 513

## Reorganiza a tabela

ind_eleicoes_fed_br <- ind_eleicoes_fed_br %>% 
  select(`Ano da eleição`,
         Cargo,
         `Cadeiras disponíveis`,
         Reapresentação,
         Reeleitos,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`)

## Arredonda e limita o numero de digitos do indicador 
## 'Conservacao'

ind_eleicoes_fed_br$Conservação <- 
  format(round(ind_eleicoes_fed_br$Conservação, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao bruta'

ind_eleicoes_fed_br$`Renovação bruta` <- 
  format(round(ind_eleicoes_fed_br$`Renovação bruta`, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao liquida'

ind_eleicoes_fed_br$`Renovação líquida` <- 
  format(round(ind_eleicoes_fed_br$`Renovação líquida`, 
               digits = 2),  
         nsmall = 2)


# 4.1.2. Estado -----------------------------------------------------------

## Cria um banco de vagas para cada ano de eleicao e estado

vagas <- df_uf %>% 
  select(`Ano da eleição`, 
         UF, 
         Vagas) %>% 
  unique()

## Junta o banco de indicadores para deputado federal com 
## o banco de vagas

ind_eleicoes_fed_uf <- left_join(ind_eleicoes_fed_uf, vagas)

## Remove as linhas desnecessarias

ind_eleicoes_fed_uf <- ind_eleicoes_fed_uf[-c(136:162),]

## Adiciona o valor 'Deputado Federal' a variavel 'Cargo'

ind_eleicoes_fed_uf$Cargo <- "Deputado Federal"

## Reorganiza a tabela

ind_eleicoes_fed_uf <- ind_eleicoes_fed_uf %>% 
  select(`Ano da eleição`,
         UF,
         Cargo,
         Vagas,
         Reapresentação,
         Reeleitos,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`) %>% 
  dplyr::rename("Cadeiras disponíveis" = "Vagas")

## Arredonda e limita o numero de digitos do indicador 
## 'Conservacao'

ind_eleicoes_fed_uf$Conservação <- 
  format(round(ind_eleicoes_fed_uf$Conservação, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao bruta'

ind_eleicoes_fed_uf$`Renovação bruta` <- 
  format(round(ind_eleicoes_fed_uf$`Renovação bruta`, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao liquida'

ind_eleicoes_fed_uf$`Renovação líquida` <- 
  format(round(ind_eleicoes_fed_uf$`Renovação líquida`, 
               digits = 2),  
         nsmall = 2)

# 4.2. Deputado Estadual --------------------------------------------------


# 4.2.1. Brasil -----------------------------------------------------------

## Remove as linhas desnecessarias

ind_eleicoes_est_br <- ind_eleicoes_est_br[-c(6),]

## Adiciona o valor 'Deputado esteral' a variavel 'Cargo'

ind_eleicoes_est_br$Cargo <- "Deputado Estadual"

## Adiciona o valor '513' a variavel 'Cadeiras disponiveis'

ind_eleicoes_est_br$`Cadeiras disponíveis` <- "1.059"

## Reorganiza a tabela

ind_eleicoes_est_br <- ind_eleicoes_est_br %>% 
  select(`Ano da eleição`,
         Cargo,
         `Cadeiras disponíveis`,
         Reapresentação,
         Reeleitos,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`)

## Arredonda e limita o numero de digitos do indicador 
## 'Conservacao'

ind_eleicoes_est_br$Conservação <- 
  format(round(ind_eleicoes_est_br$Conservação, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao bruta'

ind_eleicoes_est_br$`Renovação bruta` <- 
  format(round(ind_eleicoes_est_br$`Renovação bruta`, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao liquida'

ind_eleicoes_est_br$`Renovação líquida` <- 
  format(round(ind_eleicoes_est_br$`Renovação líquida`, 
               digits = 2),  
         nsmall = 2)


# 4.2.2. Estado -----------------------------------------------------------

## Remove as linhas desnecessarias

ind_eleicoes_est <- ind_eleicoes_est[-c(136:162),]

## Adiciona o valor 'Deputado Estadual' a variavel 'Cargo'

ind_eleicoes_est$Cargo <- "Deputado Estadual"

## Reorganiza a tabela

ind_eleicoes_est <- ind_eleicoes_est %>% 
  select(`Ano da eleição`,
         UF,
         Cargo,
         Vagas,
         Reapresentação,
         Reeleitos,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`) %>% 
  rename("Cadeiras disponíveis" = "Vagas")

## Arredonda e limita o numero de digitos do indicador 
## 'Conservacao'

ind_eleicoes_est$Conservação <- 
  format(round(ind_eleicoes_est$Conservação, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao bruta'

ind_eleicoes_est$`Renovação bruta` <- 
  format(round(ind_eleicoes_est$`Renovação bruta`, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao liquida'

ind_eleicoes_est$`Renovação líquida` <- 
  format(round(ind_eleicoes_est$`Renovação líquida`, 
               digits = 2),  
         nsmall = 2)

# 4.3. Prefeito -----------------------------------------------------------

## Remove as duplicacoes

ind_eleicoes_pf <- na.omit(ind_eleicoes_pf)

## Adiciona o valor 'Prefeito' a variavel 'Cargo'

ind_eleicoes_pf$Cargo <- "Prefeito"

## Reorganiza a tabela

ind_eleicoes_pf <- ind_eleicoes_pf %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         Reapresentação,
         Reeleitos,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`) 

## Ordena os dados em funcao do 'Ano da eleicao', 
## 'UF' e 'Nome do municipio'

ind_eleicoes_pf<- ind_eleicoes_pf %>% 
  arrange(`Ano da eleição`,UF,`Nome do município`)

## Arredonda e limita o numero de digitos do indicador 
## 'Conservacao'

ind_eleicoes_pf$Conservação <- 
  format(round(ind_eleicoes_pf$Conservação, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao bruta'

ind_eleicoes_pf$`Renovação bruta` <- 
  format(round(ind_eleicoes_pf$`Renovação bruta`, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao liquida'

ind_eleicoes_pf$`Renovação líquida` <- 
  format(round(ind_eleicoes_pf$`Renovação líquida`, 
               digits = 2),  
         nsmall = 2)

# 4.4. Vereador -----------------------------------------------------------

## Cria um banco com os municipios e seus respectivos codigos e
## numero de vagas

municipios <- vr_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`, 
         `Nome do município`, 
         Vagas)

## Remove duplicacoes

municipios <- unique(municipios)

## Junta o banco de indicadores para vereador 
## e o banco de municipios

ind_eleicoes_vr <- left_join(ind_eleicoes_vr, municipios)

## Remove valores NA

ind_eleicoes_vr <- na.omit(ind_eleicoes_vr)

## Atribui o valor 'Vereador' a variavel 'Cargo'

ind_eleicoes_vr$Cargo <- "Vereador"

## Reorganiza a tabela

ind_eleicoes_vr <- ind_eleicoes_vr %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         Vagas,
         Reapresentação,
         Reeleitos,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`) %>% 
  dplyr::rename("Cadeiras disponíveis" = "Vagas")

## Ordena os dados em funcao do 'Ano da eleicao', 
## 'UF' e 'Nome do municipio'

ind_eleicoes_vr <- ind_eleicoes_vr %>% 
  arrange(`Ano da eleição`, UF, `Nome do município`)

## Arredonda e limita o numero de digitos do indicador 
## 'Conservacao'

ind_eleicoes_vr$Conservação <- 
  format(round(ind_eleicoes_vr$Conservação, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao bruta'

ind_eleicoes_vr$`Renovação bruta` <- 
  format(round(ind_eleicoes_vr$`Renovação bruta`, 
               digits = 2),  
         nsmall = 2)

## Arredonda e limita o numero de digitos do indicador 
## 'Renovacao liquida'

ind_eleicoes_vr$`Renovação líquida` <- 
  format(round(ind_eleicoes_vr$`Renovação líquida`, 
               digits = 2),  
         nsmall = 2)


# 5. Rbind ----------------------------------------------------------------

# 5.1. Brasil -------------------------------------------------------------

# Junta os bancos de acordo com seu nivel de agregacao regional

ind_eleicoes_br <- bind_rows(ind_eleicoes_fed_br, ind_eleicoes_est_br)

# 5.2. Estado -------------------------------------------------------------

## Junta os bancos de acordo com seu nivel de agregacao regional

ind_eleicoes_uf <- bind_rows(ind_eleicoes_fed_uf, ind_eleicoes_est)

# 6. Salvando os arquivos -------------------------------------------------

# 6.1. Brasil -------------------------------------------------------------

## Salva o arquivo referente aos indicadores de renovacao em .rds

saveRDS(ind_eleicoes_br, "data/output/renov_parl_br.rds")


# 6.2. Estado -------------------------------------------------------------

## Salva o arquivo referente aos indicadores de renovacao em .rds

saveRDS(ind_eleicoes_est, "data/output/renov_parl_uf.rds")


# 6.3. Municipio ----------------------------------------------------------

## Salva o arquivo referente aos indicadores de renovacao em .rds

saveRDS(ind_eleicoes_vr, "data/output/renov_parl_mun.rds")


# 6.4. Prefeito -----------------------------------------------------------

## Salva o arquivo referente aos indicadores de renovacao em .rds

saveRDS(ind_eleicoes_pf, "data/output/renov_parl_pf.rds")


## Remove os arquivos que nao serao mais utilizados

rm(estatisticas_ano1,estatisticas_ano2,de_uf_eleitos,df_uf_eleitos,
   df_br_eleitos, pf_mun_eleitos, vr_mun_eleitos, vr_mun_cand, pf_mun_cand,
   de_uf_cand,df_uf_cand, ind_eleicoes_est,ind_eleicoes_fed_br, 
   ind_eleicoes_fed_uf,ind_eleicoes_uf,ind_eleicoes_vr,
   indicadores1,indicadores2, candidatos_ano2,candidatos)

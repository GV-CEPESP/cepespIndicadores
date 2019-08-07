
# Pacotes utilizados

library(cepespR)
library(dplyr)
library(tidyverse)
library(abjutils)

# Objetivo
#'        - Calcular os indicadores de renovacao das bancadas:
#'        - Conservacao, Renovacao bruta, Renovacao liquida,
#'        - Volatilidade eleitoral;
#'        - Limpeza e padronizacao dos dados.

      

# 1. Reeleição e derrota nas eleicoes ----------------------------------------------------------

## Cria bancos individuais para os candidatos de cada eleicao

### 1994


### 1998

df98 <- df %>% 
  filter(ANO_ELEICAO == 1998) %>% 
    select(ANO_ELEICAO, 
           SIGLA_UE,
           DESCRICAO_CARGO,
           NOME_CANDIDATO,
           CPF_CANDIDATO,
           NUM_TITULO_ELEITORAL_CANDIDATO,
           SIGLA_PARTIDO,
           DESC_SIT_TOT_TURNO,
           QTDE_VOTOS) %>% 
     mutate_all(na_if,"")

df98[df98==""]<-NA

df98 <- na.omit(df98)  

df98t <- df98 %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

### 2002

df02 <- df %>% 
  filter(ANO_ELEICAO == 2002) %>% 
   select(ANO_ELEICAO, 
         SIGLA_UE,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO,
         QTDE_VOTOS)

df02[df02==""] <-NA

df02 <- na.omit(df02)  

df02t <- df02 %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

### 2006

df06 <- df %>% 
  filter(ANO_ELEICAO == 2006) %>% 
  select(ANO_ELEICAO, 
         SIGLA_UE,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO,
         QTDE_VOTOS)

df06[df06==""] <-NA

df06 <- na.omit(df06) 

df06t <- df06 %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

### 2010

df10 <- df %>% 
  filter(ANO_ELEICAO == 2010) %>% 
  select(ANO_ELEICAO, 
         SIGLA_UE,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO,
         QTDE_VOTOS)

df10[df10==""] <-NA

df10 <- na.omit(df10)  

df10t <- df10 %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")


### 2014

df14 <- df %>% 
  filter(ANO_ELEICAO == 2014) %>% 
  select(ANO_ELEICAO, 
         SIGLA_UE,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO,
         QTDE_VOTOS)

df14[df14==""] <-NA

df14 <- na.omit(df14) 

df14t <- df14 %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")


### 2018

df18 <- df %>% 
  filter(ANO_ELEICAO == 2018) %>% 
  select(ANO_ELEICAO, 
         SIGLA_UE,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO,
         QTDE_VOTOS)

df18[df18==""] <-NA

df18 <- na.omit(df18) 

  df18t <- df18 %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
  DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

## Cria uma string com os titulos eleitorais dos candidatos de cada eleicao

### 1998

cand98 <- df98t$NUM_TITULO_ELEITORAL_CANDIDATO

### 2002

cand02 <- df02t$NUM_TITULO_ELEITORAL_CANDIDATO

### 2006

cand06 <- df06t$NUM_TITULO_ELEITORAL_CANDIDATO

### 2010

cand10 <- df10t$NUM_TITULO_ELEITORAL_CANDIDATO

### 2014

cand14 <- df14t$NUM_TITULO_ELEITORAL_CANDIDATO

### 2018

cand18 <- df18t$NUM_TITULO_ELEITORAL_CANDIDATO

rm(df98t,df02t,df06t,df10t,df14t,df18t)

### Calculo dos deputados que se canditaram novamente

### 2002

recand02 <- df02 %>% 
  filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% cand98) ## 370 candidatos se recandidataram em 2002

### 2006

recand06 <- df06 %>% 
  filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% cand02) ## 393 candidatos se recandidataram em 2002

### 2010

recand10 <- df10 %>% 
  filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% cand06) ## 379 candidatos se recandidataram em 2002

### 2014

recand14 <- df14 %>% 
  filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% cand10) ## 354 candidatos se recandidataram em 2002

### 2018

recand18 <- df18 %>% 
  filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% cand14) ## 381 candidatos se recandidataram em 2002


## Calculo dos candidatos que se reelegeram 

### 2002

reel02 <- recand02 %>% 
  dplyr::count(DESC_SIT_TOT_TURNO == "ELEITO"|
        DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA") ## 264 candidatos foram reeleitos em 2002

### 2006

reel06 <- recand06 %>% 
  dplyr::count(DESC_SIT_TOT_TURNO == "ELEITO"|
        DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA") ## 257 candidatos foram reeleitos em 2006

### 2010

reel10 <- recand10 %>% 
  dplyr::count(DESC_SIT_TOT_TURNO == "ELEITO"|
          DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA") ## 282 candidatos foram reeleitos em 2010

### 2014

reel14 <- recand14 %>% 
  dplyr::count(DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
          DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA") ## 263 candidatos foram reeleitos em 2014

### 2018

reel18 <- recand18 %>% 
  dplyr::count(DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
          DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA") ## 240 candidatos foram reeleitos em 2018


## Tabela com os resultados agregados

ind_eleicoes <- data.frame(`Ano da eleição` = c(2002,2006,2010,2014,2018),
                           Cargo = "Deputado Federal",
                           `Reapresentação` = c(370,393,379,354,381),
                           `Desistência` = c(143,120,134,159,132),
                           Reeleitos = c(264,257,282,263,240),
                           Derrotados = c(106,136,97,91,141))



# 2. Conservacao ----------------------------------------------------------

## Formula para o calculo da conservacao parlamentar

conserv <- function(reel, derr) {
  reel/(derr + reel) * 100
}

## Calcula o indice de conservacao parlamentar

ind_eleicoes$`Conservação` <- conserv(ind_eleicoes$Reeleitos, 
                                      ind_eleicoes$Derrotados)



# 3. Renovacao bruta ------------------------------------------------------

## Funcao para o calculo da renovacao bruta

renov_br <- function(desi,derr){
  (desi + derr)/(513)*100
}

## Calcula o indice de renovacao bruta para cada eleicao

ind_eleicoes$`Renovação bruta` <- renov_br(ind_eleicoes$Desistência, 
                                           ind_eleicoes$Derrotados)



# 4. Renovacao liquida ----------------------------------------------------

## Funcao para o calculo da renovacao liquida

renov_liq <- function(derr, reel){
  derr/(reel + derr)*100
}

## Calcula o indice de renovacao liquida para cada eleicao

ind_eleicoes$`Renovação líquida` <- renov_liq(ind_eleicoes$Derrotados, 
                                              ind_eleicoes$Reeleitos)


# 5. Volatilidade eleitoral -----------------------------------------------

volat_elet <- function(vt1,vt2) {
  sum(vt1 - (vt2-1))/2 
}

## Calcula a volatilidade eleitoral de todas as eleicoes

### 2002

ind_eleicoes$`Volatilidade eleitoral` <- NA

ind_eleicoes[1,10] <- volat_elet(t98df$`Percentual de votos conquistados`,
                      t02df$`Percentual de votos conquistados`)

### 2006

ind_eleicoes[2,10] <- volat_elet(t02df$`Percentual de votos conquistados`,
                                t06df$`Percentual de votos conquistados`)

### 2010

ind_eleicoes[3,10] <- volat_elet(t06df$`Percentual de votos conquistados`,
                                t10df$`Percentual de votos conquistados`)

### 2014

ind_eleicoes[4,10] <- volat_elet(t10df$`Percentual de votos conquistados`,
                                t14df$`Percentual de votos conquistados`)

### 2018

ind_eleicoes[5,10] <- volat_elet(t14df$`Percentual de votos conquistados`,
                                t18df$`Percentual de votos conquistados`)


## Padroniza o formato dos indices numericos

ind_eleicoes$Conservação <- 
  format(round(ind_eleicoes$Conservação, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes$`Renovação bruta` <- 
  format(round(ind_eleicoes$`Renovação bruta`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes$`Renovação líquida` <- 
  format(round(ind_eleicoes$`Renovação líquida`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)


ind_eleicoes <- ind_eleicoes %>% 
  rename("Ano da eleição" = "Ano.da.eleição")


# 6. Salvando os arquivos -------------------------------------------------

## Salva o arquivo referente aos indicadores de renovacao parlamentar em .csv

write.csv(ind_eleicoes, "data/output/renov_parl.csv")

## Remove os arquivos que nao serao mais utilizados

rm(df98,df02,df06,df10,df14,df18,recand02,recand06,recand10,recand14,recand18,
   reel02,reel06,reel10,reel14,reel18,t98df,t02df,t06df,t10df,t14df,t18df,
   ind_eleicoes,df,de)

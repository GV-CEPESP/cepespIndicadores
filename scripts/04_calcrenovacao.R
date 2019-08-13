
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



# 1. Formulas -------------------------------------------------------------

## Formula para o calculo da conservacao parlamentar

conserv <- function(reel, derr) {
  reel/(derr + reel) * 100
}

## Funcao para o calculo da renovacao bruta

renov_br <- function(desi,derr){
  (desi + derr)/(513)*100
}

## Funcao para o calculo da renovacao liquida

renov_liq <- function(derr, reel){
  derr/(reel + derr)*100
}

## Formula para o calculo da volatilidade eleitoral

volat_elet <- function(vt1,vt2) {
  sum(vt1 - (vt2-1))/2 
}
      

# 2. Calculo dos indicadores ----------------------------------------------------------

## Transforma a variavel `NUM_TITULO_ELEITORAL_CANDIDATO` em character

### Deputado Federal

eleicao_94$NUM_TITULO_ELEITORAL_CANDIDATO <- 
  as.character(eleicao_94$NUM_TITULO_ELEITORAL_CANDIDATO)

## Junta o banco com os candidatos de 94 aos demais candidatos

### Deputado Federal

df <- bind_rows(df,eleicao_94)

## Descarta as colunas desnecessarias

### Deputado Federal

df <- df %>% 
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)%>% 
  mutate_all(na_if,"")

### Deputado Estadual

de <- de %>% 
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)%>% 
  mutate_all(na_if,"")

## Transforma as observacoes vazias em NA

### Deputado Federal

df[df ==""]<-NA

### Deputado Estadual

de[de ==""]<-NA

## Omite os valores NA

### Deputado Federal

df <- na.omit(df) 

### Deputado Estadual

de <- na.omit(de) 

## Cria um banco com somente os candidatos eleitos em cada eleicao

### Deputado Federal

cand_df <- df %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
         DESC_SIT_TOT_TURNO == "MEDIA"|
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"
           ) %>%
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)

### Deputado Estadual

cand_de <- de %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP"
  ) %>%
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)

## For loop que calcula os indicadores de renovacao parlamentar

### Deputado Federal

ind_eleicoes_fed <- list()


for(ano in sort(unique(df$ANO_ELEICAO))){
  cat("Lendo",ano,"\n")
  ano1 <- filter(cand_df,
                  ANO_ELEICAO == ano)
  ano2 <- filter(df,
                 ANO_ELEICAO == ano+4)
  ano2 <- filter(ano2,
             NUM_TITULO_ELEITORAL_CANDIDATO %in% 
               ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
  indic1 <- ano2 %>% 
    summarise(
      `Reapresentação` = n()
    )
  indic1$`Ano da eleição` <- ano + 4
  indic2 <- ano2 %>% 
    filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>% 
    summarise(
      Reeleitos = n()
    )
  rm(ano1,ano2)
  ano1 <- filter(df1,
                 `Ano da eleição` == ano)
  ano2 <- filter(df1,
                 `Ano da eleição` == ano+4)
  indic2$`Ano da eleição` <- ano + 4
  indic1 <- left_join(indic1,indic2, by = "Ano da eleição")
  indic1$Derrotados <- indic1$Reapresentação - indic1$Reeleitos
  indic1$Desistência <- 513 - indic1$Reapresentação
  indic1$`Conservação` <- conserv(indic1$Reeleitos, 
                             indic1$Derrotados)
  indic1$`Renovação bruta` <- renov_br(indic1$Desistência,
                                  indic1$Derrotados)
  indic1$`Renovação líquida` <- renov_liq(indic1$Derrotados, 
                                     indic1$Reeleitos)
  indic1$`Volatilidade eleitoral` <- volat_elet(ano1$`Percentual de votos conquistados`,
                                                ano2$`Percentual de votos conquistados`)
  ind_eleicoes_fed <- bind_rows(ind_eleicoes_fed,indic1)
   }

## Remove as linhas desnecessarias

ind_eleicoes_fed <- ind_eleicoes_fed[-c(1,7),]

ind_eleicoes_fed$Cargo <- "Deputado Federal"

## Reorganiza a tabela

ind_eleicoes_fed <- ind_eleicoes_fed %>% 
  select(`Ano da eleição`,
         Cargo,
         Reapresentação,
         Desistência,
         Reeleitos,
         Derrotados,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`,
         `Volatilidade eleitoral`)

### Deputado Estadual

estados <- c("AC", "AL", "AM", "AP", "BA", "CE", "ES", 
             "GO", "MA", "MG","MS", "MT", "PA", "PB", 
             "PE", "PI", "PR","RJ", "RN", "RO", "RR",
             "RS", "SC", "SE", "SP", "TO")



ind_eleicoes_est <- list()


for(ano in sort(unique(de$ANO_ELEICAO))){
  for(uf in estados){
  cat("Lendo",ano,uf,"\n")
  ano1 <- filter(cand_de,
                 ANO_ELEICAO == ano,
                 UF == uf)
  ano2 <- filter(df,
                 ANO_ELEICAO == ano+4,
                 UF == uf)
  ano2 <- filter(ano2,
                 NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                   ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
  indic1 <- ano2 %>% 
    summarise(
      `Reapresentação` = n()
    )
  indic1$`Ano da eleição` <- ano + 4
  indic1$UF <- uf
  indic2 <- ano2 %>% 
    filter(DESC_SIT_TOT_TURNO == "ELEITO"|
             DESC_SIT_TOT_TURNO == "MEDIA"|
             DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
             DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>% 
    summarise(
      Reeleitos = n()
    )
  rm(ano1,ano2)
  ano1 <- filter(de1,
                 `Ano da eleição` == ano,
                 UF == uf)
  ano2 <- filter(de1,
                 `Ano da eleição` == ano+4,
                 UF == uf)
  indic2$`Ano da eleição` <- ano + 4
  indic2$UF <- uf
  indic1 <- left_join(indic1,indic2, by = c("Ano da eleição",
                                            "UF"))
  indic1$Derrotados <- indic1$Reapresentação - indic1$Reeleitos
  indic1$Desistência <- unique(ano1$Vagas) - indic1$Reapresentação
  indic1$`Conservação` <- conserv(indic1$Reeleitos, 
                                  indic1$Derrotados)
  indic1$`Renovação bruta` <- renov_br(indic1$Desistência,
                                       indic1$Derrotados)
  indic1$`Renovação líquida` <- renov_liq(indic1$Derrotados, 
                                          indic1$Reeleitos)
  indic1$`Volatilidade eleitoral` <- volat_elet(ano1$`Percentual de votos conquistados`,
                                                ano2$`Percentual de votos conquistados`)
  ind_eleicoes_est <- bind_rows(ind_eleicoes_est,indic1)
  }
}

## Remove as linhas desnecessarias

ind_eleicoes_est <- ind_eleicoes_est[-c(131:156),]

## Reorganiza a tabela

ind_eleicoes_est$Cargo <- "Deputado Estadual"

ind_eleicoes_est <- ind_eleicoes_est %>% 
  select(`Ano da eleição`,
         UF,
         Cargo,
         Reapresentação,
         Desistência,
         Reeleitos,
         Derrotados,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`,
         `Volatilidade eleitoral`)


# 3. Padronização ----------------------------------------------------------------------

## Padroniza o formato dos indices numericos

### Deputado Federal

ind_eleicoes_fed$Conservação <- 
  format(round(ind_eleicoes_fed$Conservação, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_fed$`Renovação bruta` <- 
  format(round(ind_eleicoes_fed$`Renovação bruta`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_fed$`Renovação líquida` <- 
  format(round(ind_eleicoes_fed$`Renovação líquida`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_fed$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes_fed$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)


### Deputado Estadual

ind_eleicoes_est$Conservação <- 
  format(round(ind_eleicoes_est$Conservação, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_est$`Renovação bruta` <- 
  format(round(ind_eleicoes_est$`Renovação bruta`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_est$`Renovação líquida` <- 
  format(round(ind_eleicoes_est$`Renovação líquida`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_est$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes_est$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)


# 4. Salvando os arquivos -------------------------------------------------

## Salva o arquivo referente aos indicadores de renovacao parlamentar em .csv

### Deputado Federal

write.csv(ind_eleicoes_fed, "data/output/renov_parl_fed.csv")

### Deputado Estadual

write.csv(ind_eleicoes_est, "data/output/renov_parl_est.csv")

## Remove os arquivos que nao serao mais utilizados

rm(ano1,ano2,cand_de,cand_df,de,de1,df,df1,eleicao_94,ind_eleicoes_est,
   ind_eleicoes_fed,indic1,indic2)

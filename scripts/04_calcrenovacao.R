
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

#eleicao_94$NUM_TITULO_ELEITORAL_CANDIDATO <- 
  #as.character(eleicao_94$NUM_TITULO_ELEITORAL_CANDIDATO)

## Junta o banco com os candidatos de 94 aos demais candidatos

### Deputado Federal

#df <- bind_rows(df,eleicao_94)

## Descarta as colunas desnecessarias

### Deputado Federal

df <- df %>% 
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

### Deputado Estadual

de <- de %>% 
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
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"
           ) %>%
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)

### Deputado Estadual

cand_de <- de %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP"
  ) %>%
  select(ANO_ELEICAO, 
         UF,
         DESCRICAO_CARGO,
         NOME_CANDIDATO,
         DATA_NASCIMENTO,
         CPF_CANDIDATO,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         SIGLA_PARTIDO,
         DESC_SIT_TOT_TURNO)

## For loop que calcula os indicadores de renovacao parlamentar

### Deputado Federal

ind_eleicoes_fed <- list()


for(ano in sort(unique(df$ANO_ELEICAO))){
  cat("Lendo",ano,"\n")
  
## Banco com os candidatos da proxima eleicao
  
  candidatos_ano2 <- filter(df,
                       ANO_ELEICAO == ano + 4)
## Bancos com os candidatos eleitos na primeira e
## segunda eleicao de referencia
  
  eleitos_ano1 <- filter(cand_df,
                  ANO_ELEICAO == ano)
  eleitos_ano2 <- filter(df,
                 ANO_ELEICAO == ano+4)
## Filtra os candidatos que se reapresentaram na eleicao
## seguinte e os que foram reeleitos
  
  eleitos_ano2 <- filter(eleitos_ano2, NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                          eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
  indicadores1 <- filter(candidatos_ano2,
                         NUM_TITULO_ELEITORAL_CANDIDATO %in%
                         eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
                  summarise(
                          `Reapresentação` = n()
                  )
  
## Dos candidatos que se reapresentaram na eleicao seguinte a
## eleicao de referencia, filtra-se somente os eleitos
  
  indicadores1$`Ano da eleição` <- ano + 4
  indicadores2 <- eleitos_ano2 %>% 
    filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>% 
    summarise(
      Reeleitos = n()
    )

## Remove os bancos que nao serao mais utilizados
  
  rm(eleitos_ano1,eleitos_ano2)
  
## Filtra nos bancos referentes as estatiscas gerais das
## eleicoes, somente os anos que estao sendo 
## utilizados no momento  
  estatisticas_ano1 <- filter(df1,
                            `Ano da eleição` == ano)
  estatisticas_ano2 <- filter(df1,
                            `Ano da eleição` == ano + 4)
  
## Acrescenta as colunas `Ano de eleição` bancos gerados
    
  indicadores1$`Ano da eleição` <- ano + 4
  indicadores2$`Ano da eleição` <- ano + 4

## Junta os bancos em um unico  
  
  indicadores1 <- left_join(indicadores1,
                            indicadores2, 
                            by = "Ano da eleição")
  
## Calculo dos indicadores de renovacao parlamentar  
  
  indicadores1$Derrotados <- indicadores1$Reapresentação - indicadores1$Reeleitos
  indicadores1$Desistência <- 513 - indicadores1$Reapresentação
  if(indicadores1$Reapresentação > 0){
  indicadores1$`Conservação` <- conserv(indicadores1$Reeleitos, 
                                        indicadores1$Derrotados)
  indicadores1$`Renovação bruta` <- renov_br(indicadores1$Desistência,
                                             indicadores1$Derrotados)
  indicadores1$`Renovação líquida` <- renov_liq(indicadores1$Derrotados, 
                                          indicadores1$Reeleitos)
  }
  indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                      estatisticas_ano2$`Percentual de votos conquistados`)
  ## Empilha todas as eleicoes 
  
  ind_eleicoes_fed <- bind_rows(ind_eleicoes_fed,indicadores1)
  }

## Remove as linhas desnecessarias

ind_eleicoes_fed <- ind_eleicoes_fed[-c(6),]

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
    
## Banco com os candidatos da proxima eleicao
    
  candidatos <- filter(de,
                       ANO_ELEICAO == ano + 4,
                       UF == uf)
## Bancos com os candidatos eleitos na primeira e
## segunda eleicao de referencia
  
  eleitos_ano1 <- filter(cand_de,
                 ANO_ELEICAO == ano,
                 UF == uf)
  eleitos_ano2 <- filter(cand_de,
                 ANO_ELEICAO == ano+4,
                 UF == uf)
## Filtra os candidatos que se reapresentaram na eleicao
## seguinte e os que foram reeleitos
## OBS: O estado do Rio de Janeiro possui dados incompletos
## e, por isso, foi necessario um tratamento especifico para ele
  
  if(uf =="RJ" & ano == 1998){
  eleitos_ano2 <- filter(eleitos_ano2,
                         NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                         eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO &
                         DATA_NASCIMENTO %in% eleitos_ano1$DATA_NASCIMENTO)
  indicadores1 <- filter(candidatos,
                         NUM_TITULO_ELEITORAL_CANDIDATO %in%
                         eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO &
                         DATA_NASCIMENTO %in% eleitos_ano1$DATA_NASCIMENTO) %>% 
                  summarise(
                         `Reapresentação` = n())
  }else {
  eleitos_ano2 <- filter(eleitos_ano2,
                         NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                         eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
  indicadores1 <- filter(candidatos,
                         NUM_TITULO_ELEITORAL_CANDIDATO %in%
                         eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
                  summarise(
                         `Reapresentação` = n())
        }

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
  
## Filtra nos bancos referentes as estatiscas gerais das
## eleicoes, somente os anos e uf's que estao sendo 
## utilizados no momento
  
  estatisticas_ano1 <- filter(de1,
                 `Ano da eleição` == ano,
                 UF == uf)
  estatisticas_ano2 <- filter(de1,
                 `Ano da eleição` == ano+4,
                 UF == uf)
## Acrescenta as colunas `Ano de eleição` e UF aos bancos gerados
  
  indicadores1$`Ano da eleição` <- ano + 4
  indicadores1$UF <- uf
  indicadores2$`Ano da eleição` <- ano + 4
  indicadores2$UF <- uf

## Junta os bancos em um unico  
    
  indicadores1 <- left_join(indicadores1,
                            indicadores2, 
                            by = c("Ano da eleição",
                                   "UF"))
  
## Calculo dos indicadores de renovacao parlamentar
  
  indicadores1$Derrotados <- indicadores1$Reapresentação - indicadores1$Reeleitos
  indicadores1$Desistência <- unique(estatisticas_ano1$Vagas) - indicadores1$Reapresentação
  if(indicadores1$Reapresentação > 0){
  indicadores1$`Conservação` <- conserv(indicadores1$Reeleitos, 
                                        indicadores1$Derrotados)
  indicadores1$`Renovação bruta` <- renov_br(indicadores1$Desistência,
                                             indicadores1$Derrotados)
  indicadores1$`Renovação líquida` <- renov_liq(indicadores1$Derrotados, 
                                                indicadores1$Reeleitos)
  }
  indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                     estatisticas_ano2$`Percentual de votos conquistados`)
  
## Empilha todas as ufs e eleicoes 
  
  ind_eleicoes_est <- bind_rows(ind_eleicoes_est,indicadores1)
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

ind_eleicoes_est$Conservação <- as.numeric(ind_eleicoes_est$Conservação)

ind_eleicoes_est$`Renovação bruta` <- as.numeric(ind_eleicoes_est$`Renovação bruta`)

ind_eleicoes_est$`Renovação líquida` <- as.numeric(ind_eleicoes_est$`Renovação líquida`)

ind_eleicoes_est[is.na(ind_eleicoes_est)] <- 0

# 4. Salvando os arquivos -------------------------------------------------

## Salva o arquivo referente aos indicadores de renovacao parlamentar em .csv

### Deputado Federal

write.csv(ind_eleicoes_fed, "data/output/renov_parl_fed.csv")

### Deputado Estadual

write.csv(ind_eleicoes_est, "data/output/renov_parl_est.csv")

## Remove os arquivos que nao serao mais utilizados

rm(ano1,ano2,cand_de,cand_df,de,de1,df,df1,eleicao_94,ind_eleicoes_est,
   ind_eleicoes_fed,indic1,indic2)

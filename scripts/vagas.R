
rm(list = ls())

# Pacotes utilizados

library(stringr)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(magrittr)
library(abjutils)

#Objetivo

#'       - Fazer o download dos arquivos de vagas do TSE;
#'       - Juntar os bancos de todas as eleicoes em um único.


# 1. Download dos dados ---------------------------------------------------

## Salva um link "molde" dos arquivos de vagas e faz o download para todos os anos
 
### Eleicoes estaduais

 url_vagas_est <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_ANO.zip"

 for(i in seq(1998,2018, by = 4)){
  vagas_est <- stringr::str_replace(url_vagas_est, "ANO", as.character(i)) 
  print(vagas_est)
  download.file(vagas_est, str_c("vagas", i, ".zip"))
 }

### Eleicoes municipais
 
 url_vagas_mun <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_ANO.zip"
 
 for(i in seq(2000,2014, by = 4)){
   vagas_mun <- stringr::str_replace(url_vagas_mun, "ANO", as.character(i)) 
   print(vagas_mun)
   download.file(vagas_mun, str_c("vagas_mun", i, ".zip"))
 } 

 
### Eleicao 2016
 
 url_vagas_2016 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_2016.zip"
 
 download.file(url_vagas_2016, "vagas_mun2016.zip")
 
## Cria uma lista com os arquivos com nomes correspondentes a "arquivo_vagas" 
## Tambem unzipa todos os arquivos dentro da lista 
 
### Eleicoes estaduais
 
 list_vag_est <- list.files(pattern = "vagas_est")  

 for(i in seq_along(list_vag_est)){ 
  unzip(list_vag_est [i], exdir = "vagas_est") 
 } 

### Eleicoes municipais
 
 list_vag_mun <- list.files(pattern = "vagas_mun")
 
 for(i in seq_along(list_vag_mun)){ 
   unzip(list_vag_mun [i], exdir = "vagas_mun") 
 }  
 
### Eleicao 2016
 
 unzip("vagas_mun2016.zip", exdir = "vagas_mun2016")
 
## Cria e le uma lista contendo os arquivos das pastas "vagas_est" e "vagas_mun"
## Tambem remove os arquivos referentes a sigla BR 
  
### Eleicoes estaduais
 
arq_vags_est <- list.files(path = "vagas_est")

vags_est <- list()

 for (i in seq_along(arq_vags_est)) {
  cat("lendo", arq_vags_est[i], "\n")
  vags_est[[i]] <- read.table(file = paste0("vagas_est/",arq_vags_est[i]),
                              header=F,sep=";", stringsAsFactors = FALSE)
 }

 for(i in seq_along("vagas_est")){
  br_files_vagas <- list.files(path = "vagas_est", pattern = "BR",full.names = T)
  file.remove(br_files_vagas)
 }

### Eleicoes municipais

arq_vags_mun <- list.files(path = "vagas_mun")

vags_mun <- list()

for (i in seq_along(arq_vags_mun)) {
  cat("lendo", arq_vags_mun[i], "\n")
  vags_mun[[i]] <- read.table(file = paste0("vagas_mun/",arq_vags_mun[i]),
                              header=F,sep=";", stringsAsFactors = FALSE)
}

for(i in seq_along(vags_mun)){
  br_files_vagas_mun <- list.files(path = "vags_mun", pattern = "BR",full.names = T)
  file.remove(br_files_vagas_mun)
}

### Eleicao 2016

arq_vags_mun2016 <- list.files(path = "vagas_mun2016")

vags_mun2016 <- list()

for (i in seq_along(arq_vags_mun2016)) {
  cat("lendo", arq_vags_mun2016[i], "\n")
  vags_mun2016[[i]] <- read.table(file = paste0("vagas_mun2016/", arq_vags_mun2016[i]),
                                  header = T, sep = ";", stringsAsFactors = FALSE)
}

for(i in seq_along(vags_mun2016)){
  br_files_vagas_mun2016 <- list.files(path = "vags_mun2016", pattern = "BR",full.names = T)
  file.remove(br_files_vagas_mun2016)
}

## Junta todos os arquivos em um unico

### Eleicoes estaduais

vags_est <- rbind.fill(vags)

### Eleicoes municipais

vags_mun <- rbind.fill(vags_mun)

### Eleicao 2016

vags_mun2016 <- rbind.fill(vags_mun2016)


# 2. Limpeza e padronizacao dos dados -------------------------------------

## Divide o arquivo de eleicoes estaduais em dois: "vags_fed" e "vags_est" 
## Isto é, para os cargos de Deputado federal e Deputado estadual

## Seleciona as colunas necessarias,filtra, unifica e renomeia as colunas restantes

### Deputado Federal

vags_fed <- vags %>% 
  dplyr::select(V6, V9, V10) %>%
  dplyr::filter(V9 == "DEPUTADO FEDERAL")

vags_fed <- unique(vags_fed) ## Unifica as observacoes repetidas

vags_fed <- dplyr::rename(vags_fed,"UF" = "V6", 
                   "CARGO" = "V9",
                   "VAGAS" = "V10")

### Deputado Estadual

vags_est <- vags %>% 
  select(V6, V9, V10) %>% 
  filter(V9 == "DEPUTADO ESTADUAL" | V9 == "DEPUTADO DISTRITAL")

vags_est <- unique(vags_est)

vags_est <- dplyr::rename(vags_est,"UF" = "V6", 
                          "CARGO" = "V9",
                          "VAGAS" = "V10")


### Vereador

vags_vr <- vags_mun_ %>% 
  select(V3,V5,V6, V7,V9, V10) %>% 
  filter(V9 == "VEREADOR")

vags_vr <- vags_vr %>% 
  dplyr::rename("ANO_ELEICAO" = "V3",
                "UF" = "V5",
                "COD_MUN_TSE" = "V6",
                "NOME_MUNICIPIO" = "V7",
                "CARGO" = "V9",
                "VAGAS" = "V10")

vags_vr$NM <- rm_accent(vags_vr$NOME_MUNICIPIO) ## Remove os acentos dos nomes dos municipios

vags_vr <- vags_vr %>% 
  select(ANO_ELEICAO, UF,COD_MUN_TSE, NM, CARGO, VAGAS) %>% 
  dplyr::rename("NOME_MUNICIPIO" = "NM")

vags_vr$NOME_MUNICIPIO <- str_to_upper(vags_vr$NOME_MUNICIPIO) ## Padroniza os nomes em caixa alta


### Eleicao 2016

vags_vr1 <- vags_mun2016 %>% 
  select(ANO_ELEICAO, SG_UF, SG_UE,NM_UE, DS_CARGO, QT_VAGAS) %>% 
  filter(DS_CARGO == "Vereador")

vags_vr1 <- vags_vr1 %>% 
  dplyr::rename("UF" = "SG_UF",
                "COD_MUN_TSE" = "SG_UE",
                "NOME_MUNICIPIO" = "NM_UE",
                "CARGO" = "DS_CARGO",
                "VAGAS" = "QT_VAGAS")

vags_vr1$NM <- rm_accent(vags_vr1$NOME_MUNICIPIO) ## Remove os acentos dos nomes dos municipios

vags_vr1 <- vags_vr1 %>% 
  select(ANO_ELEICAO, UF,COD_MUN_TSE, NM, CARGO, VAGAS) %>% 
  dplyr::rename("NOME_MUNICIPIO" = "NM")

vags_vr1$NOME_MUNICIPIO <- str_to_upper(vags_vr1$NOME_MUNICIPIO) ## Padroniza os nomes em caixa alta

vags_vr1$CARGO <- str_to_upper(vags_vr1$CARGO)

### Junta o arquivo correspondente a eleicao de 2016 as demais eleicoes municipais

vags_ver <- rbind(vags_vr, vags_vr1)


# 3. Salva os arquivos ----------------------------------------------------

## Salva e exporta os arquivos de vagas em .csv

### Deputado Federal

write.csv(vags_fed, "vags_fed.csv")

### Deputado Estadual

write.csv(vags_est, "vags_est.csv")

### Vereador

write.csv(vags_ver, "vags_ver.csv")



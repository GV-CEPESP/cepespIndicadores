
# Teste dados TSE x CepespData 2004
# Autor: Rebeca Carvalho
# Data: 07/03/2019


rm(list = ls())


# Pacotes utilizados

library(cepespR)
library(stringr)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(magrittr)


# 1. Download dos dados ---------------------------------------------------


# Votacao por secao eleitoral 2004


url <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2004_UF.zip"

estados <- list(c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT",
                "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR",
                "SC", "SP", "SE", "TO"))


for(i in estados[[i]]){
  votacao <- stringr::str_replace(url, "UF", as.character(i)) 
  print(votacao)
  download.file(votacao, str_c("votacao", i, ".zip"))
}

list_vot <- list.files(pattern = "votacao")
  
for(i in seq_along(list_vot)){ 
  unzip(list_vot[i], exdir = "votacao") 
}


arq_vot <- list.files(path = "votacao")

vot <- list()

for (i in seq_along(arq_vot)) {
  cat("lendo", arq_vot[i], "\n")
  vot[[i]] <- read.table(file = paste0("votacao/",arq_vot[i]),header=F,sep=";", stringsAsFactors = FALSE)
}

vot <- rbind.fill(vot)


# Votacao nominal por município e zona 2004

list_votz <- list.files(pattern = "munzona")

for(i in seq_along(list_votz)){ 
  unzip(list_votz[i], exdir = "munzona") 
}


arq_votz <- list.files(path = "munzona")

votz <- list()

for (i in seq_along(arq_votz)) {
  cat("lendo", arq_votz[i], "\n")
  votz[[i]] <- read.table(file = paste0("munzona/",arq_votz[i]),header=F,sep=";", stringsAsFactors = FALSE)
}

votz <- rbind.fill(votz)



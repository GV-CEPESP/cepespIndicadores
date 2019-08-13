
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

#'       - Fazer o download dos arquivos referentes a eleicao de 1994;
#'       - Juntar os bancos em um unico.



# 1. Download -------------------------------------------------------------

## Salva um link "molde" dos arquivos da eleicao de 1994 e 
## faz o download para todos os estados


url_1994 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_1994.zip"

download.file(url_1994, "el94.zip")



## Cria uma lista com os arquivos com nomes correspondentes a "el94_" 
## Tambem unzipa todos os arquivos dentro da lista 


unzip("el94.zip", exdir = "el94") 

## Cria e le uma lista contendo os arquivos da pasta "el94" 

arq_el94 <- list.files(path = "el94", pattern = ".txt")

elei_94 <- list()

for (i in seq_along(arq_el94)) {
  cat("lendo", arq_el94[i], "\n")
  elei_94[[i]] <- read.table(file = paste0("el94/",arq_el94[i]),
                              header=F,sep=";", stringsAsFactors = FALSE)}

## Junta todos os arquivos em um unico banco


eleicoes_94 <- rbind.fill(elei_94)


# 2. Padronizacao dos dados -----------------------------------------------

eleicoes_94 <- eleicoes_94 %>% 
  filter(V10 == "DEPUTADO FEDERAL") %>% 
  select(V3,V6,V10,V11,V19,V28,V43) %>% 
  dplyr::rename("ANO_ELEICAO" = "V3",
         "UF" = "V6",
         "DESCRICAO_CARGO" = "V10",
         "NOME_CANDIDATO" = "V11",
         "SIGLA_PARTIDO" = "V19",
         "NUM_TITULO_ELEITORAL_CANDIDATO" = "V28",
         "DESC_SIT_TOT_TURNO" = "V43")



# 3. Salvando o arquivo ---------------------------------------------------

## Salva o arquivo da eleicao de 1994 em .csv

write.csv(eleicoes_94, "data/input/eleicoes_94.csv")

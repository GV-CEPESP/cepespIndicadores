
# Título: Script Vagas
# Autor: Rebeca Carvalho

rm(list = ls())

# Pacotes utilizados

library(stringr)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(magrittr)
library(abjutils)

# 1. Download dos dados ---------------------------------------------------

 # Eleicoes estaduais

url_vagas <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_ANO.zip"

 for(i in seq(1998,2018, by = 4)){
  vagas <- stringr::str_replace(url_vagas, "ANO", as.character(i)) 
  print(vagas)
  download.file(vagas, str_c("vagas", i, ".zip"))
 }


list_vag <- list.files(pattern = "vagas")##cria uma lista com os arquivos com nomes correspondentes a "arquivo_vagas"  

 for(i in seq_along(list_vag)){ 
  unzip(list_vag [i], exdir = "vagas") ##loop para unzipar todos os arquivos contidos dentro da lista
 } 


arq_vags <- list.files(path = "vagas")

vags <- list()

 for (i in seq_along(arq_vags)) {
  cat("lendo", arq_vags[i], "\n")
  vags[[i]] <- read.table(file = paste0("vagas/",arq_vags[i]),header=F,sep=";", stringsAsFactors = FALSE)
 }

 for(i in seq_along("vagas")){
  br_files_vagas <- list.files(path = "vagas", pattern = "BR",full.names = T)
  file.remove(br_files_vagas)
 }

 
vags <- rbind.fill(vags)

vags_fed <- vags %>% 
  dplyr::select(V6, V9, V10) %>%
  dplyr::filter(V9 == "DEPUTADO FEDERAL")

vags_fed <- unique(vags_fed)

vags_fed <- dplyr::rename(vags_fed,"UF" = "V6", 
                   "CARGO" = "V9",
                   "VAGAS" = "V10")

vags_est <- vags %>% 
  select(V6, V9, V10) %>% 
  filter(V9 == "DEPUTADO ESTADUAL" | V9 == "DEPUTADO DISTRITAL")

vags_est <- unique(vags_est)

vags_est <- dplyr::rename(vags_est,"UF" = "V6", 
                          "CARGO" = "V9",
                          "VAGAS" = "V10")


 # Eleicoes municipais


url_vagas_mun_ <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_ANO.zip"

for(i in seq(2000,2014, by = 4)){
  vagas_mun_ <- stringr::str_replace(url_vagas_mun_, "ANO", as.character(i)) 
  print(vagas_mun_)
  download.file(vagas_mun_, str_c("vagas_mun_", i, ".zip"))
}



list_vag_mun <- list.files(pattern = "vagas_mun_")##cria uma lista com os arquivos com nomes correspondentes a "arquivo_vagas"  

for(i in seq_along(list_vag_mun)){ 
  unzip(list_vag_mun [i], exdir = "vagas_mun_") ##loop para unzipar todos os arquivos contidos dentro da lista
} 


arq_vags_mun <- list.files(path = "vagas_mun_")

vags_mun_ <- list()

for (i in seq_along(arq_vags_mun)) {
  cat("lendo", arq_vags_mun[i], "\n")
  vags_mun_[[i]] <- read.table(file = paste0("vagas_mun_/",arq_vags_mun[i]),header=F,sep=";", stringsAsFactors = FALSE)
}

for(i in seq_along(vags_mun_)){
  br_files_vagas_mun <- list.files(path = "vags_mun_", pattern = "BR",full.names = T)
  file.remove(br_files_vagas_mun)
}


vags_mun_ <- rbind.fill(vags_mun_)

#Eleicoes 2016

url_vagas_2016 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_2016.zip"

download.file(url_vagas_2016, "vagas_mun2016.zip")

unzip("vagas_mun2016.zip", exdir = "vagas_mun2016")

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
 
vags_mun2016 <- rbind.fill(vags_mun2016)

# Tratamentos posteriores

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

vags_vr$NM <- rm_accent(vags_vr$NOME_MUNICIPIO)

vags_vr <- vags_vr %>% 
  select(ANO_ELEICAO, UF,COD_MUN_TSE, NM, CARGO, VAGAS) %>% 
  dplyr::rename("NOME_MUNICIPIO" = "NM")

vags_vr$NOME_MUNICIPIO <- str_to_upper(vags_vr$NOME_MUNICIPIO)


# Eleicao 2016

vags_vr1 <- vags_mun2016 %>% 
  select(ANO_ELEICAO, SG_UF, SG_UE,NM_UE, DS_CARGO, QT_VAGAS) %>% 
  filter(DS_CARGO == "Vereador")

vags_vr1 <- vags_vr1 %>% 
  dplyr::rename("UF" = "SG_UF",
                "COD_MUN_TSE" = "SG_UE",
                "NOME_MUNICIPIO" = "NM_UE",
                "CARGO" = "DS_CARGO",
                "VAGAS" = "QT_VAGAS")

vags_vr1$NM <- rm_accent(vags_vr1$NOME_MUNICIPIO)

vags_vr1 <- vags_vr1 %>% 
  select(ANO_ELEICAO, UF,COD_MUN_TSE, NM, CARGO, VAGAS) %>% 
  dplyr::rename("NOME_MUNICIPIO" = "NM")

vags_vr1$NOME_MUNICIPIO <- str_to_upper(vags_vr1$NOME_MUNICIPIO)

vags_vr1$CARGO <- str_to_upper(vags_vr1$CARGO)


vags_ver <- rbind(vags_vr, vags_vr1)

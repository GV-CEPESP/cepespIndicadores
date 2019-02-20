
# Título: Script Vagas
# Autor: Rebeca Carvalho

rm(list = ls())


# 1. Download dos dados ---------------------------------------------------

 # Eleicoes estaduais

url_vagas <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_ANO.zip"

 for(i in seq(1998,2010, by = 4)){
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

 for(i in seq_along(vagas)){
  br_files_vagas <- list.files(path = "vagas", pattern = "BR",full.names = T)
  file.remove(br_files_vagas)
 }

 
arvg <- rbind.fill(vags)


 # Eleicoes municipais

url_vagas_mun <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_ANO.zip"

for(i in seq(2000,2016, by = 4)){
  vagas_mun <- stringr::str_replace(url_vagas_mun, "ANO", as.character(i)) 
  print(vagas_mun)
  download.file(vagas_mun, str_c("vagas_mun", i, ".zip"))
}


list_vag_mun <- list.files(pattern = "vagas_mun")##cria uma lista com os arquivos com nomes correspondentes a "arquivo_vagas"  

for(i in seq_along(list_vag_mun)){ 
  unzip(list_vag_mun [i], exdir = "vagas_mun") ##loop para unzipar todos os arquivos contidos dentro da lista
} 


arq_vags_mun <- list.files(path = "vagas_mun")

vags_mun <- list()

for (i in seq_along(arq_vags_mun)) {
  cat("lendo", arq_vags_mun[i], "\n")
  vags_mun[[i]] <- read.table(file = paste0("vagas_mun/",arq_vags_mun[i]),header=F,sep=";", stringsAsFactors = FALSE)
}

for(i in seq_along(vagas_mun)){
  br_files_vagas_mun <- list.files(path = "vagas_mun", pattern = "BR",full.names = T)
  file.remove(br_files_vagas_mun)
}


t <- rbind.fill(vags_mun)

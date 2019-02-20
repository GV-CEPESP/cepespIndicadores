
# Titulo: Shiny dos indicadores CepespData
# Autor: Rebeca Carvalho


rm(list = ls())


# Pacotes utilizados


library(cepespR)
library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)



# 1. Dados ----------------------------------------------------------------

 # Deputado Federal

df <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Federal",
                    regional_aggregation = "Brasil", political_aggregation = "Partido")

dfc <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Federal",
                     regional_aggregation = "Brasil", political_aggregation = "Consolidado")

 # Deputado Estadual

de <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Estadual",
                    regional_aggregation = "Estado", political_aggregation = "Partido")

dec <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", position = "Deputado Estadual",
                     regional_aggregation = "Estado", political_aggregation = "Consolidado")

 # Vereador


vr <- get_elections(year = "2000, 2004, 2008, 2012, 2016", position = "Vereador",
                    regional_aggregation = "Municipio", political_aggregation = "Partido")

vrc <- get_elections(year = "2000, 2004, 2008, 2012, 2016", position = "Vereador",
                     regional_aggregation = "Municipio", political_aggregation = "Consolidado", dev = TRUE)

 # Cidades

cidades <- dplyr::count_(vr, c("UF", "NOME_MUNICIPIO"))

cidades <- select(cidades, "UF", "NOME_MUNICIPIO")

# Quociente eleitoral e magnitude distrital 2018

magn <- read.csv2("quociente_eleitoraL_partidario.csv")

magn_fed <- filter(magn, Cargo == "Deputado Federal")

magn_est <- filter(magn, Cargo == "Deputado Estadual")

vagas_fed <- unique(select(magn_fed, c("UF", "Vagas")))

vagas_est <- unique(select(magn_est, c("UF", "Vagas")))


 # Vagas

 url_vagas <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_ANO.zip"
 
 for(i in seq(1998,2010, by = 4)){
   vagas <- stringr::str_replace(url_vagas, "ANO", as.character(i)) 
   print(vagas)
   download.file(vagas, str_c("vagas", i, ".zip"))
 }
 
 
 list_vag <- list.files(pattern = "vagas")##cria uma lista com os arquivos com nomes correspondentes a "vagas"  
 
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
 
 
 t <- arvg %>% 
   group_by(V3,V6,V9) %>% 
   count(V10)
 
 x <- filter(t, V9 == "DEPUTADO FEDERAL")

# 2. Tranformacoes primarias ----------------------------------------------

 
df <- dplyr::rename(df, "UF" = "SIGLA_UE")

df$AGREGACAO_REGIONAL <- "BRASIL"

df <- df %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS)

de$AGREGACAO_REGIONAL <- "UF"

de <- de %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS) 

vr$AGREGACAO_REGIONAL <- "MUNICIPIO"

vr <- vr %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,NOME_MUNICIPIO,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS)

 # Votação total e por UF dos partidos

df1 <- df %>%  
  dplyr::group_by(ANO_ELEICAO, UF, SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_UF = sum(QTDE_VOTOS))


df2 <- df1 %>% 
  dplyr::group_by(ANO_ELEICAO,SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    TOT_VOT_PART = sum(VOT_PART_UF)
  )

de1 <- de %>% 
  dplyr::group_by(ANO_ELEICAO, UF,SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_UF = sum(QTDE_VOTOS)
  ) 

de2 <- de1 %>% 
  dplyr::group_by(ANO_ELEICAO,SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    TOT_VOT_PART = sum(VOT_PART_UF)
  )

vr1 <- vr %>% 
  dplyr::group_by(ANO_ELEICAO, NOME_MUNICIPIO,SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_MUN = sum(QTDE_VOTOS)
  ) 

vr2 <- vr1 %>% 
  dplyr::group_by(ANO_ELEICAO,SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    TOT_VOT_PART = sum(VOT_PART_MUN)
  )

 # Votos validos de cada eleicao

  # Deputado Federal

dfc1 <- dfc %>% 
  dplyr::group_by(ANO_ELEICAO) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS = sum(QT_VOTOS_NOMINAIS,QT_VOTOS_LEGENDA)
  )

  # Deputado Estadual

dec1 <- dec %>% 
  dplyr::group_by(ANO_ELEICAO,UF) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_UF = sum(QT_VOTOS_NOMINAIS,QT_VOTOS_LEGENDA)
  )

  # Vereador

vrc1 <- vrc %>% 
  group_by(ANO_ELEICAO)



# 3. Join -----------------------------------------------------------------

 # Deputado Federal

df <- left_join(df, dfc, by = c("ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_CARGO")) 

df <- df %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL, DESCRICAO_CARGO, NUMERO_PARTIDO,SIGLA_PARTIDO,
         QTDE_VOTOS, QTD_APTOS,QTD_COMPARECIMENTO,QTD_ABSTENCOES,QT_VOTOS_NOMINAIS,
         QT_VOTOS_BRANCOS, QT_VOTOS_NULOS,QT_VOTOS_LEGENDA, QT_VOTOS_ANULADOS_APU_SEP)


df <- left_join(df, df1, by = c("ANO_ELEICAO", "UF", "SIGLA_PARTIDO"))

df <- left_join(df, df2, by = c("ANO_ELEICAO", "SIGLA_PARTIDO"))

df <- left_join(df,dfc1, by = "ANO_ELEICAO")

 # Deputado Estadual

de <- left_join(de, dec, by = c("ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_CARGO","UF")) 

de <- de %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF, AGREGACAO_REGIONAL,DESCRICAO_CARGO, NUMERO_PARTIDO,SIGLA_PARTIDO,
         QTDE_VOTOS, QTD_APTOS,QTD_COMPARECIMENTO,QTD_ABSTENCOES,QT_VOTOS_NOMINAIS,
         QT_VOTOS_BRANCOS, QT_VOTOS_NULOS,QT_VOTOS_LEGENDA, QT_VOTOS_ANULADOS_APU_SEP)


de <- left_join(de, de1, by = c("ANO_ELEICAO", "UF", "SIGLA_PARTIDO"))

de <- left_join(de, de2, by = c("ANO_ELEICAO", "SIGLA_PARTIDO"))


de <- left_join(de,dec1, by = c("ANO_ELEICAO","UF"))

 # Vereador

vr <- left_join(vr, vr1, by = c("ANO_ELEICAO", "NOME_MUNICIPIO", "SIGLA_PARTIDO"))


# 4. Calculo --------------------------------------------------------------

 

# 4.1. Indicadores de fragmentacao legislativa ----------------------------


 # Quociente eleitoral


quoc_eleit <- function(x){
  x/y
}

QE <- data.frame(unique(quoc_eleit(df$VOTOS_VALIDOS, vagas_fed$Vagas)))

  QE$ANO_ELEICAO <- c("2018", "2014", "2010", "2006", "2002", "1998") 
  
  QE <- QE %>% 
    dplyr::rename("QUOCIENTE_ELEITORAL" = "unique.quoc_eleit.df.VOTOS_VALIDOS..") %>% 
    select(ANO_ELEICAO, QUOCIENTE_ELEITORAL) %>% 
    arrange(QE$ANO_ELEICAO)
  
 # Quociente partidario
  
  
 # Fracionalizacao 
  
  
 # Fracionalizacao maxima  
  
  
 # Fragmentacao
  

 # Desproporcionalidade de Gallagher
  

 # Número efetivo de partidos
  
  
  

# 4.2. Indicadores de renovacao das bancadas ------------------------------



# 4.3. Indicadores de alienacao -------------------------------------------
  

  
# 4.4. Indicadores de distribuicao das cadeiras ---------------------------  


  
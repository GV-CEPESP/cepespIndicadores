
# Titulo: Shiny dos indicadores CepespData
# Autor: Rebeca Carvalho


rm(list = ls())


# Pacotes utilizados


library(cepespR)
library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(abjutils)
library(data.table)


# 1. Dados ----------------------------------------------------------------


source("script_vagas.R")

 # Deputado Federal

df <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Federal",
                    regional_aggregation = "Estado", political_aggregation = "Partido")

dfc <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Federal",
                     regional_aggregation = "Estado", political_aggregation = "Consolidado")

 # Deputado Estadual

de <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Estadual",
                    regional_aggregation = "Estado", political_aggregation = "Partido")

dec <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", position = "Deputado Estadual",
                     regional_aggregation = "Estado", political_aggregation = "Consolidado")

 # Vereador


vr <- get_elections(year = "2000, 2004, 2008, 2012, 2016", position = "Vereador",
                    regional_aggregation = "Municipio", political_aggregation = "Partido")

vrc <- get_elections(year = "2000, 2004, 2008, 2012, 2016", position = "Vereador",
                     regional_aggregation = "Municipio", political_aggregation = "Consolidado")

 # Cidades

cidades <- dplyr::count_(vr, c("UF", "NOME_MUNICIPIO"))

cidades <- select(cidades, "UF", "NOME_MUNICIPIO")

# Quociente eleitoral e magnitude distrital 2018

magn <- read.csv2("quociente_eleitoraL_partidario.csv")



# 2. Tranformacoes primarias ----------------------------------------------

# Deputado Federal

df$AGREGACAO_REGIONAL <- "BRASIL"

df <- df %>% 
  dplyr::select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS)

# Deputado Estadual

de$AGREGACAO_REGIONAL <- "UF"

de <- de %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS) 

# Vereador

vr$NM <- rm_accent(vr$NOME_MUNICIPIO)

vrc$NM <- rm_accent(vrc$NOME_MUNICIPIO)

vr$AGREGACAO_REGIONAL <- "MUNICIPIO"

vr <- vr %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,COD_MUN_TSE,NM,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS) %>% 
  dplyr::rename("NOME_MUNICIPIO" = "NM")

vr$NOME_MUNICIPIO <- str_to_upper(vr$NOME_MUNICIPIO)

vrc <- vrc %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF, COD_MUN_TSE,NM, DESCRICAO_CARGO, QTD_APTOS, QTD_COMPARECIMENTO, QTD_ABSTENCOES,
         QT_VOTOS_NOMINAIS, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QT_VOTOS_LEGENDA, QT_VOTOS_ANULADOS_APU_SEP) %>% 
  dplyr::rename("NOME_MUNICIPIO" = "NM")
  
vrc$NOME_MUNICIPIO <- str_to_upper(vrc$NOME_MUNICIPIO)


 # Votação UF dos partidos

# Deputado Federal

df1 <- df %>%  
  dplyr::group_by(ANO_ELEICAO, UF, SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_UF = sum(QTDE_VOTOS))

# Deputado Estadual

de1 <- de %>% 
  dplyr::group_by(ANO_ELEICAO, UF,SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_UF = sum(QTDE_VOTOS)
  ) 

# Vereador

vr1 <- vr %>% 
  dplyr::group_by(ANO_ELEICAO, COD_MUN_TSE,NOME_MUNICIPIO,SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_MUN = sum(QTDE_VOTOS)
  ) 

 # Votos validos de cada eleicao

  # Deputado Federal

dfc1 <- dfc %>% 
  dplyr::group_by(ANO_ELEICAO, UF) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_UF = sum(QT_VOTOS_NOMINAIS,QT_VOTOS_LEGENDA)
  )

  # Deputado Estadual

dec1 <- dec %>% 
  dplyr::group_by(ANO_ELEICAO,UF) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_UF = sum(QT_VOTOS_NOMINAIS,QT_VOTOS_LEGENDA)
  )

  # Vereador

vrc1 <- vrc %>% 
  group_by(ANO_ELEICAO, UF, COD_MUN_TSE,NOME_MUNICIPIO) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_MUN = sum(QT_VOTOS_NOMINAIS, QT_VOTOS_LEGENDA))



# 3. Join -----------------------------------------------------------------

 # Deputado Federal

vags_fed <- left_join(vags_fed,dfc1, by = "UF")

vags_fed <- left_join(vags_fed, df1, by = c("ANO_ELEICAO", "UF"))

fed <- left_join(dfc1, vags_fed, by = "UF")

 # Deputado Estadual

vags_est <- left_join(vags_est,dec1, by = "UF")


de <- left_join(de,dec1, by = c("ANO_ELEICAO","UF"))

est <- left_join(dec1, vags_est, by = "UF")

vags_est <- left_join(vags_est, de1, by = c("ANO_ELEICAO", "UF"))


 # Vereador

data.table::setDT(vrc1)
vrc1[, (colnames(vrc1)) := lapply(.SD, as.character), .SDcols = colnames(vrc1)] 

data.table::setDT(vags_ver)
vags_ver[, (colnames(vags_ver)) := lapply(.SD, as.character), .SDcols = colnames(vags_ver)] 

glimpse(vags_ver)

vrc1 <- dplyr::mutate_all(vrc1, .funs = toupper)

vags_ver <- left_join(vags_ver, vrc1, by = c("ANO_ELEICAO", "UF", "COD_MUN_TSE"))


# 4. Calculo --------------------------------------------------------------

 

# 4.1. Indicadores de fragmentacao legislativa ----------------------------


 # Quociente eleitoral

  ## Deputados Federais


vags_fed$QUOCIENTE_ELEITORAL <- vags_fed$VOTOS_VALIDOS_UF/as.numeric(vags_fed$VAGAS)

  ## Deputados Estaduais

vags_est$QUOCIENTE_ELEITORAL <- vags_est$VOTOS_VALIDOS_UF/as.numeric(vags_est$VAGAS)


# Quociente partidario

  ## Deputados Federais
  
vags_fed$QUOCIENTE_PARTIDARIO <- vags_fed$VOT_PART_UF/vags_fed$QUOCIENTE_ELEITORAL

  ## Deputados Estaduais

vags_est$QUOCIENTE_PARTIDARIO <- vags_est$VOT_PART_UF/vags_est$QUOCIENTE_ELEITORAL

# Numero de cadeiras

  ## Deputados Federais

vags_fed$NUM_CADEIRAS <- floor(vags_fed$QUOCIENTE_PARTIDARIO)

  ## Deputados Estaduais

vags_est$NUM_CADEIRAS <- floor(vags_est$QUOCIENTE_PARTIDARIO)
  
 # Fracionalizacao 
  
  
 # Fracionalizacao maxima  
  
  
 # Fragmentacao
  

 # Desproporcionalidade de Gallagher
  

 # Número efetivo de partidos
  
  
  

# 4.2. Indicadores de renovacao das bancadas ------------------------------



# 4.3. Indicadores de alienacao -------------------------------------------
  

  
# 4.4. Indicadores de distribuicao das cadeiras ---------------------------  


  
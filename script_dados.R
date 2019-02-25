
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


# 1. Dados ----------------------------------------------------------------

<<<<<<< HEAD
#source("script_vagas.R")
=======
source("script_vagas.R")
>>>>>>> fb1333f6ed6343e2eeefffd5d3d2ff8ba652239d

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
                     regional_aggregation = "Municipio", political_aggregation = "Consolidado", dev = TRUE)


 # Cidades

cidades <- dplyr::count_(vr, c("UF", "NOME_MUNICIPIO"))

cidades <- select(cidades, "UF", "NOME_MUNICIPIO")

# Quociente eleitoral e magnitude distrital 2018

magn <- read.csv2("quociente_eleitoraL_partidario.csv")


<<<<<<< HEAD
 
# 2. Tranformacoes primarias ----------------------------------------------

 
=======

# 2. Tranformacoes primarias ----------------------------------------------


>>>>>>> fb1333f6ed6343e2eeefffd5d3d2ff8ba652239d
df$AGREGACAO_REGIONAL <- "BRASIL"

df <- df %>% 
  dplyr::select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS)

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


de1 <- de %>% 
  dplyr::group_by(ANO_ELEICAO, UF,SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_UF = sum(QTDE_VOTOS)
  ) 

<<<<<<< HEAD
=======

>>>>>>> fb1333f6ed6343e2eeefffd5d3d2ff8ba652239d
vr1 <- vr %>% 
  dplyr::group_by(ANO_ELEICAO, NOME_MUNICIPIO,SIGLA_PARTIDO) %>% 
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
<<<<<<< HEAD
  group_by(ANO_ELEICAO,UF, NOME_MUNICIPIO) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_UF = sum(QT_VOTOS_NOMINAIS,QT_VOTOS_LEGENDA)
=======
  group_by(ANO_ELEICAO, UF, NOME_MUNICIPIO) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_MUN = sum(QT_VOTOS_NOMINAIS, QT_VOTOS_LEGENDA)
>>>>>>> fb1333f6ed6343e2eeefffd5d3d2ff8ba652239d
  )



# 3. Join -----------------------------------------------------------------

 # Deputado Federal

vags_fed <- left_join(vags_fed,dfc1, by = "UF")

<<<<<<< HEAD
df <- left_join(df, df1, by = c("ANO_ELEICAO", "UF", "SIGLA_PARTIDO"))

df <- left_join(df,dfc1, by = "ANO_ELEICAO")
=======
vags_fed <- left_join(vags_fed, df1, by = c("ANO_ELEICAO", "UF"))
>>>>>>> fb1333f6ed6343e2eeefffd5d3d2ff8ba652239d

fed <- left_join(dfc1, vagas_fed, by = "UF")

 # Deputado Estadual

vags_est <- left_join(vags_est,dec1, by = "UF")

<<<<<<< HEAD

de <- left_join(de,dec1, by = c("ANO_ELEICAO","UF"))

est <- left_join(dec1, vagas_est, by = "UF")

=======
vags_est <- left_join(vags_est, de1, by = c("ANO_ELEICAO", "UF"))


>>>>>>> fb1333f6ed6343e2eeefffd5d3d2ff8ba652239d
 # Vereador

vags_ver <- left_join(vags_ver, verc1, by = c("ANO_ELEICAO", "UF", "NOME_MUNICIPIO"))

vags_ver <- left_join(vags_mun, vrc1, by = c("ANO_ELEICAO", "UF", "NOME_MUNICIPIO"))

data.table::setDT(vrc1)
vrc1[, (colnames(vrc1)) := lapply(.SD, as.character), .SDcols = colnames(vrc1)] 


ver <- left_join(vrc1, vags_ver, by = c("ANO_ELEICAO", "UF", "NOME_MUNICIPIO"))



# 4. Calculo --------------------------------------------------------------

 

# 4.1. Indicadores de fragmentacao legislativa ----------------------------


 # Quociente eleitoral

  ## Deputados Federais


vags_fed$QUOCIENTE_ELEITORAL <- vags_fed$VOTOS_VALIDOS_UF/as.numeric(vags_fed$VAGAS)

<<<<<<< HEAD
QE <- data.frame(unique(quoc_eleit(df$VOTOS_VALIDOS)))
=======
forma
>>>>>>> fb1333f6ed6343e2eeefffd5d3d2ff8ba652239d


# Quociente partidario
  
vags_fed$QUOCIENTE_PARTIDARIO <- vags_fed$VOT_PART_UF/vags_fed$QUOCIENTE_ELEITORAL

  
 # Fracionalizacao 
  
  
 # Fracionalizacao maxima  
  
  
 # Fragmentacao
  

 # Desproporcionalidade de Gallagher
  

 # Número efetivo de partidos
  
  
  

# 4.2. Indicadores de renovacao das bancadas ------------------------------



# 4.3. Indicadores de alienacao -------------------------------------------
  

  
# 4.4. Indicadores de distribuicao das cadeiras ---------------------------  


  
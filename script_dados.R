
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
library(ggplot2)
library(ggfortify)
library(ggExtra)

# 1. Dados ----------------------------------------------------------------

source("script_vagas.R", encoding = "UTF-8")

 # Deputado Federal

df <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Federal",
                    regional_aggregation = "Estado", political_aggregation = "Partido")

dfc <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Federal",
                     regional_aggregation = "Estado", political_aggregation = "Consolidado")

dfc_ <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Federal",
                      regional_aggregation = "Estado", political_aggregation = "Candidato")

 # Deputado Estadual

de <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Estadual",
                    regional_aggregation = "Estado", political_aggregation = "Partido")

dec <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", position = "Deputado Estadual",
                     regional_aggregation = "Estado", political_aggregation = "Consolidado")

dec_ <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", position = "Deputado Estadual",
                     regional_aggregation = "Estado", political_aggregation = "Candidato")

 # Vereador


# vr <- get_elections(year = "2000, 2004, 2008, 2012, 2016", position = "Vereador",
                    #regional_aggregation = "Municipio", political_aggregation = "Partido")

# vrc <- get_elections(year = "2000, 2004, 2008, 2012, 2016", position = "Vereador",
                     #regional_aggregation = "Municipio", political_aggregation = "Consolidado")

 # Cidades

#cidades <- dplyr::count_(vr, c("UF", "NOME_MUNICIPIO"))

#cidades <- select(cidades, "UF", "NOME_MUNICIPIO")

# Quociente eleitoral e magnitude distrital 2018





# 2. Tranformacoes primarias ----------------------------------------------

# Deputado Federal

df$AGREGACAO_REGIONAL <- "BRASIL"

df <- df %>% 
  dplyr::select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS)

# Deputado Estadual

de$AGREGACAO_REGIONAL <- "UF"

de <- de %>% 
  dplyr::select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS) 

# Vereador

#vr$NM <- rm_accent(vr$NOME_MUNICIPIO)

#vrc$NM <- rm_accent(vrc$NOME_MUNICIPIO)

#vr$AGREGACAO_REGIONAL <- "MUNICIPIO"

#vr <- vr %>% 
  #select(ANO_ELEICAO, NUM_TURNO, UF,COD_MUN_TSE,NM,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS) %>% 
  #dplyr::rename("NOME_MUNICIPIO" = "NM")

#vr$NOME_MUNICIPIO <- str_to_upper(vr$NOME_MUNICIPIO)

#vrc <- vrc %>% 
  #select(ANO_ELEICAO, NUM_TURNO, UF, COD_MUN_TSE,NM, DESCRICAO_CARGO, QTD_APTOS, QTD_COMPARECIMENTO, QTD_ABSTENCOES,
         #QT_VOTOS_NOMINAIS, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QT_VOTOS_LEGENDA, QT_VOTOS_ANULADOS_APU_SEP) %>% 
  #dplyr::rename("NOME_MUNICIPIO" = "NM")
  
#vrc$NOME_MUNICIPIO <- str_to_upper(vrc$NOME_MUNICIPIO)


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

#vr1 <- vr %>% 
  #dplyr::group_by(ANO_ELEICAO, COD_MUN_TSE,NOME_MUNICIPIO,SIGLA_PARTIDO) %>% 
  #dplyr::summarise(
    #VOT_PART_MUN = sum(QTDE_VOTOS)) 

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

#vrc1 <- vrc %>% 
  #group_by(ANO_ELEICAO, UF, COD_MUN_TSE,NOME_MUNICIPIO) %>% 
  #dplyr::summarise(
    #VOTOS_VALIDOS_MUN = sum(QT_VOTOS_NOMINAIS, QT_VOTOS_LEGENDA))



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

#data.table::setDT(vrc1)
#vrc1[, (colnames(vrc1)) := lapply(.SD, as.character), .SDcols = colnames(vrc1)] 

#data.table::setDT(vags_ver)
#vags_ver[, (colnames(vags_ver)) := lapply(.SD, as.character), .SDcols = colnames(vags_ver)] 

#glimpse(vags_ver)

#vrc1 <- dplyr::mutate_all(vrc1, .funs = toupper)

#vags_ver <- left_join(vags_ver, vrc1, by = c("ANO_ELEICAO", "UF", "COD_MUN_TSE"))


# 4. Calculo --------------------------------------------------------------

 

# 4.1. Indicadores de fragmentacao legislativa ----------------------------


# 4.1.1. Numero de cadeiras -----------------------------------------------

 # Deputado Federal

num_df <- dfc_ %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|DESC_SIT_TOT_TURNO == "ELEITO POR QP"|DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

num_df <- num_df %>% 
  dplyr::group_by(ANO_ELEICAO,DESCRICAO_CARGO, SIGLA_PARTIDO, UF) %>% 
  dplyr::summarise("Cadeiras conquistadas por UF" = n())

num_df1 <- num_df %>% 
  dplyr::group_by(ANO_ELEICAO,DESCRICAO_CARGO, SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    "Total de cadeiras conquistadas" = sum(`Cadeiras conquistadas por UF`))

numc_df <- left_join(num_df, num_df1, by = c("ANO_ELEICAO", "DESCRICAO_CARGO", "SIGLA_PARTIDO"))

numc_df <- numc_df %>% 
  dplyr::select(ANO_ELEICAO, UF,DESCRICAO_CARGO, SIGLA_PARTIDO, `Cadeiras conquistadas por UF`, `Total de cadeiras conquistadas`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", "Cargo" = "DESCRICAO_CARGO", "Sigla do Partido" = "SIGLA_PARTIDO")

numc_df$Cargo <- str_to_title(numc_df$Cargo)

 # Deputado Estadual

num_de <- dec_ %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|DESC_SIT_TOT_TURNO == "ELEITO POR QP"|DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

num_de <- num_de %>% 
  dplyr::group_by(ANO_ELEICAO, DESCRICAO_CARGO, SIGLA_PARTIDO, UF) %>% 
  dplyr::summarise("Cadeiras conquistadas por UF" = n())

num_de1 <- num_de %>% 
  dplyr::group_by(ANO_ELEICAO, DESCRICAO_CARGO, SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    "Total de cadeiras conquistadas" = sum(`Cadeiras conquistadas por UF`))

numc_de <- left_join(num_de, num_de1, by = c("ANO_ELEICAO", "DESCRICAO_CARGO", "SIGLA_PARTIDO"))

numc_de <- numc_de %>% 
  dplyr::select(ANO_ELEICAO, UF,DESCRICAO_CARGO, SIGLA_PARTIDO, `Cadeiras conquistadas por UF`, `Total de cadeiras conquistadas`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", "Cargo" = "DESCRICAO_CARGO", "Sigla do Partido" = "SIGLA_PARTIDO")

numc_de$Cargo <- str_to_title(numc_de$Cargo)

# 4.1.2. Fracionalizacao --------------------------------------------------

  # Deputado Federal

num_df1$`Percentual de cadeiras` <- num_df1$`Total de cadeiras conquistadas`/513

numc_df$`Percentual de cadeiras` <- numc_df$`Total de cadeiras conquistadas`/513



fracio <- function(x){
  
  1-(sum(x^2))
}




t98df <- num_df1 %>% 
  filter(ANO_ELEICAO == 1998) 

t98df$Fracionalização <- fracio(t98df$`Percentual de cadeiras`)

t02df <- num_df1 %>% 
  filter(ANO_ELEICAO == 2002) 

t02df$Fracionalização <- fracio(t02df$`Percentual de cadeiras`)

t06df <- num_df1 %>% 
  filter(ANO_ELEICAO == 2006) 

t06df$Fracionalização <- fracio(t06df$`Percentual de cadeiras`)

t10df <- num_df1 %>% 
  filter(ANO_ELEICAO == 2010) 

t10df$Fracionalização <- fracio(t10df$`Percentual de cadeiras`)

t14df <- num_df1 %>% 
  filter(ANO_ELEICAO == 2014)

t14df$Fracionalização <- fracio(t14df$`Percentual de cadeiras`)

t18df <- num_df1 %>% 
  filter(ANO_ELEICAO == 2018) 

t18df$Fracionalização <- fracio(t18df$`Percentual de cadeiras`)


# 4.1.3. Fracionalizacao maxima  ------------------------------------------


fracio_max <- function(N, n){
  
  (N*(n-1))/(n*(N-1))
  
}



t98df$`Fracionalização máxima`<- fracio_max(513,18)

t02df$`Fracionalização máxima`<- fracio_max(513,19) 

t06df$`Fracionalização máxima`<- fracio_max(513,21)

t10df$`Fracionalização máxima`<- fracio_max(513,22)

t14df$`Fracionalização máxima`<- fracio_max(513,28)

t18df$`Fracionalização máxima`<- fracio_max(513,30)

# 4.1.4. Fragmentacao -----------------------------------------------------


frag <- function(fracio, fracio_max){
  
  fracio/fracio_max
}


  
t98df$Fragmentação <- frag(t98df$Fracionalização, t98df$`Fracionalização máxima`)

t02df$Fragmentação <- frag(t02df$Fracionalização, t02df$`Fracionalização máxima`)

t06df$Fragmentação <- frag(t06df$Fracionalização, t06df$`Fracionalização máxima`)

t10df$Fragmentação <- frag(t10df$Fracionalização, t10df$`Fracionalização máxima`)

t14df$Fragmentação <- frag(t14df$Fracionalização, t14df$`Fracionalização máxima`)

t18df$Fragmentação <- frag(t18df$Fracionalização, t18df$`Fracionalização máxima`)


frag_partdf <- bind_rows(t98df, t02df, t06df, t10df, t14df, t18df) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", "Cargo" = "DESCRICAO_CARGO", "Sigla do partido" = SIGLA_PARTIDO)

frag_partdf$Cargo <- str_to_title(frag_partdf$Cargo)


# 4.1.5.  Desproporcionalidade de Gallagher -------------------------------

  # Deputado Federal


desp_gallg <- function(Vi, Si){
  
  sqrt(1/2*sum(Vi - Si)^2)
}

# 4.1.6. Número efetivo de partidos por votos ---------------------------------------



# 4.1.7. Número efetivo de partidos por cadeiras ---------------------------------------



# 4.2. Indicadores de renovacao das bancadas ------------------------------



# 4.3. Indicadores de alienacao -------------------------------------------
  
  # Deputado Federal

dfc$Alienação <- dfc$QTD_ABSTENCOES + dfc$QT_VOTOS_BRANCOS + dfc$QT_VOTOS_NULOS

dfc <- dfc %>% 
  dplyr::select(ANO_ELEICAO,UF, DESCRICAO_CARGO, QTD_ABSTENCOES, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, Alienação) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", "Cargo" = "DESCRICAO_CARGO", "Quantidade de abstenções" = "QTD_ABSTENCOES",
                "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", "Quantidade de votos nulos" = "QT_VOTOS_NULOS")
  

dfc$Cargo <- str_to_title(dfc$Cargo)


  # Deputado Estadual

dec$Alienação <- dec$QTD_ABSTENCOES + dec$QT_VOTOS_BRANCOS + dec$QT_VOTOS_NULOS

dec <- dec %>% 
  dplyr::select(ANO_ELEICAO,UF, DESCRICAO_CARGO, QTD_ABSTENCOES, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, Alienação) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", "Cargo" = "DESCRICAO_CARGO", "Quantidade de abstenções" = "QTD_ABSTENCOES",
                "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", "Quantidade de votos nulos" = "QT_VOTOS_NULOS")

dec$Cargo <- str_to_title(dec$Cargo)

# 4.4. Indicadores de distribuicao das cadeiras ---------------------------  

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


# 5. Tabelas --------------------------------------------------------------


# 5.1. Quociente eleitoral ------------------------------------------------

 gabi<-function(string){
  
  paste0(round(string/1000,0),".", substr(round(string,0), start = nchar(round(string,0))- 2, stop = nchar(round(string,0))),
         ifelse(round(string,2)==round(string,0),"",
                paste0(",",substr(1 + round(string,2)-round(string,0),start = 3, stop = 4))))
  }


vags_fed$QUOCIENTE_ELEITORAL <-gabi(vags_fed$QUOCIENTE_ELEITORAL)
vags_fed$QUOCIENTE_PARTIDARIO <- round(vags_fed$QUOCIENTE_PARTIDARIO, digits = 2)

vags_est$QUOCIENTE_ELEITORAL <-gabi(vags_est$QUOCIENTE_ELEITORAL)
vags_est$QUOCIENTE_PARTIDARIO <- round(vags_est$QUOCIENTE_PARTIDARIO, digits = 2)

vags_fed <- vags_fed %>% 
  mutate(C = str_to_title(vags_fed$CARGO))



# Deputado Federal

vags_fed <- vags_fed %>% 
  dplyr::select(ANO_ELEICAO, UF, C, VAGAS, VOTOS_VALIDOS_UF,SIGLA_PARTIDO, VOT_PART_UF, QUOCIENTE_ELEITORAL, QUOCIENTE_PARTIDARIO) %>% 
   dplyr::rename("Ano da eleição" = "ANO_ELEICAO", "Cargo" = "C", "Vagas" = "VAGAS", "Votos válidos " = "VOTOS_VALIDOS_UF",
                 "Sigla do partido" = "SIGLA_PARTIDO", "Votos válidos do partido" = "VOT_PART_UF", "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
                 "Quociente partidário" = "QUOCIENTE_PARTIDARIO") 

qef <- vags_fed %>% 
  select(`Ano da eleição`, UF, `Quociente eleitoral`)

 qef <- unique(qef)
 
 
# Deputado Estadual
 
 vags_est <- vags_est %>% 
   mutate(C = str_to_title(vags_est$CARGO))
 
 vags_est <- vags_est %>% 
   dplyr::select(ANO_ELEICAO, UF, C, VAGAS, VOTOS_VALIDOS_UF,SIGLA_PARTIDO, VOT_PART_UF, QUOCIENTE_ELEITORAL, QUOCIENTE_PARTIDARIO) %>% 
   dplyr::rename("Ano da eleição" = "ANO_ELEICAO", "Cargo" = "C", "Vagas" = "VAGAS", "Votos válidos " = "VOTOS_VALIDOS_UF",
                 "Sigla do partido" = "SIGLA_PARTIDO", "Votos válidos do partido" = "VOT_PART_UF", "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
                 "Quociente partidário" = "QUOCIENTE_PARTIDARIO") 
 
qee <- vags_est %>% 
  select(`Ano da eleição`, UF, `Quociente eleitoral`)
 
 qee <- unique(qee)
 

# 5.2. Quociente partidario -----------------------------------------------

# Deputado Federal
 
 qpf <- vags_fed %>% 
   dplyr::select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
 
 qpf <- unique(qpf)
 

 
 # Deputado estadual
 
 qpe <- vags_est %>% 
   dplyr::select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)

 qpe <- unique(vags_est)
 
 
 

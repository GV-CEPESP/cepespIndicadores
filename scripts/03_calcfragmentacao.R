

# Pacotes utilizados

library(dplyr)
library(lubridate)
library(tidyverse)


# Objetivo
#'        - Calcular os indicadores de fragmentacao legislativa:
#'        - Numero de cadeiras,Fracionalizacao,Fragmentacao, Fragmentacao máxima,
#'        - Desproporcionalidade de Gallagher, Numero efetivo 
#'              de partidos por votos e por cadeiras;
#'        - Limpeza e padronizacao dos dados.             
        


# 1. Numero de cadeiras ---------------------------------------------------------


## Filtra os candidatos que foram eleitos

### Deputado Federal

df <- df %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

### Deputado Estadual

de <- de %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

## Soma as cadeiras conquistadas pelos partidos em cada UF

### Deputado Federal

df <- df %>% 
  dplyr::group_by(ANO_ELEICAO,
                  DESCRICAO_CARGO, 
                  SIGLA_PARTIDO, 
                  UF) %>% 
  dplyr::summarise("Cadeiras conquistadas por UF" = n())

### Deputado Estadual

de <- de %>% 
  dplyr::group_by(ANO_ELEICAO, 
                  DESCRICAO_CARGO, 
                  SIGLA_PARTIDO, 
                  UF) %>% 
  dplyr::summarise("Cadeiras conquistadas" = n())

## Soma o total de cadeiras conquistadas pelos partidos em cada eleicao

### Deputado Federal

df1 <- df %>% 
  dplyr::group_by(ANO_ELEICAO,
                  DESCRICAO_CARGO,
                  SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    "Total de cadeiras conquistadas" = 
      sum(`Cadeiras conquistadas por UF`))

## Junta os bancos de cadeiras conquistadas por UF com o total de cadeiras conquistadas

### Deputado Federal

df <- left_join(df, df1, 
                by = c("ANO_ELEICAO", 
                       "DESCRICAO_CARGO",
                       "SIGLA_PARTIDO"))

## Percentual de cadeiras conquistas pelos partidos

### Deputado Federal

df$`Percentual de cadeiras conquistadas` <- df$`Total de cadeiras conquistadas`/513

## Elimina colunas desnecessarias, renomeia as colunas restantes e padroniza-as

### Deputado Federal

df <- df %>% 
  dplyr::select(ANO_ELEICAO, 
                UF,
                DESCRICAO_CARGO, 
                SIGLA_PARTIDO, 
                `Cadeiras conquistadas por UF`,
                `Total de cadeiras conquistadas`, 
                `Percentual de cadeiras conquistadas`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "DESCRICAO_CARGO", 
                "Sigla do partido" = "SIGLA_PARTIDO")

df$Cargo <- str_to_title(df$Cargo)

### Deputado Estadual

de <- de %>% 
  dplyr::select(ANO_ELEICAO, 
                UF,
                DESCRICAO_CARGO, 
                SIGLA_PARTIDO, 
                `Cadeiras conquistadas`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "DESCRICAO_CARGO", 
                "Sigla do partido" = "SIGLA_PARTIDO") 

de$Cargo <- str_to_title(de$Cargo)


# 2. Fracionalizacao ------------------------------------------------------

## Funcao para o calculo da fracionalizacao

fracio <- function(x){
  
  1-(sum(x^2))
}

## Calculo do indice de fracionalizacao em cada eleicao

d <- list()

for(i in seq(1998,2018, by = 4)){
  d[i] <- filter(df, `Ano da eleição` == i)
  d[i]$`Fracionalização` <- fracio(d[i]$`Percentual de cadeiras consquistadas`)
  df <- bind_rows(df, d[i])
}



# 3. Fracionalizacao maxima -----------------------------------------------

## Funcao para o calculo da fracionalizacao maxima

fracio_max <- function(N, n){
  
  (N*(n-1))/(n*(N-1))
  
}

## Calculo do indice de fracionalizacao maxima em cada eleicao

for(i in seq(1998,2018, by = 4)){
  df$`Fracionalização máxima` <- fracio_max(513)
 }



# 4. Fragmentacao ---------------------------------------------------------

## Funcao para o calculo da fragmentacao

frag <- function(fracio, fracio_max){
  
  fracio/fracio_max
}

## Calculo do indice de fragmentacao em cada eleicao



# 5. Desproporcionalidade de Gallagher ------------------------------------




# 6. Numero efetivo de partidos ---------------------------------


# 6.1. Por votos ----------------------------------------------------------


# 6.2. Por cadeiras -------------------------------------------------------

## Funcao para o calculo do numero efetivo de partidos (cadeiras)

options(scipen=999)

NEP<-NA

NEPC <- function(p){
  for(i in 1:length(p)){
    NEP[[i]]<-(p[[i]]*p[[i]])
  }
  1/sum(NEP)}

## Calculo do numero efetivo de partidos (cadeiras)



# 7. Padronizacao dos dados -----------------------------------------------




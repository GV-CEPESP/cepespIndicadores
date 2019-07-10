
# Pacotes utilizados

library(cepespR)
library(dplyr)
library(tidyverse)
library(abjutils)

# Objetivo
#'        - Calcular os indicadores de alienacao:
#'        - Alienacao absoluta e percentual;
#'        - Limpeza e padronizacao dos dados.


# 1. Alienacao ------------------------------------------------------------


# 1.1. Alienacao absoluta -------------------------------------------------

## Calculo do indice de alienacao absoluta


### Deputado Federal BR

dfcb$`Alienação Absoluta` <- dfcb$QTD_ABSTENCOES + 
  dfcb$QT_VOTOS_BRANCOS + dfcb$QT_VOTOS_NULOS 

### Deputado Federal UF

dfc$`Alienação Absoluta` <- dfc$QTD_ABSTENCOES +
  dfc$QT_VOTOS_BRANCOS + dfc$QT_VOTOS_NULOS 

### Deputado Estadual BR

decb$`Alienação Absoluta` <- decb$QTD_ABSTENCOES + 
  decb$QT_VOTOS_BRANCOS + decb$QT_VOTOS_NULOS

### Deputado Estadual UF

dec$`Alienação Absoluta` <- dec$QTD_ABSTENCOES + 
  dec$QT_VOTOS_BRANCOS + dec$QT_VOTOS_NULOS


# 1.2. Alienacao percentual ----------------------------------------------

## Calculo do indice de alienacao percentual


### Deputado Federal BR

dfcb$`Alienação Percentual` <- round(100*(dfcb$QTD_ABSTENCOES + 
                                          dfcb$QT_VOTOS_BRANCOS + 
                                          dfcb$QT_VOTOS_NULOS)/
                                          dfcb$QTD_APTOS,2)

### Deputado Federal UF

dfc$`Alienação Percentual` <- round(100*(dfc$QTD_ABSTENCOES + 
                                         dfc$QT_VOTOS_BRANCOS + 
                                         dfc$QT_VOTOS_NULOS)/
                                         dfc$QTD_APTOS,2)

### Deputado Estadual BR

decb$`Alienação Percentual` <- round(100*(decb$QTD_ABSTENCOES + 
                                          decb$QT_VOTOS_BRANCOS + 
                                          decb$QT_VOTOS_NULOS)/
                                          decb$QTD_APTOS,2)

### Deputado Estadual UF

dec$`Alienação Percentual` <- round(100*(dec$QTD_ABSTENCOES + 
                                         dec$QT_VOTOS_BRANCOS + 
                                         dec$QT_VOTOS_NULOS)/
                                         dec$QTD_APTOS,2)

     

# 2. Outros cargos --------------------------------------------------------


# 3. Limpeza e padronizacao dos dados -------------------------------------

## Descarta as colunas desnecessarias,renomeia e padroniza as restantes

### Deputado Federal BR

dfcb <- dfcb %>% 
  dplyr::select(ANO_ELEICAO,DESCRICAO_CARGO, QTD_ABSTENCOES, 
                QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QTD_APTOS,
                `Alienação Absoluta`,`Alienação Percentual`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "DESCRICAO_CARGO",
                "Quantidade de abstenções" = "QTD_ABSTENCOES",
                "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", 
                "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
                "Quantidade de eleitores aptos"="QTD_APTOS") %>% 
  dplyr::arrange(`Ano da eleição`)

dfcb$Cargo <- str_to_title(dfcb$Cargo) ## Transforma a primeira letra de cada palavra
                                       ## em maiuscula  


### Deputado Federal UF

dfc <- dfc %>% 
  dplyr::select(ANO_ELEICAO,UF, DESCRICAO_CARGO, QTD_ABSTENCOES, 
                QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QTD_APTOS,
                `Alienação Absoluta`, `Alienação Percentual`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "DESCRICAO_CARGO", 
                "Quantidade de abstenções" = "QTD_ABSTENCOES",
                "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", 
                "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
                "Quantidade de eleitores aptos"="QTD_APTOS")  %>% 
  dplyr::arrange(`Ano da eleição`)

dfc$Cargo <- str_to_title(dfc$Cargo) ## Transforma a primeira letra de cada palavra
                                     ## em maiuscula  

### Deputado Estadual BR

decb <- decb %>% 
  dplyr::select(ANO_ELEICAO, DESCRICAO_CARGO, QTD_ABSTENCOES,
         QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QTD_APTOS,
         `Alienação Absoluta`,`Alienação Percentual`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
         "Cargo" = "DESCRICAO_CARGO", 
         "Quantidade de abstenções" = "QTD_ABSTENCOES",
         "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", 
         "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
         "Quantidade de eleitores aptos"="QTD_APTOS")  %>% 
  dplyr::arrange(`Ano da eleição`)

decb$Cargo <- str_to_title(decb$Cargo) ## Transforma a primeira letra de cada palavra
                                       ## em maiuscula  

### Deputado Estadual UF

dec <- dec %>% 
  dplyr::select(ANO_ELEICAO,UF, DESCRICAO_CARGO, QTD_ABSTENCOES,
                QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QTD_APTOS, 
                `Alienação Absoluta`, `Alienação Percentual`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "DESCRICAO_CARGO", 
                "Quantidade de abstenções" = "QTD_ABSTENCOES",
                "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", 
                "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
                "Quantidade de eleitores aptos"="QTD_APTOS")  %>% 
  dplyr::arrange(`Ano da eleição`)

dec$Cargo <- str_to_title(dec$Cargo) ## Transforma a primeira letra de cada palavra
                                     ## em maiuscula  

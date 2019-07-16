
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


### Cargos BR

cons_br$`Alienação Absoluta` <- cons_br$QTD_ABSTENCOES + 
  cons_br$QT_VOTOS_BRANCOS + cons_br$QT_VOTOS_NULOS 


### Cargos UF

cons_uf$`Alienação Absoluta` <- cons_uf$QTD_ABSTENCOES + 
  cons_uf$QT_VOTOS_BRANCOS + cons_uf$QT_VOTOS_NULOS


# 1.2. Alienacao percentual ----------------------------------------------

## Calculo do indice de alienacao percentual


### Cargos BR

cons_br$`Alienação Percentual` <- round(100*(cons_br$QTD_ABSTENCOES + 
                                             cons_br$QT_VOTOS_BRANCOS + 
                                             cons_br$QT_VOTOS_NULOS)/
                                             cons_br$QTD_APTOS,2)

### Cargos UF

cons_uf$`Alienação Percentual` <- round(100*(cons_uf$QTD_ABSTENCOES + 
                                            cons_uf$QT_VOTOS_BRANCOS + 
                                            cons_uf$QT_VOTOS_NULOS)/
                                            cons_uf$QTD_APTOS,2)

     

# 3. Limpeza e padronizacao dos dados -------------------------------------

## Descarta as colunas desnecessarias,renomeia e padroniza as restantes

### Cargos BR


cons_br <- cons_br %>% 
  dplyr::select(ANO_ELEICAO,NUM_TURNO,DESCRICAO_CARGO, QTD_ABSTENCOES, 
                QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QTD_APTOS,
                `Alienação Absoluta`,`Alienação Percentual`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO",
                "Turno" = "NUM_TURNO",
                "Cargo" = "DESCRICAO_CARGO",
                "Quantidade de abstenções" = "QTD_ABSTENCOES",
                "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", 
                "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
                "Quantidade de eleitores aptos"="QTD_APTOS") %>% 
  dplyr::arrange(`Ano da eleição`)


## Indica que se o cargo for "Deputado Distrital" ele passa a ser "Deputado Estadual"

cons_br$Cargo <-ifelse(cons_br$Cargo =="DEPUTADO DISTRITAL", 
                                     "Deputado Estadual", cons_br$Cargo)

## Transforma a primeira letra de cada palavra
## em maiuscula  

cons_br$Cargo <- str_to_title(cons_br$Cargo)


### Cargos UF

cons_uf <- cons_uf %>% 
  dplyr::select(ANO_ELEICAO,UF, NUM_TURNO, DESCRICAO_CARGO, QTD_ABSTENCOES,
                QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QTD_APTOS, 
                `Alienação Absoluta`, `Alienação Percentual`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO",
                "Turno" = "NUM_TURNO",
                "Cargo" = "DESCRICAO_CARGO", 
                "Quantidade de abstenções" = "QTD_ABSTENCOES",
                "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", 
                "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
                "Quantidade de eleitores aptos"="QTD_APTOS")  %>% 
  dplyr::arrange(`Ano da eleição`)

## Indica que se o cargo for "Deputado Distrital" ele passa a ser "Deputado Estadual"

cons_uf$Cargo <-ifelse(cons_uf$Cargo =="DEPUTADO DISTRITAL", 
                                     "Deputado Estadual", cons_uf$Cargo)

## Transforma a primeira letra de cada palavra
## em maiuscula  

cons_uf$Cargo <- str_to_title(cons_uf$Cargo)



# 4. Salva os arquivos ----------------------------------------------------

## Salva os arquivos referentes aos indicadores de alienacao em .csv

### Cargos BR

write.csv(cons_br, "data/output/alien_br.csv")

### Cargos UF

write.csv(cons_uf, "data/output/alien_uf.csv")

## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(cons_br, cons_uf)



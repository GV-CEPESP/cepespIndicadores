
# Pacotes utilizados

library(cepespR)
library(dplyr)
library(tidyverse)
library(abjutils)

# Objetivo
#'        - Calcular os indicadores de alienacao:
#'        - Alienacao absoluta e percentual;
#'        - Limpeza e padronizacao dos dados.


# 1. Padronizacao primaria ------------------------------------------------------------


## Descarta as colunas desnecessarias,renomeia e padroniza as restantes

### Cargos BR

cons_br <- cons_br %>% 
  dplyr::select(ANO_ELEICAO,NUM_TURNO,DESCRICAO_CARGO, QTD_ABSTENCOES, 
                QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QTD_APTOS) %>% 
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
                QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QTD_APTOS) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO",
                "Turno" = "NUM_TURNO",
                "Cargo" = "DESCRICAO_CARGO", 
                "Quantidade de abstenções" = "QTD_ABSTENCOES",
                "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", 
                "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
                "Quantidade de eleitores aptos"="QTD_APTOS")  %>% 
  dplyr::arrange(`Ano da eleição`) 


cons_uf <- cons_uf %>% 
  group_by(`Ano da eleição`,UF,Turno,Cargo) %>% 
  summarise(
    "Quantidade de abstenções" = sum(`Quantidade de abstenções`),
    "Quantidade de votos brancos" = sum(`Quantidade de votos brancos`), 
    "Quantidade de votos nulos" = sum(`Quantidade de votos nulos`), 
    "Quantidade de eleitores aptos"= sum(`Quantidade de eleitores aptos`))

## Indica que se o cargo for "Deputado Distrital" ele passa a ser "Deputado Estadual"

cons_uf$Cargo <-ifelse(cons_uf$Cargo =="DEPUTADO DISTRITAL", 
                       "Deputado Estadual", cons_uf$Cargo)

## Transforma a primeira letra de cada palavra
## em maiuscula  

cons_uf$Cargo <- str_to_title(cons_uf$Cargo)

# 2. Alienacao -------------------------------------------------


# 2.1. Alienacao absoluta -------------------------------------------------


## Calculo do indice de alienacao absoluta


### Cargos BR

cons_br$`Alienação absoluta` <- cons_br$`Quantidade de abstenções` + 
  cons_br$`Quantidade de votos brancos` + cons_br$`Quantidade de votos nulos`


### Cargos UF

cons_uf$`Alienação absoluta` <- cons_uf$`Quantidade de abstenções` + 
  cons_uf$`Quantidade de votos brancos` + cons_uf$`Quantidade de votos nulos`


# 2.2. Alienacao percentual ----------------------------------------------

## Calculo do indice de alienacao percentual


### Cargos BR

cons_br$`Alienação percentual` <- round(100*(cons_br$`Quantidade de abstenções` + 
                                             cons_br$`Quantidade de votos brancos` + 
                                             cons_br$`Quantidade de votos nulos`)/
                                             cons_br$`Quantidade de eleitores aptos`,2)

cons_br$`Percentual de abstenções` <- round(100*(cons_br$`Quantidade de abstenções`)/
                                              cons_br$`Quantidade de eleitores aptos`,2)

cons_br$`Percentual de votos brancos` <- round(100*(cons_br$`Quantidade de votos brancos`)/
                                              cons_br$`Quantidade de eleitores aptos`,2)

cons_br$`Percentual de votos nulos` <- round(100*(cons_br$`Quantidade de votos nulos`)/
                                              cons_br$`Quantidade de eleitores aptos`,2)
### Cargos UF

cons_uf$`Alienação percentual` <- round(100*(cons_uf$`Quantidade de abstenções` + 
                                            cons_uf$`Quantidade de votos brancos` + 
                                            cons_uf$`Quantidade de votos nulos`)/
                                            cons_uf$`Quantidade de eleitores aptos`,2)

cons_uf$`Percentual de abstenções` <- round(100*(cons_uf$`Quantidade de abstenções`)/
                                              cons_uf$`Quantidade de eleitores aptos`,2)

cons_uf$`Percentual de votos brancos` <- round(100*(cons_uf$`Quantidade de votos brancos`)/
                                                 cons_uf$`Quantidade de eleitores aptos`,2)

cons_uf$`Percentual de votos nulos` <- round(100*(cons_uf$`Quantidade de votos nulos`)/
                                               cons_uf$`Quantidade de eleitores aptos`,2)

     

# 3. Limpeza e padronizacao dos dados -------------------------------------

options(OutDec= ",", digits=4)

## Padroniza o formato numerico das colunas


### Brasil

cons_br$`Quantidade de abstenções` <- formatC(cons_br$`Quantidade de abstenções`, format="f", big.mark = ".", digits=0)

cons_br$`Quantidade de votos brancos` <- formatC(cons_br$`Quantidade de votos brancos`, format="f", big.mark = ".", digits=0)

cons_br$`Quantidade de votos nulos` <- formatC(cons_br$`Quantidade de votos nulos`, format="f", big.mark = ".", digits=0)

cons_br$`Quantidade de eleitores aptos` <- formatC(cons_br$`Quantidade de eleitores aptos`, format="f", big.mark = ".", digits=0)

cons_br$`Alienação absoluta` <- formatC(cons_br$`Alienação absoluta`, format="f", big.mark = ".", digits=0)

cons_br$`Alienação percentual` <- as.character(cons_br$`Alienação percentual`)

cons_br$`Percentual de abstenções` <- as.character(cons_br$`Percentual de abstenções`)

cons_br$`Percentual de votos brancos` <- as.character(cons_br$`Percentual de votos brancos`)

cons_br$`Percentual de votos nulos` <- as.character(cons_br$`Percentual de votos nulos`)


### UF

cons_uf$`Quantidade de abstenções` <- formatC(cons_uf$`Quantidade de abstenções`, format="f", big.mark = ".", digits=0)

cons_uf$`Quantidade de votos brancos` <- formatC(cons_uf$`Quantidade de votos brancos`, format="f", big.mark = ".", digits=0)

cons_uf$`Quantidade de votos nulos` <- formatC(cons_uf$`Quantidade de votos nulos`, format="f", big.mark = ".", digits=0)

cons_uf$`Quantidade de eleitores aptos` <- formatC(cons_uf$`Quantidade de eleitores aptos`, format="f", big.mark = ".", digits=0)

cons_uf$`Alienação absoluta` <- formatC(cons_uf$`Alienação absoluta`, format="f", big.mark = ".", digits=0)

cons_uf$`Alienação percentual` <- as.character(cons_uf$`Alienação percentual`)

cons_uf$`Percentual de abstenções` <- as.character(cons_uf$`Percentual de abstenções`)

cons_uf$`Percentual de votos brancos` <- as.character(cons_uf$`Percentual de votos brancos`)

cons_uf$`Percentual de votos nulos` <- as.character(cons_uf$`Percentual de votos nulos`)

## Organiza as colunas

## Brasil

cons_br <- cons_br %>% 
  select(`Ano da eleição`,
         Cargo,
         Turno,
         `Quantidade de eleitores aptos`,
         `Quantidade de abstenções`,
         `Percentual de abstenções`,
         `Quantidade de votos brancos`,
         `Percentual de votos brancos`,
         `Quantidade de votos nulos`,
         `Percentual de votos nulos`,
         `Alienação absoluta`,
         `Alienação percentual`)

## UF

cons_uf <- cons_uf %>% 
  select(`Ano da eleição`,
         UF,
         Cargo,
         Turno,
         `Quantidade de eleitores aptos`,
         `Quantidade de abstenções`,
         `Percentual de abstenções`,
         `Quantidade de votos brancos`,
         `Percentual de votos brancos`,
         `Quantidade de votos nulos`,
         `Percentual de votos nulos`,
         `Alienação absoluta`,
         `Alienação percentual`)

# 4. Salva os arquivos ----------------------------------------------------

## Salva os arquivos referentes aos indicadores de alienacao em .csv

### Cargos BR

write.csv(cons_br, "data/output/alien_br.csv")

### Cargos UF

write.csv(cons_uf, "data/output/alien_uf.csv")

## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(cons_br,cons_uf)



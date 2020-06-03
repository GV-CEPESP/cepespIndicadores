
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


### Cargos MUN

cons_mun <- cons_mun %>% 
  dplyr::select(ANO_ELEICAO,UF,COD_MUN_TSE,NOME_MUNICIPIO,NUM_TURNO, DESCRICAO_CARGO, QTD_ABSTENCOES,
                QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QTD_APTOS) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO",
                "Código do município" = "COD_MUN_TSE",
                "Nome do município" = "NOME_MUNICIPIO",
                "Turno" = "NUM_TURNO",
                "Cargo" = "DESCRICAO_CARGO", 
                "Quantidade de abstenções" = "QTD_ABSTENCOES",
                "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS", 
                "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
                "Quantidade de eleitores aptos"="QTD_APTOS")  %>% 
  dplyr::arrange(`Ano da eleição`) 


cons_mun <- cons_mun %>% 
  group_by(`Ano da eleição`,
           UF,
           `Código do município`, 
           `Nome do município`,
           Turno,
           Cargo) %>% 
  summarise(
    "Quantidade de abstenções" = sum(`Quantidade de abstenções`),
    "Quantidade de votos brancos" = sum(`Quantidade de votos brancos`), 
    "Quantidade de votos nulos" = sum(`Quantidade de votos nulos`), 
    "Quantidade de eleitores aptos"= sum(`Quantidade de eleitores aptos`))


## Transforma a primeira letra de cada palavra
## em maiuscula  

cons_mun$Cargo <- str_to_title(cons_mun$Cargo)

# 2. Alienacao -------------------------------------------------


# 2.1. Alienacao absoluta -------------------------------------------------


## Calculo do indice de alienacao absoluta


### Cargos BR

cons_br$`Alienação absoluta` <- cons_br$`Quantidade de abstenções` + 
  cons_br$`Quantidade de votos brancos` + cons_br$`Quantidade de votos nulos`


### Cargos UF

cons_uf$`Alienação absoluta` <- cons_uf$`Quantidade de abstenções` + 
  cons_uf$`Quantidade de votos brancos` + cons_uf$`Quantidade de votos nulos`


### Cargos MUN

cons_mun$`Alienação absoluta` <- cons_mun$`Quantidade de abstenções` + 
  cons_mun$`Quantidade de votos brancos` + cons_mun$`Quantidade de votos nulos`

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

### Cargos MUN


cons_mun$`Alienação percentual` <- round(100*(cons_mun$`Quantidade de abstenções` + 
                                               cons_mun$`Quantidade de votos brancos` + 
                                               cons_mun$`Quantidade de votos nulos`)/
                                          cons_mun$`Quantidade de eleitores aptos`,2)

cons_mun$`Percentual de abstenções` <- round(100*(cons_mun$`Quantidade de abstenções`)/
                                              cons_mun$`Quantidade de eleitores aptos`,2)

cons_mun$`Percentual de votos brancos` <- round(100*(cons_mun$`Quantidade de votos brancos`)/
                                                 cons_mun$`Quantidade de eleitores aptos`,2)

cons_mun$`Percentual de votos nulos` <- round(100*(cons_mun$`Quantidade de votos nulos`)/
                                               cons_mun$`Quantidade de eleitores aptos`,2)
     

# 3. Limpeza e padronizacao dos dados -------------------------------------

options(OutDec= ",")

## Padroniza o formato numerico das colunas


### Brasil

cons_br$`Quantidade de abstenções` <- formatC(cons_br$`Quantidade de abstenções`, format="f", big.mark = ".", digits=0)

cons_br$`Quantidade de votos brancos` <- formatC(cons_br$`Quantidade de votos brancos`, format="f", big.mark = ".", digits=0)

cons_br$`Quantidade de votos nulos` <- formatC(cons_br$`Quantidade de votos nulos`, format="f", big.mark = ".", digits=0)

cons_br$`Quantidade de eleitores aptos` <- formatC(cons_br$`Quantidade de eleitores aptos`, format="f", big.mark = ".", digits=0)

cons_br$`Alienação absoluta` <- formatC(cons_br$`Alienação absoluta`, format="f", big.mark = ".", digits=0)

cons_br$`Percentual de abstenções` <- gsub("\\,", ".", cons_br$`Percentual de abstenções`)

cons_br$`Percentual de abstenções` <- as.numeric(cons_br$`Percentual de abstenções`)

cons_br$`Percentual de abstenções` <- format(round(cons_br$`Percentual de abstenções`,
                                                    digits = 2),
                                              nsmall = 2)
cons_br$`Percentual de votos brancos` <- gsub("\\,", ".", cons_br$`Percentual de votos brancos`)

cons_br$`Percentual de votos brancos` <- as.numeric(cons_br$`Percentual de votos brancos`)

cons_br$`Percentual de votos brancos` <- format(round(cons_br$`Percentual de votos brancos`,
                                                       digits = 2),
                                                 nsmall = 2)

cons_br$`Percentual de votos nulos` <- gsub("\\,", ".", cons_br$`Percentual de votos nulos`)

cons_br$`Percentual de votos nulos` <- as.numeric(cons_br$`Percentual de votos nulos`)

cons_br$`Percentual de votos nulos` <- format(round(cons_br$`Percentual de votos nulos`,
                                                     digits = 2),
                                               nsmall = 2)
cons_br$`Alienação percentual` <- gsub("\\,", ".", cons_br$`Alienação percentual`)

cons_br$`Alienação percentual` <- as.numeric(cons_br$`Alienação percentual`)

cons_br$`Alienação percentual` <- format(round(cons_br$`Alienação percentual`,
                                                digits = 2),
                                          nsmall = 2)


### UF

cons_uf$`Quantidade de abstenções` <- formatC(cons_uf$`Quantidade de abstenções`, format="f", big.mark = ".", digits=0)

cons_uf$`Quantidade de votos brancos` <- formatC(cons_uf$`Quantidade de votos brancos`, format="f", big.mark = ".", digits=0)

cons_uf$`Quantidade de votos nulos` <- formatC(cons_uf$`Quantidade de votos nulos`, format="f", big.mark = ".", digits=0)

cons_uf$`Quantidade de eleitores aptos` <- formatC(cons_uf$`Quantidade de eleitores aptos`, format="f", big.mark = ".", digits=0)

cons_uf$`Alienação absoluta` <- formatC(cons_uf$`Alienação absoluta`, format="f", big.mark = ".", digits=0)

cons_uf$`Percentual de abstenções` <- gsub("\\,", ".", cons_uf$`Percentual de abstenções`)

cons_uf$`Percentual de abstenções` <- as.numeric(cons_uf$`Percentual de abstenções`)

cons_uf$`Percentual de abstenções` <- format(round(cons_uf$`Percentual de abstenções`,
                                                    digits = 2),
                                              nsmall = 2)
cons_uf$`Percentual de votos brancos` <- gsub("\\,", ".", cons_uf$`Percentual de votos brancos`)

cons_uf$`Percentual de votos brancos` <- as.numeric(cons_uf$`Percentual de votos brancos`)

cons_uf$`Percentual de votos brancos` <- format(round(cons_uf$`Percentual de votos brancos`,
                                                       digits = 2),
                                                 nsmall = 2)

cons_uf$`Percentual de votos nulos` <- gsub("\\,", ".", cons_uf$`Percentual de votos nulos`)

cons_uf$`Percentual de votos nulos` <- as.numeric(cons_uf$`Percentual de votos nulos`)

cons_uf$`Percentual de votos nulos` <- format(round(cons_uf$`Percentual de votos nulos`,
                                                     digits = 2),
                                               nsmall = 2)
cons_uf$`Alienação percentual` <- gsub("\\,", ".", cons_uf$`Alienação percentual`)

cons_uf$`Alienação percentual` <- as.numeric(cons_uf$`Alienação percentual`)

cons_uf$`Alienação percentual` <- format(round(cons_uf$`Alienação percentual`,
                                                digits = 2),
                                          nsmall = 2)

### Municipio

cons_mun$`Quantidade de abstenções` <- formatC(cons_mun$`Quantidade de abstenções`, format="f", big.mark = ".", digits=0)

cons_mun$`Quantidade de votos brancos` <- formatC(cons_mun$`Quantidade de votos brancos`, format="f", big.mark = ".", digits=0)

cons_mun$`Quantidade de votos nulos` <- formatC(cons_mun$`Quantidade de votos nulos`, format="f", big.mark = ".", digits=0)

cons_mun$`Quantidade de eleitores aptos` <- formatC(cons_mun$`Quantidade de eleitores aptos`, format="f", big.mark = ".", digits=0)

cons_mun$`Percentual de abstenções` <- as.numeric(cons_mun$`Percentual de abstenções`)

cons_mun$`Percentual de abstenções` <- format(round(cons_mun$`Percentual de abstenções`,
                                                     digits = 2),
                                               nsmall = 2)
cons_mun$`Percentual de votos brancos` <- as.numeric(cons_mun$`Percentual de votos brancos`)

cons_mun$`Percentual de votos brancos` <- format(round(cons_mun$`Percentual de votos brancos`,
                                                     digits = 2),
                                               nsmall = 2)

cons_mun$`Percentual de votos nulos` <- as.numeric(cons_mun$`Percentual de votos nulos`)

cons_mun$`Percentual de votos nulos` <- format(round(cons_mun$`Percentual de votos nulos`,
                                                        digits = 2),
                                                  nsmall = 2)

cons_mun$`Alienação percentual` <- as.numeric(cons_mun$`Alienação percentual`)

cons_mun$`Alienação percentual` <- format(round(cons_mun$`Alienação percentual`,
                                                        digits = 2),
                                                  nsmall = 2)


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

### Municipio

cons_mun <- cons_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
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

# 4. Municipios -----------------------------------------------------------

### Cria um dataframe com os municipios do Brasil


municipios <- cons_mun %>% 
  select(UF, `Código do município`,
         `Nome do município`) %>% 
  arrange(UF, `Nome do município`)

municipios <- unique(municipios)

municipios$`Município - UF` <- unique(paste(municipios$`Nome do município`, "-",
                                            municipios$UF))

municipios <- municipios %>% 
  select(`Município - UF`)

# 5. Salva os arquivos ----------------------------------------------------

## Salva os arquivos referentes aos indicadores de alienacao em .csv

### Cargos BR

write.csv(cons_br, "data/output/alien_br.csv")

### Cargos UF

write.csv(cons_uf, "data/output/alien_uf.csv")

### Cargos MUN

saveRDS(cons_mun, "data/output/alien_mun.rds")

### Municipios

write.csv(municipios, "data/output/municipios.csv")

## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(cons_br,cons_uf,cons_mun, municipios)



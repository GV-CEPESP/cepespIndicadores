
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


# 1.1. Brasil -------------------------------------------------------------

## Descarta as colunas desnecessarias,renomeia e padroniza as restantes

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


## Altera os cargos nomeados como "Deputado Distrital" 
## para "Deputado Estadual"

cons_br$Cargo <-ifelse(cons_br$Cargo =="DEPUTADO DISTRITAL", 
                       "Deputado Estadual", cons_br$Cargo)

## Transforma a primeira letra de cada palavra
## em maiuscula  

cons_br$Cargo <- str_to_title(cons_br$Cargo)

# 1.2. Estado -------------------------------------------------------------

## Descarta as colunas desnecessarias,renomeia e padroniza as restantes

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

## Agrega cada um dos valores em funcao do ano, uf, turno e cargo

cons_uf <- cons_uf %>% 
  group_by(`Ano da eleição`,UF,Turno,Cargo) %>% 
  summarise(
    "Quantidade de abstenções" = sum(`Quantidade de abstenções`),
    "Quantidade de votos brancos" = sum(`Quantidade de votos brancos`), 
    "Quantidade de votos nulos" = sum(`Quantidade de votos nulos`), 
    "Quantidade de eleitores aptos"= sum(`Quantidade de eleitores aptos`))

## Altera os cargos nomeados como "Deputado Distrital" 
## para "Deputado Estadual"

cons_uf$Cargo <-ifelse(cons_uf$Cargo =="DEPUTADO DISTRITAL", 
                       "Deputado Estadual", cons_uf$Cargo)

## Transforma a primeira letra de cada palavra
## em maiuscula  

cons_uf$Cargo <- str_to_title(cons_uf$Cargo)

# 1.3. Municipio ----------------------------------------------------------

## Descarta as colunas desnecessarias,renomeia e padroniza as restantes

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

## Agrega cada um dos valores em funcao do ano, uf, codigo do municipio,
## turno e cargo

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


# 2. Calculo dos indicadores ----------------------------------------------

# 2.1. Alienacao absoluta -------------------------------------------------

# 2.1.1. Brasil -----------------------------------------------------------

## Calculo do indice de alienacao absoluta

cons_br$`Alienação absoluta` <- cons_br$`Quantidade de abstenções` + 
  cons_br$`Quantidade de votos brancos` + cons_br$`Quantidade de votos nulos`


# 2.1.2. Estado -----------------------------------------------------------

## Calculo do indice de alienacao absoluta

cons_uf$`Alienação absoluta` <- cons_uf$`Quantidade de abstenções` + 
  cons_uf$`Quantidade de votos brancos` + cons_uf$`Quantidade de votos nulos`


# 2.1.3. Municipio --------------------------------------------------------

## Calculo do indice de alienacao absoluta

cons_mun$`Alienação absoluta` <- cons_mun$`Quantidade de abstenções` + 
  cons_mun$`Quantidade de votos brancos` + cons_mun$`Quantidade de votos nulos`

# 2.2. Alienacao percentual ----------------------------------------------


# 2.2.1. Brasil -----------------------------------------------------------

## Calculo do indice de alienacao percentual

cons_br$`Alienação percentual` <- round(100*(cons_br$`Quantidade de abstenções` + 
                                               cons_br$`Quantidade de votos brancos` + 
                                               cons_br$`Quantidade de votos nulos`)/
                                          cons_br$`Quantidade de eleitores aptos`,2)

## Calculo do percentual de abstencoes

cons_br$`Percentual de abstenções` <- round(100*(cons_br$`Quantidade de abstenções`)/
                                              cons_br$`Quantidade de eleitores aptos`,2)

## Calculo do percentual de votos brancos

cons_br$`Percentual de votos brancos` <- round(100*(cons_br$`Quantidade de votos brancos`)/
                                                 cons_br$`Quantidade de eleitores aptos`,2)

## Calculo do percentual de votos nulos

cons_br$`Percentual de votos nulos` <- round(100*(cons_br$`Quantidade de votos nulos`)/
                                               cons_br$`Quantidade de eleitores aptos`,2)


# 2.2.2. Estado -----------------------------------------------------------

## Calculo do indice de alienacao percentual

cons_uf$`Alienação percentual` <- round(100*(cons_uf$`Quantidade de abstenções` + 
                                               cons_uf$`Quantidade de votos brancos` + 
                                               cons_uf$`Quantidade de votos nulos`)/
                                          cons_uf$`Quantidade de eleitores aptos`,2)

## Calculo do percentual de abstencoes

cons_uf$`Percentual de abstenções` <- round(100*(cons_uf$`Quantidade de abstenções`)/
                                              cons_uf$`Quantidade de eleitores aptos`,2)

## Calculo do percentual de votos brancos

cons_uf$`Percentual de votos brancos` <- round(100*(cons_uf$`Quantidade de votos brancos`)/
                                                 cons_uf$`Quantidade de eleitores aptos`,2)

## Calculo do percentual de votos nulos

cons_uf$`Percentual de votos nulos` <- round(100*(cons_uf$`Quantidade de votos nulos`)/
                                               cons_uf$`Quantidade de eleitores aptos`,2)


# 2.2.3. Municipio --------------------------------------------------------

## Calculo do indice de alienacao percentual

cons_mun$`Alienação percentual` <- round(100*(cons_mun$`Quantidade de abstenções` + 
                                                cons_mun$`Quantidade de votos brancos` + 
                                                cons_mun$`Quantidade de votos nulos`)/
                                           cons_mun$`Quantidade de eleitores aptos`,2)

## Calculo do percentual de abstencoes

cons_mun$`Percentual de abstenções` <- round(100*(cons_mun$`Quantidade de abstenções`)/
                                               cons_mun$`Quantidade de eleitores aptos`,2)

## Calculo do percentual de votos brancos

cons_mun$`Percentual de votos brancos` <- round(100*(cons_mun$`Quantidade de votos brancos`)/
                                                  cons_mun$`Quantidade de eleitores aptos`,2)

## Calculo do percentual de votos nulos

cons_mun$`Percentual de votos nulos` <- round(100*(cons_mun$`Quantidade de votos nulos`)/
                                                cons_mun$`Quantidade de eleitores aptos`,2)

# 3. Limpeza e padronizacao dos dados -------------------------------------


# 3.1. Brasil -------------------------------------------------------------

## Padroniza o formato numerico da variavel
## 'Quantidade de abstencoes'

cons_br$`Quantidade de abstenções` <- formatC(cons_br$`Quantidade de abstenções`, 
                                              format="f", 
                                              big.mark = ".", 
                                              digits=0)

## Padroniza o formato numerico da variavel
## 'Quantidade de votos brancos'

cons_br$`Quantidade de votos brancos` <- formatC(cons_br$`Quantidade de votos brancos`, 
                                                 format="f", 
                                                 big.mark = ".", 
                                                 digits=0)

## Padroniza o formato numerico da variavel
## 'Quantidade de votos nulos'

cons_br$`Quantidade de votos nulos` <- formatC(cons_br$`Quantidade de votos nulos`, 
                                               format="f", 
                                               big.mark = ".", 
                                               digits=0)

## Padroniza o formato numerico da variavel
## 'Quantidade de eleiores aptos'

cons_br$`Quantidade de eleitores aptos` <- formatC(cons_br$`Quantidade de eleitores aptos`, 
                                                   format="f", 
                                                   big.mark = ".", 
                                                   digits=0)

## Padroniza o formato numerico da variavel
## 'Alienacao absoluta'

cons_br$`Alienação absoluta` <- formatC(cons_br$`Alienação absoluta`, 
                                        format="f", 
                                        big.mark = ".", 
                                        digits=0)

## Altera a pontuacao usada na variavel 'Percentual de abstencoes'

cons_br$`Percentual de abstenções` <- gsub("\\,", ".", 
                                           cons_br$`Percentual de abstenções`)

## Transforma o formato da variavel em numerico

cons_br$`Percentual de abstenções` <- as.numeric(cons_br$`Percentual de abstenções`)

## Padroniza o formato numerico da variavel
## 'Percentual de abstencoes'

cons_br$`Percentual de abstenções` <- format(round(cons_br$`Percentual de abstenções`,
                                                   digits = 2),
                                                   nsmall = 2)
## Altera a pontuacao usada na variavel 
## 'Percentual de votos brancos'

cons_br$`Percentual de votos brancos` <- gsub("\\,", ".", 
                                              cons_br$`Percentual de votos brancos`)

## Transforma o formato da variavel em numerico

cons_br$`Percentual de votos brancos` <- as.numeric(cons_br$`Percentual de votos brancos`)

## Padroniza o formato numerico da variavel
## 'Percentual de votos brancos'

cons_br$`Percentual de votos brancos` <- format(round(cons_br$`Percentual de votos brancos`,
                                                      digits = 2),
                                                      nsmall = 2)
## Altera a pontuacao usada na variavel 
## 'Percentual de votos nulos'

cons_br$`Percentual de votos nulos` <- gsub("\\,", ".", 
                                            cons_br$`Percentual de votos nulos`)

## Transforma o formato da variavel em numerico

cons_br$`Percentual de votos nulos` <- as.numeric(cons_br$`Percentual de votos nulos`)

## Padroniza o formato numerico da variavel
## 'Percentual de votos nulos'

cons_br$`Percentual de votos nulos` <- format(round(cons_br$`Percentual de votos nulos`,
                                                    digits = 2),
                                                    nsmall = 2)
## Altera a pontuacao usada na variavel 
## 'Alienacao percentual'

cons_br$`Alienação percentual` <- gsub("\\,", ".", 
                                       cons_br$`Alienação percentual`)

## Transforma o formato da variavel em numerico

cons_br$`Alienação percentual` <- as.numeric(cons_br$`Alienação percentual`)

## Padroniza o formato numerico da variavel
## 'Alienacao percentual'

cons_br$`Alienação percentual` <- format(round(cons_br$`Alienação percentual`,
                                               digits = 2),
                                               nsmall = 2)

## Organiza a tabela

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

# 3.2. Estado -------------------------------------------------------------

## Padroniza o formato numerico da variavel
## 'Quantidade de abstencoes'

cons_uf$`Quantidade de abstenções` <- formatC(cons_uf$`Quantidade de abstenções`, 
                                              format="f", 
                                              big.mark = ".", 
                                              digits=0)

## Padroniza o formato numerico da variavel
## 'Quantidade de votos brancos'

cons_uf$`Quantidade de votos brancos` <- formatC(cons_uf$`Quantidade de votos brancos`, 
                                                 format="f", 
                                                 big.mark = ".", 
                                                 digits=0)

## Padroniza o formato numerico da variavel
## 'Quantidade de votos nulos'

cons_uf$`Quantidade de votos nulos` <- formatC(cons_uf$`Quantidade de votos nulos`, 
                                               format="f", 
                                               big.mark = ".", 
                                               digits=0)

## Padroniza o formato numerico da variavel
## 'Quantidade de eleiores aptos'

cons_uf$`Quantidade de eleitores aptos` <- formatC(cons_uf$`Quantidade de eleitores aptos`, 
                                                   format="f", 
                                                   big.mark = ".", 
                                                   digits=0)

## Padroniza o formato numerico da variavel
## 'Alienacao absoluta'

cons_uf$`Alienação absoluta` <- formatC(cons_uf$`Alienação absoluta`, 
                                        format="f", 
                                        big.mark = ".", 
                                        digits=0)

## Altera a pontuacao usada na variavel 'Percentual de abstencoes'

cons_uf$`Percentual de abstenções` <- gsub("\\,", ".", cons_uf$`Percentual de abstenções`)

## Transforma o formato da variavel em numerico

cons_uf$`Percentual de abstenções` <- as.numeric(cons_uf$`Percentual de abstenções`)

## Padroniza o formato numerico da variavel
## 'Percentual de abstencoes'

cons_uf$`Percentual de abstenções` <- format(round(cons_uf$`Percentual de abstenções`,
                                                   digits = 2),
                                                   nsmall = 2)

## Altera a pontuacao usada na variavel 'Percentual de votos brancos'

cons_uf$`Percentual de votos brancos` <- gsub("\\,", ".", 
                                              cons_uf$`Percentual de votos brancos`)

## Transforma o formato da variavel em numerico

cons_uf$`Percentual de votos brancos` <- as.numeric(cons_uf$`Percentual de votos brancos`)

## Padroniza o formato numerico da variavel
## 'Percentual de votos brancos'

cons_uf$`Percentual de votos brancos` <- format(round(cons_uf$`Percentual de votos brancos`,
                                                      digits = 2),
                                                      nsmall = 2)

## Altera a pontuacao usada na variavel 'Percentual de votos nulos'

cons_uf$`Percentual de votos nulos` <- gsub("\\,", ".", cons_uf$`Percentual de votos nulos`)

## Transforma o formato da variavel em numerico

cons_uf$`Percentual de votos nulos` <- as.numeric(cons_uf$`Percentual de votos nulos`)

## Padroniza o formato numerico da variavel
## 'Percentual de votos nulos'

cons_uf$`Percentual de votos nulos` <- format(round(cons_uf$`Percentual de votos nulos`,
                                                    digits = 2),
                                                    nsmall = 2)

## Altera a pontuacao usada na variavel 'Alienacao percentual'

cons_uf$`Alienação percentual` <- gsub("\\,", ".", cons_uf$`Alienação percentual`)

## Transforma o formato da variavel em numerico

cons_uf$`Alienação percentual` <- as.numeric(cons_uf$`Alienação percentual`)

## Padroniza o formato numerico da variavel
## 'Alienacao percentual'

cons_uf$`Alienação percentual` <- format(round(cons_uf$`Alienação percentual`,
                                               digits = 2),
                                               nsmall = 2)

## Organiza a tabela

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

# 3.3. Municipio ----------------------------------------------------------

## Padroniza o formato numerico da variavel
## 'Quantidade de abstencoes'

cons_mun$`Quantidade de abstenções` <- formatC(cons_mun$`Quantidade de abstenções`, 
                                               format="f", 
                                               big.mark = ".", 
                                               digits=0)

## Padroniza o formato numerico da variavel
## 'Quantidade de votos brancos'

cons_mun$`Quantidade de votos brancos` <- formatC(cons_mun$`Quantidade de votos brancos`, 
                                                  format="f", 
                                                  big.mark = ".", 
                                                  digits=0)

## Padroniza o formato numerico da variavel
## 'Quantidade de votos nulos'

cons_mun$`Quantidade de votos nulos` <- formatC(cons_mun$`Quantidade de votos nulos`, 
                                                format="f", 
                                                big.mark = ".", 
                                                digits=0)


## Padroniza o formato numerico da variavel
## 'Quantidade de eleiores aptos'

cons_mun$`Quantidade de eleitores aptos` <- formatC(cons_mun$`Quantidade de eleitores aptos`, 
                                                    format="f", 
                                                    big.mark = ".", 
                                                    digits=0)
## Padroniza o formato numerico da variavel
## 'Alienacao absoluta'

cons_mun$`Alienação absoluta` <- formatC(cons_mun$`Alienação absoluta`, 
                                         format="f", 
                                         big.mark = ".", 
                                         digits=0)

## Transforma o formato da variavel em numerico

cons_mun$`Percentual de abstenções` <- as.numeric(cons_mun$`Percentual de abstenções`)

## Padroniza o formato numerico da variavel
## 'Percentual de abstencoes'

cons_mun$`Percentual de abstenções` <- format(round(cons_mun$`Percentual de abstenções`,
                                                    digits = 2),
                                                    nsmall = 2)

## Transforma o formato da variavel em numerico

cons_mun$`Percentual de votos brancos` <- as.numeric(cons_mun$`Percentual de votos brancos`)

## Padroniza o formato numerico da variavel
## 'Percentual de votos brancos'

cons_mun$`Percentual de votos brancos` <- format(round(cons_mun$`Percentual de votos brancos`,
                                                       digits = 2),
                                                       nsmall = 2)

## Transforma o formato da variavel em numerico

cons_mun$`Percentual de votos nulos` <- as.numeric(cons_mun$`Percentual de votos nulos`)

## Padroniza o formato numerico da variavel
## 'Percentual de votos nulos'

cons_mun$`Percentual de votos nulos` <- format(round(cons_mun$`Percentual de votos nulos`,
                                                     digits = 2),
                                                     nsmall = 2)

## Transforma o formato da variavel em numerico

cons_mun$`Alienação percentual` <- as.numeric(cons_mun$`Alienação percentual`)

## Padroniza o formato numerico da variavel
## 'Alienacao percentual'

cons_mun$`Alienação percentual` <- format(round(cons_mun$`Alienação percentual`,
                                                digits = 2),
                                                nsmall = 2)

## Organiza a tabela

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

## Remove duplicacoes

municipios <- unique(municipios)

## Cria a variavel 'Municipio - UF'

municipios$`Município - UF` <- unique(paste(municipios$`Nome do município`, "-",
                                            municipios$UF))

## Remove as variaveis que nao serao utilizadas

municipios <- municipios %>% 
  select(`Município - UF`)

# 5. Salva os arquivos ----------------------------------------------------


# 5.1. Brasil -------------------------------------------------------------

## Salva os arquivos referentes aos indicadores de alienacao em .rds

saveRDS(cons_br, "data/output/alien_br.rds")


# 5.2. Estado -------------------------------------------------------------

## Salva os arquivos referentes aos indicadores de alienacao em .rds

saveRDS(cons_uf, "data/output/alien_uf.rds")


# 5.3. Municipio ----------------------------------------------------------

## Salva os arquivos referentes aos indicadores de alienacao em .rds

saveRDS(cons_mun, "data/output/alien_mun.rds")


# 5.4. Outros -------------------------------------------------------------

## Salva o arquivo referente aos municipios brasileiros em .rds

saveRDS(municipios, "data/output/municipios.rds")

## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(cons_br,cons_uf,cons_mun, municipios)



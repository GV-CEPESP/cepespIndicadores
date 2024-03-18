
## PACOTES UTILIZADOS

library(cepespR)
library(cepesputils)
library(plyr)
library(tidyverse)
library(abjutils)
library(fansi)

## AMBIENTE

options(timeout = 999999)

options(scipen = 99999)

## Diretório de referência

dir <- c("F:/Public/Documents/cepespIndicadores/")

# 1. Source ---------------------------------------------------------------

## Realiza o cálculo, padronização e exportação dos 
## indicadores eleitorais em formato .rds

source("scripts/funcoes.R", 
       encoding = "UTF-8")

source("scripts/01_data.R", 
       encoding = "UTF-8")

source("scripts/02_calcfragmentacao.R", 
       encoding = "UTF-8")

source("scripts/03_calcrenovacao.R", 
       encoding = "UTF-8")

source("scripts/04_calcalienacao.R", 
       encoding = "UTF-8")

source("scripts/05_calcvolatilidade.R", 
       encoding = "UTF-8")

source("scripts/06_agregmunicipios.R", 
       encoding = "UTF-8")

source("scripts/07_media_e_agregacao.R", 
       encoding = "UTF-8")

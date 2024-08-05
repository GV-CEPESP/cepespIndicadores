
## PACOTES UTILIZADOS

library(cepespR)
library(cepesputils)
library(plyr)
library(tidyverse)
library(abjutils)
library(fansi)
library(purrr)
library(arrow)

## AMBIENTE

options(timeout = 999999,
        scipen = 99999)

## Diretório de referência

dir <- c("F:/Public/Documents/cepespIndicadores/")

## dir <- "" ## Máquina local

# 1. Source ---------------------------------------------------------------

## Realiza o cálculo, padronização e exportação dos 
## indicadores eleitorais em formato .rds

lapply(list.files(path = "functions",
                  full.names = T), 
       source)

source("scripts/cepesp/01_data.R", 
       encoding = "UTF-8")

source("scripts/cepesp/02_calcfragmentacao.R", 
       encoding = "UTF-8")

source("scripts/cepesp/03_calcrenovacao.R", 
       encoding = "UTF-8")

source("scripts/cepesp/04_calcalienacao.R", 
       encoding = "UTF-8")

source("scripts/cepesp/05_calcvolatilidade.R", 
       encoding = "UTF-8")

source("scripts/cepesp/06_agregmunicipios.R", 
       encoding = "UTF-8")

source("scripts/cepesp/07_media_e_agregacao.R", 
       encoding = "UTF-8")

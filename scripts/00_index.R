
## PACOTES UTILIZADOS

library(cepespR)
library(cepesputils)
library(plyr)
library(tidyverse)
library(abjutils)
library(fansi)

## AMBIENTE

options(timeout = 999999)
options(scipen = 999)

## Calcula os indicadores eleitorais

source("scripts/funcoes.R", 
       encoding = "UTF-8")

source("scripts/01_join.R", 
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

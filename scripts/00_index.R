
rm(list = ls()) # Limpa a área de trabalho do R


## Pacotes

library(cepespR)
library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(abjutils)
library(data.table)
library(fansi)
library(stringi)

## Bancos de Indicadores

source("scripts/01_join.R", encoding = "UTF-8")
source("scripts/02_calcdistribuicao.R", encoding = "UTF-8")
source("scripts/03_calcfragmentacao.R", encoding = "UTF-8")
source("scripts/04_calcrenovacao.R", encoding = "UTF-8")
source("Scripts/05_calcalienacao.R", encoding = "UTF-8")
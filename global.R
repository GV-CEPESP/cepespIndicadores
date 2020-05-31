
# Pacotes utilizados

library(cepespR)
library(knitr)
library(plyr)
library(tidyverse)
library(shiny)
library(shinyBS)
library(readr)
library(shinythemes)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(shinydashboardPlus)
library(rintrojs)
library(jsonlite)
library(shinybusy)




# Objetivo
#'        - Carregar os arquivos usados no app;
#'        - Rodar o app.
#'        


# 1. Data -----------------------------------------------------------------

## Carrega os arquivos com os indicadores pré-calculados

files <- list.files(file.path(getwd(),"/data/output"), pattern = ".txt")

for(i in files){
  df <- read_csv(file.path(getwd(),"/data/output",i), 
                 col_types = cols(.default = 'c'))
  df <- df[,2:length(df)]
  assign(paste(substr(i,1,nchar(i)-4)), df)
  
}

rm(df)




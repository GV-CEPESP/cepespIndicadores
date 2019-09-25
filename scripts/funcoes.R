
## Pacotes utilizados

library(dplyr)
library(lubridate)
library(tidyverse)

## Objetivo:
#'         - Definir as funcoes utilizadas nos 
#'         - demais scripts do app.



# 1. Funcao gabi ----------------------------------------------------------


## Funcao para padronizacao do formato numerico dos dados

gabi <- function(string){
  ifelse(string > 1000000, 
         (paste0(floor(string/1000000),".",floor(string/1000)-floor(string/1000000)*1000,
                 ".", substr(floor(string), start = nchar(floor(string))- 2, stop = nchar(floor(string))),
                 ifelse(round(string,2)==floor(string),"",
                        paste0(",",substr(1 + round(string,2)-round(string,0),start = 3, stop = 4))))),
         
         (paste0(floor(string/1000),".", substr(floor(string), start = nchar(floor(string))- 2, stop = 
                                                  nchar(floor(string))),
                 
                 ifelse(round(string,2)==round(string,0),"",
                        paste0(",",substr(1 + round(string,2)-floor(string),start = 3, stop = 4))))))
}



# 2. Funcao 'pont_virg' ---------------------------------------------------


## Funcao que aplica a funcao 'gabi' somente nos casos em que ha 
## necessidade de pontuacao


pont_virg <- function(string){
  ifelse(
    test = string < 1000,
    yes = l <- string,
    no = l <- gabi(string))
}
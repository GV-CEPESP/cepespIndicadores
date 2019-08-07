

# Pacotes utilizados

library(dplyr)
library(lubridate)
library(tidyverse)


# Objetivo
#'        - Calcular os indicadores de distribuicao
#'                      de cadeiras:
#'        - Quociente eleitoral e quociente partidario
#'        - Limpeza e padronizacao dos dados em um formato adequado.



# 1. Quociente eleitoral --------------------------------------------------

## Calculo do quociente eleitoral

### Deputado Federal


vags_fed$QUOCIENTE_ELEITORAL <- as.numeric(vags_fed$VOTOS_VALIDOS_UF)/as.numeric(vags_fed$VAGAS)



### Deputado Estadual


vags_est$QUOCIENTE_ELEITORAL <- as.numeric(vags_est$VOTOS_VALIDOS_UF)/as.numeric(vags_est$VAGAS)



# 2. Quociente partidario -------------------------------------------------

## Calculo do quociente partidario

### Deputado Federal

vags_fed$QUOCIENTE_PARTIDARIO <- as.numeric(vags_fed$VOT_PART_UF)/as.numeric(vags_fed$QUOCIENTE_ELEITORAL)

### Deputado Estadual

vags_est$QUOCIENTE_PARTIDARIO <- as.numeric(vags_est$VOT_PART_UF)/as.numeric(vags_est$QUOCIENTE_ELEITORAL)



# 3. Padronizacao dos dados -----------------------------------------------

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


## Aplicacao da funcao 'gabi' e arredondamento dos valores dos indicadores calculados

### Deputado federal

vags_fed$QUOCIENTE_ELEITORAL<- round(vags_fed$QUOCIENTE_ELEITORAL, digits = 0)

vags_fed$QUOCIENTE_PARTIDARIO <- round(vags_fed$QUOCIENTE_PARTIDARIO, digits = 2)

vags_fed$QUOCIENTE_ELEITORAL <- gabi(vags_fed$QUOCIENTE_ELEITORAL)

vags_fed$VOTOS_VALIDOS_UF <- gabi(vags_fed$VOTOS_VALIDOS_UF)

vags_fed$VOT_PART_UF <- gabi(vags_fed$VOT_PART_UF)


### Deputado estadual

vags_est$QUOCIENTE_ELEITORAL <-gabi(vags_est$QUOCIENTE_ELEITORAL)

vags_est$QUOCIENTE_PARTIDARIO <- round(vags_est$QUOCIENTE_PARTIDARIO, digits = 2)


vags_est$VOTOS_VALIDOS_UF <- gabi(vags_est$VOTOS_VALIDOS_UF)

vags_est$VOT_PART_UF <- gabi(vags_est$VOT_PART_UF)



## Descarta as colunas desnecessarias, padroniza e renomeia as restantes

### Deputado Federal 

vags_fed <-  vags_fed %>% 
  dplyr::select(ANO_ELEICAO, 
                UF,
                CARGO, 
                VAGAS,
                VOTOS_VALIDOS_UF,
                SIGLA_PARTIDO,
                VOT_PART_UF,
                QUOCIENTE_ELEITORAL,
                QUOCIENTE_PARTIDARIO) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO",
                "Cargo" = "CARGO",
                "Cadeiras oferecidas" = "VAGAS",
                "Votos válidos" = "VOTOS_VALIDOS_UF",
                "Sigla do partido" = "SIGLA_PARTIDO",
                "Votos do partido" = "VOT_PART_UF",
                "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
                "Quociente partidário" = "QUOCIENTE_PARTIDARIO")
  
vags_fed$Cargo <- str_to_title(vags_fed$Cargo) ## Transforma a primeira letra de cada palavra
                                               ## em maiuscula



### Deputado Estadual

vags_est <-  vags_est %>% 
  dplyr::select(ANO_ELEICAO, 
                UF,
                CARGO, 
                VAGAS,
                VOTOS_VALIDOS_UF,
                SIGLA_PARTIDO,
                VOT_PART_UF,
                QUOCIENTE_ELEITORAL,
                QUOCIENTE_PARTIDARIO) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO",
                "Cargo" = "CARGO",
                "Cadeiras oferecidas" = "VAGAS",
                "Votos válidos" = "VOTOS_VALIDOS_UF",
                "Sigla do partido" = "SIGLA_PARTIDO",
                "Votos do partido" = "VOT_PART_UF",
                "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
                "Quociente partidário" = "QUOCIENTE_PARTIDARIO")

vags_est$Cargo <- str_to_title(vags_est$Cargo) ## Transforma a primeira letra de cada palavra
                                               ## em maiuscula



# 4. Salva os arquivos ----------------------------------------------------

## Salva os arquivos referentes aos indicadores de distribuicao
## de cadeiras em .csv

### Deputado Federal

write.csv(vags_fed, "data/output/distcad_fed.csv")

### Deputado Estadual

write.csv(vags_est, "data/output/distcad_est.csv")

## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(vags_est,vags_fed)

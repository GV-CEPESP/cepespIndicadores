
# Pacotes utilizados

library(cepespR)
library(dplyr)
library(tidyverse)
library(abjutils)

# Objetivo
#'        - Fazer o download dos dados;
#'        - Limpar os dados;
#'        - Juntar os bancos de dados em um único.


# 1. Download --------------------------------------------------------


## Download dos dados no cepespR para os cargos de

### Deputado Federal


dfp <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018",
                     position = "Deputado Federal",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Partido")

dfc <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                     position = "Deputado Federal",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Consolidado")

dfcb <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                      position = "Deputado Federal",
                      regional_aggregation = "Brasil", 
                      political_aggregation = "Consolidado")

df <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                    position = "Deputado Federal",
                    regional_aggregation = "Estado", 
                    political_aggregation = "Candidato")


### Deputado Estadual

dep <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018",
                     position = "Deputado Estadual",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Partido")

dec <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", 
                     position = "Deputado Estadual",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Consolidado")

decb <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", 
                      position = "Deputado Estadual",
                      regional_aggregation = "Brasil", 
                      political_aggregation = "Consolidado")

de <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", 
                    position = "Deputado Estadual",
                    regional_aggregation = "Estado", 
                    political_aggregation = "Candidato")


## Carrega os arquivos de vagas

vags_fed <- read_csv("vags_fed.csv") ### Deputado Federal

vags_est <- read_csv("vags_est.csv") ### Deputado Estadual


# 2. Calculo da votacao dos partidos e dos votos validos ------------------

## Votação UF dos partidos

### Deputado Federal

dfp <- dfp %>%  
  dplyr::group_by(ANO_ELEICAO, 
                  UF,
                  SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_UF = sum(QTDE_VOTOS))

### Deputado Estadual

dep <- dep %>% 
  dplyr::group_by(ANO_ELEICAO,
                  UF,
                  SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_UF = sum(QTDE_VOTOS)
  ) 

## Votos validos de cada eleicao

### Deputado Federal

dfc1 <- dfc %>% 
  dplyr::group_by(ANO_ELEICAO,
                  UF) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_UF = sum(QT_VOTOS_NOMINAIS,
                           QT_VOTOS_LEGENDA)
  )

### Deputado Estadual

dec1 <- dec %>% 
  dplyr::group_by(ANO_ELEICAO,
                  UF) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_UF = sum(QT_VOTOS_NOMINAIS,
                           QT_VOTOS_LEGENDA)
  )


# 3. Join ---------------------------------------------------------


## Junta os bancos de vagas, consolidado e resultado das eleicoes

### Deputado Federal

vags_fed <- left_join(vags_fed,
                      dfc1, 
                      by = "UF")

vags_fed <- left_join(vags_fed, 
                      dfp,
                      by = c("ANO_ELEICAO", "UF"))


### Deputado Estadual

vags_est <- left_join(vags_est,
                      dec1, 
                      by = "UF")

vags_est <- left_join(vags_est, 
                      dep, 
                      by = c("ANO_ELEICAO", "UF"))




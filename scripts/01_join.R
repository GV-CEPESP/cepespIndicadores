
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

## Outros cargos BR

pr_br <- get_elections(year = "2002,2006,2010,2014,2018", 
                        position = "Presidente",
                        regional_aggregation = "Brasil", 
                        political_aggregation = "Consolidado")

gov_br <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                        position = "Governador",
                        regional_aggregation = "Brasil", 
                        political_aggregation = "Consolidado")

sen_br <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                        position = "Senador",
                        regional_aggregation = "Brasil", 
                        political_aggregation = "Consolidado")


## Outros Cargos UF

pr_uf <- get_elections(year = "2002,2006,2010,2014,2018", 
                    position = "Presidente",
                    regional_aggregation = "Estado", 
                    political_aggregation = "Consolidado")

gov_uf <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                     position = "Governador",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Consolidado")

sen_uf <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                     position = "Senador",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Consolidado")



## Carrega os arquivos de vagas

### Deputado Federal

vags_fed <- read_csv("data/input/vags_fed.csv") 

### Deputado Estadual

vags_est <- read_csv("data/input/vags_est.csv") 

## Carrega o arquivo da eleicao de 1994

eleicao_94 <- read.csv("data/input/eleicoes_94.csv")


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
  dplyr::group_by(ANO_ELEICAO,UF) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_UF = sum(QT_VOTOS_NOMINAIS,
                           QT_VOTOS_LEGENDA)
  )

dfc2 <- dfc %>% 
  dplyr::group_by(ANO_ELEICAO) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS = sum(QT_VOTOS_NOMINAIS,
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
                      by = c("ANO_ELEICAO", 
                             "UF"))


### Deputado Estadual

vags_est <- left_join(vags_est,
                      dec1, 
                      by = "UF")

vags_est <- left_join(vags_est, 
                      dep, 
                      by = c("ANO_ELEICAO", "UF"))

est <- vags_est

fed <- vags_fed


## Junta os bancos sobre o consolidado em um unico

## Consolidado BR

cons_br <- rbind(pr_br, gov_br, sen_br, dfcb, decb)

## Consolidado UF

cons_uf <- rbind(pr_uf, gov_uf,sen_uf, dfc, dec)


## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(dec,dec1,dfc,dfc1,decb,dfcb,pr_br, pr_uf,
   gov_br,gov_uf, sen_br, sen_uf,dep)



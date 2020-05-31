
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
                     political_aggregation = "Partido",
                     cached = TRUE)

dfc <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                     position = "Deputado Federal",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Consolidado",
                     cached = TRUE)

dfcb <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                      position = "Deputado Federal",
                      regional_aggregation = "Brasil", 
                      political_aggregation = "Consolidado",
                      cached = TRUE)

df <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                    position = "Deputado Federal",
                    regional_aggregation = "Estado", 
                    political_aggregation = "Candidato",
                    cached = TRUE)


### Deputado Estadual

dep <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018",
                     position = "Deputado Estadual",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Partido",
                     cached = TRUE)

dec <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", 
                     position = "Deputado Estadual",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Consolidado",
                     cached = TRUE)

decb <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", 
                      position = "Deputado Estadual",
                      regional_aggregation = "Brasil", 
                      political_aggregation = "Consolidado",
                      cached = TRUE)

de <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", 
                    position = "Deputado Estadual",
                    regional_aggregation = "Estado", 
                    political_aggregation = "Candidato",
                    cached = TRUE)

## Vereador


vrp <- get_elections(year = "2000,2004,2008,2012,2016",
                     position = "Vereador",
                     regional_aggregation = "Municipio", 
                     political_aggregation = "Partido",
                     cached = TRUE)

vrc <- get_elections(year = "2000,2004,2008,2012,2016", 
                     position = "Vereador",
                     regional_aggregation = "Municipio", 
                     political_aggregation = "Consolidado",
                     cached = TRUE)

vrcm <- get_elections(year = "2000,2004,2008,2012,2016", 
                      position = "Vereador",
                      regional_aggregation = "Municipio", 
                      political_aggregation = "Consolidado",
                      cached = TRUE)

anos <- c(2000, 2004, 2008, 2012, 2016)

vr <- list()

for(i in anos){
temp <- get_elections(year = i, 
                    position = "Vereador",
                    regional_aggregation = "Municipio", 
                    political_aggregation = "Candidato",
                    cached = TRUE)
temp$CODIGO_LEGENDA <- as.character(temp$CODIGO_LEGENDA)
vr <- rbind(vr, temp)
}

rm(temp)


## Outros cargos BR

pr_br <- get_elections(year = "2002,2006,2010,2014,2018", 
                        position = "Presidente",
                        regional_aggregation = "Brasil", 
                        political_aggregation = "Consolidado",
                        cached = TRUE)

gov_br <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                        position = "Governador",
                        regional_aggregation = "Brasil", 
                        political_aggregation = "Consolidado",
                        cached = TRUE)

sen_br <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                        position = "Senador",
                        regional_aggregation = "Brasil", 
                        political_aggregation = "Consolidado",
                        cached = TRUE)


## Outros Cargos UF

pr_uf <- get_elections(year = "2002,2006,2010,2014,2018", 
                    position = "Presidente",
                    regional_aggregation = "Estado", 
                    political_aggregation = "Consolidado",
                    cached = TRUE)

gov_uf <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                     position = "Governador",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Consolidado",
                     cached = TRUE)

sen_uf1 <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                     position = "Senador",
                     regional_aggregation = "Estado", 
                     political_aggregation = "Consolidado",
                     cached = TRUE)

sen_uf <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                        position = "Senador",
                        regional_aggregation = "Brasil", 
                        political_aggregation = "Candidato",
                        cached = TRUE)


sen_uft <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", 
                         position = "Senador",
                         regional_aggregation = "Estado", 
                         political_aggregation = "Partido",
                         cached = TRUE)


## Outros cargos MUN

pfcm <- get_elections(year = "2000,2004,2008,2012,2016", 
                      position = "Prefeito",
                      regional_aggregation = "Municipio", 
                      political_aggregation = "Consolidado",
                      cached = TRUE)


## Carrega os arquivos de vagas

### Deputado Federal

vags_fed <- read_csv("data/input/vags_fed.csv") 

### Deputado Estadual

vags_est <- read_csv("data/input/vags_est.csv")

### Vereador

vags_ver <- read_csv("data/input/vags_ver.csv") 

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

## Votacao municipal dos partidos
### Vereador

vrp <- vrp %>% 
  dplyr::group_by(ANO_ELEICAO,
                  UF,
                  COD_MUN_TSE,
                  SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    VOT_PART_MUN = sum(QTDE_VOTOS)
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

### Vereador

vrc1 <- vrc %>% 
  dplyr::group_by(ANO_ELEICAO,
                  UF,
                  COD_MUN_TSE) %>% 
  dplyr::summarise(
    VOTOS_VALIDOS_MUN = sum(QT_VOTOS_NOMINAIS,
                           QT_VOTOS_LEGENDA)
  )

## Calculo das vagas para senador em cada eleicao

### Senador (Brasil)


x <- as.data.frame(table(sen_uf$ANO_ELEICAO, sen_uf$DESC_SIT_TOT_TURNO))

x <- x[-c(1:12, 19:42),]

x <- x %>% 
  dplyr::select(Var1, Freq) %>% 
  rename("ANO_ELEICAO" = "Var1",
         "Vagas" = "Freq")

x$ANO_ELEICAO <- as.character(x$ANO_ELEICAO)

sen_uf$ANO_ELEICAO <- as.character(sen_uf$ANO_ELEICAO)

sen_uf <- left_join(sen_uf, x, by = "ANO_ELEICAO")

rm(x)



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

### Vereador

vags_ver$COD_MUN_TSE <- as.character(vags_ver$COD_MUN_TSE)

vags_ver$COD_MUN_TSE <- str_pad(vags_ver$COD_MUN_TSE, width = 5, pad = 0)

vags_ver <- left_join(vags_ver,
                      vrc1)

vags_ver <- left_join(vags_ver, 
                      vrp)


est <- vags_est

fed <- vags_fed

ver <- vags_ver


## Junta os bancos sobre o consolidado em um unico

## Consolidado BR

cons_br <- rbind(pr_br, gov_br, sen_br, dfcb, decb)

## Consolidado UF

cons_uf <- rbind(pr_uf, gov_uf,sen_uf1, dfc, dec)

## Consolidado MUN

cons_mun <- rbind(pfcm, vrcm)


## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(dec,dec1,dfc,dfc1,decb,dfcb,pr_br, pr_uf,
   gov_br,gov_uf, sen_br, dep)



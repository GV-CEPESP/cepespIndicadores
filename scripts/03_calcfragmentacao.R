

# Pacotes utilizados

library(dplyr)
library(lubridate)
library(tidyverse)


# Objetivo
#'        - Calcular os indicadores de fragmentacao legislativa:
#'        - Numero de cadeiras,Fracionalizacao,Fragmentacao, Fragmentacao máxima,
#'        - Desproporcionalidade de Gallagher, Numero efetivo 
#'              de partidos por votos e por cadeiras;
#'        - Limpeza e padronizacao dos dados.             
        


# 1. Numero de cadeiras ---------------------------------------------------------


## Filtra os candidatos que foram eleitos

### Deputado Federal

dft <- df %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

### Deputado Estadual

det <- de %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

## Soma as cadeiras conquistadas pelos partidos em cada UF

### Deputado Federal

dft <- dft %>% 
  dplyr::group_by(ANO_ELEICAO,
                  DESCRICAO_CARGO, 
                  SIGLA_PARTIDO, 
                  UF) %>% 
  dplyr::summarise("Cadeiras conquistadas por UF" = n())


### Deputado Estadual

det <- det %>% 
  dplyr::group_by(ANO_ELEICAO, 
                  DESCRICAO_CARGO, 
                  SIGLA_PARTIDO, 
                  UF) %>% 
  dplyr::summarise("Cadeiras conquistadas por UF" = n())

det <- det %>% 
  rename("CARGO" = "DESCRICAO_CARGO")

## Soma o total de cadeiras conquistadas pelos partidos em cada eleicao

### Deputado Federal

df1 <- dft %>% 
  dplyr::group_by(ANO_ELEICAO,
                  DESCRICAO_CARGO,
                  SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    "Total de cadeiras conquistadas" = 
      sum(`Cadeiras conquistadas por UF`))



## Junta os bancos de cadeiras conquistadas por UF com o total de cadeiras conquistadas

### Deputado Federal

dfp <- dfp %>% 
  dplyr::group_by(ANO_ELEICAO, SIGLA_PARTIDO) %>% 
  summarise(
    "Total de votos conquistados" = sum(VOT_PART_UF)
  )

df1 <- left_join(dfp,df1, by = c("ANO_ELEICAO", 
                                 "SIGLA_PARTIDO"))

df1 <- left_join(df1, dfc2, by = c("ANO_ELEICAO"))

### Deputado Estadual


de1 <- left_join(est,det, by = c("ANO_ELEICAO", 
                                 "CARGO",
                                 "UF",
                                 "SIGLA_PARTIDO"))


## Calcula o percentual de votos conquistados por cada partido

### Deputado Federal

df1$`Percentual de votos conquistados` <- df1$`Total de votos conquistados`/df1$VOTOS_VALIDOS

### Deputado Estadual

de1$`Percentual de votos conquistados` <- de1$VOT_PART_UF/de1$VOTOS_VALIDOS_UF

## Percentual de cadeiras conquistas pelos partidos

### Deputado Federal

df1$`Percentual de cadeiras conquistadas` <- df1$`Total de cadeiras conquistadas`/513

### Deputado Estadual

de1$`Percentual de cadeiras conquistadas` <- de1$`Cadeiras conquistadas por UF`/de1$VAGAS

## Elimina colunas desnecessarias, renomeia as colunas restantes e padroniza-as

### Deputado Federal

df1 <- df1 %>% 
  dplyr::select(ANO_ELEICAO,
                DESCRICAO_CARGO,
                VOTOS_VALIDOS,
                SIGLA_PARTIDO,
                `Total de votos conquistados`,
                `Total de cadeiras conquistadas`,
                `Percentual de votos conquistados`,
                `Percentual de cadeiras conquistadas`) %>% 
   dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                 "Cargo" = "DESCRICAO_CARGO",
                "Votos válidos" = "VOTOS_VALIDOS", 
                "Sigla do partido" = "SIGLA_PARTIDO")

df1$Cargo <- str_to_title(df1$Cargo)

df1 <- na.omit(df1)


### Deputado Estadual

de1 <- de1 %>% 
  dplyr::select(ANO_ELEICAO,
                UF,
                CARGO,
                VAGAS,
                VOTOS_VALIDOS_UF,
                SIGLA_PARTIDO,
                VOT_PART_UF,
                `Cadeiras conquistadas por UF`,
                `Percentual de votos conquistados`,
                `Percentual de cadeiras conquistadas`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "CARGO",
                "Vagas" = "VAGAS",
                "Votos válidos" = "VOTOS_VALIDOS_UF",
                "Sigla do partido" = "SIGLA_PARTIDO",
                "Total de votos conquistados" = "VOT_PART_UF",
                "Total de cadeiras conquistadas" = "Cadeiras conquistadas por UF")

de1$Cargo <- str_to_title(de1$Cargo)

de1 <- na.omit(de1)


# 2. Fracionalizacao ------------------------------------------------------

## Funcao para o calculo da fracionalizacao

fracio <- function(x){
  
  1-(sum(x^2))
}

## Calculo do indice de fracionalizacao em cada eleicao


### Deputado Federal

df2 <- list()


for(ano in sort(unique(df1$`Ano da eleição`))){
    cat("Lendo",ano,"\n")
    t <- filter(df1,
                `Ano da eleição` == ano)
    t$`Fracionalização` <- fracio(t$`Percentual de cadeiras conquistadas`)
    print(t)
    df2 <- bind_rows(df2,t)
  }

rm(df1)

### Deputado Estadual



t2 <- list()

for(ano in sort(unique(de1$`Ano da eleição`))){
  for(uf in sort(unique(de1$UF))){
    cat(ano,uf, "\n")
    t <- filter(de1,
        `Ano da eleição` == ano & 
          UF == uf)
  t$`Fracionalização` <- fracio(t$`Percentual de cadeiras conquistadas`)
  print(t)
  t2 <- bind_rows(t2,t)
  }
}



# 3. Fracionalizacao maxima -----------------------------------------------

## Funcao para o calculo da fracionalizacao maxima

fracio_max <- function(N, n){
  
  (N*(n-1))/(n*(N-1))
  
}

## Calculo do indice de fracionalizacao maxima em cada eleicao

t98df$`Fracionalização máxima`<- fracio_max(513,18)

t02df$`Fracionalização máxima`<- fracio_max(513,19) 

t06df$`Fracionalização máxima`<- fracio_max(513,21)

t10df$`Fracionalização máxima`<- fracio_max(513,22)

t14df$`Fracionalização máxima`<- fracio_max(513,28)

t18df$`Fracionalização máxima`<- fracio_max(513,30)


# 4. Fragmentacao ---------------------------------------------------------

## Funcao para o calculo da fragmentacao

frag <- function(fracio, fracio_max){
  
  fracio/fracio_max
}

## Calculo do indice de fragmentacao em cada eleicao

t98df$Fragmentação <- frag(t98df$Fracionalização, t98df$`Fracionalização máxima`)

t02df$Fragmentação <- frag(t02df$Fracionalização, t02df$`Fracionalização máxima`)

t06df$Fragmentação <- frag(t06df$Fracionalização, t06df$`Fracionalização máxima`)

t10df$Fragmentação <- frag(t10df$Fracionalização, t10df$`Fracionalização máxima`)

t14df$Fragmentação <- frag(t14df$Fracionalização, t14df$`Fracionalização máxima`)

t18df$Fragmentação <- frag(t18df$Fracionalização, t18df$`Fracionalização máxima`)



# 5. Desproporcionalidade de Gallagher ------------------------------------



desp_gallag <- function(V,C){
  idx <- sqrt(sum((V*100 - C*100) ^ 2, na.rm = TRUE) / 2)
}


t98df$`Desproporcionalidade de gallagher` <- desp_gallag(t98df$`Percentual de votos conquistados`,
                 t98df$`Percentual de cadeiras conquistadas`)

t02df$`Desproporcionalidade de gallagher` <- desp_gallag(t02df$`Percentual de votos conquistados`,
                                                         t02df$`Percentual de cadeiras conquistadas`)

t06df$`Desproporcionalidade de gallagher` <- desp_gallag(t06df$`Percentual de votos conquistados`,
                                                         t06df$`Percentual de cadeiras conquistadas`)

t10df$`Desproporcionalidade de gallagher` <- desp_gallag(t10df$`Percentual de votos conquistados`,
                                                         t10df$`Percentual de cadeiras conquistadas`)

t14df$`Desproporcionalidade de gallagher` <- desp_gallag(t14df$`Percentual de votos conquistados`,
                                                         t14df$`Percentual de cadeiras conquistadas`)

t18df$`Desproporcionalidade de gallagher` <- desp_gallag(t18df$`Percentual de votos conquistados`,
                                                         t18df$`Percentual de cadeiras conquistadas`)

# 6. Numero efetivo de partidos ---------------------------------

## Funcao para o calculo do numero efetivo de partidos 

options(scipen=999)

NEPV <- NA

NEP <- function(p){
  for(i in 1:length(p)){
    NEPV[[i]]<-(p[[i]]*p[[i]])
  }
  1/sum(NEPV)}

# 6.1. Por votos ----------------------------------------------------------

## Calculo do numero efetivo de partidos por votos

t98df$`Número efetivo de partidos por votos` <- NEP(t98df$`Percentual de votos conquistados`)

t02df$`Número efetivo de partidos por votos` <- NEP(t02df$`Percentual de votos conquistados`)

t06df$`Número efetivo de partidos por votos` <- NEP(t06df$`Percentual de votos conquistados`)

t10df$`Número efetivo de partidos por votos` <- NEP(t10df$`Percentual de votos conquistados`)

t14df$`Número efetivo de partidos por votos` <- NEP(t14df$`Percentual de votos conquistados`)

t18df$`Número efetivo de partidos por votos` <- NEP(t18df$`Percentual de votos conquistados`)


# 6.2. Por cadeiras -------------------------------------------------------


## Calculo do numero efetivo de partidos por cadeiras

t98df$`Número efetivo de partidos por cadeiras` <- NEP(t98df$`Percentual de cadeiras conquistadas`)

t02df$`Número efetivo de partidos por cadeiras` <- NEP(t02df$`Percentual de cadeiras conquistadas`)

t06df$`Número efetivo de partidos por cadeiras` <- NEP(t06df$`Percentual de cadeiras conquistadas`)

t10df$`Número efetivo de partidos por cadeiras` <- NEP(t10df$`Percentual de cadeiras conquistadas`)

t14df$`Número efetivo de partidos por cadeiras` <- NEP(t14df$`Percentual de cadeiras conquistadas`)

t18df$`Número efetivo de partidos por cadeiras` <- NEP(t18df$`Percentual de cadeiras conquistadas`)



# 7. Padronizacao dos dados -----------------------------------------------

## Junta todos bancos de fragmentacao partidaria em um único

frag_part_fed <- bind_rows(t98df, t02df, t06df, t10df, t14df, t18df) 

## Arredonda em duas casas decimas os indices calculados

frag_part_fed$`Percentual de votos conquistados`<- 
  format(round(frag_part_fed$`Percentual de votos conquistados`, 
               digits = 2),  
         nsmall = 2)

frag_part_fed$`Percentual de cadeiras conquistadas` <- 
  format(round(frag_part_fed$`Percentual de cadeiras conquistadas`, 
               digits = 2),  
         nsmall = 2)

frag_part_fed$Fracionalização <- 
  format(round(frag_part_fed$Fracionalização, 
               digits = 2),  
         nsmall = 2)

frag_part_fed$`Fracionalização máxima` <- 
  format(round(frag_part_fed$`Fracionalização máxima`, 
               digits = 2), 
         nsmall = 2)

frag_part_fed$Fragmentação <- 
  format(round(frag_part_fed$Fragmentação, 
               digits = 2),
         nsmall = 2)

frag_part_fed$`Desproporcionalidade de gallagher` <- 
  format(round(frag_part_fed$`Desproporcionalidade de gallagher`, 
               digits = 2), 
         nsmall = 2)

frag_part_fed$`Número efetivo de partidos por votos` <- 
  format(round(frag_part_fed$`Número efetivo de partidos por votos`, 
               digits = 2), 
         nsmall = 2)

frag_part_fed$`Número efetivo de partidos por cadeiras` <- 
  format(round(frag_part_fed$`Número efetivo de partidos por cadeiras`, 
               digits = 2), 
         nsmall = 2)


frag_part_fed$`Votos válidos` <- gabi(frag_part_fed$`Votos válidos`)

frag_part_fed$`Total de votos conquistados` <- gabi(frag_part_fed$`Total de votos conquistados`)

# 8. Salva o arquivo ------------------------------------------------------

## Salva os arquivos referentes aos indicadores de fragmentacao
## partidaria em .csv

## Deputado Federal

write.csv(frag_part_fed, "data/output/frag_part_fed.csv")

## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(df1,frag_part_fed,dft,det,dfc2,dfp,de1,dep)



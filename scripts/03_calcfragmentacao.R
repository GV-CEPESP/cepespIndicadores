

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

## Calcula o numero de partidos parlamentares para cada eleicao

### Deputado Federal


dfr <- df1 %>% 
  group_by(`Ano da eleição`) %>% 
  summarise(
    `Número de partidos parlamentares` = n()
  )

df1 <- left_join(df1,dfr, 
                 by = "Ano da eleição")

### Deputado Estadual


dfr <- de1 %>% 
  group_by(`Ano da eleição`, UF) %>% 
  summarise(
    `Número de partidos parlamentares` = n()
  )

de1 <- left_join(de1,dfr, 
                 by = c("Ano da eleição", "UF"))


# 2. Formulas ------------------------------------------------------

## Funcao para o calculo da fracionalizacao

fracio <- function(x){
  
  1-(sum(x^2))
}

## Funcao para o calculo da fracionalizacao maxima

fracio_max <- function(N, n){
  
  (N*(n-1))/(n*(N-1))
  
}

## Funcao para o calculo da fragmentacao

frag <- function(fracio, fracio_max){
  
  fracio/fracio_max
}

## Funcao para o calculo da desproporcionalidade de Gallagher

desp_gallag <- function(V,C){
  idx <- sqrt(sum((V*100 - C*100) ^ 2, na.rm = TRUE) / 2)
}

## Funcao para o calculo do numero efetivo de partidos 

nep <- function(pe){
  1/sum(pe*pe)
}


# 3. Calculo dos indicadores ----------------------------------------------

## Calculo dos indicadores de framentacao partidaria

### Deputado Federal

frag_part_fed <- list()


for(ano in sort(unique(df1$`Ano da eleição`))){
  cat("Lendo",ano,"\n")
  t <- filter(df1,
              `Ano da eleição` == ano)
  t$`Fracionalização` <- fracio(t$`Percentual de cadeiras conquistadas`)
  t$`Fracionalização máxima` <- fracio_max(513,t$`Número de partidos parlamentares`)
  t$`Fragmentação` <- frag(t$`Fracionalização`, 
                           t$`Fracionalização máxima`)
  t$`Desproporcionalidade de gallagher` <- desp_gallag(t$`Percentual de votos conquistados`,
                                                       t$`Percentual de cadeiras conquistadas`)
  t$`Número efetivo de partidos por votos` <- nep(t$`Percentual de votos conquistados`)
  t$`Número efetivo de partidos por cadeiras` <- nep(t$`Percentual de cadeiras conquistadas`)
  print(t)
  frag_part_fed <- bind_rows(frag_part_fed,t)
}

### Deputado Estadual

estados <- c("AC", "AL", "AM", "AP", "BA", "CE", "ES", 
             "GO", "MA", "MG","MS", "MT", "PA", "PB", 
             "PE", "PI", "PR","RJ", "RN", "RO", "RR",
             "RS", "SC", "SE", "SP", "TO")


frag_part_est <- list()

for(ano in sort(unique(de1$`Ano da eleição`))){
  for(uf in estados){
    cat(ano,uf, "\n")
    t <- filter(de1,
                `Ano da eleição` == ano & 
                  UF == uf)
    t$`Fracionalização` <- fracio(t$`Percentual de cadeiras conquistadas`)
    t$`Fracionalização máxima` <- fracio_max(t$Vagas,t$`Número de partidos parlamentares`)
    t$`Fragmentação` <- frag(t$Fracionalização,
                             t$`Fracionalização máxima`)
    t$`Desproporcionalidade de gallagher` <- desp_gallag(t$`Percentual de votos conquistados`,
                                                         t$`Percentual de cadeiras conquistadas`)
    NEPV <- NA
    t$`Número efetivo de partidos por votos` <- nep(t$`Percentual de votos conquistados`)
    t$`Número efetivo de partidos por cadeiras` <- nep(t$`Percentual de cadeiras conquistadas`)
    print(t)
    frag_part_est <- bind_rows(frag_part_est,t)
  }
}




# 4. Padronizacao dos dados -----------------------------------------------


## Arredonda em duas casas decimas os indices calculados


### Deputado Federal

frag_part_fed <- frag_part_fed %>% 
  select(`Ano da eleição`, 
         Cargo, 
         `Votos válidos`,
         `Sigla do partido`,
         `Total de votos conquistados`,
         `Total de cadeiras conquistadas`,
         `Percentual de votos conquistados`,
         `Percentual de cadeiras conquistadas`,
         Fracionalização,
         `Fracionalização máxima`,
         Fragmentação,
         `Desproporcionalidade de gallagher`, 
         `Número efetivo de partidos por votos`,
         `Número efetivo de partidos por cadeiras`)


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

### Deputado Estadual

frag_part_est<- frag_part_est %>% 
  select(`Ano da eleição`,
         UF,
         Cargo,
         Vagas,
         `Votos válidos`,
         `Sigla do partido`,
         `Total de votos conquistados`,
         `Total de cadeiras conquistadas`,
         `Percentual de votos conquistados`,
         `Percentual de cadeiras conquistadas`,
         Fracionalização,
         `Fracionalização máxima`,
         Fragmentação,
         `Desproporcionalidade de gallagher`, 
         `Número efetivo de partidos por votos`,
         `Número efetivo de partidos por cadeiras`)


frag_part_est$`Percentual de votos conquistados`<- 
  format(round(frag_part_est$`Percentual de votos conquistados`, 
               digits = 2),  
         nsmall = 2)

frag_part_est$`Percentual de cadeiras conquistadas` <- 
  format(round(frag_part_est$`Percentual de cadeiras conquistadas`, 
               digits = 2),  
         nsmall = 2)

frag_part_est$Fracionalização <- 
  format(round(frag_part_est$Fracionalização, 
               digits = 2),  
         nsmall = 2)

frag_part_est$`Fracionalização máxima` <- 
  format(round(frag_part_est$`Fracionalização máxima`, 
               digits = 2), 
         nsmall = 2)

frag_part_est$Fragmentação <- 
  format(round(frag_part_est$Fragmentação, 
               digits = 2),
         nsmall = 2)

frag_part_est$`Desproporcionalidade de gallagher` <- 
  format(round(frag_part_est$`Desproporcionalidade de gallagher`, 
               digits = 2), 
         nsmall = 2)

frag_part_est$`Número efetivo de partidos por votos` <- 
  format(round(frag_part_est$`Número efetivo de partidos por votos`, 
               digits = 2), 
         nsmall = 2)

frag_part_est$`Número efetivo de partidos por cadeiras` <- 
  format(round(frag_part_est$`Número efetivo de partidos por cadeiras`, 
               digits = 2), 
         nsmall = 2)


frag_part_est$`Votos válidos` <- gabi(frag_part_est$`Votos válidos`)

frag_part_est$`Total de votos conquistados` <- gabi(frag_part_est$`Total de votos conquistados`)

# 5. Salva o arquivo ------------------------------------------------------

## Salva os arquivos referentes aos indicadores de fragmentacao
## partidaria em .csv

### Deputado Federal

write.csv(frag_part_fed, "data/output/frag_part_fed.csv")

### Deputado Estadual

write.csv(frag_part_est, "data/output/frag_part_est.csv")


## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(frag_part_fed,frag_part_est,dft,det,dfc2,dfp,dfr,de1,df1,t)





# Pacotes utilizados

library(dplyr)
library(lubridate)
library(tidyverse)


# Objetivo
#'        - Calcular os indicadores de fragmentacao legislativa:
#'        - Numero efetivo de partidos eleitoral,Numero efetivo de partidos legislativo,
#'        - Fracionalizacao, Fragmentacao máxima, Fragmentacao, 
#'        - Desproporcionalidade, Quociente eleitoral e Quociente partidário
#'        - Limpeza e padronizacao dos dados.             
        

# 1. Formulas ------------------------------------------------------

## Funcao para o calculo do numero efetivo de partidos 

nep <- function(pe){
  1/sum(pe*pe)
}


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


# 2. Calculo dos indicadores de distribuicao de cadeiras ------------------


# 2.1. Quociente eleitoral --------------------------------------------------

## Calculo do quociente eleitoral

### Deputado Federal


vags_fed$QUOCIENTE_ELEITORAL <- as.numeric(vags_fed$VOTOS_VALIDOS_UF)/
  as.numeric(vags_fed$VAGAS)



### Deputado Estadual


vags_est$QUOCIENTE_ELEITORAL <- as.numeric(vags_est$VOTOS_VALIDOS_UF)/
  as.numeric(vags_est$VAGAS)



# 2.2. Quociente partidario -------------------------------------------------

## Calculo do quociente partidario

### Deputado Federal

vags_fed$QUOCIENTE_PARTIDARIO <- as.numeric(vags_fed$VOT_PART_UF)/
  as.numeric(vags_fed$QUOCIENTE_ELEITORAL)

### Deputado Estadual

vags_est$QUOCIENTE_PARTIDARIO <- as.numeric(vags_est$VOT_PART_UF)/
  as.numeric(vags_est$QUOCIENTE_ELEITORAL)



# 2.3. Padronizacao dos dados -----------------------------------------------

## Aplicacao da funcao 'pont_virg' e arredondamento dos valores dos indicadores calculados

### Deputado federal

options(OutDec= ",")


vags_fed$QUOCIENTE_ELEITORAL<- round(vags_fed$QUOCIENTE_ELEITORAL, digits = 0)

vags_fed$QUOCIENTE_PARTIDARIO <- round(vags_fed$QUOCIENTE_PARTIDARIO, digits = 2)

vags_fed$QUOCIENTE_ELEITORAL <- pont_virg(vags_fed$QUOCIENTE_ELEITORAL)

vags_fed$VOTOS_VALIDOS_UF <- pont_virg(vags_fed$VOTOS_VALIDOS_UF)

vags_fed$VOT_PART_UF <- pont_virg(vags_fed$VOT_PART_UF)


### Deputado estadual

vags_est$QUOCIENTE_PARTIDARIO <- round(vags_est$QUOCIENTE_PARTIDARIO, digits = 2)

vags_est$QUOCIENTE_ELEITORAL <-pont_virg(vags_est$QUOCIENTE_ELEITORAL)


vags_est$VOTOS_VALIDOS_UF <- pont_virg(vags_est$VOTOS_VALIDOS_UF)

vags_est$VOT_PART_UF <- pont_virg(vags_est$VOT_PART_UF)



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

vags_fed$`Quociente partidário` <- as.character(vags_fed$`Quociente partidário`) 


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

vags_est$`Quociente partidário` <- as.character(vags_est$`Quociente partidário`)


# 3. Numero de cadeiras ---------------------------------------------------------


## Filtra os candidatos que foram eleitos

### Deputado Federal (Brasil)

dft_br <- df %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")


### Deputado Federal (UF)

dft_uf <- df %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")


### Deputado Estadual

det <- de %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

### Senador (Brasil)

sen_br <- sen_uf %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO")


## Soma as cadeiras conquistadas pelos partidos em cada UF

### Deputado Federal (Brasil)

dft_br <- dft_br %>% 
  dplyr::group_by(ANO_ELEICAO,
                  DESCRICAO_CARGO, 
                  SIGLA_PARTIDO, 
                  UF) %>% 
  dplyr::summarise("Cadeiras conquistadas por UF" = n())

### Deputado Federal (UF)

dft_uf <- dft_uf %>% 
  dplyr::group_by(ANO_ELEICAO,
                  UF,
                  DESCRICAO_CARGO, 
                  SIGLA_PARTIDO) %>% 
  dplyr::summarise("Total de cadeiras conquistadas" = n())


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

### Deputado Federal (Brasil)

df1_br <- dft_br %>% 
  dplyr::group_by(ANO_ELEICAO,
                  DESCRICAO_CARGO,
                  SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    "Total de cadeiras conquistadas" = 
      sum(`Cadeiras conquistadas por UF`))


### Senador (Brasil)

sen_br <- sen_br %>% 
  dplyr::group_by(ANO_ELEICAO,
                  DESCRICAO_CARGO,
                  Vagas,
                  SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    "Total de cadeiras conquistadas" = n())

## Junta os bancos de cadeiras conquistadas por UF com o total de cadeiras conquistadas

### Deputado Federal (Brasil)

dfp_br <- dfp %>% 
  dplyr::group_by(ANO_ELEICAO, SIGLA_PARTIDO) %>% 
  summarise(
    "Total de votos conquistados" = sum(VOT_PART_UF)
  )

df1_br <- left_join(dfp_br,df1_br, by = c("ANO_ELEICAO", 
                                 "SIGLA_PARTIDO"))

df1_br <- left_join(df1_br, dfc2, by = c("ANO_ELEICAO"))


### Deputado Federal (UF)

dfp_uf <- dfp %>% 
  dplyr::group_by(ANO_ELEICAO, UF, SIGLA_PARTIDO) %>% 
  summarise(
    "Total de votos conquistados" = sum(VOT_PART_UF)
  )

df1_uf <- left_join(dfp_uf,dft_uf, by = c("ANO_ELEICAO",
                                          "UF",
                                          "SIGLA_PARTIDO"))

df1_uf <- rename(df1_uf, 
                 "CARGO" = "DESCRICAO_CARGO")

df1_uf$CARGO <- "DEPUTADO FEDERAL"

fed <- rename(fed, 
                 "Total de votos conquistados" = "VOT_PART_UF")

df1_uf <- left_join(df1_uf, fed, by = c("ANO_ELEICAO", 
                                        "CARGO",
                                        "UF",
                                        "SIGLA_PARTIDO",
                                        "Total de votos conquistados"))


### Deputado Estadual


de1 <- left_join(est,det, by = c("ANO_ELEICAO", 
                                 "CARGO",
                                 "UF",
                                 "SIGLA_PARTIDO"))

### Senador (Brasil)


sen_uft1 <- sen_uft %>% 
  dplyr::group_by(ANO_ELEICAO, SIGLA_PARTIDO) %>% 
  summarise(
    "Total de votos conquistados" = sum(QTDE_VOTOS)
  )

sen_uf2 <- sen_uf1 %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(
    "Votos válidos" = sum(QT_VOTOS_NOMINAIS)
  )


sen_uft1$ANO_ELEICAO <- as.character(sen_uft1$ANO_ELEICAO)

sen_uf2$ANO_ELEICAO <- as.character(sen_uf2$ANO_ELEICAO)

sen_br <- left_join(sen_br, sen_uft1, by = c("ANO_ELEICAO", 
                                          "SIGLA_PARTIDO"))

sen_br <- left_join(sen_br, sen_uf2, by = c("ANO_ELEICAO"))


## Calcula o percentual de votos conquistados por cada partido

### Deputado Federal (Brasil)

df1_br$`Percentual de votos conquistados` <- df1_br$`Total de votos conquistados`/df1_br$VOTOS_VALIDOS

### Deputado Federal (UF)

df1_uf$`Percentual de votos conquistados` <- df1_uf$`Total de votos conquistados`/df1_uf$VOTOS_VALIDOS_UF

### Deputado Estadual

de1$`Percentual de votos conquistados` <- de1$VOT_PART_UF/de1$VOTOS_VALIDOS_UF


### Senador (Brasil)

sen_br$`Percentual de votos conquistados` <- sen_br$`Total de votos conquistados`/sen_br$`Votos válidos`


## Percentual de cadeiras conquistas pelos partidos

### Deputado Federal (Brasil)

df1_br$`Percentual de cadeiras conquistadas` <- df1_br$`Total de cadeiras conquistadas`/513

### Deputado Federal (UF)

df1_uf$`Percentual de cadeiras conquistadas` <- df1_uf$`Total de cadeiras conquistadas`/df1_uf$VAGAS

### Deputado Estadual

de1$`Percentual de cadeiras conquistadas` <- de1$`Cadeiras conquistadas por UF`/de1$VAGAS

### Senador (Brasil)

sen_br$`Percentual de cadeiras conquistadas` <- sen_br$`Total de cadeiras conquistadas`/sen_br$Vagas

## Elimina colunas desnecessarias, renomeia as colunas restantes e padroniza-as

### Deputado Federal (Brasil)

df1_br <- df1_br %>% 
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

df1_br$Cargo <- str_to_title(df1_br$Cargo)

df1_br[is.na(df1_br)] <- 0

### Deputado Federal (UF)

df1_uf <- df1_uf %>% 
  dplyr::select(ANO_ELEICAO,
                UF,
                CARGO,
                VAGAS,
                VOTOS_VALIDOS_UF,
                SIGLA_PARTIDO,
                `Total de votos conquistados`,
                `Total de cadeiras conquistadas`,
                `Percentual de votos conquistados`,
                `Percentual de cadeiras conquistadas`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "CARGO",
                "Vagas" = "VAGAS",
                "Votos válidos" = "VOTOS_VALIDOS_UF", 
                "Sigla do partido" = "SIGLA_PARTIDO")

df1_uf$Cargo <- str_to_title(df1_uf$Cargo)

df1_uf[is.na(df1_uf)] <- 0


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

de1[is.na(de1)] <- 0


### Senador (Brasil)

sen_br <- sen_br %>% 
  dplyr::select(ANO_ELEICAO,
                DESCRICAO_CARGO,
                Vagas,
                `Votos válidos`,
                SIGLA_PARTIDO,
                `Total de votos conquistados`,
                `Total de cadeiras conquistadas`,
                `Percentual de votos conquistados`,
                `Percentual de cadeiras conquistadas`) %>% 
  dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "DESCRICAO_CARGO",
                "Sigla do partido" = "SIGLA_PARTIDO")

sen_br$Cargo <- str_to_title(sen_br$Cargo)


## Calcula o numero de partidos parlamentares para cada eleicao

### Deputado Federal (Brasil)


dfr_br <- df1_br %>% 
  group_by(`Ano da eleição`) %>% 
  summarise(
    `Número de partidos parlamentares` = n()
  )

df1_br <- left_join(df1_br,dfr_br, 
                 by = "Ano da eleição")

### Deputado Federal (UF)


dfr_uf <- df1_uf %>% 
  group_by(`Ano da eleição`, UF) %>% 
  summarise(
    `Número de partidos parlamentares` = n()
  )

df1_uf <- left_join(df1_uf,dfr_uf, 
                    by = c("Ano da eleição", "UF"))

### Deputado Estadual


dfr <- de1 %>% 
  group_by(`Ano da eleição`, UF) %>% 
  summarise(
    `Número de partidos parlamentares` = n()
  )

de1 <- left_join(de1,dfr, 
                 by = c("Ano da eleição", "UF"))


### Senador (Brasil)

senr <- sen_br %>% 
  group_by(`Ano da eleição`) %>% 
  summarise(
    `Número de partidos parlamentares` = n()
  )

sen_br <- left_join(sen_br,senr, 
                 by = "Ano da eleição")


rm(sen_uf1,sen_uf2,sen_uft,sen_uft1,senr)


# 4. Calculo dos indicadores ----------------------------------------------

## Calculo dos indicadores de framentacao partidaria

### Deputado Federal (Brasil)

frag_leg_fed_br <- list()


for(ano in sort(unique(df1_br$`Ano da eleição`))){
  cat("Lendo",ano,"\n")
  t <- filter(df1_br,
              `Ano da eleição` == ano)
  NEPV <- NA
  t$`Número efetivo de partidos eleitoral` <- nep(t$`Percentual de votos conquistados`)
  t$`Número efetivo de partidos legislativo` <- nep(t$`Percentual de cadeiras conquistadas`)
  t$`Fracionalização` <- fracio(t$`Percentual de cadeiras conquistadas`)
  t$`Fracionalização máxima` <- fracio_max(513,t$`Número de partidos parlamentares`)
  t$`Fragmentação` <- frag(t$`Fracionalização`, 
                           t$`Fracionalização máxima`)
  t$`Desproporcionalidade` <- desp_gallag(t$`Percentual de votos conquistados`,
                                          t$`Percentual de cadeiras conquistadas`)
  frag_leg_fed_br <- bind_rows(frag_leg_fed_br,t)
}

### Deputado Federal (UF)

frag_leg_fed_uf <- list()


for(ano in sort(unique(df1_uf$`Ano da eleição`))){
  for(uf in sort(unique(df1_uf$UF))){
  cat("Lendo",ano,uf,"\n")
  t <- filter(df1_uf,
              `Ano da eleição` == ano,
              UF == uf)
  NEPV <- NA
  t$`Número efetivo de partidos eleitoral` <- nep(t$`Percentual de votos conquistados`)
  t$`Número efetivo de partidos legislativo` <- nep(t$`Percentual de cadeiras conquistadas`)
  t$`Fracionalização` <- fracio(t$`Percentual de cadeiras conquistadas`)
  t$`Fracionalização máxima` <- fracio_max(t$Vagas,t$`Número de partidos parlamentares`)
  t$`Fragmentação` <- frag(t$`Fracionalização`, 
                           t$`Fracionalização máxima`)
  t$`Desproporcionalidade` <- desp_gallag(t$`Percentual de votos conquistados`,
                                          t$`Percentual de cadeiras conquistadas`)
  frag_leg_fed_uf <- bind_rows(frag_leg_fed_uf,t)
}
}

### Deputado Estadual




frag_leg_est <- list()

for(ano in sort(unique(de1$`Ano da eleição`))){
  for(uf in sort(unique(de1$UF))){
    cat(ano,uf, "\n")
    t <- filter(de1,
                `Ano da eleição` == ano & 
                  UF == uf)
    NEPV <- NA
    t$`Número efetivo de partidos eleitoral` <- nep(t$`Percentual de votos conquistados`)
    t$`Número efetivo de partidos legislativo` <- nep(t$`Percentual de cadeiras conquistadas`)
    t$`Fracionalização` <- fracio(t$`Percentual de cadeiras conquistadas`)
    t$`Fracionalização máxima` <- fracio_max(t$Vagas,t$`Número de partidos parlamentares`)
    t$`Fragmentação` <- frag(t$Fracionalização,
                             t$`Fracionalização máxima`)
    t$`Desproporcionalidade` <- desp_gallag(t$`Percentual de votos conquistados`,
                                            t$`Percentual de cadeiras conquistadas`)
    frag_leg_est <- bind_rows(frag_leg_est,t)
  }
}


### Senador (Brasil)

frag_leg_sen_br <- list()


for(ano in sort(unique(sen_br$`Ano da eleição`))){
  cat("Lendo",ano,"\n")
  t <- filter(sen_br,
              `Ano da eleição` == ano)
  NEPV <- NA
  t$`Número efetivo de partidos eleitoral` <- nep(t$`Percentual de votos conquistados`)
  t$`Número efetivo de partidos legislativo` <- nep(t$`Percentual de cadeiras conquistadas`)
  t$`Fracionalização` <- fracio(t$`Percentual de cadeiras conquistadas`)
  t$`Fracionalização máxima` <- fracio_max(t$Vagas,t$`Número de partidos parlamentares`)
  t$`Fragmentação` <- frag(t$`Fracionalização`, 
                           t$`Fracionalização máxima`)
  t$`Desproporcionalidade` <- desp_gallag(t$`Percentual de votos conquistados`,
                                          t$`Percentual de cadeiras conquistadas`)
  frag_leg_sen_br <- bind_rows(frag_leg_sen_br,t)
}


# 5. Padronizacao dos dados -----------------------------------------------


## Remove as colunas desnecessarias e arredonda em duas casas decimais os indices calculados


### Deputado Federal (Brasil)

options(OutDec= ",")

frag_leg_fed_br <- frag_leg_fed_br %>% 
  select(`Ano da eleição`, 
         Cargo,
         `Votos válidos`,
         `Sigla do partido`,
         `Total de votos conquistados`,
         `Total de cadeiras conquistadas`,
         `Percentual de votos conquistados`,
         `Percentual de cadeiras conquistadas`,
         `Número efetivo de partidos eleitoral`,
         `Número efetivo de partidos legislativo`,
         Fracionalização,
         `Fracionalização máxima`,
         Fragmentação,
         `Desproporcionalidade`)

frag_leg_fed_br$`Ano da eleição` <- as.character(frag_leg_fed_br$`Ano da eleição`)

frag_leg_fed_br$`Número efetivo de partidos eleitoral` <- 
  format(round(frag_leg_fed_br$`Número efetivo de partidos eleitoral`, 
               digits = 2), 
         nsmall = 2)

frag_leg_fed_br$`Número efetivo de partidos legislativo` <- 
  format(round(frag_leg_fed_br$`Número efetivo de partidos legislativo`, 
               digits = 2), 
         nsmall = 2)

frag_leg_fed_br$`Percentual de votos conquistados`<- 
  format(round(frag_leg_fed_br$`Percentual de votos conquistados`, 
               digits = 2),  
         nsmall = 2)

frag_leg_fed_br$`Percentual de cadeiras conquistadas` <- 
  format(round(frag_leg_fed_br$`Percentual de cadeiras conquistadas`, 
               digits = 2),  
         nsmall = 2)

frag_leg_fed_br$Fracionalização <- 
  format(round(frag_leg_fed_br$Fracionalização, 
               digits = 2),  
         nsmall = 2)

frag_leg_fed_br$`Fracionalização máxima` <- 
  format(round(frag_leg_fed_br$`Fracionalização máxima`, 
               digits = 2), 
         nsmall = 2)

frag_leg_fed_br$Fragmentação <- 
  format(round(frag_leg_fed_br$Fragmentação, 
               digits = 2),
         nsmall = 2)

frag_leg_fed_br$`Desproporcionalidade` <- 
  format(round(frag_leg_fed_br$`Desproporcionalidade`, 
               digits = 2), 
         nsmall = 2)

frag_leg_fed_br$`Votos válidos` <- pont_virg(frag_leg_fed_br$`Votos válidos`)

frag_leg_fed_br$`Total de votos conquistados` <- pont_virg(frag_leg_fed_br$`Total de votos conquistados`)

### Deputado Federal (UF)

frag_leg_fed_uf <- frag_leg_fed_uf %>% 
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
         `Número efetivo de partidos eleitoral`,
         `Número efetivo de partidos legislativo`,
         Fracionalização,
         `Fracionalização máxima`,
         Fragmentação,
         `Desproporcionalidade`)

frag_leg_fed_uf$`Número efetivo de partidos eleitoral` <- 
  format(round(frag_leg_fed_uf$`Número efetivo de partidos eleitoral`, 
               digits = 2), 
         nsmall = 2)

frag_leg_fed_uf$`Número efetivo de partidos legislativo` <- 
  format(round(frag_leg_fed_uf$`Número efetivo de partidos legislativo`, 
               digits = 2), 
         nsmall = 2)

frag_leg_fed_uf$`Percentual de votos conquistados`<- 
  format(round(frag_leg_fed_uf$`Percentual de votos conquistados`, 
               digits = 2),  
         nsmall = 2)

frag_leg_fed_uf$`Percentual de cadeiras conquistadas` <- 
  format(round(frag_leg_fed_uf$`Percentual de cadeiras conquistadas`, 
               digits = 2),  
         nsmall = 2)

frag_leg_fed_uf$Fracionalização <- 
  format(round(frag_leg_fed_uf$Fracionalização, 
               digits = 2),  
         nsmall = 2)

frag_leg_fed_uf$`Fracionalização máxima` <- 
  format(round(frag_leg_fed_uf$`Fracionalização máxima`, 
               digits = 2), 
         nsmall = 2)

frag_leg_fed_uf$Fragmentação <- 
  format(round(frag_leg_fed_uf$Fragmentação, 
               digits = 2),
         nsmall = 2)

frag_leg_fed_uf$`Desproporcionalidade` <- 
  format(round(frag_leg_fed_uf$`Desproporcionalidade`, 
               digits = 2), 
         nsmall = 2)

frag_leg_fed_uf$`Votos válidos` <- pont_virg(frag_leg_fed_uf$`Votos válidos`)

frag_leg_fed_uf$`Total de votos conquistados` <- pont_virg(frag_leg_fed_uf$`Total de votos conquistados`)

### Deputado Estadual

frag_leg_est<- frag_leg_est %>% 
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
         `Número efetivo de partidos eleitoral`,
         `Número efetivo de partidos legislativo`,
         Fracionalização,
         `Fracionalização máxima`,
         Fragmentação,
         `Desproporcionalidade`)

frag_leg_est$`Número efetivo de partidos eleitoral` <- 
  format(round(frag_leg_est$`Número efetivo de partidos eleitoral`, 
               digits = 2), 
         nsmall = 2)

frag_leg_est$`Número efetivo de partidos legislativo` <- 
  format(round(frag_leg_est$`Número efetivo de partidos legislativo`, 
               digits = 2), 
         nsmall = 2)

frag_leg_est$`Percentual de votos conquistados`<- 
  format(round(frag_leg_est$`Percentual de votos conquistados`, 
               digits = 2),  
         nsmall = 2)

frag_leg_est$`Percentual de cadeiras conquistadas` <- 
  format(round(frag_leg_est$`Percentual de cadeiras conquistadas`, 
               digits = 2),  
         nsmall = 2)

frag_leg_est$Fracionalização <- 
  format(round(frag_leg_est$Fracionalização, 
               digits = 2),  
         nsmall = 2)

frag_leg_est$`Fracionalização máxima` <- 
  format(round(frag_leg_est$`Fracionalização máxima`, 
               digits = 2), 
         nsmall = 2)

frag_leg_est$Fragmentação <- 
  format(round(frag_leg_est$Fragmentação, 
               digits = 2),
         nsmall = 2)

frag_leg_est$`Desproporcionalidade` <- 
  format(round(frag_leg_est$`Desproporcionalidade`, 
               digits = 2), 
         nsmall = 2)

frag_leg_est$`Votos válidos` <- pont_virg(frag_leg_est$`Votos válidos`)

frag_leg_est$`Total de votos conquistados` <- pont_virg(frag_leg_est$`Total de votos conquistados`)


### Senador (Brasil)


frag_leg_sen_br <- frag_leg_sen_br %>% 
  ungroup() %>% 
  select(`Ano da eleição`,
         Cargo,
         `Votos válidos`,
         `Sigla do partido`,
         `Total de votos conquistados`,
         `Total de cadeiras conquistadas`,
         `Percentual de votos conquistados`,
         `Percentual de cadeiras conquistadas`,
         `Número efetivo de partidos eleitoral`,
         `Número efetivo de partidos legislativo`,
         Fracionalização,
         `Fracionalização máxima`,
         Fragmentação,
         `Desproporcionalidade`) 

frag_leg_sen_br$`Número efetivo de partidos eleitoral` <- 
  format(round(frag_leg_sen_br$`Número efetivo de partidos eleitoral`, 
               digits = 2), 
         nsmall = 2)

frag_leg_sen_br$`Número efetivo de partidos legislativo` <- 
  format(round(frag_leg_sen_br$`Número efetivo de partidos legislativo`, 
               digits = 2), 
         nsmall = 2)

frag_leg_sen_br$`Percentual de votos conquistados`<- 
  format(round(frag_leg_sen_br$`Percentual de votos conquistados`, 
               digits = 2),  
         nsmall = 2)

frag_leg_sen_br$`Percentual de cadeiras conquistadas` <- 
  format(round(frag_leg_sen_br$`Percentual de cadeiras conquistadas`, 
               digits = 2),  
         nsmall = 2)

frag_leg_sen_br$Fracionalização <- 
  format(round(frag_leg_sen_br$Fracionalização, 
               digits = 2),  
         nsmall = 2)

frag_leg_sen_br$`Fracionalização máxima` <- 
  format(round(frag_leg_sen_br$`Fracionalização máxima`, 
               digits = 2), 
         nsmall = 2)

frag_leg_sen_br$Fragmentação <- 
  format(round(frag_leg_sen_br$Fragmentação, 
               digits = 2),
         nsmall = 2)

frag_leg_sen_br$`Desproporcionalidade` <- 
  format(round(frag_leg_sen_br$`Desproporcionalidade`, 
               digits = 2), 
         nsmall = 2)

frag_leg_sen_br$`Votos válidos` <- pont_virg(frag_leg_sen_br$`Votos válidos`)

frag_leg_sen_br$`Total de votos conquistados` <- pont_virg(frag_leg_sen_br$`Total de votos conquistados`)

### Junta os bancos de acordo com seu nivel de agregacao regional


frag_leg_br <- bind_rows(frag_leg_fed_br, frag_leg_sen_br)

frag_leg_br$`Número efetivo de partidos legislativo` <- as.character(frag_leg_br$`Número efetivo de partidos legislativo`)
frag_leg_br$Desproporcionalidade <- as.character(frag_leg_br$Desproporcionalidade)


frag_leg_uf <- bind_rows(frag_leg_fed_uf, frag_leg_est)

frag_leg_uf$`Número efetivo de partidos legislativo` <- as.character(frag_leg_uf$`Número efetivo de partidos legislativo`)
frag_leg_uf$Desproporcionalidade <- as.character(frag_leg_uf$Desproporcionalidade)


# 6. Salva os arquivos ------------------------------------------------------

## Salva os arquivos referentes aos indicadores de fragmentacao
## legislativa em .csv

### Fragmentacao legislativa (Brasil)

write.csv(frag_leg_br, "data/output/frag_leg_br.csv")

### Fragmentacao legislativa (UF)

write.csv(frag_leg_uf, "data/output/frag_leg_uf.csv")


###  Distribuicao de cadeiras (Deputado Federal)

write.csv(vags_fed, "data/output/distcad_fed.csv")

### Distribuicao de cadeiras (Deputado Estadual)

write.csv(vags_est, "data/output/distcad_est.csv")


## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(frag_leg_fed_br,frag_leg_fed_uf,frag_leg_est,frag_leg_sen_br,
   frag_leg_br, frag_leg_uf,dft_br,dft_uf,det,dfc2,dfp_br,dfp_uf,
   dfr_br,dfr_uf,t,est,fed, sen_br, sen_uf,vags_est,vags_fed)



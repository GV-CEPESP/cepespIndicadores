# Pacotes utilizados

library(cepespR)
library(dplyr)
library(tidyverse)
library(abjutils)

# Objetivo
#'        - Calcular os indicadores de volatilidade:
#'        - Volatilidade eleitoral, Volatilidade parlamentar;
#'        - Limpeza e padronizacao dos dados.


# 1. Formula -------------------------------------------------------------


## Formula para o calculo da volatilidade

volat_elet <- function(vt1,vt2) {
  sum(vt1 - (vt2-1))/2 
}


# 2. Calculo dos indicadores ----------------------------------------------------------

## For loop que calcula os indicadores de volatilidade 

anos <- c(1998,2002,2006,2010,2014)

### Deputado Federal (Brasil)


ind_eleicoes_fed_br <- list()


for(ano in anos){
  cat("Lendo",ano,"\n")
  indicadores1 <- filter(df1_br,
                         `Ano da eleição` == ano+4)
  
  ## Filtra nos bancos referentes as estatiscas gerais das
  ## eleicoes, somente os anos que estao sendo 
  ## utilizados no momento  
  estatisticas_ano1 <- filter(df1_br,
                              `Ano da eleição` == ano)
  estatisticas_ano2 <- filter(df1_br,
                              `Ano da eleição` == ano + 4)
  
  ## Calculo dos indicadores de volatilidade 
  indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                      estatisticas_ano2$`Percentual de votos conquistados`)
  indicadores1$`Volatilidade parlamentar` <- volat_elet(estatisticas_ano1$`Percentual de cadeiras conquistadas`,
                                                      estatisticas_ano2$`Percentual de cadeiras conquistadas`)
  ## Empilha todas as eleicoes 
  
  ind_eleicoes_fed_br <- bind_rows(ind_eleicoes_fed_br,indicadores1)
}


ind_eleicoes_fed_br$Cargo <- "Deputado Federal"

### Deputado Federal (UF)

ind_eleicoes_fed_uf <- list()


for(ano in anos){
  for(uf in sort(unique(df1_uf$UF))){
    cat("Lendo",ano,uf,"\n")
  indicadores1 <- filter(df1_uf,
                         `Ano da eleição` == ano+4,
                         UF == uf)
  
  ## Filtra nos bancos referentes as estatiscas gerais das
  ## eleicoes, somente os anos que estao sendo 
  ## utilizados no momento  
  estatisticas_ano1 <- filter(df1_uf,
                              `Ano da eleição` == ano,
                              UF == uf)
  estatisticas_ano2 <- filter(df1_uf,
                              `Ano da eleição` == ano + 4,
                              UF == uf)
  
  ## Calculo dos indicadores de volatilidade 
  indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                      estatisticas_ano2$`Percentual de votos conquistados`)
  indicadores1$`Volatilidade parlamentar` <- volat_elet(estatisticas_ano1$`Percentual de cadeiras conquistadas`,
                                                        estatisticas_ano2$`Percentual de cadeiras conquistadas`)
  ## Empilha todas as eleicoes 
  
  ind_eleicoes_fed_uf <- bind_rows(ind_eleicoes_fed_uf,indicadores1)
  }
}

### Deputado Estadual 

ind_eleicoes_est <- list()


for(ano in anos){
  for(uf in sort(unique(de1$UF))){
    cat("Lendo",ano,uf,"\n")
    indicadores1 <- filter(de1,
                           `Ano da eleição` == ano+4,
                           UF == uf)
    
    ## Filtra nos bancos referentes as estatiscas gerais das
    ## eleicoes, somente os anos que estao sendo 
    ## utilizados no momento  
    estatisticas_ano1 <- filter(de1,
                                `Ano da eleição` == ano,
                                UF == uf)
    estatisticas_ano2 <- filter(de1,
                                `Ano da eleição` == ano + 4,
                                UF == uf)
    
    ## Calculo dos indicadores de volatilidade 
    indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                        estatisticas_ano2$`Percentual de votos conquistados`)
    indicadores1$`Volatilidade parlamentar` <- volat_elet(estatisticas_ano1$`Percentual de cadeiras conquistadas`,
                                                          estatisticas_ano2$`Percentual de cadeiras conquistadas`)
    ## Empilha todas as eleicoes 
    
    ind_eleicoes_est <- bind_rows(ind_eleicoes_est,indicadores1)
  }
}

### Vereador

anos_mun <- c(2000,2004,2008,2012,2016)

ind_eleicoes_vr <- list()


for(ano in anos_mun){
  for(municipio in sort(unique(vr1$`Código do município`))){
    cat("Lendo",ano,municipio,"\n")
    indicadores1 <- filter(vr1,
                           `Ano da eleição` == ano+4,
                           `Código do município` == municipio)
    
    ## Filtra nos bancos referentes as estatiscas gerais das
    ## eleicoes, somente os anos que estao sendo 
    ## utilizados no momento  
    estatisticas_ano1 <- filter(vr1,
                                `Ano da eleição` == ano,
                                `Código do município` == municipio)
    estatisticas_ano2 <- filter(vr1,
                                `Ano da eleição` == ano + 4,
                                `Código do município` == municipio)
    if(nrow(estatisticas_ano1 > 0)){
    ## Calculo dos indicadores de volatilidade 
    indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                        estatisticas_ano2$`Percentual de votos conquistados`)
    indicadores1$`Volatilidade parlamentar` <- volat_elet(estatisticas_ano1$`Percentual de cadeiras conquistadas`,
                                                          estatisticas_ano2$`Percentual de cadeiras conquistadas`)
    ## Empilha todas as eleicoes 
    
    ind_eleicoes_vr <- bind_rows(ind_eleicoes_vr,indicadores1)
    }
  }
}


### Junta os bancos agregados por UF

ind_eleicoes_est <- bind_rows(ind_eleicoes_fed_uf, ind_eleicoes_est)


# 3. Padronizacao dos dados -----------------------------------------------

### Volatilidade (Brasil)

ind_eleicoes_fed_br <- ind_eleicoes_fed_br %>% 
  select(`Ano da eleição`, 
         Cargo,
         `Volatilidade eleitoral`, 
         `Volatilidade parlamentar`
         ) %>% 
  unique()

### Volatilidade (UF)

ind_eleicoes_est <- ind_eleicoes_est %>% 
  select(`Ano da eleição`,
         UF,
         Cargo,
         `Volatilidade eleitoral`, 
         `Volatilidade parlamentar`
  ) %>% 
  unique()

### Municipio

teste <- ind_eleicoes_vr %>% 
  filter(`Volatilidade eleitoral` < 0)

ind_eleicoes_vr <- ind_eleicoes_vr %>% 
  filter(`Volatilidade eleitoral` > 0)

ind_eleicoes_vr <- ind_eleicoes_vr %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Volatilidade eleitoral`, 
         `Volatilidade parlamentar`
  ) %>% 
  unique()

## Padroniza o formato numerico

options(OutDec= ",")

### Volatilidade (Brasil)

ind_eleicoes_fed_br$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes_fed_br$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_fed_br$`Volatilidade parlamentar` <- 
  format(round(ind_eleicoes_fed_br$`Volatilidade parlamentar`, 
               digits = 2),  
         nsmall = 2)

### Volatilidade (UF)

ind_eleicoes_est$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes_est$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_est$`Volatilidade parlamentar` <- 
  format(round(ind_eleicoes_est$`Volatilidade parlamentar`, 
               digits = 2),  
         nsmall = 2)

### Volatilidade (MUN)

ind_eleicoes_vr$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes_vr$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_vr$`Volatilidade parlamentar` <- 
  format(round(ind_eleicoes_vr$`Volatilidade parlamentar`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_vr <- ind_eleicoes_vr %>% 
  arrange(UF)

# 4. Salva os arquivos ----------------------------------------------------

### Volatilidade (Brasil)

write.csv(ind_eleicoes_fed_br, "data/output/vol_br.csv")

### Volatilidade (UF)

write.csv(ind_eleicoes_est, "data/output/vol_uf.csv")

### Volatilidade (MUN)

write.csv(ind_eleicoes_vr, "data/output/vol_mun.csv")

## Remove os arquivos que nao serao mais utilizados

rm(list = ls())


# Pacotes utilizados

library(cepespR)
library(tidyverse)
library(abjutils)

# Objetivos

#'         - Agregar os dados em funcao do numero de habitantes e de eleitores aptos;
#'         - Sao 7 faixas de agregacao em funcao do numero eleitores aptos:
#'         
#'              1. Até 5 mil eleitores;
#'              2. De 5 a 10 mil eleitores;
#'              3. De 10 a 20 mil eleitores;
#'              4. De 20 a 50 mil eleitores;
#'              5. De 50 a 100 mil eleitores;
#'              6. De 100 a 200 mil eleitores;
#'              7. Acima de 200 mil eleitores;
#'              
#'         - Sao 7 faixas de agregacao em funcao do numero de habitantes:
#'         
#'              1. Até 5 mil habitantes (23,39% dos municípios e 2,3% da população total);
#'              2. De 5 a 10 mil habitantes (21,79% dos municípios e 4,49% da população total);
#'              3. De 10 a 20 mil habitantes (25,15% dos municípios e 10,35% da população total);
#'              4. De 20 a 50 mil habitantes (18,74% dos municípios e 16,45% da população total);
#'              5. De 50 a 100 mil habitantes (5,82% dos municípios e 11,67% da população total);
#'              6. De 100 a 500 mil habitantes (4,40% dos municípios e 25,46% da população total);
#'              7. Acima de 500 mil habitantes (0,62% dos municípios e 29,28% da população total);
      



# 1. Data -----------------------------------------------------------------

## Faz o download do banco que contem o numero de eleitores aptos por ano, uf e municipio

vrcm <- get_elections(year = "2000,2004,2008,2012,2016", 
                      position = "Vereador",
                      regional_aggregation = "Municipio", 
                      political_aggregation = "Consolidado",
                      cached = TRUE)

## Carrega os indicadores ja calculados

files <- list.files(file.path(getwd(),"/data/output"), 
                    pattern = "mun.rds")

for(i in files){
  df <- readRDS(paste0("data/output/",i))
  df <- df[,1:length(df)]
  assign(paste(substr(i,1,nchar(i)-4)), df)
  
}

rm(df,i,files)


### Cria um banco com o numero de eleitores aptos por municipio

aptos <- vrcm %>% 
  select("ANO_ELEICAO",
         "UF",
         "COD_MUN_TSE",
         "QTD_APTOS") %>% 
  rename("Ano da eleição" = "ANO_ELEICAO",
         "Código do município" = "COD_MUN_TSE",
         "Eleitores aptos" = "QTD_APTOS")

### Transforma a coluna ano da eleição em caracter

aptos$`Ano da eleição` <- as.character(aptos$`Ano da eleição`)

# 2. Join ------------------------------------------------------


# 2.1. Eleitores aptos ----------------------------------------------------


files <- ls(pattern = "_mun")

for(i in 1:length(ls(pattern = "_mun"))){
  
  cat("Lendo", ls(pattern = "_mun")[i] , "\n")
  df <- left_join(get(ls(pattern = "_mun")[i]), aptos)
  df <- df[,1:length(df)]
  assign(paste(substr(ls(pattern = "_mun")[i],1,nchar(ls(pattern = "_mun")[i]))), df)
  
}


# 3. Cria os intervalos ---------------------------------------------------

options(scipen = 999)

## Cria os intervalos de eleitores aptos

pretty_breaks <- c(0,5000,10000,20000,50000,100000,200000)

## Cria uma variavel com o maior numero de eleitores aptos

max <- max(frag_leg_mun$`Eleitores aptos`)

## Cria as quebras e as legendas 

labels <- c()

brks <- c(pretty_breaks, max)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]


## Cria uma variavel com os valores dos intervalos 

### Fragmentacao legislativa

frag_leg_mun$`Eleitores aptos` <- cut(frag_leg_mun$`Eleitores aptos`, 
                                      breaks = brks, 
                                      include.lowest = TRUE, 
                                      labels = labels)

frag_leg_mun <- frag_leg_mun %>% 
  mutate(`Eleitores aptos` = ifelse(`Eleitores aptos` == 5000,
                       "Até 5 mil eleitores",
                       ifelse(`Eleitores aptos` == 10000,
                              "De 5 a 10 mil eleitores",
                              ifelse(`Eleitores aptos` == 20000,
                                     "De 10 a 20 mil eleitores",
                                     ifelse(`Eleitores aptos` == 50000,
                                            "De 20 a 50 mil eleitores",
                                            ifelse(`Eleitores aptos` == 100000,
                                                   "De 50 a 100 mil eleitores",
                                                   ifelse(`Eleitores aptos` == 200000,
                                                          "De 100 a 200 mil eleitores",
                                                          ifelse(`Eleitores aptos` == 8886195,
                                                                 "Acima de 200 mil eleitores",
                                                                 NA))))))))

### Distribuicao de cadeiras

distcad_mun$`Eleitores aptos` <- cut(distcad_mun$`Eleitores aptos`, 
                                      breaks = brks, 
                                      include.lowest = TRUE, 
                                      labels = labels)

distcad_mun <- distcad_mun %>% 
  mutate(`Eleitores aptos` = ifelse(`Eleitores aptos` == 5000,
                       "Até 5 mil eleitores",
                       ifelse(`Eleitores aptos` == 10000,
                              "De 5 a 10 mil eleitores",
                              ifelse(`Eleitores aptos` == 20000,
                                     "De 10 a 20 mil eleitores",
                                     ifelse(`Eleitores aptos` == 50000,
                                            "De 20 a 50 mil eleitores",
                                            ifelse(`Eleitores aptos` == 100000,
                                                   "De 50 a 100 mil eleitores",
                                                   ifelse(`Eleitores aptos` == 200000,
                                                          "De 100 a 200 mil eleitores",
                                                          ifelse(`Eleitores aptos` == 8886195,
                                                                 "Acima de 200 mil eleitores",
                                                                 NA))))))))

### Renovacao parlamentar

renov_parl_mun$`Eleitores aptos` <- cut(renov_parl_mun$`Eleitores aptos`, 
                                      breaks = brks, 
                                      include.lowest = TRUE, 
                                      labels = labels)

renov_parl_mun <- renov_parl_mun %>% 
  mutate(`Eleitores aptos` = ifelse(`Eleitores aptos` == 5000,
                       "Até 5 mil eleitores",
                       ifelse(`Eleitores aptos` == 10000,
                              "De 5 a 10 mil eleitores",
                              ifelse(`Eleitores aptos` == 20000,
                                     "De 10 a 20 mil eleitores",
                                     ifelse(`Eleitores aptos` == 50000,
                                            "De 20 a 50 mil eleitores",
                                            ifelse(`Eleitores aptos` == 100000,
                                                   "De 50 a 100 mil eleitores",
                                                   ifelse(`Eleitores aptos` == 200000,
                                                          "De 100 a 200 mil eleitores",
                                                          ifelse(`Eleitores aptos` == 8886195,
                                                                 "Acima de 200 mil eleitores",
                                                                 NA))))))))

### Alienacao

alien_mun$`Eleitores aptos` <- cut(alien_mun$`Eleitores aptos`, 
                                      breaks = brks, 
                                      include.lowest = TRUE, 
                                      labels = labels)

alien_mun <- alien_mun %>% 
  mutate(`Eleitores aptos` = ifelse(`Eleitores aptos` == 5000,
                       "Até 5 mil eleitores",
                       ifelse(`Eleitores aptos` == 10000,
                              "De 5 a 10 mil eleitores",
                              ifelse(`Eleitores aptos` == 20000,
                                     "De 10 a 20 mil eleitores",
                                     ifelse(`Eleitores aptos` == 50000,
                                            "De 20 a 50 mil eleitores",
                                            ifelse(`Eleitores aptos` == 100000,
                                                   "De 50 a 100 mil eleitores",
                                                   ifelse(`Eleitores aptos` == 200000,
                                                          "De 100 a 200 mil eleitores",
                                                          ifelse(`Eleitores aptos` == 8886195,
                                                                 "Acima de 200 mil eleitores",
                                                                 NA))))))))

### Volatilidade eleitoral

vol_mun$`Eleitores aptos` <- cut(vol_mun$`Eleitores aptos`, 
                                      breaks = brks, 
                                      include.lowest = TRUE, 
                                      labels = labels)

vol_mun <- vol_mun %>% 
  mutate(`Eleitores aptos` = ifelse(`Eleitores aptos` == 5000,
                       "Até 5 mil eleitores",
                       ifelse(`Eleitores aptos` == 10000,
                              "De 5 a 10 mil eleitores",
                              ifelse(`Eleitores aptos` == 20000,
                                     "De 10 a 20 mil eleitores",
                                     ifelse(`Eleitores aptos` == 50000,
                                            "De 20 a 50 mil eleitores",
                                            ifelse(`Eleitores aptos` == 100000,
                                                   "De 50 a 100 mil eleitores",
                                                   ifelse(`Eleitores aptos` == 200000,
                                                          "De 100 a 200 mil eleitores",
                                                          ifelse(`Eleitores aptos` == 8886195,
                                                                 "Acima de 200 mil eleitores",
                                                                 NA))))))))


# 4. Padroniza os dados ---------------------------------------------------

### Fragmentacao legislativa

frag_leg_mun <- frag_leg_mun %>% 
  dplyr::select(`Ano da eleição`,
                UF, 
                `Código do município`,
                `Nome do município`,
                Cargo,
                `Vagas`,
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
                `Desproporcionalidade`,
                `Eleitores aptos`) %>% 
  dplyr::rename("Cadeiras disponíveis" = "Vagas")


### Distribuicao de cadeiras

distcad_mun <- distcad_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Cadeiras oferecidas`,
         `Votos válidos`,
         `Sigla do partido`,
         `Votos do partido`,
         `Quociente eleitoral`,
         `Quociente partidário`,
         `Eleitores aptos`) %>% 
  rename("Cadeiras disponíveis" = "Cadeiras oferecidas")

### Renovacao parlamentar

renov_parl_mun <- renov_parl_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Cadeiras disponíveis`,
         Reapresentação,
         Reeleitos,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`,
         `Eleitores aptos`
         )

### Alienacao

alien_mun <- alien_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Quantidade de eleitores aptos`,
         `Quantidade de abstenções`,
         `Percentual de abstenções`,
         `Quantidade de votos brancos`,
         `Percentual de votos brancos`,
         `Quantidade de votos nulos`,
         `Percentual de votos nulos`,
         `Alienação absoluta`,
         `Alienação percentual`,
         `Eleitores aptos`)

### Volatilidade eleitoral

vol_mun <- vol_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Volatilidade eleitoral`,
         `Volatilidade parlamentar`,
         `Eleitores aptos`)


# 5. Salva os arquivos ----------------------------------------------------

for (i in 1:length(ls(pattern = "_mun"))) {
  saveRDS(get(ls(pattern = "_mun")[i]),paste0("data/output/",ls(pattern = "_mun")[i],'.rds'),
          compress=TRUE)
}




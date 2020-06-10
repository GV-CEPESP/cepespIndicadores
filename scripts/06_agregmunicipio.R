
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

### Cria um banco com o numero de eleitores aptos por municipio

aptos_mun <- vrcm %>% 
  select("ANO_ELEICAO",
         "UF",
         "COD_MUN_TSE",
         "QTD_APTOS") %>% 
  rename("Ano da eleição" = "ANO_ELEICAO",
         "Código do município" = "COD_MUN_TSE",
         "Eleitores aptos" = "QTD_APTOS")

## Carrega os indicadores ja calculados

files <- list.files(file.path(getwd(),"/data/output"), 
                    pattern = "mun.rds")

for(i in files){
  df <- readRDS(paste0("data/output/",i))
  df <- df[,1:length(df)]
  assign(paste(substr(i,1,nchar(i)-4)), df)
  
}

rm(df,i,files)

                   

# 2. Agrega os dados ------------------------------------------------------


# 2.1. Eleitores aptos ----------------------------------------------------


### Fragmentacao legislativa

aptos_mun$`Ano da eleição` <- as.character(aptos_mun$`Ano da eleição`)

frag_leg_mun <- left_join(frag_leg_mun, aptos_mun)

frag_leg_mun <- frag_leg_mun %>% 
  dplyr::select(`Ano da eleição`,
                  UF, 
                  `Código do município`,
                  `Nome do município`,
                  Cargo,
                  `Vagas`,
                  `Eleitores aptos`,
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
                  `Desproporcionalidade`) %>% 
  dplyr::rename("Cadeiras disponíveis" = "Vagas")

### Distribuicao de cadeiras

distcad_mun <- left_join(distcad_mun, aptos_mun)


distcad_mun <- distcad_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Cadeiras oferecidas`,
         `Eleitores aptos`,
         `Votos válidos`,
         `Sigla do partido`,
         `Votos do partido`,
         `Quociente eleitoral`,
         `Quociente partidário`) %>% 
  rename("Cadeiras disponíveis" = "Cadeiras oferecidas")

### Renovacao parlamentar

renov_parl_mun <- left_join(renov_parl_mun, aptos_mun)


renov_parl_mun <- renov_parl_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Cadeiras disponíveis`,
         `Eleitores aptos`,
         Reapresentação,
         Reeleitos,
         Conservação,
         `Renovação bruta`,
         `Renovação líquida`)

### Alienacao

alien_mun <- left_join(alien_mun, aptos_mun)

alien_mun <- alien_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Eleitores aptos`,
         `Quantidade de eleitores aptos`,
         `Quantidade de abstenções`,
         `Percentual de abstenções`,
         `Quantidade de votos brancos`,
         `Percentual de votos brancos`,
         `Quantidade de votos nulos`,
         `Percentual de votos nulos`,
         `Alienação absoluta`,
         `Alienação percentual`)

### Volatilidade eleitoral e parlamentar

vol_mun <- left_join(vol_mun, aptos_mun)

vol_mun <- vol_mun %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Eleitores aptos`,
         `Volatilidade eleitoral`,
         `Volatilidade parlamentar`)



# 3. Salva os arquivos ----------------------------------------------------

for (i in 1:length(ls())) {
  saveRDS(get(ls()[i]),paste0("data/output/",ls()[i],'.rds'),
          compress=TRUE)
}


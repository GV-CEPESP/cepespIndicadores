# Titulo: Correcao do nome dos municipios
# Autor: Rebeca Carvalho
# Data: 31/05/2020


# Pacotes utilizados

library(tidyverse)


# Objetivos

#'        - Padronizar o nome dos municipios em todos os arquivos;




# 1. Data -----------------------------------------------------------------


frag_leg_mun <- read_csv("data/output/frag_leg_mun.txt")

distcad_mun <- read_csv("data/output/distcad_mun.txt")

renov_parl_mun <- read_csv("data/output/renov_parl_mun.txt")

alien_mun <- read_csv("data/output/alien_mun.csv")

vol_mun <- read_csv("data/output/vol_mun.txt")


# 2. Padronizacao ---------------------------------------------------------

### Municipios

municipios <- alien_mun %>% 
  select(UF, `Código do município`,
         `Nome do município`) %>% 
  arrange(UF, `Nome do município`)

municipios <- unique(municipios)

## Arquivo frag_leg_mun


frag_leg_mun <- frag_leg_mun %>% 
  select(-`Nome do município`)

frag_leg_mun <- left_join(frag_leg_mun,
                          municipios)

frag_leg_mun <- frag_leg_mun %>% 
  select(`Ano da eleição`,
         UF, 
         `Código do município`,
         `Nome do município`,
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
         Desproporcionalidade)

### Arquivo distcad_mun

distcad_mun <- distcad_mun %>% 
  select(-`Nome do município`)

distcad_mun <- left_join(distcad_mun,
                          municipios)

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
         `Quociente partidário`)


### Arquivo renov_parl_mun


renov_parl_mun <- renov_parl_mun %>% 
  select(-`Nome do município`)

renov_parl_mun <- left_join(renov_parl_mun,
                          municipios)

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
        `Renovação líquida`)

### Arquivo vol_mun

vol_mun <- vol_mun %>% 
  select(-`Nome do município`)

vol_mun <- left_join(vol_mun,
                     municipios)

vol_mun <- vol_mun %>% 
  select(`Ano da eleição`,
         UF, 
         `Código do município`,
         `Nome do município`,
         Cargo,
         `Volatilidade eleitoral`,
         `Volatilidade parlamentar`)


# 4.Salva os arquivos -----------------------------------------------------

### Arquivo frag_leg_mun

write.csv(frag_leg_mun, "data/output/frag_leg_mun.csv")

### Arquivo distcad_mun

saveRDS(distcad_mun, "data/output/distcad_mun.rds")

### Arquivo renov_parl_mun

write.csv(renov_parl_mun, "data/output/renov_parl_mun.csv")

### Arquivo vol_mun

saveRDS(vol_mun, "data/output/vol_mun.rds")



## OBJETIVOS

#'        - Calcular os indicadores de Volatilidade:
#'        
#'        - 1. Volatilidade Eleitoral; e
#'        - 2. Volatilidade Parlamentar.

#'        - Limpeza e padronização dos dados gerados.

# 1. Cadeiras e Votos -----------------------------------------------------

## 1.1. Eleições Gerais ----------------------------------------------------

### 1.1.1. Brasil -----------------------------------------------------------

#### 1.1.1.1. Deputado Federal -----------------------------------------------

## Calcula o percentual de votos e cadeiras conquistadas pelos 
## partidos

df_br_cadeiras_conq <- indic_perc_votos_cadeiras(df_uf_cand,
                                                 agregacao = "BR")

#### 1.1.1.2. Deputado Estadual ----------------------------------------------

de_br_cadeiras_conq <- indic_perc_votos_cadeiras(de_uf_cand,
                                                 agregacao = "BR")

### 1.1.2. Estado -----------------------------------------------------------

#### 1.1.2.1. Deputado Federal -----------------------------------------------

df_uf_cadeiras_conq <- indic_perc_votos_cadeiras(df_uf_cand,
                                                 agregacao = "UF")

#### 1.1.2.2. Deputado Estadual ----------------------------------------------

de_uf_cadeiras_conq <- indic_perc_votos_cadeiras(de_uf_cand,
                                                 agregacao = "UF")

## 1.2. Eleições Municipais ------------------------------------------------

### 1.2.1. Município --------------------------------------------------------

#### 1.2.1.1. Prefeito -------------------------------------------------------

pf_mun_cadeiras_conq <- indic_perc_votos_cadeiras(pf_mun_cand,
                                                  agregacao = "PF_MUN")

#### 1.2.1.2. Vereador -------------------------------------------------------

vr_mun_cadeiras_conq <- indic_perc_votos_cadeiras(vr_mun_cand,
                                                  agregacao = "VR_MUN")

# 2. Indicadores ----------------------------------------------------------

## 2.1. Eleições Gerais ----------------------------------------------------

### 2.1.1. Brasil -----------------------------------------------------------

#### 2.1.1.1. Deputado Federal -----------------------------------------------

## Calcula os indicadores de 'Volatilidade'

volat_df_br <- indic_volat(df_br_cadeiras_conq,
                           agregacao = "BR")

#### 2.1.1.2. Deputado Estadual ----------------------------------------------

## Calcula os indicadores de 'Volatilidade'

volat_de_br <- indic_volat(de_br_cadeiras_conq,
                           agregacao = "BR")

### 2.1.2. Estado -----------------------------------------------------------

#### 2.1.2.1. Deputado Federal -----------------------------------------------

## Calcula os indicadores de 'Volatilidade'

volat_df_uf <- indic_volat(df_uf_cadeiras_conq,
                           agregacao = "UF")

#### 2.1.2.2. Deputado Estadual ----------------------------------------------

## Calcula os indicadores de 'Volatilidade'

volat_de_uf <- indic_volat(de_uf_cadeiras_conq,
                           agregacao = "UF")

## 2.2. Eleições Municipais ------------------------------------------------

### 2.2.1. Município --------------------------------------------------------

#### 2.2.1.1. Prefeito -------------------------------------------------------

## Calcula os indicadores de 'Volatilidade'

volat_pf_mun <- indic_volat(pf_mun_cadeiras_conq,
                            agregacao = "PF_MUN")

#### 2.2.1.2. Vereador -------------------------------------------------------

## Calcula os indicadores de 'Volatilidade'

volat_vr_mun <- indic_volat(vr_mun_cadeiras_conq,
                            agregacao = "VR_MUN")

# 3. Padronização ---------------------------------------------------------

## 3.1. Eleições Gerais ----------------------------------------------------

### 3.1.1. Brasil -----------------------------------------------------------

#### 3.1.1.1. Deputado Federal -----------------------------------------------

volat_df_br <- padroniz_volat(volat_df_br,
                              agregacao = "BR")

#### 3.1.1.2. Deputado Estadual ----------------------------------------------

volat_de_br <- padroniz_volat(volat_de_br,
                              agregacao = "BR")

### 3.1.2. Estado -----------------------------------------------------------

#### 3.1.2.1. Deputado Federal -----------------------------------------------

volat_df_uf <- padroniz_volat(volat_df_uf,
                              agregacao = "UF")

#### 3.1.2.2. Deputado Estadual ----------------------------------------------

volat_de_uf <- padroniz_volat(volat_de_uf,
                              agregacao = "UF")

## 3.2. Eleições Municipais ------------------------------------------------

### 3.2.1. Município --------------------------------------------------------

#### 3.2.1.1. Prefeito -------------------------------------------------------

volat_pf_mun <- padroniz_volat(volat_pf_mun,
                               agregacao = "PF_MUN") 

#### 3.2.1.2. Vereador -------------------------------------------------------

volat_vr_mun <- padroniz_volat(volat_vr_mun,
                               agregacao = "VR_MUN")

# 4. Rbind ----------------------------------------------------------------

## Junta os arquivos de 'Volatilidade' em um arquivo único

volatilidade_final <- bind_rows(volat_df_br,
                                volat_de_br,
                                volat_df_uf,
                                volat_de_uf,
                                volat_pf_mun,
                                volat_vr_mun) %>% 
  arrange(`Ano da eleição`,
          Turno,
          `Agregação regional`,
          `Cargo`,
          `UF`,
          `Município`)

# 5. Exporta --------------------------------------------------------------

## Exporta os indicadores de 'Volatilidade' no formato .rds

saveRDS(volatilidade_final, 
        "data/output/volatilidade_final.rds")

# 6. Limpa Área de Trabalho -----------------------------------------------

## Remove da área de trabalho os dados que 
## não serão mais utilizados

rm(list = ls())

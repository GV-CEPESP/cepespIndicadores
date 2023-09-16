
## OBJETIVOS

#'        - Calcular os indicadores de Reeleição:

#'        - 1. Reeleição;
#'        - 2. Reeleição Líquida;
#'        - 3. Renovação;
#'        - 4. Renovação Líquida; e
#'        - 5. Recandidaturas.

#'        - Limpeza e padronização dos dados gerados.

# 1. Candidatos Eleitos ---------------------------------------------------

## Verificando quais candidatos foram eleitos por cargo

## 1.1. Eleições Gerais ----------------------------------------------------

### 1.1.1. Deputado Federal -------------------------------------------------

df_uf_eleitos <- eleitos(df_uf_cand, 
                         agregacao = "UF")

### 1.1.2. Deputado Estadual ------------------------------------------------

de_uf_eleitos <- eleitos(de_uf_cand, 
                         agregacao = "UF")

## 1.2. Eleições Municipais ------------------------------------------------

### 1.2.1. Vereador ---------------------------------------------------------

vr_mun_eleitos <- eleitos(vr_mun_cand, 
                          agregacao = "MUN")

# 2. Indicadores ----------------------------------------------------------

## 2.1. Eleições Gerais ----------------------------------------------------

### 2.1.1. Brasil -----------------------------------------------------------

#### 2.1.1.1. Deputado Federal -----------------------------------------------

## Calcula os indicadores de 'Reeleição'

reel_df_br <- indic_reel(df_uf_cand,
                         df_uf_eleitos,
                         agregacao = "BR")

#### 2.1.1.2. Deputado Estadual ----------------------------------------------

## Calcula os indicadores de 'Reeleição'

reel_de_br <- indic_reel(de_uf_cand,
                         de_uf_eleitos,
                         agregacao = "BR")

### 2.1.2. Estado -----------------------------------------------------------

#### 2.1.2.1. Deputado Federal -----------------------------------------------

reel_df_uf <- indic_reel(df_uf_cand,
                         df_uf_eleitos,
                         agregacao = "UF")

#### 2.1.2.2. Deputado Estadual ----------------------------------------------

reel_de_uf <- indic_reel(de_uf_cand,
                         de_uf_eleitos,
                         agregacao = "UF")

## 2.2. Eleições Municipais -----------------------------------------------

### 2.2.1. Município --------------------------------------------------------

#### 2.2.1.1. Vereador -------------------------------------------------------

reel_vr_mun <- indic_reel(vr_mun_cand,
                          vr_mun_eleitos,
                          agregacao = "MUN")

# 3. Padronização ---------------------------------------------------------

## 3.1. Eleições Gerais ----------------------------------------------------

### 3.1.1. Brasil -----------------------------------------------------------

#### 3.1.1.1. Deputado Federal -----------------------------------------------

reel_df_br <- padroniz_reel(reel_df_br,
                            agregacao = "BR")

#### 3.1.1.2. Deputado Estadual ----------------------------------------------

reel_de_br <- padroniz_reel(reel_de_br,
                            agregacao = "BR")

### 3.1.2. Estado -----------------------------------------------------------

#### 3.1.2.1. Deputado Federal -----------------------------------------------

reel_df_uf <- padroniz_reel(reel_df_uf,
                            agregacao = "UF")

#### 3.1.2.2. Deputado Estadual ----------------------------------------------

reel_de_uf <- padroniz_reel(reel_de_uf,
                            agregacao = "UF")

## 3.2. Eleições Municipais ------------------------------------------------

### 3.2.1. Município --------------------------------------------------------

#### 3.2.1.1. Vereador -------------------------------------------------------

reel_vr_mun <- padroniz_reel(reel_vr_mun,
                             agregacao = "MUN")

# 4. Rbind ----------------------------------------------------------------

## 4.1. Brasil -------------------------------------------------------------

## Junta os bancos de acordo com o nível de agregação regional

reel_br <- bind_rows(reel_df_br, 
                     reel_de_br)

## 4.2. Estado -------------------------------------------------------------

## Junta os bancos de acordo com o nível de agregação regional

reel_uf <- bind_rows(reel_df_uf, 
                     reel_de_uf)

# 5. Exporta --------------------------------------------------------------

## Salva os arquivos referentes aos indicadores de 'Reeleição' em .rds

saveRDS(reel_br, 
        "data/output/reeleicao_br.rds")

saveRDS(reel_uf, 
        "data/output/reeleicao_uf.rds")

saveRDS(reel_vr_mun, 
        "data/output/reeleicao_mun.rds")

## Remove os arquivos que nao serao mais utilizados

rm(estatisticas_ano1,estatisticas_ano2,de_uf_eleitos,df_uf_eleitos, 
   vr_mun_eleitos, vr_mun_cand, pf_mun_cand,
   de_uf_cand,df_uf_cand)


## OBJETIVOS

#'        - Calcular os indicadores de Fragmentação:
        
#'        - 1. Quociente Eleitoral; 
#'        - 2. Quociente Partidário;
#'        - 3. Número Efetivo de Partidos Eleitoral;
#'        - 4. Número Efetivo de Partidos Legislativo;
#'        - 5. Fracionalização;
#'        - 6. Fragmentação Máxima, 
#'        - 7. Fragmentação; e
#'        - 8. Desproporcionalidade.

#'        - Limpeza e padronização dos dados gerados.             

# 1. Distribuição de Cadeiras ---------------------------------------------

## 1.1. Eleições Gerais ----------------------------------------------------

### 1.1.1. Deputado Federal -------------------------------------------------

## Calcula os indicadores de 'Distribuição de Cadeiras"

distcad_df <- indic_disticad(df_uf_cand, 
                             cargo = "DF") 

### 1.1.2. Deputado Estadual ------------------------------------------------

## Calcula os indicadores de 'Distribuição de Cadeiras"

distcad_de <- indic_disticad(de_uf_cand, 
                             cargo = "DE") 

## 1.2. Eleições Municipais ------------------------------------------------

### 1.2.1. Vereador ---------------------------------------------------------

## Calcula os indicadores de 'Distribuição de Cadeiras"

distcad_vr <- indic_disticad(vr_mun_cand, 
                             cargo = "VR") 

# 2. Cadeiras e Votos -----------------------------------------------------

## 2.1. Eleições Gerais ----------------------------------------------------

### 2.1.1. Brasil -----------------------------------------------------------

#### 2.1.1.1. Senador --------------------------------------------------------

## Calcula o percentual de votos e cadeiras conquistadas pelos 
## partidos

sen_br_eleitos <- indic_cadeiras_conq(sen_uf_cand,
                                      agregacao = "BR")

#### 2.1.1.2. Deputado Federal -----------------------------------------------

## Calcula o percentual de votos e cadeiras conquistadas pelos 
## partidos

df_br_eleitos <- indic_cadeiras_conq(df_uf_cand,
                                     agregacao = "BR")

#### 2.1.1.3. Deputado Estadual ----------------------------------------------

de_br_eleitos <- indic_cadeiras_conq(de_uf_cand,
                                     agregacao = "BR")

### 2.1.2. Estado -----------------------------------------------------------

#### 2.1.2.1. Deputado Federal -----------------------------------------------

df_uf_eleitos <- indic_cadeiras_conq(df_uf_cand,
                                     agregacao = "UF")

#### 2.1.2.2. Deputado Estadual ----------------------------------------------

de_uf_eleitos <- indic_cadeiras_conq(de_uf_cand,
                                     agregacao = "UF")

## 2.2. Eleições Municipais ------------------------------------------------

### 2.2.1. Município --------------------------------------------------------

#### 2.2.1.1. Prefeito -------------------------------------------------------

pf_mun_eleitos <- indic_cadeiras_conq(pf_mun_cand,
                                      agregacao = "MUN")

#### 2.2.1.2. Vereador -------------------------------------------------------

vr_mun_eleitos <- indic_cadeiras_conq(vr_mun_cand,
                                      agregacao = "MUN")

# 3. Indicadores ----------------------------------------------------------

## 3.1. Eleições Gerais ----------------------------------------------------

### 3.1.1. Brasil -----------------------------------------------------------

#### 3.1.1.1. Senador --------------------------------------------------------

fragment_sen_br <- indic_frag(sen_br_eleitos,
                              agregacao = "BR")

#### 3.1.1.2. Deputado Federal -----------------------------------------------

fragment_df_br <- indic_frag(df_br_eleitos,
                             agregacao = "BR")

### 3.1.2. Estado -----------------------------------------------------------

#### 3.1.2.1. Deputado Federal -----------------------------------------------

fragment_df_uf <- indic_frag(df_uf_eleitos,
                             agregacao = "UF")

#### 3.1.2.2. Deputado Estadual ----------------------------------------------

fragment_de_uf <- indic_frag(de_uf_eleitos,
                             agregacao = "UF")

## 3.2. Eleições Municipais ------------------------------------------------

### 3.2.1. Município --------------------------------------------------------

#### 3.2.1.1. Prefeito -------------------------------------------------------

## Calcula o indicador 'Número Efetivo de Partidos Eleitoral'

fragment_pf_mun <- indic_frag(pf_mun_eleitos,
                              agregacao = "PF")

#### 3.2.1.2. Vereador -------------------------------------------------------

## Calcula os indicadores de fragmentação em cada ano e município

fragment_vr_mun <- indic_frag(vr_mun_eleitos,
                              agregacao = "VR")
   
# 4. Padronização ---------------------------------------------------------

## 4.1. Eleições Gerais ----------------------------------------------------

### 4.1.1. Brasil -----------------------------------------------------------

#### 4.1.1.1. Senador --------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

fragment_sen_br <- padroniz_frag(fragment_sen_br, 
                                 agregacao = "BR")
  
#### 4.1.1.2. Deputado Federal -----------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

fragment_df_br <- padroniz_frag(fragment_df_br,
                                agregacao = "BR")
  
### 4.1.2. Estado -----------------------------------------------------------

#### 4.1.2.1. Deputado Federal -----------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

distcad_df <- padroniz_distcad(distcad_df,
                               agregacao = "UF")

fragment_df_uf <- padroniz_frag(fragment_df_uf,
                                agregacao = "UF")
 
#### 4.1.2.2. Deputado Estadual ----------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

distcad_de <- padroniz_distcad(distcad_de,
                               agregacao = "UF")

fragment_de_uf <- padroniz_frag(fragment_de_uf,
                                agregacao = "UF")
  
## 4.2. Eleições Municipais ------------------------------------------------

### 4.2.1. Município --------------------------------------------------------

#### 4.2.1.1. Prefeito -------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

fragment_pf_mun <- padroniz_frag(fragment_pf_mun,
                                 agregacao = "PF")

#### 4.2.1.2. Vereador -------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

distcad_vr <- padroniz_distcad(distcad_vr,
                               agregacao = "MUN")

fragment_vr_mun <- padroniz_frag(fragment_vr_mun,
                                 agregacao = "VR")

# 5. Rbind ----------------------------------------------------------------

## 5.1. Brasil -------------------------------------------------------------

## Junta os arquivos de 'Fragmentação' de acordo 
## com o nível de agregação regional

fragment_br <- bind_rows(fragment_sen_br, 
                         fragment_df_br)

## 5.2. Estado -------------------------------------------------------------

## Junta os bancos de acordo com seu nível de agregação regional

fragment_uf <- bind_rows(fragment_df_uf, 
                         fragment_de_uf)

# 6. Exporta --------------------------------------------------------------

## Exporta os indicadores de 'Fragmentação' no formato .rds

## 6.1. Distribuição de Cadeiras -------------------------------------------

saveRDS(distcad_df, 
        "data/output/distribuicao_cadeiras_df.rds")

saveRDS(distcad_de, 
        "data/output/distribuicao_cadeiras_de.rds")

saveRDS(distcad_vr, 
        "data/output/distribuicao_cadeiras_vr.rds")

## 6.2. Fragmentação -------------------------------------------------------

saveRDS(fragment_br, 
        "data/output/fragmentacao_br.rds")

saveRDS(fragment_uf, 
        "data/output/fragmentacao_uf.rds")

saveRDS(fragment_pf_mun, 
        "data/output/fragmentacao_pf.rds")

saveRDS(fragment_vr_mun, 
        "data/output/fragmentacao_vr.rds")

## Remove da área de trabalho os dados que 
## não serão mais utilizados

rm(fragment_sen_br, fragment_df_br, fragment_df_uf,
   fragment_de_uf, fragment_pf_mun, fragment_vr_mun,
   fragment_br, fragment_uf, sen_br_eleitos, df_br_eleitos,
   df_uf_eleitos, de_uf_eleitos, pf_mun_eleitos, vr_mun_eleitos)

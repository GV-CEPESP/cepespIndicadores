
## OBJETIVOS

#'        - Calcular os indicadores de Fragmentação:

#'        - 1. Quociente Eleitoral; e
#'        - 2. Quociente Partidário.

#'        - Limpeza e padronização dos dados gerados.

# 1. Indicadores ----------------------------------------------------------

## 1.1. Eleições Gerais ----------------------------------------------------

### 1.1.1. Brasil -----------------------------------------------------------

#### 1.1.1.1. Deputado Federal -----------------------------------------------

## Calcula os indicadores de 'Distribuição de Cadeiras"

distcad_df_br <- indic_distcad(df_uf_cand, 
                               agregacao = "BR") 

### 1.1.2. UF ---------------------------------------------------------------

#### 1.1.2.1. Deputado Federal -----------------------------------------------

## Calcula os indicadores de 'Distribuição de Cadeiras"

distcad_df_uf <- indic_distcad(df_uf_cand, 
                               agregacao = "UF") 

#### 1.1.2.2. Deputado Estadual ----------------------------------------------

## Calcula os indicadores de 'Distribuição de Cadeiras"

distcad_de_uf <- indic_distcad(de_uf_cand, 
                               agregacao = "UF") 

## 1.2. Eleições Municipais ------------------------------------------------

### 1.2.1. Município --------------------------------------------------------

#### 1.2.1.1. Vereador -------------------------------------------------------

## Calcula os indicadores de 'Distribuição de Cadeiras"

distcad_vr_mun <- indic_distcad(vr_mun_cand, 
                                agregacao = "MUN") 

# 2. Padronização ---------------------------------------------------------

## 2.1. Eleições Gerais ----------------------------------------------------

### 2.1.1. Brasil -----------------------------------------------------------

#### 2.1.1.1. Deputado Federal -----------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

distcad_df_br <- padroniz_quoc(distcad_df_br,
                               agregacao = "BR")

### 2.1.2. UF ---------------------------------------------------------------

#### 2.1.2.1. Deputado Federal -----------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

distcad_df_uf <- padroniz_quoc(distcad_df_uf,
                               agregacao = "UF")

#### 2.1.2.2. Deputado Estadual ----------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

distcad_de_uf <- padroniz_quoc(distcad_de_uf,
                               agregacao = "UF")

## 2.2. Eleições Municipais ------------------------------------------------

### 2.2.1. Município --------------------------------------------------------

#### 2.2.1.1. Vereador -------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

distcad_vr_mun <- padroniz_quoc(distcad_vr_mun,
                                agregacao = "MUN")

# 3. Rbind ----------------------------------------------------------------

## Junta os arquivos de 'Quociente Eleitoral e Partidário' em um arquivo único

quoc_eleitoral_final <- bind_rows(distcad_df_br,
                                             distcad_df_uf,
                                             distcad_de_uf,
                                             distcad_vr_mun) %>% 
  arrange(`Ano da eleição`,
          `Agregação regional`,
          `Cargo`,
          `UF`,
          `Município`,
          `Sigla do partido`)


# 4. Exporta --------------------------------------------------------------

## Exporta os indicadores de 'Fragmentação' no formato .rds

saveRDS(quoc_eleitoral_final, 
        "data/output/quociente_eleitoral_final.rds")

# 5. Limpa Área de Trabalho -----------------------------------------------

## Remove da área de trabalho os dados que 
## não serão mais utilizados

rm(quoc_eleitoral_final, distcad_df_br,
   distcad_df_uf, distcad_de_uf, distcad_vr_mun)

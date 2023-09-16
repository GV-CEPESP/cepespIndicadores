
## OBJETIVOS

#'        - Calcular os indicadores de Participação e Alienação:

#'        - 1. Abstenção Absoluta;
#'        - 2. Abstenção Percentual;
#'        - 3. Votos Nulos Absolutos;
#'        - 4. Votos Nulos Percentuais;
#'        - 5. Votos Brancos Absolutos;
#'        - 6. Votos Brancos Percentuais;
#'        - 7. Alienação Absoluta; e 
#'        - 8. Alienação Percentual.
        
#'        - Limpeza e padronização dos dados gerados.

# 1. Indicadores ----------------------------------------------------------

## 1.1. Eleições Gerais ----------------------------------------------------

### 1.1.1. Brasil -----------------------------------------------------------

## Calcula os indicadores de 'Participação e Alienação'

cons_br <- indic_particip_alien(cons_br)

### 1.1.2. Estado -----------------------------------------------------------

## Calcula os indicadores de 'Participação e Alienação'

cons_uf <- indic_particip_alien(cons_uf)

## 1.2. Eleições Municipais ------------------------------------------------

### 1.2.1. Município --------------------------------------------------------

## Calcula os indicadores de 'Participação e Alienação'

cons_mun <- indic_particip_alien(cons_mun)

# 2. Padronização ---------------------------------------------------------

## 2.1. Eleições Gerais ----------------------------------------------------

### 2.1.1. Brasil -----------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

cons_br <- padroniz_particip_alien(cons_br,
                                   agregacao = "BR")

### 2.1.2. UF ---------------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

cons_uf <- padroniz_particip_alien(cons_uf,
                                   agregacao = "UF")

## 2.2. Eleições Municipais ------------------------------------------------

### 2.2.1. Município --------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

cons_mun <- padroniz_particip_alien(cons_mun,
                                    agregacao = "MUN")

# 3. Exporta --------------------------------------------------------------

## Salva os arquivos referentes aos indicadores de 'Participação e Alienação' 
## em .rds

saveRDS(cons_br, 
        "data/output/participacao_alienacao_br.rds")

saveRDS(cons_uf, 
        "data/output/participacao_alienacao_uf.rds")

saveRDS(cons_mun, 
        "data/output/participacao_alienacao_mun.rds")

## Remove da área de trabalho os dados que não
## serão mais utilizados

rm(cons_br,
   cons_uf,
   cons_mun)

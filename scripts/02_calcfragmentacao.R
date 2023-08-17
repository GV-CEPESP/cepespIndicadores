
## OBJETIVOS

#'        - Calcular os indicadores de Fragmentação:
        
#'        - 1. Número Efetivo de Partidos Eleitoral;
#'        - 2. Número Efetivo de Partidos Legislativo,
#'        - 3. Fracionalização;
#'        - 4. Fragmentação Máxima, 
#'        - 5. Fragmentação; 
#'        - 6. Desproporcionalidade; 
#'        - 7. Quociente Eleitoral; e 
#'        - 8. Quociente Partidário.

#'        - Limpeza e padronização dos dados gerados.             

# 1. Distribuição de Cadeiras ---------------------------------------------

## 1.1. Quociente Eleitoral ------------------------------------------------

### 1.1.1. Eleições Gerais --------------------------------------------------

#### 1.1.1.1. Deputado Federal -----------------------------------------------

distcad_df <- df_uf_cand %>% 
  mutate(QUOCIENTE_ELEITORAL = quoc_eleitoral(QT_VOTOS_VALIDOS,
                                              QT_VAGAS))

#### 1.1.1.2. Deputado Estadual ----------------------------------------------

distcad_de <- de_uf_cand %>% 
  mutate(QUOCIENTE_ELEITORAL = quoc_eleitoral(QT_VOTOS_VALIDOS,
                                              QT_VAGAS))

### 1.1.2. Eleições Municipais ----------------------------------------------

#### 1.1.2.1. Vereador -------------------------------------------------------

distcad_vr <- vr_mun_cand %>% 
  mutate(QUOCIENTE_ELEITORAL = quoc_eleitoral(QT_VOTOS_VALIDOS,
                                              QT_VAGAS))

## 1.2. Quociente Partidário -----------------------------------------------

### 1.2.1. Eleições Gerais --------------------------------------------------

#### 1.2.1.1. Deputado Federal -----------------------------------------------

distcad_df <- distcad_df %>% 
  mutate(QUOCIENTE_PARTIDARIO = quoc_eleitoral(VOT_PART_UF,
                                               QUOCIENTE_ELEITORAL))

#### 1.2.1.2. Deputado Estadual ----------------------------------------------

distcad_de <- distcad_de %>% 
  mutate(QUOCIENTE_PARTIDARIO = quoc_eleitoral(VOT_PART_UF,
                                               QUOCIENTE_ELEITORAL))

### 1.2.2. Eleições Municipais ----------------------------------------------

#### 1.2.2.1. Vereador -------------------------------------------------------

distcad_vr <- distcad_vr %>% 
  mutate(QUOCIENTE_PARTIDARIO = quoc_eleitoral(VOT_PART_MUN,
                                               QUOCIENTE_ELEITORAL))

# 2. Cadeiras e Votos -----------------------------------------------------

## 2.1. Eleições Gerais ----------------------------------------------------

### 2.1.1. Brasil -----------------------------------------------------------

#### 2.1.1.1. Senador --------------------------------------------------------

## Calcula o percentual de votos e cadeiras conquistadas pelos 
## partidos

sen_br_eleitos <- cadeiras_conq_br(sen_uf_cand)

#### 2.1.1.2. Deputado Federal -----------------------------------------------

## Calcula o percentual de votos e cadeiras conquistadas pelos 
## partidos

df_br_eleitos <- cadeiras_conq_br(df_uf_cand)

#### 2.1.1.3. Deputado Estadual ----------------------------------------------

de_br_eleitos <- cadeiras_conq_br(de_uf_cand)

### 2.1.2. Estado -----------------------------------------------------------

#### 2.1.2.1. Deputado Federal -----------------------------------------------

df_uf_eleitos <- cadeiras_conq_uf(df_uf_cand)

#### 2.1.2.2. Deputado Estadual ----------------------------------------------

de_uf_eleitos <- cadeiras_conq_uf(de_uf_cand)

## 2.2. Eleições Municipais ------------------------------------------------

### 2.2.1. Município --------------------------------------------------------

#### 2.2.1.1. Prefeito -------------------------------------------------------

pf_mun_eleitos <- cadeiras_conq_mun(pf_mun_cand)

#### 2.2.1.2. Vereador -------------------------------------------------------

vr_mun_eleitos <- cadeiras_conq_mun(vr_mun_cand)

# 3. Indicadores ----------------------------------------------------------

## 3.1. Eleições Gerais ----------------------------------------------------

### 3.1.1. Brasil -----------------------------------------------------------

#### 3.1.1.1. Senador --------------------------------------------------------

frag_leg_sen_br <- frag_leg_br(sen_br_eleitos)

#### 3.1.1.2. Deputado Federal -----------------------------------------------

frag_leg_df_br <- frag_leg_br(df_br_eleitos)

### 3.1.2. Estado -----------------------------------------------------------

#### 3.1.2.1. Deputado Federal -----------------------------------------------

frag_leg_df_uf <- frag_leg_uf(df_uf_eleitos)

#### 3.1.2.2. Deputado Estadual ----------------------------------------------

frag_leg_de_uf <- frag_leg_uf(de_uf_eleitos)

## 3.2. Eleições Municipais ------------------------------------------------

### 3.2.1. Município --------------------------------------------------------

## Cria uma lista com os códigos dos municípios brasileiros

mun <- unique(pf_mun_eleitos$COD_MUN_TSE)

#### 3.2.1.1. Prefeito -------------------------------------------------------

## Cria uma lista vazia onde os dados serão armazenados

frag_leg_pf_mun <- list()

## For loop que calcula os indicadores de fragmentacao 
## partidaria para cada ano

for(ano in seq(2000, 2020, by = 4)){
  for(municipio in mun){
    
    cat("Lendo", ano, municipio, "\n")
    
    ## Cria um arquivo temporario onde os dados da eleicao 
    ## corrente serao armazenados
    
    temp <- pf_mun_eleitos %>% 
      filter(ANO_ELEICAO == ano &
             COD_MUN_TSE == municipio)
    
    for(turno in unique(temp$NUM_TURNO)){
      
      cat("Turno", turno, "\n")
      
      temp <- temp %>% 
        filter(NUM_TURNO == turno)
      
      ## Calcula o indicador 'Número Efetivo de Partidos Eleitoral'
      
      temp$`Número efetivo de partidos eleitoral` <- num_efetivo_part(temp$PERC_VOTOS)
      
      ## Empilha os indicadores calculados no banco criado
      
      frag_leg_pf_mun <- bind_rows(frag_leg_pf_mun,
                                   temp)
      
    }
  }
}

#### 3.2.1.2. Vereador -------------------------------------------------------

## Cria uma lista vazia onde os dados serão armazenados

frag_leg_vr_mun <- list()

## For loop que calcula os indicadores de fragmentação 
## partidaria para cada ano

for(ano in seq(2000, 2020, by = 4)){
  for(municipio in mun){
    
    cat("Lendo", ano, municipio, "\n")
    
    ## Cria um arquivo temporario onde os dados da eleicao 
    ## corrente serao armazenados
    
    temp <- vr_mun_eleitos %>% 
      filter(ANO_ELEICAO == ano &
             COD_MUN_TSE == municipio)
    
    ## Calcula o indicador 'Número Efetivo de Partidos Eleitoral'
    
    temp$`Número efetivo de partidos eleitoral` <- num_efetivo_part(temp$PERC_VOTOS)
    
    ## Calcula o indicador 'Numero efetivo de partidos legislativo'
    
    temp$`Número efetivo de partidos legislativo` <- num_efetivo_part(temp$PERC_CADEIRAS)
    
    ## Calcula o indicador 'Fracionalização'
    
    temp$`Fracionalização` <- fracionalizacao(temp$PERC_CADEIRAS)
    
    ## Calcula o indicador 'Fracionalização máxima'
    
    temp$`Fracionalização máxima` <- fracionalizacao_max(temp$QT_VAGAS, 
                                                         temp$NUM_PART_PARLAMENT)
    
    ## Calcula o indicador 'Fragmentação'
    
    temp$`Fragmentação` <- fragmentaco(temp$`Fracionalização`, 
                                       temp$`Fracionalização máxima`)
    
    ## Calcula o indicador 'Desproporcionalide'
    
    temp$`Desproporcionalidade` <- desp_gallagher(temp$PERC_VOTOS,
                                                  temp$PERC_CADEIRAS)
    
    ## Empilha os indicadores calculados no banco criado
    
    frag_leg_vr_mun <- bind_rows(frag_leg_vr_mun,
                                 temp)
    
  }
}

# 4. Padronização ---------------------------------------------------------

## 4.1. Eleições Gerais ----------------------------------------------------

### 4.1.1. Brasil -----------------------------------------------------------

#### 4.1.1.1. Senador --------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

frag_leg_sen_br <- padroniz_frag_br(frag_leg_sen_br)
  
#### 4.1.1.2. Deputado Federal -----------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

frag_leg_df_br <- padroniz_frag_br(frag_leg_df_br)
  
### 4.1.2. Estado -----------------------------------------------------------

#### 4.1.2.1. Deputado Federal -----------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

frag_leg_df_uf <- padroniz_frag_uf(frag_leg_df_uf)
 
#### 4.1.2.2. Deputado Estadual ----------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

frag_leg_de_uf <- padroniz_frag_uf(frag_leg_de_uf)
  
## 4.2. Eleições Municipais ------------------------------------------------

### 4.2.1. Município --------------------------------------------------------

#### 4.2.1.1. Prefeito -------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

frag_leg_pf_mun <- frag_leg_pf_mun %>% 
  ungroup() %>% 
  rename("Ano da eleição" = "ANO_ELEICAO",
         "Código do município" = "COD_MUN_TSE",
         "Cargo" = "DESCRICAO_CARGO",
         "Votos válidos" = "QT_VOTOS_VALIDOS",
         "Sigla do partido" = "SIGLA_PARTIDO",
         "Total de votos conquistados" = "VOT_PART_MUN",
         "Percentual de votos conquistados" = "PERC_VOTOS") %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         Cargo,
         `Votos válidos`,
         `Sigla do partido`,
         `Total de votos conquistados`,
         `Percentual de votos conquistados`,
         `Número efetivo de partidos eleitoral`) %>% 
  mutate(`Percentual de votos conquistados` = format(round(`Percentual de votos conquistados`,
                                                           digits = 2),
                                                     nsmall = 2),
         `Número efetivo de partidos eleitoral` = format(round(`Número efetivo de partidos eleitoral`,
                                                               digits = 2),
                                                         nsmall = 2),
         `Votos válidos` = pont_virg(`Votos válidos`),
         `Total de votos conquistados` = pont_virg(`Total de votos conquistados`)) %>% 
  arrange(`Ano da eleição`,
          UF,
          `Código do município`)

#### 4.2.1.2. Vereador -------------------------------------------------------

## Organiza as variáveis e arredonda em duas casas decimais 
## os índices calculados

frag_leg_vr_mun <- padroniz_frag_mun(frag_leg_vr_mun)

# 5. Rbind ----------------------------------------------------------------

## 5.1. Brasil -------------------------------------------------------------

## Junta os bancos de acordo com seu nível de agregação regional

frag_leg_br <- bind_rows(frag_leg_sen_br, 
                         frag_leg_fed_br)

## 5.2. Estado -------------------------------------------------------------

## Junta os bancos de acordo com seu nível de agregação regional

frag_leg_uf <- bind_rows(frag_leg_fed_uf, 
                         frag_leg_est_uf)

# 6. Exporta --------------------------------------------------------------

## Exporta os arquivos no formato .rds

## 6.1. Distribuição de Cadeiras -------------------------------------------

saveRDS(distcad_df, 
        "data/output/distcad_df.rds")

saveRDS(distcad_de, 
        "data/output/distcad_de.rds")

saveRDS(distcad_vr, 
        "data/output/distcad_vr.rds")

## 6.2. Fragmentação -------------------------------------------------------

saveRDS(frag_leg_br, 
        "data/output/frag_leg_br.rds")

saveRDS(frag_leg_uf, 
        "data/output/frag_leg_uf.rds")

saveRDS(frag_leg_vr_mun, 
        "data/output/frag_leg_mun.rds")

saveRDS(frag_leg_pf_mun, 
        "data/output/frag_leg_pf.rds")

## Remove da área de trabalho os bancos que 
## não serão mais utilizados

rm(frag_leg_fed_br,frag_leg_fed_uf,frag_leg_est,frag_leg_sen_br,
   frag_leg_vr,frag_leg_br, frag_leg_uf,sen_br_eleitos,df_br_eleitos,
   df_uf_eleitos, de_uf_eleitos,vr_mun_eleitos,pf_mun_eleitos, df_br_votos, 
   df_uf_votos, df_br_part_parl,df_uf_part_parl, vr_mun_part_parl,
   sen_br_cand)

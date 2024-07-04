
## OBJETIVOS

#'        - Preparar os dados do CepespData para o cálculo dos 
#'          indicadores.

# 1. Import ---------------------------------------------------------------

## Carregando os arquivos pré-processados contendo as vagas por
## cargo e ano da eleição

vagas_sen <- readRDS(paste0(dir,
                            "data/input/vagas_senadores_1998_2022.rds"))

vagas_dep <- readRDS(paste0(dir,
                            "data/input/vagas_deputados_fed_est_1998_2022.rds"))

vagas_ver <- readRDS(paste0(dir,
                            "data/input/vagas_vereadores_2000_2020.rds"))

## 1.1. CEPESP DATA --------------------------------------------------------

### 1.1.1. Resumo da Eleição ------------------------------------------------

## Cria uma lista com os nomes dos arquivos de detalhe_votacao_secao

# arquivos <- list.files(path = "F:/Public/Documents/repositorioTSE/data/output/Detalhe/",
#                        pattern = "detalhe_votacao_secao.*parquet")

#### 1.1.1.1. Eleições Gerais ------------------------------------------------

## Cria uma lista vazia onde os dados serão armazenados

# resumo_gr <- list()
# 
# ## Carrega e empilha os dados já consolidados do CEPESP DATA
# 
# for(arquivo in seq_along(arquivos)){
# 
#   if(grepl(paste0(seq(1998,2022,4),
#                   collapse = "|"),
#            arquivos[arquivo]) == TRUE){
# 
#     cat("Lendo", arquivos[arquivo], "\n")
# 
#     ## Lendo o arquivo
# 
#     temp <- read_parquet(paste0("F:/Public/Documents/repositorioTSE/data/output/Detalhe/",
#                                 arquivos[arquivo]))
# 
#     ## Agregando os dados apenas por UF
# 
#     temp <- temp %>%
#       rename("DESCRICAO_UE" = "NOME_UE") %>%
#       group_by(across(ANO_ELEICAO:NOME_UF)) %>%
#       summarise(across(QTDE_APTOS:QTDE_VOTOS_ANULADOS_APU_SEP,
#                        as.numeric),
#                 across(QTDE_APTOS:QTDE_VOTOS_ANULADOS_APU_SEP,
#                        ~sum(.,
#                             na.rm = TRUE))) %>%
#       unique()
# 
#     ## Empilhando os dados
# 
#     resumo_gr <- bind_rows(resumo_gr,
#                            temp)
# 
#     ## Removendo os dados que não serão mais utilizados
# 
#     rm(temp)
# 
#     ## Liberando a memória do R
# 
#     gc()
# 
#   }
# 
# }
# 
# ## Salvando os dados já consolidados
# 
# saveRDS(resumo_gr,
#         paste0(dir,
#                "data/input/resumo_gr.rds"))

## Carregando os dados já consolidados

resumo_gr <- readRDS(paste0(dir,
                            "data/input/resumo_gr.rds"))

#### 1.1.1.2. Eleições Municipais --------------------------------------------

## Cria uma lista vazia onde os dados serão armazenados

# resumo_mun <- list()
# 
# ## Carrega e empilha os dados já consolidados do CEPESP DATA
# 
# for(arquivo in seq_along(arquivos)){
#   
#   if(grepl(paste0(seq(2000,2020,4), 
#                   collapse = "|"),
#            arquivos[arquivo]) == TRUE){
#     
#     cat("Lendo", arquivos[arquivo], "\n")
#     
#     ## Lendo o arquivo
#     
#     temp <- read_parquet(paste0("F:/Public/Documents/repositorioTSE/data/output/Detalhe/",
#                                 arquivos[arquivo]))
#     
#     ## Agregando os dados apenas por município
#     
#     temp <- temp %>% 
#       rename("DESCRICAO_UE" = "NOME_UE") %>% 
#       group_by(across(ANO_ELEICAO:NOME_MUNICIPIO)) %>% 
#       summarise(across(QTDE_APTOS:QTDE_VOTOS_ANULADOS_APU_SEP,
#                        as.numeric),
#                 across(QTDE_APTOS:QTDE_VOTOS_ANULADOS_APU_SEP,
#                        ~sum(., 
#                             na.rm = TRUE))) %>% 
#       unique()
#     
#     ## Empilhando os dados 
#     
#     resumo_mun <- bind_rows(resumo_mun,
#                             temp)
#     
#     ## Removendo os dados que não serão mais utilizados
#     
#     rm(temp)
#     
#     ## Liberando a memória do R
#     
#     gc()
#     
#   }
#   
# }
# 
# ## Salvando os dados já consolidados 
# 
# saveRDS(resumo_mun,
#         paste0(dir,
#                "data/input/resumo_mun.rds"))

## Carregando os dados já consolidados

resumo_mun <- readRDS(paste0(dir,
                             "data/input/resumo_mun.rds"))

### 1.1.2. Candidatos, Coligações e Votos -----------------------------------

## Cria uma lista com os nomes dos arquivos finais do CEPESP DATA

# arquivos <- list.files(path = "F:/Public/Documents/repositorioTSE/data/output/JoinFinal",
#                        pattern = "votacao_secao_coli_cand.*[.]parquet")

#### 1.1.2.1. Eleições Gerais ------------------------------------------------

## Cria uma lista onde os dados serão armazenados

# final_cepespdata_gr <- list()
# 
# ## Carregando o arquivo único contendo o banco de dados gerado pelo CEPESP DATA
# ## a partir do join dos dados de Candidatos, Coligações e Votos
# 
# for(arquivo in seq_along(arquivos)){
# 
#   if(grepl(paste0(seq(1998,2022,4),
#                   collapse = "|"),
#            arquivos[arquivo]) == TRUE){
# 
#     cat("Lendo", arquivos[arquivo], "\n")
# 
#     ## Lendo o arquivo
# 
#     temp <- read_parquet(paste0("F:/Public/Documents/repositorioTSE/data/output/JoinFinal/",
#                                 arquivos[arquivo]))
# 
#     gc() ## Liberando memória do R
#     
#     gc()
#     
#     cat("Agregando os dados \n")
# 
#     ## Agregando os dados apenas por UF
# 
#     temp <- temp %>%
#       mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS),
#              NUM_FEDERACAO = as.character(NUM_FEDERACAO)) %>%
#       group_by(across(c(ID_CEPESP,
#                         ANO_ELEICAO:NOME_UF,
#                         CODIGO_CARGO:SIT_PREST_CONTAS))) %>%
#       summarise(QTDE_VOTOS = sum(QTDE_VOTOS,
#                                  na.rm = TRUE)) %>%
#       unique()
# 
#     ## Empilhando os dados
# 
#     final_cepespdata_gr <- bind_rows(final_cepespdata_gr,
#                                      temp)
# 
#     ## Removendo os dados que não serão mais utilizados
# 
#     rm(temp)
# 
#     ## Liberando a memória do R
# 
#     gc()
# 
#   }
# 
# }
# 
# ## Salvando os dados já consolidados
# 
# saveRDS(final_cepespdata_gr,
#         paste0(dir,
#                "data/input/final_cepespdata_gr.rds"))

## Carregando os dados já consolidados

final_cepespdata_gr <- readRDS(paste0(dir,
                                      "data/input/final_cepespdata_gr.rds"))

#### 1.1.2.2. Eleições Municipais --------------------------------------------

## Cria uma lista onde os dados serão armazenados

#final_cepespdata_mun <- list()

## Carregando o arquivo único contendo o banco de dados gerado pelo CEPESP DATA
## a partir do join dos dados de Candidatos, Coligações e Votos

# for(arquivo in seq_along(arquivos)){
# 
#   if(grepl(paste0(seq(2000,2020,4),
#                   collapse = "|"),
#            arquivos[arquivo]) == TRUE){
# 
#     cat("Lendo", arquivos[arquivo], "\n")
# 
#     ## Lendo o arquivo
# 
#     temp <- arrow::open_dataset(paste0("F:/Public/Documents/repositorioTSE/data/output/JoinFinal/",
#                                 arquivos[arquivo]))
#     
#     ## Liberando a memória do R
#     
#     gc()
#     gc()
# 
#     ## Agregando os dados apenas por município
#     
#     cat("Agregando os dados \n")
# 
#     temp <- temp %>%
#       mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS),
#              NUM_FEDERACAO = as.character(NUM_FEDERACAO)) %>%
#       group_by(across(c(ID_CEPESP,
#                         ANO_ELEICAO:NOME_MUNICIPIO,
#                         CODIGO_CARGO:SIT_PREST_CONTAS))) %>%
#       summarise(QTDE_VOTOS = sum(QTDE_VOTOS,
#                                  na.rm = TRUE)) %>%
#       unique() %>% 
#       collect()
# 
#     ## Empilhando os dados
# 
#     final_cepespdata_mun <- bind_rows(final_cepespdata_mun,
#                                       temp)
# 
#     ## Removendo os dados que não serão mais utilizados
# 
#     rm(temp)
# 
#     ## Liberando a memória do R
# 
#     gc()
# 
#   }
# 
# }
# 
# ## Salvando os dados já consolidados
# 
# saveRDS(final_cepespdata_mun,
#         paste0(dir,
#                "data/input/final_cepespdata_mun.rds"))

## Carregando os dados já consolidados

final_cepespdata_mun <- readRDS(paste0(dir,
                                       "data/input/final_cepespdata_mun.rds"))

# 2. Clean ----------------------------------------------------------------

## 2.1. CEPESP DATA --------------------------------------------------------

### 2.1.1. Resumo da Eleição ------------------------------------------------

#### 2.1.1.1. Eleições Gerais ------------------------------------------------

## Renomeando as colunas e adequando ao formato adotado
## no CEPESP DATA

resumo_gr <- resumo_gr %>% 
  filter(TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA") %>% 
  rename("SIGLA_UF" = "UF") %>% 
  mutate(QTDE_VOTOS_VALIDOS = QTDE_VOTOS_NOMINAIS + QTDE_VOTOS_LEGENDA - QTDE_VOTOS_ANULADOS_APU_SEP) %>% 
  arrange(SIGLA_UF,
          CODIGO_CARGO)

## Separando os dados em bases por cargo, agregação regional e
## agregação política

### PRESIDENTE ###

pr_br_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "PRESIDENTE") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO) %>% 
  summarise(QTDE_APTOS = sum(QTDE_APTOS,
                             na.rm = TRUE),
            QTDE_COMPARECIMENTO = sum(QTDE_COMPARECIMENTO,
                                      na.rm = TRUE),
            QTDE_ABSTENCOES = sum(QTDE_ABSTENCOES,
                                  na.rm = TRUE),
            QTDE_VOTOS_VALIDOS = sum(QTDE_VOTOS_VALIDOS,
                                     na.rm = TRUE),
            QTDE_VOTOS_NOMINAIS = sum(QTDE_VOTOS_NOMINAIS,
                                      na.rm = TRUE),
            QTDE_VOTOS_LEGENDA = sum(QTDE_VOTOS_LEGENDA,
                                     na.rm = TRUE),
            QTDE_VOTOS_BRANCOS = sum(QTDE_VOTOS_BRANCOS,
                                     na.rm = TRUE),
            QTDE_VOTOS_NULOS = sum(QTDE_VOTOS_NULOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_ANULADOS_APU_SEP = sum(QTDE_VOTOS_ANULADOS_APU_SEP,
                                              na.rm = TRUE))

pr_uf_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "PRESIDENTE")

### SENADOR ###

sen_br_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "SENADOR") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO) %>% 
  summarise(QTDE_APTOS = sum(QTDE_APTOS,
                            na.rm = TRUE),
            QTDE_COMPARECIMENTO = sum(QTDE_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTDE_ABSTENCOES = sum(QTDE_ABSTENCOES,
                                 na.rm = TRUE),
            QTDE_VOTOS_VALIDOS = sum(QTDE_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NOMINAIS = sum(QTDE_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QTDE_VOTOS_LEGENDA = sum(QTDE_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QTDE_VOTOS_BRANCOS = sum(QTDE_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NULOS = sum(QTDE_VOTOS_NULOS,
                                 na.rm = TRUE),
            QTDE_VOTOS_ANULADOS_APU_SEP = sum(QTDE_VOTOS_ANULADOS_APU_SEP,
                                    na.rm = TRUE))

sen_uf_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "SENADOR")

### DEPUTADO FEDERAL ###

df_br_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO) %>% 
  summarise(QTDE_APTOS = sum(QTDE_APTOS,
                            na.rm = TRUE),
            QTDE_COMPARECIMENTO = sum(QTDE_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTDE_ABSTENCOES = sum(QTDE_ABSTENCOES,
                                 na.rm = TRUE),
            QTDE_VOTOS_VALIDOS = sum(QTDE_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NOMINAIS = sum(QTDE_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QTDE_VOTOS_LEGENDA = sum(QTDE_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QTDE_VOTOS_BRANCOS = sum(QTDE_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NULOS = sum(QTDE_VOTOS_NULOS,
                                 na.rm = TRUE),
            QTDE_VOTOS_ANULADOS_APU_SEP = sum(QTDE_VOTOS_ANULADOS_APU_SEP,
                                    na.rm = TRUE))

df_uf_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL")

### GOVERNADOR ###

gov_br_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "GOVERNADOR") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO) %>% 
  summarise(QTDE_APTOS = sum(QTDE_APTOS,
                            na.rm = TRUE),
            QTDE_COMPARECIMENTO = sum(QTDE_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTDE_ABSTENCOES = sum(QTDE_ABSTENCOES,
                                 na.rm = TRUE),
            QTDE_VOTOS_VALIDOS = sum(QTDE_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NOMINAIS = sum(QTDE_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QTDE_VOTOS_LEGENDA = sum(QTDE_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QTDE_VOTOS_BRANCOS = sum(QTDE_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NULOS = sum(QTDE_VOTOS_NULOS,
                                 na.rm = TRUE),
            QTDE_VOTOS_ANULADOS_APU_SEP = sum(QTDE_VOTOS_ANULADOS_APU_SEP,
                                    na.rm = TRUE))

gov_uf_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "GOVERNADOR")

### DEPUTADO ESTADUAL ###

de_br_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO %in% c("DEPUTADO ESTADUAL",
                                "DEPUTADO DISTRITAL")) %>% 
  mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                  "DEPUTADO ESTADUAL",
                                  DESCRICAO_CARGO),
         CODIGO_CARGO = ifelse(CODIGO_CARGO == 8,
                               7,
                               CODIGO_CARGO)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO) %>% 
  summarise(QTDE_APTOS = sum(QTDE_APTOS,
                            na.rm = TRUE),
            QTDE_COMPARECIMENTO = sum(QTDE_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTDE_ABSTENCOES = sum(QTDE_ABSTENCOES,
                                 na.rm = TRUE),
            QTDE_VOTOS_VALIDOS = sum(QTDE_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NOMINAIS = sum(QTDE_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QTDE_VOTOS_LEGENDA = sum(QTDE_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QTDE_VOTOS_BRANCOS = sum(QTDE_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NULOS = sum(QTDE_VOTOS_NULOS,
                                 na.rm = TRUE),
            QTDE_VOTOS_ANULADOS_APU_SEP = sum(QTDE_VOTOS_ANULADOS_APU_SEP,
                                    na.rm = TRUE))

de_uf_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO ESTADUAL") 

#### 2.1.1.2. Eleições Municipais --------------------------------------------

## Reorganizando as informações

resumo_mun <- resumo_mun %>% 
  filter(TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA") %>% 
  rename("SIGLA_UF" = "UF") %>% 
  mutate(QTDE_VOTOS_VALIDOS = QTDE_VOTOS_NOMINAIS + QTDE_VOTOS_LEGENDA - QTDE_VOTOS_ANULADOS_APU_SEP,
         AGREG_ELEITORES_APTOS = case_when(QTDE_APTOS <= 5000 ~ "Até 5.000 eleitores",
                                           QTDE_APTOS > 5000 &
                                             QTDE_APTOS <= 10000 ~ "De 5.001 até 10.000 eleitores",
                                           QTDE_APTOS > 10000 &
                                             QTDE_APTOS <= 20000 ~ "De 10.001 até 20.000 eleitores",
                                           QTDE_APTOS > 20000 &
                                             QTDE_APTOS <= 50000 ~ "De 20.001 até 50.000 eleitores",
                                           QTDE_APTOS > 50000 &
                                             QTDE_APTOS <= 100000 ~ "De 50.001 até 100.000 eleitores",
                                           QTDE_APTOS > 100000 &
                                             QTDE_APTOS <= 200000 ~ "De 100.001 até 200.000 eleitores",
                                           QTDE_APTOS > 200000 ~ "Acima de 200.000 eleitores",
                                           T ~ NA_character_)) %>% 
  arrange(SIGLA_UF,
          NOME_MUNICIPIO,
          CODIGO_CARGO)

## Salvando as informações sobre o intervalo de eleitores aptos

eleitores_aptos <- resumo_mun %>% 
  filter(TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA") %>% 
  mutate(AGREG_ELEITORES_APTOS = factor(AGREG_ELEITORES_APTOS,
                                        levels = c("Até 5.000 eleitores",
                                                   "De 5.001 até 10.000 eleitores",
                                                   "De 10.001 até 20.000 eleitores",
                                                   "De 20.001 até 50.000 eleitores",
                                                   "De 50.001 até 100.000 eleitores",
                                                   "De 100.001 até 200.000 eleitores",
                                                   "Acima de 200.000 eleitores"))) %>% 
  ungroup() %>% 
  select(ANO_ELEICAO,
         SIGLA_UF,
         COD_MUN_TSE,
         AGREG_ELEITORES_APTOS) %>% 
  unique()

## Separando os dados em bases por cargo e agregação regional

### PREFEITO ###

pf_mun_cons <- resumo_mun %>% 
  filter(DESCRICAO_CARGO == "PREFEITO") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           SIGLA_UF,
           COD_MUN_TSE,
           COD_MUN_IBGE,
           NOME_MUNICIPIO,
           CODIGO_CARGO,
           DESCRICAO_CARGO) %>% 
  summarise(QTDE_APTOS = sum(QTDE_APTOS,
                            na.rm = TRUE),
            QTDE_COMPARECIMENTO = sum(QTDE_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTDE_ABSTENCOES = sum(QTDE_ABSTENCOES,
                                 na.rm = TRUE),
            QTDE_VOTOS_VALIDOS = sum(QTDE_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NOMINAIS = sum(QTDE_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QTDE_VOTOS_LEGENDA = sum(QTDE_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QTDE_VOTOS_BRANCOS = sum(QTDE_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NULOS = sum(QTDE_VOTOS_NULOS,
                                 na.rm = TRUE),
            QTDE_VOTOS_ANULADOS_APU_SEP = sum(QTDE_VOTOS_ANULADOS_APU_SEP,
                                    na.rm = TRUE))

### VEREADOR ###

vr_mun_cons <- resumo_mun %>% 
  filter(DESCRICAO_CARGO == "VEREADOR") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           SIGLA_UF,
           COD_MUN_TSE,
           COD_MUN_IBGE,
           NOME_MUNICIPIO,
           CODIGO_CARGO,
           DESCRICAO_CARGO) %>% 
  summarise(QTDE_APTOS = sum(QTDE_APTOS,
                            na.rm = TRUE),
            QTDE_COMPARECIMENTO = sum(QTDE_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTDE_ABSTENCOES = sum(QTDE_ABSTENCOES,
                                 na.rm = TRUE),
            QTDE_VOTOS_VALIDOS = sum(QTDE_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NOMINAIS = sum(QTDE_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QTDE_VOTOS_LEGENDA = sum(QTDE_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QTDE_VOTOS_BRANCOS = sum(QTDE_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QTDE_VOTOS_NULOS = sum(QTDE_VOTOS_NULOS,
                                 na.rm = TRUE),
            QTDE_VOTOS_ANULADOS_APU_SEP = sum(QTDE_VOTOS_ANULADOS_APU_SEP,
                                    na.rm = TRUE))

### 2.1.2. Candidatos e Votos -----------------------------------------------

#### 2.1.2.1. Eleições Gerais ------------------------------------------------

## Filtrando apenas os cargos de interesse e organizando
## as informações

final_cepespdata_gr <- final_cepespdata_gr %>% 
  filter(!DESCRICAO_CARGO %in% c("CONSELHEIRO DISTRITAL",
                                 "GOVERNADOR")) %>% 
  filter(TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA") %>% 
  rename("SIGLA_UF" = "UF")

## Separando os dados em bases por cargo, agregação regional e
## agregação política

### SENADOR ###

sen_uf_cand <- final_cepespdata_gr %>% 
  filter(DESCRICAO_CARGO == "SENADOR") %>% 
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97")) %>%
  filter(is.na(NOME_CANDIDATO) &
           nchar(NUMERO_CANDIDATO) == 2 |
           !is.na(NOME_CANDIDATO))

sen_uf_part <- final_cepespdata_gr %>% 
  filter(DESCRICAO_CARGO == "SENADOR") %>% 
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97")) %>% 
  mutate(NUMERO_PARTIDO = ifelse(is.na(NUMERO_PARTIDO) &
                                   NOME_CANDIDATO == "VOTO LEGENDA",
                                 NUMERO_CANDIDATO,
                                 NUMERO_PARTIDO)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           SIGLA_UF,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_UF = sum(QTDE_VOTOS,
                             na.rm = TRUE)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           NUMERO_PARTIDO) %>% 
  mutate(VOT_PART_BR = sum(VOT_PART_UF,
                          na.rm = TRUE))

### DEPUTADO FEDERAL ###

df_uf_cand <- final_cepespdata_gr %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL" & 
         nchar(NUMERO_CANDIDATO) > 2) %>% 
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97")) %>%
  filter(is.na(NOME_CANDIDATO) &
           nchar(NUMERO_CANDIDATO) == 2 |
           !is.na(NOME_CANDIDATO))

df_uf_part <- final_cepespdata_gr %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>% 
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97")) %>% 
  mutate(NUMERO_PARTIDO = ifelse(is.na(NUMERO_PARTIDO) &
                                 NOME_CANDIDATO == "VOTO LEGENDA",
                                 NUMERO_CANDIDATO,
                                 NUMERO_PARTIDO)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           SIGLA_UF,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_UF = sum(QTDE_VOTOS,
                              na.rm = TRUE)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           NUMERO_PARTIDO) %>% 
  mutate(VOT_PART_BR = sum(VOT_PART_UF,
                           na.rm = TRUE))

### DEPUTADO ESTADUAL ###

de_uf_cand <- final_cepespdata_gr %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO ESTADUAL" & 
         nchar(NUMERO_CANDIDATO) > 2) %>% 
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97")) %>%
  filter(is.na(NOME_CANDIDATO) &
           nchar(NUMERO_CANDIDATO) == 2 |
           !is.na(NOME_CANDIDATO))

de_uf_part <- final_cepespdata_gr %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO ESTADUAL") %>% 
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97")) %>% 
  mutate(NUMERO_PARTIDO = ifelse(is.na(NUMERO_PARTIDO) &
                                   NOME_CANDIDATO == "VOTO LEGENDA",
                                 NUMERO_CANDIDATO,
                                 NUMERO_PARTIDO)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           SIGLA_UF,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_UF = sum(QTDE_VOTOS,
                              na.rm = TRUE)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           NUMERO_PARTIDO) %>% 
  mutate(VOT_PART_BR = sum(VOT_PART_UF,
                           na.rm = TRUE))

#### 2.1.2.2. Eleições Municipais --------------------------------------------

## Renomeando as colunas e organizando as informações

final_cepespdata_mun <- final_cepespdata_mun %>% 
  filter(DESCRICAO_CARGO %in% c("PREFEITO",
                                "VEREADOR")) %>% 
  filter(TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA") %>% 
  rename("SIGLA_UF" = "UF")

## Separando os dados em bases por cargo, agregação regional e
## agregação política

### PREFEITO ###

pf_mun_cand <- final_cepespdata_mun %>% 
  filter(DESCRICAO_CARGO == "PREFEITO") %>% 
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97"))

pf_mun_part <- final_cepespdata_mun %>% 
  filter(DESCRICAO_CARGO == "PREFEITO") %>% 
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97")) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           SIGLA_UF,
           COD_MUN_TSE,
           COD_MUN_IBGE,
           NOME_MUNICIPIO,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_MUN = sum(QTDE_VOTOS,
                               na.rm = TRUE))

### VEREADOR ###

vr_mun_cand <- final_cepespdata_mun %>% 
  filter(DESCRICAO_CARGO == "VEREADOR" & 
         nchar(NUMERO_CANDIDATO) > 2) %>%
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97")) %>%
  filter(is.na(NOME_CANDIDATO) &
           nchar(NUMERO_CANDIDATO) == 2 |
           !is.na(NOME_CANDIDATO))

vr_mun_part <- final_cepespdata_mun %>% 
  filter(DESCRICAO_CARGO == "VEREADOR") %>% 
  filter(!NUMERO_CANDIDATO %in% c("95","96", "97")) %>% 
  mutate(NUMERO_PARTIDO = ifelse(is.na(NUMERO_PARTIDO) &
                                   NOME_CANDIDATO == "VOTO LEGENDA",
                                 NUMERO_CANDIDATO,
                                 NUMERO_PARTIDO)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           SIGLA_UF,
           COD_MUN_TSE,
           COD_MUN_IBGE,
           NOME_MUNICIPIO,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_MUN = sum(QTDE_VOTOS,
                               na.rm = TRUE))

## Remove os arquivos que não serão mais utilizados

rm(resumo_gr, resumo_mun,
   final_cepespdata_gr, final_cepespdata_mun)

# 3. Join -----------------------------------------------------------------

## 3.1. Partidos -----------------------------------------------------------

### 3.1.1. Eleições Gerais --------------------------------------------------

### SENADOR ###

sen_uf_cand <- left_join(sen_uf_cand,
                         sen_uf_part)

### DEPUTADO FEDERAL ###

df_uf_cand <- left_join(df_uf_cand,
                        df_uf_part)

### DEPUTADO ESTADUAL ###

de_uf_cand <- left_join(de_uf_cand,
                        de_uf_part)

## Remove os arquivos que não serão mais utilizados

rm(sen_uf_part, df_uf_part, de_uf_part)

### 3.1.2. Eleições Municipais ----------------------------------------------

### PREFEITO ###

pf_mun_cand <- left_join(pf_mun_cand,
                         pf_mun_part)

### VEREADOR ###

vr_mun_cand <- left_join(vr_mun_cand,
                         vr_mun_part)

## Remove os arquivos que não serão mais utilizados

rm(pf_mun_part, vr_mun_part)

## 3.2. Vagas --------------------------------------------------------------

### 3.2.1. Eleições Gerais --------------------------------------------------

## Alterando os arquivos de vagas para o formato long

vagas_sen <- vagas_sen %>% 
  rename("SIGLA_UF" = "UF") %>% 
  pivot_longer(cols = `1998`:`2022`, 
               names_to = "ANO_ELEICAO",
               values_to = "QTDE_VAGAS") %>%
  mutate(ANO_ELEICAO = as.character(ANO_ELEICAO),
         CODIGO_CARGO = str_pad(CODIGO_CARGO,
                       width = 2,
                       side = "left",
                       pad = 0)) %>%
  select(ANO_ELEICAO,
         SIGLA_UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QTDE_VAGAS) %>% 
  arrange(ANO_ELEICAO,
          SIGLA_UF)

vagas_dep <- vagas_dep %>% 
  rename("SIGLA_UF" = "UF") %>% 
  pivot_longer(cols = `1998`:`2022`, 
               names_to = "ANO_ELEICAO",
               values_to = "QTDE_VAGAS") %>%
  mutate(ANO_ELEICAO = as.character(ANO_ELEICAO),
         DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                  "DEPUTADO ESTADUAL",
                                  DESCRICAO_CARGO),
         CODIGO_CARGO = ifelse(CODIGO_CARGO == 8,
                               7,
                               CODIGO_CARGO),
         CODIGO_CARGO = str_pad(CODIGO_CARGO,
                                width = 2,
                                side = "left",
                                pad = 0)) %>% 
  select(ANO_ELEICAO,
         SIGLA_UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QTDE_VAGAS) %>% 
  arrange(ANO_ELEICAO,
          SIGLA_UF)

## Realizando o join com o resumo das eleições

### SENADOR ###

sen_uf_cons <- left_join(sen_uf_cons,
                         vagas_sen) %>% 
  select(ANO_ELEICAO,
         NUM_TURNO,
         TIPO_ELEICAO,
         CODIGO_ELEICAO,
         DESCRICAO_ELEICAO,
         DATA_ELEICAO,
         SIGLA_UE,
         DESCRICAO_UE,
         CODIGO_MACRO,
         NOME_MACRO,
         SIGLA_UF,
         NOME_UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QTDE_VAGAS,
         QTDE_APTOS,
         QTDE_COMPARECIMENTO,
         QTDE_ABSTENCOES,
         QTDE_VOTOS_VALIDOS,
         QTDE_VOTOS_NOMINAIS:QTDE_VOTOS_ANULADOS_APU_SEP) %>% 
  unique()

### DEPUTADO FEDERAL ###

df_uf_cons <- left_join(df_uf_cons,
                        vagas_dep) %>% 
  select(ANO_ELEICAO,
         NUM_TURNO,
         TIPO_ELEICAO,
         CODIGO_ELEICAO,
         DESCRICAO_ELEICAO,
         DATA_ELEICAO,
         SIGLA_UE,
         DESCRICAO_UE,
         CODIGO_MACRO,
         NOME_MACRO,
         SIGLA_UF,
         NOME_UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QTDE_VAGAS,
         QTDE_APTOS,
         QTDE_COMPARECIMENTO,
         QTDE_ABSTENCOES,
         QTDE_VOTOS_VALIDOS,
         QTDE_VOTOS_NOMINAIS:QTDE_VOTOS_ANULADOS_APU_SEP) %>% 
  arrange(ANO_ELEICAO,
          SIGLA_UF,
          CODIGO_CARGO) %>% 
  unique()

### DEPUTADO ESTADUAL ###

de_uf_cons <- left_join(de_uf_cons,
                        vagas_dep) %>% 
  select(ANO_ELEICAO,
         NUM_TURNO,
         TIPO_ELEICAO,
         CODIGO_ELEICAO,
         DESCRICAO_ELEICAO,
         DATA_ELEICAO,
         SIGLA_UE,
         DESCRICAO_UE,
         CODIGO_MACRO,
         NOME_MACRO,
         SIGLA_UF,
         NOME_UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QTDE_VAGAS,
         QTDE_APTOS,
         QTDE_COMPARECIMENTO,
         QTDE_ABSTENCOES,
         QTDE_VOTOS_VALIDOS,
         QTDE_VOTOS_NOMINAIS:QTDE_VOTOS_ANULADOS_APU_SEP) %>% 
  arrange(ANO_ELEICAO,
          SIGLA_UF,
          CODIGO_CARGO) %>% 
  unique()

### 3.2.2. Eleições Municipais ----------------------------------------------

## Alterando os arquivos de vagas para o formato long

vagas_ver <- vagas_ver %>% 
  rename("SIGLA_UF" = "UF") %>% 
  pivot_longer(cols = `2000`:`2020`, 
               names_to = "ANO_ELEICAO",
               values_to = "QTDE_VAGAS") %>% 
  mutate(ANO_ELEICAO = as.character(ANO_ELEICAO),
         CODIGO_CARGO = str_pad(CODIGO_CARGO,
                                width = 2,
                                side = "left",
                                pad = 0)) %>%
  select(ANO_ELEICAO,
         SIGLA_UF,
         COD_MUN_TSE,
         COD_MUN_IBGE,
         NOME_MUNICIPIO,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QTDE_VAGAS) %>% 
  arrange(ANO_ELEICAO,
          SIGLA_UF,
          NOME_MUNICIPIO)

## Realizando o join com o resumo das eleições

### VEREADOR ###

vr_mun_cons <- left_join(vr_mun_cons,
                         vagas_ver) %>% 
  ungroup() %>% 
  select(ANO_ELEICAO,
         NUM_TURNO,
         SIGLA_UF,
         COD_MUN_TSE,
         COD_MUN_IBGE,
         NOME_MUNICIPIO,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QTDE_VAGAS,
         QTDE_APTOS,
         QTDE_COMPARECIMENTO,
         QTDE_ABSTENCOES,
         QTDE_VOTOS_VALIDOS,
         QTDE_VOTOS_NOMINAIS:QTDE_VOTOS_ANULADOS_APU_SEP) %>% 
  arrange(ANO_ELEICAO,
          SIGLA_UF,
          NOME_MUNICIPIO,
          CODIGO_CARGO) %>% 
  unique()

## 3.3. Candidatos ---------------------------------------------------------

### 3.3.1. Eleições Gerais --------------------------------------------------

## Realizando o join com os candidatos

### SENADOR ###

sen_uf_cand <- right_join(sen_br_cons,
                          sen_uf_cand) %>% 
  filter(NOME_CANDIDATO != "") %>% 
  rename("QTDE_VOTOS_VALIDOS_BR" = "QTDE_VOTOS_VALIDOS")

### DEPUTADO FEDERAL ###

df_uf_cand <- right_join(df_br_cons,
                         df_uf_cand) %>% 
  filter(NOME_CANDIDATO != "") %>% 
  select(ANO_ELEICAO,
         NUM_TURNO,
         SIGLA_UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QTDE_VOTOS_VALIDOS,
         ID_CEPESP,
         NOME_CANDIDATO:VOT_PART_BR) %>% 
  rename("QTDE_VOTOS_VALIDOS_BR" = "QTDE_VOTOS_VALIDOS")

df_uf_cand <- right_join(df_uf_cons,
                         df_uf_cand) %>% 
  filter(NOME_CANDIDATO != "")

### DEPUTADO ESTADUAL ###

de_uf_cand <- right_join(de_br_cons,
                         de_uf_cand) %>% 
  filter(NOME_CANDIDATO != "") %>% 
  select(ANO_ELEICAO,
         NUM_TURNO,
         SIGLA_UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QTDE_VOTOS_VALIDOS,
         ID_CEPESP,
         NOME_CANDIDATO:VOT_PART_BR) %>% 
  rename("QTDE_VOTOS_VALIDOS_BR" = "QTDE_VOTOS_VALIDOS")

de_uf_cand <- right_join(de_uf_cons,
                         de_uf_cand) %>% 
  filter(NOME_CANDIDATO != "")

### 3.3.2. Eleições Municipais ----------------------------------------------

## Realizando o join com os candidatos

### PREFEITO ###

pf_mun_cand <- right_join(pf_mun_cons,
                          pf_mun_cand) %>% 
  filter(NOME_CANDIDATO != "")

### VEREADOR ###

vr_mun_cand <- right_join(vr_mun_cons,
                          vr_mun_cand) %>% 
  filter(NOME_CANDIDATO != "")

# 4. Consolidate ----------------------------------------------------------

## 4.1. Brasil -------------------------------------------------------------

## Consolida os resumos das eleições com agregação 'BRASIL' em um único
## banco de dados

cons_br <- rbind(pr_br_cons, 
                 gov_br_cons, 
                 sen_br_cons, 
                 df_br_cons, 
                 de_br_cons)

## 4.2. Estado -------------------------------------------------------------

## Consolida os resumo das eleições com agregação 'Estado' em um único
## banco de dados 

cons_uf <- rbind.fill(pr_uf_cons, 
                      gov_uf_cons,
                      sen_uf_cons, 
                      df_uf_cons,
                      de_uf_cons) %>% 
  select(ANO_ELEICAO:DESCRICAO_CARGO,
         QTDE_VAGAS,
         QTDE_APTOS:QTDE_ABSTENCOES,
         QTDE_VOTOS_VALIDOS,
         QTDE_VOTOS_NOMINAIS:QTDE_VOTOS_ANULADOS_APU_SEP) %>% 
  mutate(QTDE_VAGAS = ifelse(is.na(QTDE_VAGAS),
                           1,
                           QTDE_VAGAS))

## 4.3. Município ----------------------------------------------------------

## Consolida os resumo das eleições com agregação 'Município' em um único
## banco de dados 

cons_mun <- rbind.fill(pf_mun_cons, 
                       vr_mun_cons) %>% 
  select(ANO_ELEICAO:DESCRICAO_CARGO,
         QTDE_VAGAS,
         QTDE_APTOS:QTDE_ABSTENCOES,
         QTDE_VOTOS_VALIDOS,
         QTDE_VOTOS_NOMINAIS:QTDE_VOTOS_ANULADOS_APU_SEP) %>% 
  mutate(QTDE_VAGAS = ifelse(is.na(QTDE_VAGAS),
                           1,
                           QTDE_VAGAS))

## Remove da área de trabalho os dados que não serão mais utilizados

rm(de_br_cons, df_br_cons,
   sen_br_cons, sen_uf_cons,
   gov_br_cons, gov_uf_cons, 
   pr_br_cons, pr_uf_cons,
   vagas_sen)

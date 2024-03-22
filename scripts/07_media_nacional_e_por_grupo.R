
## OBJETIVOS

#'        - Calcular a média de todos os indicadores nos seguintes 
#'          níveis de agregação regional:
#'        
#'        - 1. Brasil;
#'        - 2. UF; e 
#'        - 3. Quantidade agregada de eleitores aptos.

#'        - Limpeza e padronização dos dados gerados.

# 1. Data -----------------------------------------------------------------

## Cria uma lista com os nomes dos arquivos finais

arquivos <- list.files(path = "data/output",
                       pattern = "final",
                       full.names = T) %>% 
  set_names(nm = basename(.) %>% 
            tools::file_path_sans_ext())

## Lendo os arquivos e salvando em uma lista de data frames

arquivos <- map(arquivos, 
                readRDS)

## Salva cada data frame da lista em seu próprio objeto 

pmap(.l = list(.x = names(arquivos), .y = arquivos), 
     .f = ~assign(.x, .y, envir = .GlobalEnv))

## Removendo a lista

rm(arquivos)


# 2. Indicadores ----------------------------------------------------------

## 2.1. Brasil -------------------------------------------------------------

### 2.1.1. Quociente Eleitoral e Partidário ---------------------------------

## Calculando a média nacional dos indicadores de 
## 'Quociente Eleitoral e Partidário'

media_quoc_eleitoral_partidario_nacional <- quociente_eleitoral_partidario_final %>% 
  filter(`Agregação regional` %in% c("UF",
                                     "Município")) %>% 
  mutate(Tipo = "Média nacional",
         Turno = 1) %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Cargo,
         Tipo,
         `Quociente eleitoral`,
         `Quociente partidário`) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo) %>% 
  summarise(across(`Quociente eleitoral`:`Quociente partidário`,
                   ~mean(., na.rm = TRUE)),
            `Quociente eleitoral` = round(`Quociente eleitoral`,
                                          0),
            `Quociente eleitoral` = round(`Quociente eleitoral`,
                                          2),
            across(everything(), ~ifelse(is.nan(.),
                                         NA,
                                         .)))

### 2.1.2. Fragmentação -----------------------------------------------------

## Calculando a média nacional dos indicadores de 'Fragmentação'

media_fragmentacao_nacional <- fragmentacao_final %>% 
  filter(`Agregação regional` %in% c("UF",
                                     "Município")) %>% 
  filter(`Número efetivo de partidos eleitoral` < 100) %>% 
  filter(Turno != 2) %>% 
  mutate(Tipo = "Média nacional") %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Cargo,
         Tipo,
         `Número efetivo de partidos eleitoral`:Desproporcionalidade) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo) %>% 
  summarise(across(`Número efetivo de partidos eleitoral`:Desproporcionalidade,
            ~mean(., na.rm = TRUE)),
            across(`Número efetivo de partidos eleitoral`:Desproporcionalidade,
            ~round(.,2)),
            across(everything(), ~ifelse(is.nan(.),
                                         NA,
                                         .)))

### 2.1.3. Reeleição --------------------------------------------------------

## Calculando a média nacional dos indicadores de 'Reeleição'

media_reeleicao_nacional <- reeleicao_final %>% 
  filter(`Agregação regional` %in% c("UF",
                                     "Município")) %>% 
  filter(Recandidaturas != Inf) %>% 
  mutate(Tipo = "Média nacional",
         Turno = 1) %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Cargo,
         Tipo,
         Recandidaturas:`Renovação líquida`) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo) %>% 
  summarise(across(Recandidaturas:`Renovação líquida`,
                   ~mean(., na.rm = TRUE)),
            across(Recandidaturas:`Renovação líquida`,
                   ~round(.,2)))

### 2.1.4. Participação e Alienação -----------------------------------------

## Calculando a média nacional dos indicadores de 'Participação e Alienação'

media_participacao_nacional <- participacao_alienacao_final %>% 
  filter(`Agregação regional` %in% c("UF",
                                     "Município")) %>% 
  filter(Turno != 2) %>% 
  mutate(Tipo = "Média nacional") %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Cargo,
         Tipo,
         `Quantidade de abstenções`:`Alienação percentual`) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo) %>% 
  summarise(across(`Quantidade de abstenções`:`Alienação percentual`,
                   ~mean(., na.rm = TRUE)),
            across(starts_with(c("Percentual",
                               "Alienação perc")),
                   ~round(.,2)),
            across(starts_with(c("Quantidade",
                               "Alienação abs")),
                   ~round(.,0)))

### 2.1.5. Volatilidade -----------------------------------------------------

## Calculando a média nacional dos indicadores de 'Volatilidade'

media_volatilidade_nacional <- volatilidade_final %>% 
  filter(`Agregação regional` %in% c("UF",
                                     "Município")) %>% 
  filter(Turno != 2) %>% 
  mutate(Tipo = "Média nacional") %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Cargo,
         Tipo,
         `Volatilidade eleitoral`:`Volatilidade parlamentar`) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo) %>% 
  summarise(across(`Volatilidade eleitoral`:`Volatilidade parlamentar`,
                   ~mean(., na.rm = TRUE)),
            across(`Volatilidade eleitoral`:`Volatilidade parlamentar`,
                   ~round(.,2)),
            across(everything(), ~ifelse(is.nan(.),
                                         NA,
                                         .)))

## 2.2. Quantidade de eleitores aptos --------------------------------------

### 2.2.1. Quociente Eleitoral e Partidário ---------------------------------

## Calculando a média do grupo dos indicadores de 
## 'Quociente Eleitoral e Partidário'

media_quoc_eleitoral_partidario_eleitoresapt <- quociente_eleitoral_partidario_final %>% 
  filter(`Agregação regional` == "Município") %>% 
  filter(!is.na(`Quantidade agregada de eleitores aptos`)) %>% 
  mutate(Tipo = "Média do grupo",
         Turno = 1,
         `Agregação regional` = "Quantidade de eleitores aptos") %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Cargo,
         Tipo,
         `Quociente eleitoral`,
         `Quociente partidário`,
         `Quantidade agregada de eleitores aptos`) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo,
           `Quantidade agregada de eleitores aptos`) %>% 
  summarise(across(`Quociente eleitoral`:`Quociente partidário`,
                   ~mean(., na.rm = TRUE)),
            `Quociente eleitoral` = round(`Quociente eleitoral`,
                                          0),
            `Quociente eleitoral` = round(`Quociente eleitoral`,
                                          2),
            across(everything(), ~ifelse(is.nan(.),
                                         NA,
                                         .)))

### 2.2.2. Fragmentação -----------------------------------------------------

## Calculando a média do grupo dos indicadores de 'Fragmentação'

media_fragmentacao_eleitoresapt <- fragmentacao_final %>% 
  filter(`Agregação regional` == "Município") %>% 
  filter(`Número efetivo de partidos eleitoral` < 100) %>% 
  filter(!is.na(`Quantidade agregada de eleitores aptos`)) %>% 
  filter(Turno != 2) %>% 
  mutate(Tipo = "Média do grupo",
         `Agregação regional` = "Quantidade de eleitores aptos") %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Cargo,
         Tipo,
         `Quantidade agregada de eleitores aptos`,
         `Número efetivo de partidos eleitoral`:Desproporcionalidade) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo,
           `Quantidade agregada de eleitores aptos`) %>% 
  summarise(across(`Número efetivo de partidos eleitoral`:Desproporcionalidade,
                   ~mean(., na.rm = TRUE)),
            across(`Número efetivo de partidos eleitoral`:Desproporcionalidade,
                   ~round(.,2)),
            across(everything(), ~ifelse(is.nan(.),
                                         NA,
                                         .)))

### 2.2.3. Reeleição --------------------------------------------------------

## Calculando a média do grupo dos indicadores de 'Reeleição'

media_reeleicao_eleitoresapt <- reeleicao_final %>% 
  filter(`Agregação regional` == "Município") %>% 
  filter(Recandidaturas != Inf) %>% 
  filter(!is.na(`Quantidade agregada de eleitores aptos`)) %>% 
  mutate(Tipo = "Média do grupo",
         Turno = 1,
         `Agregação regional` = "Quantidade de eleitores aptos") %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Cargo,
         Tipo,
         `Quantidade agregada de eleitores aptos`,
         Recandidaturas:`Renovação líquida`) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo,
           `Quantidade agregada de eleitores aptos`) %>% 
  summarise(across(Recandidaturas:`Renovação líquida`,
                   ~mean(., na.rm = TRUE)),
            across(Recandidaturas:`Renovação líquida`,
                   ~round(.,2)))

### 2.2.4. Participação e Alienação -----------------------------------------

## Calculando a média do grupo dos indicadores de 'Participação e Alienação'

media_participacao_eleitoresapt <- participacao_alienacao_final %>% 
  filter(`Agregação regional` == "Município") %>% 
  filter(Turno != 2) %>% 
  filter(!is.na(`Quantidade agregada de eleitores aptos`)) %>% 
  mutate(Tipo = "Média do grupo",
         `Agregação regional` = "Quantidade de eleitores aptos") %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Tipo,
         Cargo,
         `Quantidade agregada de eleitores aptos`,
         `Quantidade de abstenções`:`Alienação percentual`) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo,
           `Quantidade agregada de eleitores aptos`) %>% 
  summarise(across(`Quantidade de abstenções`:`Alienação percentual`,
                   ~mean(., na.rm = TRUE)),
            across(starts_with(c("Percentual",
                                 "Alienação perc")),
                   ~round(.,2)),
            across(starts_with(c("Quantidade",
                                 "Alienação abs")),
                   ~round(.,0)))

### 2.2.5. Volatilidade -----------------------------------------------------

## Calculando a média do grupo dos indicadores de 'Volatilidade'

media_volatilidade_eleitoresapt <- volatilidade_final %>% 
  filter(`Agregação regional` == "Município") %>% 
  filter(Turno != 2) %>% 
  filter(!is.na(`Quantidade agregada de eleitores aptos`)) %>% 
  mutate(Tipo = "Média do grupo",
         `Agregação regional` = "Quantidade de eleitores aptos") %>% 
  select(`Ano da eleição`,
         Turno,
         `Agregação regional`,
         Cargo,
         Tipo,
         `Quantidade agregada de eleitores aptos`,
         `Volatilidade eleitoral`:`Volatilidade parlamentar`) %>% 
  group_by(`Ano da eleição`,
           Turno,
           `Agregação regional`,
           Cargo,
           Tipo,
           `Quantidade agregada de eleitores aptos`) %>% 
  summarise(across(`Volatilidade eleitoral`:`Volatilidade parlamentar`,
                   ~mean(., na.rm = TRUE)),
            across(`Volatilidade eleitoral`:`Volatilidade parlamentar`,
                   ~round(.,2)),
            across(everything(), ~ifelse(is.nan(.),
                                         NA,
                                         .)))

# 3. Rbind ----------------------------------------------------------------

## 3.1. Quociente Eleitoral e Partidário -----------------------------------

## Empilhando as médias nacionais e por grupo em arquivos únicos

media_quoc_eleitoral_partidario_final <- bind_rows(media_quoc_eleitoral_partidario_eleitoresapt,
                                                   media_quoc_eleitoral_partidario_nacional) %>% 
  rename("Quantidade de eleitores aptos" = "Quantidade agregada de eleitores aptos") %>% 
  arrange(`Ano da eleição`, 
          Turno,
          `Agregação regional`,
          Cargo,
          Tipo,
          `Quantidade de eleitores aptos`)

## 3.2. Fragmentação -------------------------------------------------------

## Empilhando as médias nacionais e por grupo em arquivos únicos

media_fragmentacao_final <- bind_rows(media_fragmentacao_eleitoresapt,
                                      media_fragmentacao_nacional) %>% 
  rename("Quantidade de eleitores aptos" = "Quantidade agregada de eleitores aptos") %>% 
  arrange(`Ano da eleição`, 
          Turno,
          `Agregação regional`,
          Cargo,
          Tipo,
          `Quantidade de eleitores aptos`)

## 3.3. Reeleição ----------------------------------------------------------

## Empilhando as médias nacionais e por grupo em arquivos únicos

media_reeleicao_final <- bind_rows(media_reeleicao_eleitoresapt,
                                   media_reeleicao_nacional) %>% 
  rename("Quantidade de eleitores aptos" = "Quantidade agregada de eleitores aptos") %>% 
  arrange(`Ano da eleição`, 
          Turno,
          `Agregação regional`,
          Cargo,
          Tipo,
          `Quantidade de eleitores aptos`)

## 3.4. Participação e Alienação -------------------------------------------

## Empilhando as médias nacionais e por grupo em arquivos únicos

media_participacao_final <- bind_rows(media_participacao_eleitoresapt,
                                      media_participacao_nacional) %>% 
  rename("Quantidade de eleitores aptos" = "Quantidade agregada de eleitores aptos") %>% 
  arrange(`Ano da eleição`,
          Turno,
          `Agregação regional`,
          Cargo,
          Tipo,
          `Quantidade de eleitores aptos`)

## 3.5. Volatilidade -------------------------------------------------------

## Empilhando as médias nacionais e por grupo em arquivos únicos

media_volatilidade_final <- bind_rows(media_volatilidade_eleitoresapt,
                                      media_volatilidade_nacional) %>% 
  rename("Quantidade de eleitores aptos" = "Quantidade agregada de eleitores aptos") %>% 
  arrange(`Ano da eleição`, 
          Turno,
          `Agregação regional`,
          Cargo,
          Tipo,
          `Quantidade de eleitores aptos`)

# 4. Exporta --------------------------------------------------------------

## Exporta a média dos indicadores de 'Quociente Eleitoral e Partidário' 
## no formato .rds

saveRDS(media_quoc_eleitoral_partidario_final, 
        "data/output/media_quociente_eleitoral_partidario_final.rds")


## Exporta a média dos indicadores de 'Fragmentação' no formato .rds

saveRDS(media_fragmentacao_final, 
        "data/output/media_fragmentacao_final.rds")

## Exporta a média dos indicadores de 'Reeleição' no formato .rds

saveRDS(media_reeleicao_final, 
        "data/output/media_reeleicao_final.rds")

## Exporta a média dos indicadores de 'Participação e Alienação' no formato .rds

saveRDS(media_participacao_final, 
        "data/output/media_participacao_alienacao_final.rds")

## Exporta a média dos indicadores de 'Volatilidade' no formato .rds

saveRDS(media_volatilidade_final, 
        "data/output/media_volatilidade_final.rds")

# 5. Limpa Área de Trabalho -----------------------------------------------

## Remove da área de trabalho os dados que 
## não serão mais utilizados

rm(list = ls())

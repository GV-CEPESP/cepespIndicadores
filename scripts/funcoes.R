
## OBJETIVOS

#'         - Definir as funções utilizadas para cálculo dos indicadores eleitorais
#'           e padronização do formato dos dados.

# 1. Fórmulas -------------------------------------------------------------

## 1.1. Número Efetivo de Partidos -----------------------------------------

## Função para o cálculo do 'Número Efetivo de Partidos' 

num_efetivo_part <- function(prop_votos_cadeiras){
  
  1 / sum(prop_votos_cadeiras ^ 2)
  
}

## 1.2. Fracionalização ----------------------------------------------------

## Função para o cálculo da 'Fracionalização'

fracionalizacao <- function(perc_cadeiras_part){
  
  1 - (sum(perc_cadeiras_part ^ 2))
  
}

## 1.3. Fracionalização Máxima ---------------------------------------------

## Função para o cálculo da 'Fracionalização Máxima'

fracionalizacao_max <- function(num_cadeiras, num_partidos_repres){
  
  (num_cadeiras * (num_partidos_repres - 1)) / 
    
    (num_partidos_repres * (num_cadeiras - 1))
  
}

## 1.4. Fragmentação -------------------------------------------------------

## Função para o cálculo da 'Fragmentação'

fragmentaco <- function(fracionalizacao, fracionalizacao_max){
  
  fracionalizacao/fracionalizacao_max
  
}

## 1.5. Desproporcionalidade -----------------------------------------------

## Função para o cálculo da 'Desproporcionalidade de Gallagher'

desp_gallagher <- function(perc_votos, perc_cadeiras){
  
  sqrt(sum((perc_votos * 100 - perc_cadeiras * 100) ^ 2, 
           na.rm = TRUE) / 2)
  
}

## 1.6. Quociente Eleitoral ------------------------------------------------

## Função para o cálculo do 'Quociente Eleitoral'

quoc_eleitoral <- function(votos_validos, qt_vagas){
  
  as.numeric(votos_validos) / as.numeric(qt_vagas)
  
}

## 1.7. Quociente Partidário -----------------------------------------------

## Função para o cálculo do 'Quociente Partidário'

quoc_partidario <- function(tot_votos_partido, quoc_eleitoral){
  
  as.numeric(tot_votos_partido) / as.numeric(quoc_eleitoral)
  
}

## 1.8. Cadeiras Conquistadas ----------------------------------------------

## Calcula o percentual de votos e cadeiras conquistadas pelos partidos 
## em diferentes agregações regionais

### 1.8.1. Brasil -----------------------------------------------------------

cadeiras_conq_br <- function(data){
  
  ## Filtra os candidatos que foram eleitos
  
  data <- data %>% 
    filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
           DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA")
  
  ## Soma o total de cadeiras conquistadas pelos partidos em cada eleição e
  ## organiza os dados
  
  data <- data %>% 
    group_by(ANO_ELEICAO,
             DESCRICAO_CARGO,
             QT_VOTOS_VALIDOS,
             SIGLA_PARTIDO) %>% 
    mutate(TOT_CADEIRAS = n()) %>% 
    select(ANO_ELEICAO,
           DESCRICAO_CARGO,
           QT_VOTOS_VALIDOS,
           SIGLA_PARTIDO,
           VOT_PART_BR,
           TOT_CADEIRAS) %>%
    unique() %>% 
    group_by(ANO_ELEICAO) %>% 
    mutate(QT_VAGAS = sum(TOT_CADEIRAS,
                          na.rm = TRUE)) %>% 
    select(ANO_ELEICAO,
           DESCRICAO_CARGO,
           QT_VAGAS,
           QT_VOTOS_VALIDOS,
           SIGLA_PARTIDO,
           VOT_PART_BR,
           TOT_CADEIRAS) %>% 
    unique()
  
  ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e
  ## o número de partidos parlamentares em cada eleição
  
  data <- data %>% 
    mutate(PERC_VOTOS = VOT_PART_BR/QT_VOTOS_VALIDOS,
           PERC_CADEIRAS = TOT_CADEIRAS/QT_VAGAS) %>% 
    group_by(ANO_ELEICAO) %>% 
    mutate(NUM_PART_PARLAMENT = n())
  
}

### 1.8.2. Estado -----------------------------------------------------------

cadeiras_conq_uf <- function(data){
  
  data <- data %>% 
    filter(DESC_SIT_TOT_TURNO == "ELEITO"|
             DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
             DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
             DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA")
  
  ## Soma o total de cadeiras conquistadas pelos partidos em cada ano e uf e
  ## organiza os dados
  
  data <- data %>% 
    group_by(ANO_ELEICAO,
             UF,
             DESCRICAO_CARGO,
             QT_VOTOS_VALIDOS,
             SIGLA_PARTIDO) %>% 
    mutate(TOT_CADEIRAS = n()) %>% 
    select(ANO_ELEICAO,
           UF,
           DESCRICAO_CARGO,
           QT_VOTOS_VALIDOS,
           SIGLA_PARTIDO,
           VOT_PART_UF,
           TOT_CADEIRAS) %>%
    unique() %>% 
    group_by(ANO_ELEICAO,
             UF) %>% 
    mutate(QT_VAGAS = sum(TOT_CADEIRAS,
                          na.rm = TRUE)) %>% 
    select(ANO_ELEICAO,
           UF,
           DESCRICAO_CARGO,
           QT_VAGAS,
           QT_VOTOS_VALIDOS,
           SIGLA_PARTIDO,
           VOT_PART_UF,
           TOT_CADEIRAS) %>% 
    unique()
  
  ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e uf, bem como o
  ## número de partidos parlamentares em cada eleição
  
  data <- data %>% 
    mutate(PERC_VOTOS = VOT_PART_UF/QT_VOTOS_VALIDOS,
           PERC_CADEIRAS = TOT_CADEIRAS/QT_VAGAS) %>% 
    group_by(ANO_ELEICAO,
             UF) %>% 
    mutate(NUM_PART_PARLAMENT = n())
  
}

### 1.8.3. Município --------------------------------------------------------

cadeiras_conq_mun <- function(data){
  
  ## Filtra os candidatos que foram eleitos
  
  data <- data %>% 
    filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
           DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA")
  
  ## Soma o total de cadeiras conquistadas pelos partidos em cada ano e uf e
  ## organiza os dados
  
  data <- data %>% 
    group_by(ANO_ELEICAO,
             NUM_TURNO,
             UF,
             COD_MUN_TSE,
             DESCRICAO_CARGO,
             QT_VOTOS_VALIDOS,
             SIGLA_PARTIDO) %>% 
    mutate(TOT_CADEIRAS = n()) %>% 
    select(ANO_ELEICAO,
           NUM_TURNO,
           UF,
           COD_MUN_TSE,
           DESCRICAO_CARGO,
           QT_VOTOS_VALIDOS,
           SIGLA_PARTIDO,
           VOT_PART_MUN,
           TOT_CADEIRAS) %>%
    unique() %>% 
    group_by(ANO_ELEICAO,
             UF,
             COD_MUN_TSE) %>% 
    mutate(QT_VAGAS = sum(TOT_CADEIRAS,
                          na.rm = TRUE)) %>% 
    select(ANO_ELEICAO,
           NUM_TURNO,
           UF,
           COD_MUN_TSE,
           DESCRICAO_CARGO,
           QT_VAGAS,
           QT_VOTOS_VALIDOS,
           SIGLA_PARTIDO,
           VOT_PART_MUN,
           TOT_CADEIRAS) %>% 
    unique()
  
  ## Calcula o percentual de votos e cadeiras conquistadas por cada partido e uf, bem como o
  ## número de partidos parlamentares em cada eleição
  
  data <- data %>% 
    mutate(PERC_VOTOS = VOT_PART_MUN/QT_VOTOS_VALIDOS,
           PERC_CADEIRAS = TOT_CADEIRAS/QT_VAGAS) %>% 
    group_by(ANO_ELEICAO,
             UF,
             COD_MUN_TSE) %>% 
    mutate(NUM_PART_PARLAMENT = n())
  
}

# 2. Indicadores ----------------------------------------------------------

## 2.1. Fragmentação -------------------------------------------------------

### 2.1.1. Brasil -----------------------------------------------------------

frag_leg_br <- function(data){
  
  ## Cria uma lista vazia onde os dados serão armazenados
  
  frag_leg <- list()
  
  ## For loop que calcula os indicadores de fragmentação 
  ## para cada ano
  
  for(ano in seq(1998, 2022, by = 4)){
    
    cat("Lendo", ano, "\n")
    
    ## Cria um arquivo temporario onde os dados da eleição
    ## corrente serão armazenados
    
    temp <- data %>%
      filter(ANO_ELEICAO == ano)
    
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
    
    frag_leg <- bind_rows(frag_leg,
                          temp)
    
  }
  
  return(frag_leg)
  
}

### 2.1.2. Estado -----------------------------------------------------------

frag_leg_uf <- function(data){
  
  ## Cria uma lista com os estados brasileiros
  
  ufs <- unique(df_uf_eleitos$UF)
  
  ## Cria uma lista vazia onde os dados serão armazenados
  
  frag_leg <- list()
  
  ## For loop que calcula os indicadores de fragmentação 
  ## partidária para cada ano e uf
  
  for(ano in seq(1998, 2022, by = 4)){
    for(uf in ufs){
      
      cat("Lendo", ano, uf, "\n")
      
      ## Cria um arquivo temporario onde os dados da eleição
      ## corrente serão armazenados
      
      temp <- data %>%
        filter(ANO_ELEICAO == ano &
               UF == uf)
      
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
      
      frag_leg <- bind_rows(frag_leg,
                            temp)
      
    }
  }
  
  return(frag_leg)
  
}

# 2. Formatação -----------------------------------------------------------

# 1. Funcao gabi ----------------------------------------------------------


## Funcao para padronizacao do formato numerico dos dados

gabi <- function(string){
  ifelse(string > 1000000, 
         (paste0(floor(string/1000000),".",floor(string/1000)-floor(string/1000000)*1000,
                 ".", substr(floor(string), start = nchar(floor(string))- 2, stop = nchar(floor(string))),
                 ifelse(round(string,2)==floor(string),"",
                        paste0(",",substr(1 + round(string,2)-round(string,0),start = 3, stop = 4))))),
         
         (paste0(floor(string/1000),".", substr(floor(string), start = nchar(floor(string))- 2, stop = 
                                                  nchar(floor(string))),
                 
                 ifelse(round(string,2)==round(string,0),"",
                        paste0(",",substr(1 + round(string,2)-floor(string),start = 3, stop = 4))))))
}



# 2. Funcao 'pont_virg' ---------------------------------------------------


## Funcao que aplica a funcao 'gabi' somente nos casos em que ha 
## necessidade de pontuacao


pont_virg <- function(string){
  ifelse(
    test = string < 1000,
    yes = l <- string,
    no = l <- gabi(string))
}

# 3. Padronização ---------------------------------------------------------

## 3.1. Distribuição de Cadeiras -------------------------------------------

### 3.1.1. Estado -----------------------------------------------------------

padroniz_distcad_uf <- function(data){
  
  data <- data %>% 
    select(ANO_ELEICAO, 
           UF,
           DESCRICAO_CARGO, 
           QT_VAGAS,
           QT_VOTOS_VALIDOS,
           SIGLA_PARTIDO,
           VOT_PART_UF,
           QUOCIENTE_ELEITORAL,
           QUOCIENTE_PARTIDARIO) %>% 
    mutate(QUOCIENTE_ELEITORAL = pont_virg(round(QUOCIENTE_ELEITORAL, 0)),
           QUOCIENTE_PARTIDARIO = round(QUOCIENTE_PARTIDARIO, 2),
           QT_VOTOS_VALIDOS = pont_virg(QT_VOTOS_VALIDOS),
           VOT_PART_UF = pont_virg(VOT_PART_UF),
           DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
    rename("Ano da eleição" = "ANO_ELEICAO",
           "Cargo" = "DESCRICAO_CARGO",
           "Cadeiras oferecidas" = "QT_VAGAS",
           "Votos válidos" = "QT_VOTOS_VALIDOS",
           "Sigla do partido" = "SIGLA_PARTIDO",
           "Votos do partido" = "VOT_PART_UF",
           "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
           "Quociente partidário" = "QUOCIENTE_PARTIDARIO")
  
}

### 3.1.2. Município --------------------------------------------------------

padroniz_distcad_mun <- function(data){
  
  data <- data %>% 
    select(ANO_ELEICAO, 
           UF,
           COD_MUN_TSE,
           DESCRICAO_CARGO, 
           QT_VAGAS,
           QT_VOTOS_VALIDOS,
           SIGLA_PARTIDO,
           VOT_PART_UF,
           QUOCIENTE_ELEITORAL,
           QUOCIENTE_PARTIDARIO) %>% 
    mutate(QUOCIENTE_ELEITORAL = pont_virg(round(QUOCIENTE_ELEITORAL, 0)),
           QUOCIENTE_PARTIDARIO = round(QUOCIENTE_PARTIDARIO, 2),
           QT_VOTOS_VALIDOS = pont_virg(QT_VOTOS_VALIDOS),
           VOT_PART_UF = pont_virg(VOT_PART_UF),
           DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
    rename("Ano da eleição" = "ANO_ELEICAO",
           "Código do município" = "COD_MUN_TSE",
           "Cargo" = "DESCRICAO_CARGO",
           "Cadeiras oferecidas" = "QT_VAGAS",
           "Votos válidos" = "QT_VOTOS_VALIDOS",
           "Sigla do partido" = "SIGLA_PARTIDO",
           "Votos do partido" = "VOT_PART_UF",
           "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
           "Quociente partidário" = "QUOCIENTE_PARTIDARIO")
  
}

# 3.2. Fragmentação -------------------------------------------------------

### 3.2.1. Brasil -----------------------------------------------------------

padroniz_frag_br <- function(data){
  
  data <- data %>% 
    ungroup() %>% 
    rename("Ano da eleição" = "ANO_ELEICAO",
           "Cargo" = "DESCRICAO_CARGO",
           "Votos válidos" = "QT_VOTOS_VALIDOS",
           "Sigla do partido" = "SIGLA_PARTIDO",
           "Total de votos conquistados" = "VOT_PART_BR",
           "Total de cadeiras conquistadas" = "TOT_CADEIRAS",
           "Percentual de votos conquistados" = "PERC_VOTOS",
           "Percentual de cadeiras conquistadas" = "PERC_CADEIRAS") %>% 
    select(`Ano da eleição`,
           Cargo,
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
    mutate(`Percentual de votos conquistados` = format(round(`Percentual de votos conquistados`,
                                                             digits = 2),
                                                       nsmall = 2),
           `Percentual de cadeiras conquistadas` = format(round(`Percentual de cadeiras conquistadas`,
                                                                digits = 2),
                                                          nsmall = 2),
           `Número efetivo de partidos eleitoral` = format(round(`Número efetivo de partidos eleitoral`,
                                                                 digits = 2),
                                                           nsmall = 2),
           `Número efetivo de partidos legislativo` = format(round(`Número efetivo de partidos legislativo`,
                                                                   digits = 2),
                                                             nsmall = 2),
           `Fracionalização` = format(round(`Fracionalização`,
                                            digits = 2),
                                      nsmall = 2),
           `Fracionalização máxima` = format(round(`Fracionalização máxima`,
                                                   digits = 2),
                                             nsmall = 2),
           `Fragmentação` = format(round(`Fragmentação`,
                                         digits = 2),
                                   nsmall = 2),
           `Desproporcionalidade` = format(round(`Desproporcionalidade`,
                                                 digits = 2),
                                           nsmall = 2),
           `Votos válidos` = pont_virg(`Votos válidos`),
           `Total de votos conquistados` = pont_virg(`Total de votos conquistados`))
  
  }

### 3.2.2. Estado -----------------------------------------------------------

padroniz_frag_uf <- function(data){
  
  data <- data %>% 
    ungroup() %>% 
    rename("Ano da eleição" = "ANO_ELEICAO",
           "Cargo" = "DESCRICAO_CARGO",
           "Votos válidos" = "QT_VOTOS_VALIDOS",
           "Sigla do partido" = "SIGLA_PARTIDO",
           "Total de votos conquistados" = "VOT_PART_UF",
           "Total de cadeiras conquistadas" = "TOT_CADEIRAS",
           "Percentual de votos conquistados" = "PERC_VOTOS",
           "Percentual de cadeiras conquistadas" = "PERC_CADEIRAS") %>% 
    select(`Ano da eleição`,
           UF,
           Cargo,
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
    mutate(`Percentual de votos conquistados` = format(round(`Percentual de votos conquistados`,
                                                             digits = 2),
                                                       nsmall = 2),
           `Percentual de cadeiras conquistadas` = format(round(`Percentual de cadeiras conquistadas`,
                                                                digits = 2),
                                                          nsmall = 2),
           `Número efetivo de partidos eleitoral` = format(round(`Número efetivo de partidos eleitoral`,
                                                                 digits = 2),
                                                           nsmall = 2),
           `Número efetivo de partidos legislativo` = format(round(`Número efetivo de partidos legislativo`,
                                                                   digits = 2),
                                                             nsmall = 2),
           `Fracionalização` = format(round(`Fracionalização`,
                                            digits = 2),
                                      nsmall = 2),
           `Fracionalização máxima` = format(round(`Fracionalização máxima`,
                                                   digits = 2),
                                             nsmall = 2),
           `Fragmentação` = format(round(`Fragmentação`,
                                         digits = 2),
                                   nsmall = 2),
           `Desproporcionalidade` = format(round(`Desproporcionalidade`,
                                                 digits = 2),
                                           nsmall = 2),
           `Votos válidos` = pont_virg(`Votos válidos`),
           `Total de votos conquistados` = pont_virg(`Total de votos conquistados`))
  
}

### 3.2.3. Município --------------------------------------------------------

padroniz_frag_mun <- function(data){
  
  data <- data %>% 
    ungroup() %>% 
    rename("Ano da eleição" = "ANO_ELEICAO",
           "Código do município" = "COD_MUN_TSE",
           "Cargo" = "DESCRICAO_CARGO",
           "Votos válidos" = "QT_VOTOS_VALIDOS",
           "Sigla do partido" = "SIGLA_PARTIDO",
           "Total de votos conquistados" = "VOT_PART_MUN",
           "Total de cadeiras conquistadas" = "TOT_CADEIRAS",
           "Percentual de votos conquistados" = "PERC_VOTOS",
           "Percentual de cadeiras conquistadas" = "PERC_CADEIRAS") %>% 
    select(`Ano da eleição`,
           UF,
           `Código do município`,
           Cargo,
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
    mutate(`Percentual de votos conquistados` = format(round(`Percentual de votos conquistados`,
                                                             digits = 2),
                                                       nsmall = 2),
           `Percentual de cadeiras conquistadas` = format(round(`Percentual de cadeiras conquistadas`,
                                                                digits = 2),
                                                          nsmall = 2),
           `Número efetivo de partidos eleitoral` = format(round(`Número efetivo de partidos eleitoral`,
                                                                 digits = 2),
                                                           nsmall = 2),
           `Número efetivo de partidos legislativo` = format(round(`Número efetivo de partidos legislativo`,
                                                                   digits = 2),
                                                             nsmall = 2),
           `Fracionalização` = format(round(`Fracionalização`,
                                            digits = 2),
                                      nsmall = 2),
           `Fracionalização máxima` = format(round(`Fracionalização máxima`,
                                                   digits = 2),
                                             nsmall = 2),
           `Fragmentação` = format(round(`Fragmentação`,
                                         digits = 2),
                                   nsmall = 2),
           `Desproporcionalidade` = format(round(`Desproporcionalidade`,
                                                 digits = 2),
                                           nsmall = 2),
           `Votos válidos` = pont_virg(`Votos válidos`),
           `Total de votos conquistados` = pont_virg(`Total de votos conquistados`))
  
  }

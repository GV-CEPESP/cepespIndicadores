
## OBJETIVOS

#'         - Definir as funções utilizadas para cálculo dos indicadores eleitorais
#'           e padronização do formato dos dados.

# 1. Fórmulas -------------------------------------------------------------

## 1.1. Fragmentação -------------------------------------------------------

### 1.1.1. Número Efetivo de Partidos ---------------------------------------

## Função para o cálculo do 'Número Efetivo de Partidos' 

num_efetivo_part <- function(prop_votos_cadeiras){
  
  1 / sum(prop_votos_cadeiras ^ 2)
  
}

### 1.1.2. Fracionalização --------------------------------------------------

## Função para o cálculo da 'Fracionalização'

fracionalizacao <- function(perc_cadeiras_part){
  
  1 - (sum(perc_cadeiras_part ^ 2))
  
}

### 1.1.3. Fracionalização Máxima -------------------------------------------

## Função para o cálculo da 'Fracionalização Máxima'

fracionalizacao_max <- function(num_cadeiras, num_partidos_repres){
  
  (num_cadeiras * (num_partidos_repres - 1)) / 
    
    (num_partidos_repres * (num_cadeiras - 1))
  
}

### 1.1.4. Fragmentação -----------------------------------------------------

## Função para o cálculo da 'Fragmentação'

fragmentaco <- function(fracionalizacao, fracionalizacao_max){
  
  fracionalizacao/fracionalizacao_max
  
}

### 1.1.5. Desproporcionalidade ---------------------------------------------

## Função para o cálculo da 'Desproporcionalidade de Gallagher'

desp_gallagher <- function(perc_votos, perc_cadeiras){
  
  sqrt(sum((perc_votos * 100 - perc_cadeiras * 100) ^ 2, 
           na.rm = TRUE) / 2)
  
}

## 1.2. Distribuição de Cadeiras -------------------------------------------

### 1.2.1. Quociente Eleitoral ----------------------------------------------

## Função para o cálculo do 'Quociente Eleitoral'

quoc_eleitoral <- function(votos_validos, qt_vagas){
  
  as.numeric(votos_validos) / as.numeric(qt_vagas)
  
}

### 1.2.2. Quociente Partidário ---------------------------------------------

## Função para o cálculo do 'Quociente Partidário'

quoc_partidario <- function(tot_votos_partido, quoc_eleitoral){
  
  as.numeric(tot_votos_partido) / as.numeric(quoc_eleitoral)
  
}

## 1.3. Reeleição ----------------------------------------------------------

### 1.3.1. Reeleição --------------------------------------------------------

## Função para o cálculo da 'Reeleição' 

reeleicao <- function(num_reeleitos, vagas){
  
  (num_reeleitos / vagas) * 100
  
}

### 1.3.2. Reeleição Líquida ------------------------------------------------

## Função para o cálculo da 'Reeleição Líquida' 

reeleicao_liq <- function(num_reeleitos, num_derrotados){
  
  (num_reeleitos / (num_reeleitos + num_derrotados)) * 100
  
}

### 1.3.3. Renovação --------------------------------------------------------

## Função para o cálculo da 'Renovação' 

renovacao <- function(indic_reeleicao){
  
  100 - indic_reeleicao
  
}

### 1.3.4. Renovação Líquida ------------------------------------------------

## Função para o cálculo da 'Renovação Líquida' 

renovacao_liq <- function(indic_reeleicao_liq){
  
  100 - indic_reeleicao_liq
  
}

### 1.3.5. Recandidaturas ---------------------------------------------------

## Função para o cálculo das 'Recandidaturas' 

recandidaturas <- function(num_recandidatos, num_eleitos_ant){
  
  (num_recandidatos / num_eleitos_ant) * 100
  
}

## 1.4. Participação e Alienação -------------------------------------------

### 1.4.1. Abstenção Percentual ---------------------------------------------

## Função para o cálculo da 'Abstenção Percentual'

abstencao_percent <- function(quant_abstencoes, quant_eleitores_apt){
  
  (quant_abstencoes / quant_eleitores_apt) * 100

  }

### 1.4.2. Votos Brancos Percentuais ----------------------------------------

## Função para o cálculo dos 'Votos Brancos Percentuais'

votosbrancos_percent <- function(quant_votosbrancos, quant_eleitores_apt){
  
  (quant_votosbrancos / quant_eleitores_apt) * 100

}

### 1.4.3. Votos Nulos Percentuais ------------------------------------------

## Função para o cálculo dos 'Votos Nulos Percentuais'

votosnulos_percent <- function(quant_votosnulos, quant_eleitores_apt){
  
  (quant_votosnulos / quant_eleitores_apt) * 100
  
}

### 1.4.4. Alienação Absoluta -----------------------------------------------

## Função para o cálculo da 'Alienação Absoluta'

alienacao_absoluta <- function(quant_abstencoes, quant_votosnulos, quant_votosbrancos){
  
  
  quant_abstencoes + quant_votosnulos + quant_votosbrancos
  
}

### 1.4.5. Alienação Percentual ---------------------------------------------

## Função para o cálculo da 'Alienação Percentual'

alienacao_percentual <- function(quant_abstencoes, quant_votosnulos, 
                                 quant_votosbrancos, quant_eleitores_apt){
  
  
  ((quant_abstencoes + quant_votosnulos + quant_votosbrancos) / quant_eleitores_apt) * 100
  
}

## 1.5. Volatilidade -------------------------------------------------------

## Função para o cálculo da 'Volatilidade'

volatilidade <- function(prop_votos_cadeirasT, prop_votos_cadeiras_T1) {
  
  sum(abs((prop_votos_cadeiras_T * 100) - (prop_votos_cadeiras_T1 * 100))) / 2 

  }

# 2. Indicadores ----------------------------------------------------------

## 2.1. Distribuição de Cadeiras -------------------------------------------

## Calcula os quocientes eleitoral e partidário para cada um dos
## cargos proporcionais em disputa

indic_disticad <- function(data,
                           cargo = c("DF", "DE", "VR")){
  
  if(cargo %in% c("DF",
                  "DE")){
    
    ## Calcula o quociente eleitoral e partidário para os 
    ## Deputados Federais e Deputados Estaduais
    
    data <- data %>% 
      mutate(QUOCIENTE_ELEITORAL = quoc_eleitoral(QT_VOTOS_VALIDOS,
                                                  QT_VAGAS),
             QUOCIENTE_PARTIDARIO = quoc_eleitoral(VOT_PART_UF,
                                                   QUOCIENTE_ELEITORAL))
    
  } else if(cargo == "VR"){
    
    ## Calcula o quociente eleitoral e partidário para os 
    ## Deputados Federais e Deputados Estaduais
    
    data <- data %>% 
      mutate(QUOCIENTE_ELEITORAL = quoc_eleitoral(QT_VOTOS_VALIDOS,
                                                  QT_VAGAS),
             QUOCIENTE_PARTIDARIO = quoc_eleitoral(VOT_PART_MUN,
                                                   QUOCIENTE_ELEITORAL))
    
  }
  
  return(data)
  
}

## 2.1. Cadeiras Conquistadas ----------------------------------------------

## Calcula o percentual de votos e cadeiras conquistadas pelos partidos 
## em diferentes agregações regionais

indic_cadeiras_conq <- function(data, 
                                agregacao = c("BR", "UF", "MUN")){
  
  if(agregacao == "BR"){
  
  ## Filtra os candidatos que foram eleitos
  
  data <- data %>% 
    filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
           DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
           DESC_SIT_TOT_TURNO == "MÉDIA" |
           DESC_SIT_TOT_TURNO == "MEDIA")
  
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
  
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
               DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
               DESC_SIT_TOT_TURNO == "MÉDIA" |
               DESC_SIT_TOT_TURNO == "MEDIA")
    
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
    
  } else if(agregacao == "MUN"){
    
    ## Filtra os candidatos que foram eleitos
    
    data <- data %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
               DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
               DESC_SIT_TOT_TURNO == "MÉDIA" |
               DESC_SIT_TOT_TURNO == "MEDIA")
    
    ## Soma o total de cadeiras conquistadas pelos partidos em cada ano e uf e
    ## organiza os dados
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               NUM_TURNO,
               UF,
               COD_MUN_TSE,
               NOME_MUNICIPIO,
               DESCRICAO_CARGO,
               QT_VOTOS_VALIDOS,
               SIGLA_PARTIDO) %>% 
      mutate(TOT_CADEIRAS = n()) %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             UF,
             COD_MUN_TSE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO,
             QT_VOTOS_VALIDOS,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             TOT_CADEIRAS) %>%
      unique() %>% 
      group_by(ANO_ELEICAO,
               UF,
               NOME_MUNICIPIO) %>% 
      mutate(QT_VAGAS = sum(TOT_CADEIRAS,
                            na.rm = TRUE)) %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             UF,
             COD_MUN_TSE,
             NOME_MUNICIPIO,
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
               NOME_MUNICIPIO) %>% 
      mutate(NUM_PART_PARLAMENT = n())
    
  }
}

## 2.2. Fragmentação -------------------------------------------------------

indic_frag <- function(data, 
                     agregacao = c("BR", "UF", "PF", "VR")){
  
  if(agregacao == "BR"){
    
    ## Calcula os indicadores de fragmentação em cada ano e município
    
    data <- data %>% 
      group_by(ANO_ELEICAO) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS),
             `Número efetivo de partidos legislativo` = num_efetivo_part(PERC_CADEIRAS),
             `Fracionalização` = fracionalizacao(PERC_CADEIRAS),
             `Fracionalização máxima` = fracionalizacao_max(QT_VAGAS, 
                                                            NUM_PART_PARLAMENT),
             `Fragmentação` = fragmentaco(`Fracionalização`,
                                          `Fracionalização máxima`),
             `Desproporcionalidade` = desp_gallagher(PERC_VOTOS,
                                                     PERC_CADEIRAS))
    
  } else if(agregacao == "UF"){
    
    ## Calcula os indicadores de fragmentação em cada ano e município
      
      data <- data %>% 
      group_by(ANO_ELEICAO,
               UF) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS),
             `Número efetivo de partidos legislativo` = num_efetivo_part(PERC_CADEIRAS),
             `Fracionalização` = fracionalizacao(PERC_CADEIRAS),
             `Fracionalização máxima` = fracionalizacao_max(QT_VAGAS, 
                                                            NUM_PART_PARLAMENT),
             `Fragmentação` = fragmentaco(`Fracionalização`,
                                          `Fracionalização máxima`),
             `Desproporcionalidade` = desp_gallagher(PERC_VOTOS,
                                                     PERC_CADEIRAS))
      
  } else if(agregacao == "PF"){
    
    ## Calcula os indicadores de fragmentação em cada ano, turno e município
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               NUM_TURNO, 
               COD_MUN_TSE) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS))
  
  
  } else if(agregacao == "VR"){
    
    ## Calcula os indicadores de fragmentação em cada ano e município
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               NUM_TURNO, 
               COD_MUN_TSE) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS),
             `Número efetivo de partidos legislativo` = num_efetivo_part(PERC_CADEIRAS),
             `Fracionalização` = fracionalizacao(PERC_CADEIRAS),
             `Fracionalização máxima` = fracionalizacao_max(QT_VAGAS, 
                                                            NUM_PART_PARLAMENT),
             `Fragmentação` = fragmentaco(`Fracionalização`,
                                          `Fracionalização máxima`),
             `Desproporcionalidade` = desp_gallagher(PERC_VOTOS,
                                                     PERC_CADEIRAS))
  }
  
  return(data)
}

## 2.3. Eleitos ------------------------------------------------------------

eleitos <- function(data,
                    agregacao = c("UF", "MUN")){
  
  if(agregacao == "UF"){
    
    data <- data %>% 
      select(ANO_ELEICAO, 
             NUM_TURNO,
             UF,
             CODIGO_CARGO,
             DESCRICAO_CARGO,
             NOME_CANDIDATO,
             DATA_NASCIMENTO,
             CPF_CANDIDATO,
             NUM_TITULO_ELEITORAL_CANDIDATO,
             SIGLA_PARTIDO,
             DESC_SIT_TOT_TURNO) %>% 
      filter(DESC_SIT_TOT_TURNO == "ELEITO"|
             DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
             DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
             DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
             DESC_SIT_TOT_TURNO == "MÉDIA" |
             DESC_SIT_TOT_TURNO == "MEDIA")
    
    } else if(agregacao == "MUN"){
      
      data <- data %>% 
        select(ANO_ELEICAO, 
               NUM_TURNO,
               UF,
               COD_MUN_TSE,
               NOME_MUNICIPIO,
               CODIGO_CARGO,
               DESCRICAO_CARGO,
               NOME_CANDIDATO,
               DATA_NASCIMENTO,
               CPF_CANDIDATO,
               NUM_TITULO_ELEITORAL_CANDIDATO,
               SIGLA_PARTIDO,
               DESC_SIT_TOT_TURNO) %>% 
        filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
               DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
               DESC_SIT_TOT_TURNO == "MÉDIA" |
               DESC_SIT_TOT_TURNO == "MEDIA") 
      
    }
}

## 2.4. Reeleição ----------------------------------------------------------

## Função para calcular os indicadores de 'Reeleição'

indic_reel <- function(candidatos,
                       eleitos,
                       agregacao = c("BR", "UF", "MUN")){
  
  ## Desabilitando as mensagens do 'dplyr'
  
  options(dplyr.summarise.inform = FALSE)
  
  ## Lista temporária onde os dados serão armazenados
  
  temp <- list()
  
  if(agregacao == "BR"){
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(1998, 2018, by = 4)){
      
      cat("Lendo",ano,"\n")
      
      ## Banco com os candidatos da próxima eleição em 
      ## relação ao ano corrente
      
      candidatos_ano2 <- candidatos %>% 
        filter(ANO_ELEICAO == ano + 4) %>% 
        mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                        "DEPUTADO ESTADUAL",
                                        DESCRICAO_CARGO))
      
      ## Bancos com os candidatos eleitos na primeira
      ## eleição de referência
      
      eleitos_ano1 <- eleitos %>% 
        filter(ANO_ELEICAO == ano) %>% 
        mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                        "DEPUTADO ESTADUAL",
                                        DESCRICAO_CARGO))
      
      ## Bancos com os candidatos eleitos na segunda
      ## eleição de referencia
      
      eleitos_ano2 <- eleitos %>% 
        filter(ANO_ELEICAO == ano + 4) %>% 
        mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                        "DEPUTADO ESTADUAL",
                                        DESCRICAO_CARGO))
      
      ## Filtra os candidatos que se reapresentaram na eleição
      ## seguinte ao ano corrente e os que foram reeleitos
      
      eleitos_ano2 <- eleitos_ano2 %>% 
        filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
               eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
      
      ## Dos candidatos que se reapresentaram na eleição seguinte ao
      ## ano de referência, filtra-se somente os eleitos 
      
      indicadores2 <- eleitos_ano2 %>% 
        filter(DESC_SIT_TOT_TURNO == "ELEITO"|
                 DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
                 DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
                 DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
                 DESC_SIT_TOT_TURNO == "MÉDIA" |
                 DESC_SIT_TOT_TURNO == "MEDIA") %>% 
        group_by(ANO_ELEICAO) %>% 
        summarise(REELEITOS = n()) 
      
      ## Calcula o total de candidatos que se reapresentaram na 
      ## eleição seguinte ao ano corrente e junta com o número 
      ## de reeleitos
      
      suppressMessages(
      indicadores1 <- candidatos_ano2 %>% 
        filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                 eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
        mutate(REAPRESENTACAO = n()) %>% 
        select(ANO_ELEICAO,
               UF,
               DESCRICAO_CARGO,
               QT_VAGAS,
               REAPRESENTACAO) %>% 
        unique() %>% 
        group_by(ANO_ELEICAO,
                 DESCRICAO_CARGO) %>% 
        reframe(QT_VAGAS = sum(QT_VAGAS,
                               na.rm = TRUE),
                REAPRESENTACAO = REAPRESENTACAO) %>% 
        unique() %>% 
        left_join(indicadores2))
      
      ## Calcula os indicadores de 'Renovação'
      
      indicadores1 <- indicadores1 %>% 
        mutate(DERROTADOS = REAPRESENTACAO - REELEITOS,
               DESISTENCIA = QT_VAGAS - REAPRESENTACAO,
               REELEICAO = reeleicao(REELEITOS, QT_VAGAS),
               REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS, 
                                                 DERROTADOS),
               RENOVACAO = renovacao(REELEICAO),
               RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
               RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                               QT_VAGAS))
      
      ## Empilha os indicadores calculados no banco criado
      
      temp <- bind_rows(temp, 
                        indicadores1)
    } 
    
  } else if(agregacao == "UF"){
    
    ## Lista dos estados brasileiros
    
    ufs <- c("AC", "AL", "AP", "AM", "BA", 
             "CE", "DF", "ES", "GO", "MA", 
             "MT", "MS", "MG", "PA", "PB", 
             "PR", "PE", "PI", "RJ", "RN", 
             "RS", "RO", "RR", "SC", "SP", 
             "SE", "TO")
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(1998, 2018, by = 4)){
      for(uf in ufs){
      
      cat("Lendo", ano, uf, "\n")
      
      ## Banco com os candidatos da próxima eleição em 
      ## relação ao ano corrente
      
      candidatos_ano2 <- candidatos %>% 
        filter(ANO_ELEICAO == ano + 4 &
               UF == uf)
      
      ## Bancos com os candidatos eleitos na primeira
      ## eleição de referência
      
      eleitos_ano1 <- eleitos %>% 
        filter(ANO_ELEICAO == ano &
               UF == uf)
      
      ## Bancos com os candidatos eleitos na segunda
      ## eleição de referencia
      
      eleitos_ano2 <- eleitos %>% 
        filter(ANO_ELEICAO == ano + 4 &
               UF == uf)
      
      ## Filtra os candidatos que se reapresentaram na eleição
      ## seguinte ao ano corrente e os que foram reeleitos
      
      eleitos_ano2 <- eleitos_ano2 %>% 
        filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
               eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
      
      ## Dos candidatos que se reapresentaram na eleição seguinte ao
      ## ano de referência, filtra-se somente os eleitos 
      
      indicadores2 <- eleitos_ano2 %>% 
        filter(DESC_SIT_TOT_TURNO == "ELEITO"|
                 DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
                 DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
                 DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
                 DESC_SIT_TOT_TURNO == "MÉDIA" |
                 DESC_SIT_TOT_TURNO == "MEDIA") %>% 
        group_by(ANO_ELEICAO,
                 UF) %>% 
        summarise(REELEITOS = n()) 
      
      ## Calcula o total de candidatos que se reapresentaram na 
      ## eleição seguinte ao ano corrente e junta com o número 
      ## de reeleitos
      
      suppressMessages(
      indicadores1 <- candidatos_ano2 %>% 
        filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
                 eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
        mutate(REAPRESENTACAO = n()) %>% 
        select(ANO_ELEICAO,
               DESCRICAO_CARGO,
               UF,
               QT_VAGAS,
               REAPRESENTACAO) %>% 
        unique() %>% 
        group_by(ANO_ELEICAO,
                 DESCRICAO_CARGO,
                 UF) %>% 
        reframe(QT_VAGAS = sum(QT_VAGAS,
                               na.rm = TRUE),
                REAPRESENTACAO = REAPRESENTACAO) %>% 
        unique() %>% 
        left_join(indicadores2))
      
      ## Calcula os indicadores de 'Renovação'
      
      indicadores1 <- indicadores1 %>% 
        mutate(DERROTADOS = REAPRESENTACAO - REELEITOS,
               DESISTENCIA = QT_VAGAS - REAPRESENTACAO,
               REELEICAO = reeleicao(REELEITOS, QT_VAGAS),
               REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS, 
                                                 DERROTADOS),
               RENOVACAO = renovacao(REELEICAO),
               RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
               RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                               QT_VAGAS))
      
      ## Empilha os indicadores calculados no banco criado
      
      temp <- bind_rows(temp, 
                        indicadores1)
      
      }
    } 
    
  } else if(agregacao == "MUN"){
    
    ## Lista de municípios brasileiros
    
    municipios <- unique(pf_mun_cand$COD_MUN_TSE)
    
    ## For loop que calcula os indicadores de 'Reeleição'
    ## para cada ano
    
    for(ano in seq(2000, 2016, by = 4)){
      for(municipio in seq_along(municipios)){
        
      cat("Lendo", ano, municipio, "\n")
      
      ## Banco com os candidatos da próxima eleição em 
      ## relação ao ano corrente
      
      candidatos_ano2 <- candidatos %>% 
        filter(ANO_ELEICAO == ano + 4 &
               COD_MUN_TSE == municipios[municipio]) 
      
      ## Bancos com os candidatos eleitos na primeira
      ## eleição de referência
      
      eleitos_ano1 <- eleitos %>% 
        filter(ANO_ELEICAO == ano &
               COD_MUN_TSE == municipios[municipio])
      
      ## Bancos com os candidatos eleitos na segunda
      ## eleição de referencia
      
      eleitos_ano2 <- eleitos %>% 
        filter(ANO_ELEICAO == ano + 4 &
               COD_MUN_TSE == municipios[municipio])
      
      ## Filtra os candidatos que se reapresentaram na eleição
      ## seguinte ao ano corrente e os que foram reeleitos
      
      eleitos_ano2 <- eleitos_ano2 %>% 
        filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
               eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO)
      
      ## Dos candidatos que se reapresentaram na eleição seguinte ao
      ## ano de referência, filtra-se somente os eleitos 
      
      indicadores2 <- eleitos_ano2 %>% 
        filter(DESC_SIT_TOT_TURNO == "ELEITO"|
               DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
               DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA" |
               DESC_SIT_TOT_TURNO == "ELEITO POR MÉDIA" |
               DESC_SIT_TOT_TURNO == "MÉDIA" |
               DESC_SIT_TOT_TURNO == "MEDIA") %>% 
        group_by(ANO_ELEICAO,
                 UF,
                 COD_MUN_TSE) %>% 
        summarise(REELEITOS = n()) 
      
      ## Calcula o total de candidatos que se reapresentaram na 
      ## eleição seguinte ao ano corrente e junta com o número 
      ## de reeleitos
      
      suppressMessages(
      indicadores1 <- candidatos_ano2 %>% 
        filter(NUM_TITULO_ELEITORAL_CANDIDATO %in% 
               eleitos_ano1$NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
        mutate(REAPRESENTACAO = n()) %>% 
        select(ANO_ELEICAO,
               DESCRICAO_CARGO,
               UF,
               COD_MUN_TSE,
               NOME_MUNICIPIO,
               QT_VAGAS,
               REAPRESENTACAO) %>% 
        unique() %>% 
        group_by(ANO_ELEICAO,
                 DESCRICAO_CARGO,
                 UF,
                 COD_MUN_TSE,
                 NOME_MUNICIPIO) %>% 
        reframe(QT_VAGAS = sum(QT_VAGAS,
                               na.rm = TRUE),
                REAPRESENTACAO = REAPRESENTACAO) %>% 
        unique() %>% 
        left_join(indicadores2))
      
      ## Calcula os indicadores de 'Renovação'
      
      indicadores1 <- indicadores1 %>% 
        mutate(DERROTADOS = REAPRESENTACAO - REELEITOS,
               DESISTENCIA = QT_VAGAS - REAPRESENTACAO,
               REELEICAO = reeleicao(REELEITOS, QT_VAGAS),
               REELEICAO_LIQUIDA = reeleicao_liq(REELEITOS, 
                                                 DERROTADOS),
               RENOVACAO = renovacao(REELEICAO),
               RENOVACAO_LIQUIDA = renovacao_liq(REELEICAO_LIQUIDA),
               RECANDIDATURAS = recandidaturas(REAPRESENTACAO,
                                               QT_VAGAS))
      
      ## Empilha os indicadores calculados no banco criado
      
      temp <- bind_rows(temp, 
                        indicadores1)
      
      }
      
        }
      } 
      
    
  return(temp)
  
} 

## 2.5. Participação e Alienação -------------------------------------------

indic_particip_alien <- function(data){
    
    data <- data %>% 
      mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                      "DEPUTADO ESTADUAL",
                                      DESCRICAO_CARGO),
             ABSTENCAO_PERCENTUAL = abstencao_percent(QTD_ABSTENCOES,
                                                      QTD_APTOS),
             VOTOS_BRANCOS_PERCENTUAIS = votosbrancos_percent(QT_VOTOS_BRANCOS,
                                                              QTD_APTOS),
             VOTOS_NULOS_PERCENTUAIS = votosnulos_percent(QT_VOTOS_NULOS,
                                                          QTD_APTOS),
             ALIENACAO_ABSOLUTA = alienacao_absoluta(QTD_ABSTENCOES,
                                                     QT_VOTOS_NULOS,
                                                     QT_VOTOS_BRANCOS),
             ALIENACAO_PERCENTUAL = alienacao_percentual(QTD_ABSTENCOES,
                                                         QT_VOTOS_NULOS,
                                                         QT_VOTOS_BRANCOS,
                                                         QTD_APTOS))
  
}

## 2.6. Volatilidade -------------------------------------------------------


  
# 3. Padronização ---------------------------------------------------------

## 3.1. Ponto e Vírgula ----------------------------------------------------

## Função para inserir pontuação e vírgula em números grandes
## e pequenos

pont_virg <- function(string,
                      tipo = c("inteiro", "decimal")){
  
  if(tipo == "inteiro"){
    
    suppressWarnings(
      
      formatC(string, 
              digits = 0,
              format = "f",
              big.mark = ".",
              small.mark = ",",
              decimal.mark = ",")
      
    )
    
  } else if(tipo == "decimal"){
  
  suppressWarnings(
    
    formatC(string, 
            digits = 2,
            format = "f",
            big.mark = ".",
            small.mark = ",",
            decimal.mark = ",",
            flag = "#")
    
    )
    
  }
  
}

## 3.2. Distribuição de Cadeiras -------------------------------------------

## Função para padronização dos dados de distribuição de cadeiras

padroniz_distcad <- function(data, 
                             agregacao = c("UF", "MUN")){
  
  if(agregacao == "UF"){
  
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
    mutate(QUOCIENTE_ELEITORAL = pont_virg(round(QUOCIENTE_ELEITORAL, 0),
                                           tipo = "inteiro"),
           QUOCIENTE_PARTIDARIO = pont_virg(round(QUOCIENTE_PARTIDARIO, 0),
                                            tipo = "inteiro"),
           QT_VOTOS_VALIDOS = pont_virg(QT_VOTOS_VALIDOS,
                                        tipo = "inteiro"),
           VOT_PART_UF = pont_virg(VOT_PART_UF,
                                   tipo = "inteiro"),
           DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
    rename("Ano da eleição" = "ANO_ELEICAO",
           "Cargo" = "DESCRICAO_CARGO",
           "Cadeiras oferecidas" = "QT_VAGAS",
           "Votos válidos" = "QT_VOTOS_VALIDOS",
           "Sigla do partido" = "SIGLA_PARTIDO",
           "Votos do partido" = "VOT_PART_UF",
           "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
           "Quociente partidário" = "QUOCIENTE_PARTIDARIO")
  
  } else if(agregacao == "MUN"){
    
    data <- data %>% 
      select(ANO_ELEICAO, 
             UF,
             COD_MUN_TSE,
             NOME_MUNICIPIO,
             DESCRICAO_CARGO, 
             QT_VAGAS,
             QT_VOTOS_VALIDOS,
             SIGLA_PARTIDO,
             VOT_PART_MUN,
             QUOCIENTE_ELEITORAL,
             QUOCIENTE_PARTIDARIO) %>% 
      mutate(QUOCIENTE_ELEITORAL = pont_virg(round(QUOCIENTE_ELEITORAL, 0),
                                             tipo = "inteiro"),
             QUOCIENTE_PARTIDARIO = pont_virg(round(QUOCIENTE_PARTIDARIO, 0),
                                              tipo = "inteiro"),
             QT_VOTOS_VALIDOS = pont_virg(QT_VOTOS_VALIDOS,
                                          tipo = "inteiro"),
             VOT_PART_MUN = pont_virg(VOT_PART_MUN,
                                      tipo = "inteiro"),
             DESCRICAO_CARGO = str_to_title(DESCRICAO_CARGO)) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras oferecidas" = "QT_VAGAS",
             "Votos válidos" = "QT_VOTOS_VALIDOS",
             "Sigla do partido" = "SIGLA_PARTIDO",
             "Votos do partido" = "VOT_PART_MUN",
             "Quociente eleitoral" = "QUOCIENTE_ELEITORAL",
             "Quociente partidário" = "QUOCIENTE_PARTIDARIO")
    
  }
  
}

## 3.3. Fragmentação -------------------------------------------------------

## Função para padronização dos indcadores de 'Fragmentação'

padroniz_frag <- function(data, 
                          agregacao = c("BR", "UF", "PF", "MUN")){
  
  if(agregacao == "BR"){
  
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
    mutate(`Percentual de votos conquistados` = pont_virg(round(`Percentual de votos conquistados`,
                                                                digits = 2),
                                                          tipo = "decimal"),
           `Percentual de cadeiras conquistadas` = pont_virg(round(`Percentual de cadeiras conquistadas`,
                                                                   digits = 2),
                                                             tipo = "decimal"),
           `Número efetivo de partidos eleitoral` = pont_virg(round(`Número efetivo de partidos eleitoral`,
                                                                    digits = 2),
                                                              tipo = "decimal"),
           `Número efetivo de partidos legislativo` = pont_virg(round(`Número efetivo de partidos legislativo`,
                                                                      digits = 2),
                                                                tipo = "decimal"),
           `Fracionalização` = pont_virg(round(`Fracionalização`,
                                               digits = 2),
                                         tipo = "decimal"),
           `Fracionalização máxima` = pont_virg(round(`Fracionalização máxima`,
                                                      digits = 2),
                                                tipo = "decimal"),
           `Fragmentação` = pont_virg(round(`Fragmentação`,
                                            digits = 2),
                                      tipo = "decimal"),
           `Desproporcionalidade` = pont_virg(round(`Desproporcionalidade`,
                                                    digits = 2),
                                              tipo = "decimal"),
           `Votos válidos` = pont_virg(`Votos válidos`,
                                       tipo = "inteiro"),
           `Total de votos conquistados` = pont_virg(`Total de votos conquistados`,
                                                     tipo = "inteiro"))
  
  } else if(agregacao == "UF"){
    
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
      mutate(`Percentual de votos conquistados` = pont_virg(round(`Percentual de votos conquistados`,
                                                                  digits = 2),
                                                            tipo = "decimal"),
             `Percentual de cadeiras conquistadas` = pont_virg(round(`Percentual de cadeiras conquistadas`,
                                                                     digits = 2),
                                                               tipo = "decimal"),
             `Número efetivo de partidos eleitoral` = pont_virg(round(`Número efetivo de partidos eleitoral`,
                                                                      digits = 2),
                                                                tipo = "decimal"),
             `Número efetivo de partidos legislativo` = pont_virg(round(`Número efetivo de partidos legislativo`,
                                                                        digits = 2),
                                                                  tipo = "decimal"),
             `Fracionalização` = pont_virg(round(`Fracionalização`,
                                                 digits = 2),
                                           tipo = "decimal"),
             `Fracionalização máxima` = pont_virg(round(`Fracionalização máxima`,
                                                        digits = 2),
                                                  tipo = "decimal"),
             `Fragmentação` = pont_virg(round(`Fragmentação`,
                                              digits = 2),
                                        tipo = "decimal"),
             `Desproporcionalidade` = pont_virg(round(`Desproporcionalidade`,
                                                      digits = 2),
                                                tipo = "decimal"),
             `Votos válidos` = pont_virg(`Votos válidos`,
                                         tipo = "inteiro"),
             `Total de votos conquistados` = pont_virg(`Total de votos conquistados`,
                                                       tipo = "inteiro"))
    
  } else if(agregacao == "PF"){
    
    data <- data %>% 
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
      mutate(`Percentual de votos conquistados` = pont_virg(round(`Percentual de votos conquistados`,
                                                                  digits = 2),
                                                            tipo = "decimal"),
             `Número efetivo de partidos eleitoral` = pont_virg(round(`Número efetivo de partidos eleitoral`,
                                                                      digits = 2),
                                                                tipo = "decimal"),
             `Votos válidos` = pont_virg(`Votos válidos`,
                                         tipo = "inteiro"),
             `Total de votos conquistados` = pont_virg(`Total de votos conquistados`,
                                                       tipo = "inteiro")) %>% 
      arrange(`Ano da eleição`,
              UF,
              `Código do município`)
  
  
  } else if(agregacao == "VR"){
    
    data <- data %>% 
      ungroup() %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
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
             `Nome do município`,
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
      mutate(`Percentual de votos conquistados` = pont_virg(round(`Percentual de votos conquistados`,
                                                                  digits = 2),
                                                            tipo = "decimal"),
             `Percentual de cadeiras conquistadas` = pont_virg(round(`Percentual de cadeiras conquistadas`,
                                                                     digits = 2),
                                                               tipo = "decimal"),
             `Número efetivo de partidos eleitoral` = pont_virg(round(`Número efetivo de partidos eleitoral`,
                                                                      digits = 2),
                                                                tipo = "decimal"),
             `Número efetivo de partidos legislativo` = pont_virg(round(`Número efetivo de partidos legislativo`,
                                                                        digits = 2),
                                                                  tipo = "decimal"),
             `Fracionalização` = pont_virg(round(`Fracionalização`,
                                                 digits = 2),
                                           tipo = "decimal"),
             `Fracionalização máxima` = pont_virg(round(`Fracionalização máxima`,
                                                        digits = 2),
                                                  tipo = "decimal"),
             `Fragmentação` = pont_virg(round(`Fragmentação`,
                                              digits = 2),
                                        tipo = "decimal"),
             `Desproporcionalidade` = pont_virg(round(`Desproporcionalidade`,
                                                      digits = 2),
                                                tipo = "decimal"),
             `Votos válidos` = pont_virg(`Votos válidos`,
                                         tipo = "inteiro"),
             `Total de votos conquistados` = pont_virg(`Total de votos conquistados`,
                                                       tipo = "inteiro"))
    
    }
  
}

## 3.4. Reeleição ----------------------------------------------------------

## Função para padronização dos indicadores de 'Reeleição'

padroniz_reel <- function(data,
                          agregacao = c("BR", "UF", "MUN")){
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             QT_VAGAS,
             REELEICAO,
             REELEICAO_LIQUIDA,
             RENOVACAO,
             RENOVACAO_LIQUIDA,
             RECANDIDATURAS) %>% 
      mutate(QT_VAGAS = pont_virg(QT_VAGAS,
                                  tipo = "inteiro"),
             REELEICAO = pont_virg(round(REELEICAO,
                                      digits = 2),
                                   tipo = "decimal"),
             REELEICAO_LIQUIDA = pont_virg(round(REELEICAO_LIQUIDA,
                                        digits = 2),
                                        tipo = "decimal"),
             RENOVACAO = pont_virg(round(RENOVACAO,
                                      digits = 2),
                                   tipo = "decimal"),
             RENOVACAO_LIQUIDA = pont_virg(round(RENOVACAO_LIQUIDA,
                                      digits = 2),
                                      tipo = "decimal"),
             RECANDIDATURAS = pont_virg(round(RECANDIDATURAS,
                                      digits = 2),
                                      tipo = "decimal")) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Reeleição" = "REELEICAO",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação" = "RENOVACAO",
             "Renovação líquida" = "RENOVACAO_LIQUIDA",
             "Recandidaturas" = "RECANDIDATURAS") %>% 
      arrange(`Ano da eleição`)
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             UF,
             QT_VAGAS,
             REELEICAO,
             REELEICAO_LIQUIDA,
             RENOVACAO,
             RENOVACAO_LIQUIDA,
             RECANDIDATURAS) %>% 
      mutate(QT_VAGAS = pont_virg(QT_VAGAS,
                                  tipo = "inteiro"),
             REELEICAO = pont_virg(round(REELEICAO,
                                         digits = 2),
                                   tipo = "decimal"),
             REELEICAO_LIQUIDA = pont_virg(round(REELEICAO_LIQUIDA,
                                                 digits = 2),
                                           tipo = "decimal"),
             RENOVACAO = pont_virg(round(RENOVACAO,
                                         digits = 2),
                                   tipo = "decimal"),
             RENOVACAO_LIQUIDA = pont_virg(round(RENOVACAO_LIQUIDA,
                                                 digits = 2),
                                           tipo = "decimal"),
             RECANDIDATURAS = pont_virg(round(RECANDIDATURAS,
                                              digits = 2),
                                        tipo = "decimal")) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Reeleição" = "REELEICAO",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação" = "RENOVACAO",
             "Renovação líquida" = "RENOVACAO_LIQUIDA",
             "Recandidaturas" = "RECANDIDATURAS") %>% 
      arrange(`Ano da eleição`,
              UF)
    
  } else if(agregacao == "MUN"){
    
    data <- data %>% 
      select(ANO_ELEICAO,
             DESCRICAO_CARGO,
             UF,
             COD_MUN_TSE,
             NOME_MUNICIPIO,
             QT_VAGAS,
             REELEICAO,
             REELEICAO_LIQUIDA,
             RENOVACAO,
             RENOVACAO_LIQUIDA,
             RECANDIDATURAS) %>% 
      mutate(QT_VAGAS = pont_virg(QT_VAGAS,
                                  tipo = "inteiro"),
             REELEICAO = pont_virg(round(REELEICAO,
                                         digits = 2),
                                   tipo = "decimal"),
             REELEICAO_LIQUIDA = pont_virg(round(REELEICAO_LIQUIDA,
                                                 digits = 2),
                                           tipo = "decimal"),
             RENOVACAO = pont_virg(round(RENOVACAO,
                                         digits = 2),
                                   tipo = "decimal"),
             RENOVACAO_LIQUIDA = pont_virg(round(RENOVACAO_LIQUIDA,
                                                 digits = 2),
                                           tipo = "decimal"),
             RECANDIDATURAS = pont_virg(round(RECANDIDATURAS,
                                              digits = 2),
                                        tipo = "decimal")) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Cargo" = "DESCRICAO_CARGO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Cadeiras disponíveis" = "QT_VAGAS",
             "Reeleição" = "REELEICAO",
             "Reeleição líquida" = "REELEICAO_LIQUIDA",
             "Renovação" = "RENOVACAO",
             "Renovação líquida" = "RENOVACAO_LIQUIDA",
             "Recandidaturas" = "RECANDIDATURAS") %>% 
      arrange(`Ano da eleição`,
              UF,
              `Nome do município`)
    
  }
  
}

## 3.5. Participação e Alienação -------------------------------------------

padroniz_particip_alien <- function(data,
                                    agregacao = c("BR", "UF", "MUN")){
  
  if(agregacao == "BR"){
    
    data <- data %>% 
      ungroup() %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             DESCRICAO_CARGO,
             QTD_APTOS,
             QTD_COMPARECIMENTO,
             QTD_ABSTENCOES, 
             ABSTENCAO_PERCENTUAL,
             QT_VOTOS_BRANCOS, 
             VOTOS_BRANCOS_PERCENTUAIS,
             QT_VOTOS_NULOS,
             VOTOS_NULOS_PERCENTUAIS,
             ALIENACAO_ABSOLUTA,
             ALIENACAO_PERCENTUAL) %>% 
      mutate(QTD_APTOS = pont_virg(QTD_APTOS,
                                   tipo = "inteiro"),
             QTD_COMPARECIMENTO = pont_virg(QTD_COMPARECIMENTO,
                                            tipo = "inteiro"),
             QTD_ABSTENCOES = pont_virg(QTD_ABSTENCOES,
                                        tipo = "inteiro"),
             ABSTENCAO_PERCENTUAL = pont_virg(round(ABSTENCAO_PERCENTUAL,
                                                    digits = 2),
                                              tipo = "decimal"),
             QT_VOTOS_BRANCOS = pont_virg(QT_VOTOS_BRANCOS,
                                          tipo = "inteiro"),
             VOTOS_BRANCOS_PERCENTUAIS = pont_virg(round(VOTOS_BRANCOS_PERCENTUAIS,
                                                         digits = 2),
                                                   tipo = "decimal"),
             QT_VOTOS_NULOS = pont_virg(QT_VOTOS_NULOS,
                                        tipo = "inteiro"),
             VOTOS_NULOS_PERCENTUAIS = pont_virg(round(VOTOS_NULOS_PERCENTUAIS,
                                                       digits = 2),
                                                 tipo = "decimal"),
             ALIENACAO_ABSOLUTA = pont_virg(ALIENACAO_ABSOLUTA,
                                            tipo = "inteiro"),
             ALIENACAO_PERCENTUAL = pont_virg(round(ALIENACAO_PERCENTUAL,
                                                    digits = 2),
                                              tipo = "decimal")) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "Cargo" = "DESCRICAO_CARGO",
             "Quantidade de eleitores aptos" = "QTD_APTOS",
             "Quantidade de comparecimento" = "QTD_COMPARECIMENTO",
             "Quantidade de abstenções" = "QTD_ABSTENCOES",
             "Percentual de abstenções" = "ABSTENCAO_PERCENTUAL",
             "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS",
             "Percentual de votos brancos" = "VOTOS_BRANCOS_PERCENTUAIS",
             "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
             "Percentual de votos nulos" = "VOTOS_NULOS_PERCENTUAIS",
             "Alienação absoluta" = "ALIENACAO_ABSOLUTA",
             "Alienação percentual" = "ALIENACAO_PERCENTUAL") %>% 
      arrange(`Ano da eleição`)
    
  } else if(agregacao == "UF"){
    
    data <- data %>% 
      ungroup() %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             DESCRICAO_CARGO,
             UF,
             QTD_APTOS,
             QTD_COMPARECIMENTO,
             QTD_ABSTENCOES, 
             ABSTENCAO_PERCENTUAL,
             QT_VOTOS_BRANCOS, 
             VOTOS_BRANCOS_PERCENTUAIS,
             QT_VOTOS_NULOS,
             VOTOS_NULOS_PERCENTUAIS,
             ALIENACAO_ABSOLUTA,
             ALIENACAO_PERCENTUAL) %>% 
      mutate(QTD_APTOS = pont_virg(QTD_APTOS,
                                   tipo = "inteiro"),
             QTD_COMPARECIMENTO = pont_virg(QTD_COMPARECIMENTO,
                                            tipo = "inteiro"),
             QTD_ABSTENCOES = pont_virg(QTD_ABSTENCOES,
                                        tipo = "inteiro"),
             ABSTENCAO_PERCENTUAL = pont_virg(round(ABSTENCAO_PERCENTUAL,
                                                    digits = 2),
                                              tipo = "decimal"),
             QT_VOTOS_BRANCOS = pont_virg(QT_VOTOS_BRANCOS,
                                          tipo = "inteiro"),
             VOTOS_BRANCOS_PERCENTUAIS = pont_virg(round(VOTOS_BRANCOS_PERCENTUAIS,
                                                         digits = 2),
                                                   tipo = "decimal"),
             QT_VOTOS_NULOS = pont_virg(QT_VOTOS_NULOS,
                                        tipo = "inteiro"),
             VOTOS_NULOS_PERCENTUAIS = pont_virg(round(VOTOS_NULOS_PERCENTUAIS,
                                                       digits = 2),
                                                 tipo = "decimal"),
             ALIENACAO_ABSOLUTA = pont_virg(ALIENACAO_ABSOLUTA,
                                            tipo = "inteiro"),
             ALIENACAO_PERCENTUAL = pont_virg(round(ALIENACAO_PERCENTUAL,
                                                    digits = 2),
                                              tipo = "decimal")) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "Cargo" = "DESCRICAO_CARGO",
             "Quantidade de eleitores aptos" = "QTD_APTOS",
             "Quantidade de comparecimento" = "QTD_COMPARECIMENTO",
             "Quantidade de abstenções" = "QTD_ABSTENCOES",
             "Percentual de abstenções" = "ABSTENCAO_PERCENTUAL",
             "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS",
             "Percentual de votos brancos" = "VOTOS_BRANCOS_PERCENTUAIS",
             "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
             "Percentual de votos nulos" = "VOTOS_NULOS_PERCENTUAIS",
             "Alienação absoluta" = "ALIENACAO_ABSOLUTA",
             "Alienação percentual" = "ALIENACAO_PERCENTUAL") %>% 
      arrange(`Ano da eleição`,
              UF)
    
  } else if(agregacao == "MUN"){
    
    data <- data %>%
      ungroup() %>% 
      select(ANO_ELEICAO,
             NUM_TURNO,
             ANO_ELEICAO,
             NUM_TURNO,
             DESCRICAO_CARGO,
             UF,
             COD_MUN_TSE,
             NOME_MUNICIPIO,
             QTD_APTOS,
             QTD_COMPARECIMENTO,
             QTD_ABSTENCOES, 
             ABSTENCAO_PERCENTUAL,
             QT_VOTOS_BRANCOS, 
             VOTOS_BRANCOS_PERCENTUAIS,
             QT_VOTOS_NULOS,
             VOTOS_NULOS_PERCENTUAIS,
             ALIENACAO_ABSOLUTA,
             ALIENACAO_PERCENTUAL) %>% 
      mutate(QTD_APTOS = pont_virg(QTD_APTOS,
                                   tipo = "inteiro"),
             QTD_COMPARECIMENTO = pont_virg(QTD_COMPARECIMENTO,
                                            tipo = "inteiro"),
             QTD_ABSTENCOES = pont_virg(QTD_ABSTENCOES,
                                        tipo = "inteiro"),
             ABSTENCAO_PERCENTUAL = pont_virg(round(ABSTENCAO_PERCENTUAL,
                                                    digits = 2),
                                              tipo = "decimal"),
             QT_VOTOS_BRANCOS = pont_virg(QT_VOTOS_BRANCOS,
                                          tipo = "inteiro"),
             VOTOS_BRANCOS_PERCENTUAIS = pont_virg(round(VOTOS_BRANCOS_PERCENTUAIS,
                                                         digits = 2),
                                                   tipo = "decimal"),
             QT_VOTOS_NULOS = pont_virg(QT_VOTOS_NULOS,
                                        tipo = "inteiro"),
             VOTOS_NULOS_PERCENTUAIS = pont_virg(round(VOTOS_NULOS_PERCENTUAIS,
                                                       digits = 2),
                                                 tipo = "decimal"),
             ALIENACAO_ABSOLUTA = pont_virg(ALIENACAO_ABSOLUTA,
                                            tipo = "inteiro"),
             ALIENACAO_PERCENTUAL = pont_virg(round(ALIENACAO_PERCENTUAL,
                                                    digits = 2),
                                              tipo = "decimal")) %>% 
      rename("Ano da eleição" = "ANO_ELEICAO",
             "Turno" = "NUM_TURNO",
             "Cargo" = "DESCRICAO_CARGO",
             "Código do município" = "COD_MUN_TSE",
             "Nome do município" = "NOME_MUNICIPIO",
             "Quantidade de eleitores aptos" = "QTD_APTOS",
             "Quantidade de comparecimento" = "QTD_COMPARECIMENTO",
             "Quantidade de abstenções" = "QTD_ABSTENCOES",
             "Percentual de abstenções" = "ABSTENCAO_PERCENTUAL",
             "Quantidade de votos brancos" = "QT_VOTOS_BRANCOS",
             "Percentual de votos brancos" = "VOTOS_BRANCOS_PERCENTUAIS",
             "Quantidade de votos nulos" = "QT_VOTOS_NULOS", 
             "Percentual de votos nulos" = "VOTOS_NULOS_PERCENTUAIS",
             "Alienação absoluta" = "ALIENACAO_ABSOLUTA",
             "Alienação percentual" = "ALIENACAO_PERCENTUAL") %>% 
      arrange(`Ano da eleição`,
              UF,
              `Nome do município`)
    
  }
  
}

## 3.6. Volatilidade -------------------------------------------------------



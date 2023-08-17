
## OBJETIVOS

#'        - Realizar a coleta dos metadados referentes as eleições
#'          ocorridas entre 1998 e 2022.

#'        - Padronizar e consolidar os dados coletados em bases de dados
#'          agregadas por: eleições gerais e eleições municipais.


# 1. Import ---------------------------------------------------------------

## 1.1. TSE ----------------------------------------------------------------

## Carregando os arquivos pré-processados contendo as vagas por
## cargo e ano da eleição

vagas_sen <- readRDS("data/input/vagas_senadores_1998_2022.rds")

vagas_dep <- readRDS("data/input/vagas_deputados_fed_est_1998_2022.rds")

vagas_ver <- readRDS("data/input/vagas_vereadores_2000_2020.rds")

### 1.1.1. Resumo da Eleição ------------------------------------------------

#### 1.1.1.1. Eleições Gerais ------------------------------------------------

## URL dos dados 

url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_munzona/detalhe_votacao_munzona_ANO.zip"

## Lista com os anos das eleições gerais faltantes

anos <- seq(1998, 2022, by = 4)

## Loop que faz o download dos dados

for(i in anos){

  cat("Lendo", i, "\n")

  resumo_gr <- stringr::str_replace_all(url,
                                     "ANO",
                                     as.character(i))

  download.file(resumo_gr,
                str_c("resumo_gr",
                      i,
                      ".zip"))

  }

## Cria uma lista com os nomes dos arquivos baixados

list_dados <- list.files(pattern = "resumo_gr")

## Loop que unzipa os dados

for(i in seq_along(list_dados)){

  cat("Lendo", list_dados[i], "\n")

  unzip(list_dados[i],
        exdir = "data/input/Resumo da Eleição/Eleições Gerais")

  file.remove(list_dados[i])

}

## Cria uma lista com os nomes dos arquivos extraídos

list_dados <- list.files(path = "data/input/Resumo da Eleição/Eleições Gerais",
                         pattern = "detalhe_votacao_munzona")

## Remove o arquivo 'BRASIL'

for(i in seq_along(list_dados)){

  br_files_vagas <- list.files(path = "data/input/Resumo da Eleição/Eleições Gerais",
                               pattern = "BRASIL",
                               full.names = TRUE)

  file.remove(br_files_vagas)

}

## Atualiza a lista de arquivos extraídos

list_dados <- list.files(path = "data/input/Resumo da Eleição/Eleições Gerais",
                         pattern = "detalhe_votacao_munzona")

## Cria uma lista vazia onde os dados serão armazenados

resumo_gr <- list()

## For loop que lê os arquivos

for (i in seq_along(list_dados)) {

  cat("lendo", list_dados[i], "\n")

    temp <- read.table(file = paste0("data/input/Resumo da Eleição/Eleições Gerais/",
                                     list_dados[i]),
                       header = TRUE,
                       sep = ";",
                       stringsAsFactors = FALSE,
                       fileEncoding = "latin1")
    
    if(grepl("2022|2018", list_dados[i])){

      temp <- temp %>%
        mutate(NM_TIPO_ELEICAO = str_to_upper(NM_TIPO_ELEICAO)) %>% 
        filter(NM_TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA" |
                 NM_TIPO_ELEICAO == "ELEICAO ORDINARIA") %>% 
        rename("QT_VOTOS_NOMINAIS" = "QT_VOTOS_NOMINAIS_VALIDOS",
               "QT_VOTOS_LEGENDA" = "QT_VOTOS_LEGENDA_VALIDOS",
               "QT_VOTOS_ANULADOS" = "QT_TOTAL_VOTOS_ANULADOS") %>%
        select(ANO_ELEICAO,
               NR_TURNO,
               SG_UF,
               CD_CARGO,
               DS_CARGO,
               QT_APTOS,
               QT_COMPARECIMENTO,
               QT_ABSTENCOES,
               QT_VOTOS_VALIDOS,
               QT_VOTOS_NOMINAIS,
               QT_VOTOS_LEGENDA,
               QT_VOTOS_BRANCOS,
               QT_VOTOS_NULOS,
               QT_VOTOS_ANULADOS) %>%
        group_by(ANO_ELEICAO,
                 NR_TURNO,
                 SG_UF,
                 CD_CARGO,
                 DS_CARGO) %>%
        summarise(QT_APTOS = sum(QT_APTOS),
                  QT_COMPARECIMENTO = sum(QT_COMPARECIMENTO),
                  QT_ABSTENCOES = sum(QT_ABSTENCOES),
                  QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS),
                  QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS),
                  QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA),
                  QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS),
                  QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS),
                  QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS))
      
    } else {
     
      temp <- temp %>%
        mutate(QT_VOTOS_VALIDOS = QT_VOTOS_NOMINAIS + QT_VOTOS_LEGENDA,
               NM_TIPO_ELEICAO = str_to_upper(NM_TIPO_ELEICAO)) %>%
        filter(NM_TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA" |
               NM_TIPO_ELEICAO == "ELEICAO ORDINARIA") %>% 
        select(ANO_ELEICAO,
               NR_TURNO,
               SG_UF,
               CD_CARGO,
               DS_CARGO,
               QT_APTOS,
               QT_COMPARECIMENTO,
               QT_ABSTENCOES,
               QT_VOTOS_VALIDOS,
               QT_VOTOS_NOMINAIS,
               QT_VOTOS_LEGENDA,
               QT_VOTOS_BRANCOS,
               QT_VOTOS_NULOS,
               QT_VOTOS_ANULADOS) %>%
        group_by(ANO_ELEICAO,
                 NR_TURNO,
                 SG_UF,
                 CD_CARGO,
                 DS_CARGO) %>%
        summarise(QT_APTOS = sum(QT_APTOS),
                  QT_COMPARECIMENTO = sum(QT_COMPARECIMENTO),
                  QT_ABSTENCOES = sum(QT_ABSTENCOES),
                  QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS),
                  QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS),
                  QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA),
                  QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS),
                  QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS),
                  QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS)) 
      
    }

      resumo_gr[[i]] <- temp

}

## Empilhando os bancos

resumo_gr <- rbind.fill(resumo_gr)

## Salvando os dados já agregados

saveRDS(resumo_gr,
        "data/input/Resumo da Eleição/Eleições Gerais/resumo_gr.rds")

## Lendo arquivo já compilado

resumo_gr <- readRDS("data/input/Resumo da Eleição/Eleições Gerais/resumo_gr.rds")

#### 1.1.1.2. Eleições Municipais --------------------------------------------

## URL dos dados

url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_munzona/detalhe_votacao_munzona_ANO.zip"

## Lista com os anos das eleições municipais faltantes

anos <- seq(2000, 2020, by = 4)

## Loop que faz o download dos dados

for(i in anos){

  cat("Lendo", i, "\n")

  resumo_mun <- stringr::str_replace_all(url,
                                         "ANO",
                                         as.character(i))

  download.file(resumo_mun,
                str_c("resumo_mun",
                      i,
                      ".zip"))
}

## Cria uma lista com os nomes dos arquivos baixados

list_dados <- list.files(pattern = "resumo_mun")

## Loop que unzipa os dados

for(i in seq_along(list_dados)){

  cat("Lendo", list_dados[i], "\n")

  unzip(list_dados[i],
        exdir = "data/input/Resumo da Eleição/Eleições Municipais")

  file.remove(list_dados[i])

}

## Cria uma lista com os nomes dos arquivos extraídos

list_dados <- list.files(path = "data/input/Resumo da Eleição/Eleições Municipais",
                         pattern = "detalhe_votacao_munzona")

## Remove o arquivo 'BRASIL'

for(i in seq_along(list_dados)){

  br_files_vagas <- list.files(path = "data/input/Resumo da Eleição/Eleições Municipais",
                               pattern = "BRASIL",
                               full.names = TRUE)

  file.remove(br_files_vagas)

 }

## Atualiza a lista de arquivos extraídos

list_dados <- list.files(path = "data/input/Resumo da Eleição/Eleições Municipais",
                         pattern = "detalhe_votacao_munzona")

## Cria uma lista vazia onde os dados serão armazenados

resumo_mun <- list()

## For loop que lê os arquivos

for (i in seq_along(list_dados)) {

  cat("lendo", list_dados[i], "\n")

    temp <- read.table(file = paste0("data/input/Resumo da Eleição/Eleições Municipais/",
                                     list_dados[i]),
                       header = TRUE,
                       sep = ";",
                       stringsAsFactors = FALSE,
                       fileEncoding = "latin1")
    
    if(grepl("2020", list_dados[i])){

    temp <- temp %>%
      mutate(NM_TIPO_ELEICAO = str_to_upper(NM_TIPO_ELEICAO)) %>% 
      filter(NM_TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA" |
             NM_TIPO_ELEICAO == "ELEICAO ORDINARIA") %>% 
      rename("QT_VOTOS_NOMINAIS" = "QT_VOTOS_NOMINAIS_VALIDOS",
             "QT_VOTOS_LEGENDA" = "QT_VOTOS_LEGENDA_VALIDOS",
             "QT_VOTOS_ANULADOS" = "QT_TOTAL_VOTOS_ANULADOS") %>%
      select(ANO_ELEICAO,
             NR_TURNO,
             SG_UF,
             CD_MUNICIPIO,
             CD_CARGO,
             DS_CARGO,
             QT_APTOS,
             QT_COMPARECIMENTO,
             QT_ABSTENCOES,
             QT_VOTOS_VALIDOS,
             QT_VOTOS_NOMINAIS,
             QT_VOTOS_LEGENDA,
             QT_VOTOS_BRANCOS,
             QT_VOTOS_NULOS,
             QT_VOTOS_ANULADOS) %>%
      group_by(ANO_ELEICAO,
               NR_TURNO,
               SG_UF,
               CD_MUNICIPIO,
               CD_CARGO,
               DS_CARGO) %>%
      summarise(QT_APTOS = sum(QT_APTOS),
                QT_COMPARECIMENTO = sum(QT_COMPARECIMENTO),
                QT_ABSTENCOES = sum(QT_ABSTENCOES),
                QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS),
                QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS),
                QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA),
                QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS),
                QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS),
                QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS))
    
    } else {
      
      temp <- temp %>%
        mutate(QT_VOTOS_VALIDOS = QT_VOTOS_NOMINAIS + QT_VOTOS_LEGENDA,
               NM_TIPO_ELEICAO = str_to_upper(NM_TIPO_ELEICAO)) %>% 
        filter(NM_TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA" |
               NM_TIPO_ELEICAO == "ELEICAO ORDINARIA") %>% 
        select(ANO_ELEICAO,
               NR_TURNO,
               SG_UF,
               CD_MUNICIPIO,
               CD_CARGO,
               DS_CARGO,
               QT_APTOS,
               QT_COMPARECIMENTO,
               QT_ABSTENCOES,
               QT_VOTOS_VALIDOS,
               QT_VOTOS_NOMINAIS,
               QT_VOTOS_LEGENDA,
               QT_VOTOS_BRANCOS,
               QT_VOTOS_NULOS,
               QT_VOTOS_ANULADOS) %>%
        group_by(ANO_ELEICAO,
                 NR_TURNO,
                 SG_UF,
                 CD_MUNICIPIO,
                 CD_CARGO,
                 DS_CARGO) %>%
        summarise(QT_APTOS = sum(QT_APTOS),
                  QT_COMPARECIMENTO = sum(QT_COMPARECIMENTO),
                  QT_ABSTENCOES = sum(QT_ABSTENCOES),
                  QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS),
                  QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS),
                  QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA),
                  QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS),
                  QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS),
                  QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS))
      
    }

    resumo_mun[[i]] <- temp

    }

## Empilhando os bancos

resumo_mun <- rbind.fill(resumo_mun)

## Salvando os dados já agregados

saveRDS(resumo_mun,
        "data/input/Resumo da Eleição/Eleições Municipais/resumo_mun.rds")

## Lendo arquivo já compilado

resumo_mun <- readRDS("data/input/Resumo da Eleição/Eleições Municipais/resumo_mun.rds")

### 1.1.2. Candidatos -------------------------------------------------------

#### 1.1.2.1. Eleições Gerais ------------------------------------------------

## URL dos dados

url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_ANO.zip"

## Lista com os anos das eleições gerais faltantes

anos <- seq(1998, 2022, by = 4)

## Loop que faz o download dos dados

for(i in anos){

  cat("Lendo", i, "\n")

  candidatos <- stringr::str_replace(url,
                                     "ANO",
                                     as.character(i))

  download.file(candidatos,
                str_c("candidatos_gr_",
                      i,
                      ".zip"))

}

## Cria uma lista com os nomes dos arquivos baixados

list_dados <- list.files(pattern = "candidatos_gr")

## Loop que unzipa os dados

for(i in seq_along(list_dados)){

  cat("Lendo", list_dados[i], "\n")

  unzip(list_dados[i],
        exdir = "data/input/Candidatos/Eleições Gerais")

  file.remove(list_dados[i])

}

## Cria uma lista com os nomes dos arquivos extraídos

list_dados <- list.files(path = "data/input/Candidatos/Eleições Gerais",
                         pattern = "consulta_cand")

## Remove o arquivo 'BRASIL' e 'BR'

for(i in seq_along(list_dados)){

  br_files_vagas <- list.files(path = "data/input/Candidatos/Eleições Gerais",
                               pattern = "BRASIL|BR",
                               full.names = TRUE)

  file.remove(br_files_vagas)

}

## Atualiza a lista de arquivos extraídos

list_dados <- list.files(path = "data/input/Candidatos/Eleições Gerais",
                         pattern = "consulta_cand")

## Cria uma lista vazia onde os dados serão armazenados

candidatos_gr <- list()

## For loop que lê os arquivos

for (i in seq_along(list_dados)) {

  cat("lendo", list_dados[i], "\n")

  temp <- read.table(file = paste0("data/input/Candidatos/Eleições Gerais/",
                                   list_dados[i]),
                     header = TRUE,
                     sep = ";",
                     stringsAsFactors = FALSE,
                     fileEncoding = "latin1")

  temp <- temp %>%
    mutate(NM_TIPO_ELEICAO = str_to_upper(NM_TIPO_ELEICAO)) %>% 
    filter(NM_TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA" |
           NM_TIPO_ELEICAO == "ELEICAO ORDINARIA") %>% 
    select(ANO_ELEICAO,
           NR_TURNO,
           SG_UF,
           CD_CARGO,
           DS_CARGO,
           NR_CANDIDATO,
           NM_CANDIDATO,
           NM_URNA_CANDIDATO,
           NR_CPF_CANDIDATO,
           NR_TITULO_ELEITORAL_CANDIDATO,
           DS_DETALHE_SITUACAO_CAND,
           TP_AGREMIACAO,
           NR_PARTIDO,
           SG_PARTIDO,
           SQ_COLIGACAO,
           NM_COLIGACAO,
           DS_COMPOSICAO_COLIGACAO,
           SG_UF_NASCIMENTO,
           NM_MUNICIPIO_NASCIMENTO,
           DT_NASCIMENTO,
           DS_GENERO,
           DS_COR_RACA,
           DS_GRAU_INSTRUCAO,
           DS_OCUPACAO,
           DS_SIT_TOT_TURNO)

  candidatos_gr[[i]] <- temp

}

## Juntando os arquivos

candidatos_gr <- rbind.fill(candidatos_gr)

## Salvando os dados já agregados

saveRDS(candidatos_gr,
        "data/input/Candidatos/Eleições Gerais/candidatos_gr.rds")

## Lendo arquivo já compilado

candidatos_gr <- readRDS("data/input/Candidatos/Eleições Gerais/candidatos_gr.rds")

#### 1.1.2.2. Eleições Municipais --------------------------------------------

## URL dos dados 

url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_ANO.zip"

## Lista com os anos das eleições municipais faltantes

anos <- seq(2000, 2020, by = 4)

## Loop que faz o download dos dados

for(i in anos){

  cat("Lendo", i, "\n")

  candidatos <- stringr::str_replace(url,
                                     "ANO",
                                     as.character(i))

  download.file(candidatos,
                str_c("candidatos_mun_",
                      i,
                      ".zip"))

}

## Cria uma lista com os nomes dos arquivos baixados

list_dados <- list.files(pattern = "candidatos_mun")

## Loop que unzipa os dados

for(i in seq_along(list_dados)){

  cat("Lendo", list_dados[i], "\n")

  unzip(list_dados[i],
        exdir = "data/input/Candidatos/Eleições Municipais")

 file.remove(list_dados[i])

}

## Cria uma lista com os nomes dos arquivos extraídos

list_dados <- list.files(path = "data/input/Candidatos/Eleições Municipais",
                         pattern = "consulta_cand")

## Remove o arquivo 'BRASIL'

for(i in seq_along(list_dados)){

  br_files_vagas <- list.files(path = "data/input/Candidatos/Eleições Municipais",
                               pattern = "BRASIL",
                               full.names = TRUE)

  file.remove(br_files_vagas)

}

## Atualiza a lista de arquivos extraídos

list_dados <- list.files(path = "data/input/Candidatos/Eleições Municipais",
                         pattern = "consulta_cand")

## Cria uma lista vazia onde os dados serão armazenados

candidatos_mun <- list()

## For loop que lê os arquivos

for (i in seq_along(list_dados)) {

  cat("lendo", list_dados[i], "\n")

  temp <- read.table(file = paste0("data/input/Candidatos/Eleições Municipais/",
                                   list_dados[i]),
                     header = TRUE,
                     sep = ";",
                     stringsAsFactors = FALSE,
                     fileEncoding = "latin1")

  temp <- temp %>%
    mutate(NM_TIPO_ELEICAO = str_to_upper(NM_TIPO_ELEICAO)) %>% 
    filter(NM_TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA" |
           NM_TIPO_ELEICAO == "ELEICAO ORDINARIA") %>% 
    select(ANO_ELEICAO,
           NR_TURNO,
           SG_UF,
           SG_UE,
           CD_CARGO,
           DS_CARGO,
           NR_CANDIDATO,
           NM_CANDIDATO,
           NM_URNA_CANDIDATO,
           NR_CPF_CANDIDATO,
           NR_TITULO_ELEITORAL_CANDIDATO,
           DS_SITUACAO_CANDIDATURA,
           DS_DETALHE_SITUACAO_CAND,
           TP_AGREMIACAO,
           NR_PARTIDO,
           SG_PARTIDO,
           SQ_COLIGACAO,
           NM_COLIGACAO,
           DS_COMPOSICAO_COLIGACAO,
           SG_UF_NASCIMENTO,
           NM_MUNICIPIO_NASCIMENTO,
           DT_NASCIMENTO,
           DS_GENERO,
           DS_COR_RACA,
           DS_GRAU_INSTRUCAO,
           DS_OCUPACAO,
           DS_SIT_TOT_TURNO)

  candidatos_mun[[i]] <- temp

}

## Juntando os arquivos

candidatos_mun <- rbind.fill(candidatos_mun)

## Salvando os dados já agregados

saveRDS(candidatos_mun,
        "data/input/Candidatos/Eleições Municipais/candidatos_mun.rds")

## Lendo arquivo já compilado

candidatos_mun <- readRDS("data/input/Candidatos/Eleições Municipais/candidatos_mun.rds")

### 1.1.3. Votos ------------------------------------------------------------

#### 1.1.3.1. Eleições Gerais ------------------------------------------------

## URL de referência

url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_ANO_UF.zip"

## Lista com os anos das eleições gerais faltantes

anos <- seq(1998, 2022, by = 4)

## Lista com as siglas dos estados brasileiros

ufs <- c("AC", "AL", "AM", "AP", "BA",
         "CE", "DF", "ES","GO", "MA", "MG",
         "MS", "MT", "PA", "PB", "PE", "PI",
         "PR", "RJ", "RN", "RO", "RR","RS",
         "SC", "SE", "SP", "TO")

## Loop que faz o download dos dados

for(i in anos){
  for(j in ufs){

    cat("Lendo", i, "\n")

    votacao_secao <- stringr::str_replace_all(url,
                                              c(ANO = i,
                                                UF = j))

    download.file(votacao_secao,
                  str_c("votacao_secao_gr_",
                        i,
                        "_",
                        j,
                        ".zip"))
  }
}

## Cria uma lista com os nomes dos arquivos baixados

list_dados <- list.files(pattern = "votacao_secao_gr")

## Loop que unzipa os dados

for(i in seq_along(list_dados)){

  cat("Lendo", list_dados[i], "\n")

  unzip(list_dados[i],
        exdir = "data/input/Votos/Eleições Gerais")

  file.remove(list_dados[i])

}

## Cria uma lista com os nomes dos arquivos extraídos

list_dados <- list.files(path = "data/input/Votos/Eleições Gerais",
                         pattern = "votacao_secao_")

## Cria uma lista vazia onde os dados serão armazenados

votos_gr <- list()

## For loop que lê os arquivos

for (i in seq_along(list_dados)) {

  cat("lendo", list_dados[i], "\n")

    temp <- read.table(file = paste0("data/input/Votos/Eleições Gerais/",
                                     list_dados[i]),
                       header = TRUE,
                       sep = ";",
                       stringsAsFactors = FALSE,
                       fileEncoding = "latin1")

    temp <- temp %>%
      mutate(NM_TIPO_ELEICAO = str_to_upper(NM_TIPO_ELEICAO)) %>% 
      filter(NM_TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA" |
             NM_TIPO_ELEICAO == "ELEICAO ORDINARIA") %>% 
      select(ANO_ELEICAO,
             NR_TURNO,
             SG_UF,
             CD_MUNICIPIO,
             CD_CARGO,
             DS_CARGO,
             NR_VOTAVEL,
             QT_VOTOS) %>%
      group_by(ANO_ELEICAO,
               NR_TURNO,
               SG_UF,
               CD_MUNICIPIO,
               CD_CARGO,
               DS_CARGO,
               NR_VOTAVEL) %>%
      summarise(QT_VOTOS = sum(QT_VOTOS))

    votos_gr[[i]] <- temp

    }

## Empilhando os bancos

votos_gr <- rbind.fill(votos_gr)

## Salvando os dados já agregados

saveRDS(votos_gr,
        "data/input/Votos/Eleições Gerais/votacao_secao_nom_part_gr.rds")

## Lendo os arquivos pré-processados

votos_gr <- readRDS("data/input/Votos/Eleições Gerais/votacao_secao_nom_part_gr.rds")

#### 1.1.3.2. Eleições Municipais --------------------------------------------

## URL de referência

url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_ANO_UF.zip"

## Lista com os anos das eleições gerais

anos <- seq(2000, 2020, by = 4)

anos <- 2008

## Lista com os estados brasileiros

ufs <- c("AC", "AL", "AM", "AP", "BA",
         "CE", "ES","GO", "MA", "MG",
         "MS", "MT", "PA", "PB", "PE", "PI",
         "PR", "RJ", "RN", "RO", "RR","RS",
         "SC", "SE", "SP", "TO")

ufs <- c("SP", "TO")

## Loop que faz o download dos dados

for(i in anos){
  for(j in ufs){

    cat("Lendo", i, "\n")

    votacao_secao <- stringr::str_replace_all(url,
                                              c(ANO = i,
                                                UF = j))

    download.file(votacao_secao,
                  str_c("votacao_secao_mun_",
                        i,
                        "_",
                        j,
                        ".zip"))
  }
}

## Cria uma lista com os nomes dos arquivos baixados

list_dados <- list.files(pattern = "votacao_secao_mun")

## Loop que unzipa os dados

for(i in seq_along(list_dados)){

  cat("Lendo", list_dados[i], "\n")

  unzip(list_dados[i],
        exdir = "data/input/Votos/Eleições Municipais")

  file.remove(list_dados[i])

}

## Cria uma lista com os nomes dos arquivos extraídos

list_dados <- list.files(path = "data/input/Votos/Eleições Municipais",
                         pattern = "votacao_secao_")

## Cria uma lista vazia onde os dados serão armazenados

votos_mun <- list()

## For loop que lê os arquivos

for (i in seq_along(list_dados)) {

  cat("lendo", list_dados[i], "\n")

    temp <- read.table(file = paste0("data/input/Votos/Eleições Municipais/",
                                     list_dados[i]),
                       header = TRUE,
                       sep = ";",
                       stringsAsFactors = FALSE,
                       fileEncoding = "latin1")


    temp <- temp %>%
      mutate(NM_TIPO_ELEICAO = str_to_upper(NM_TIPO_ELEICAO)) %>% 
      filter(NM_TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA" |
             NM_TIPO_ELEICAO == "ELEICAO ORDINARIA" |
             NM_TIPO_ELEICAO == "ELEIÇÕES 2008") %>% 
      select(ANO_ELEICAO,
             NR_TURNO,
             SG_UF,
             CD_MUNICIPIO,
             CD_CARGO,
             DS_CARGO,
             NR_VOTAVEL,
             QT_VOTOS) %>%
      group_by(ANO_ELEICAO,
               NR_TURNO,
               SG_UF,
               CD_MUNICIPIO,
               CD_CARGO,
               DS_CARGO,
               NR_VOTAVEL) %>%
      summarise(QT_VOTOS = sum(QT_VOTOS))

    votos_mun[[i]] <- temp

}

## Empilhando os dados

votos_mun <- rbind.fill(votos_mun)

## Salvando os dados já agregados

saveRDS(votos_mun,
        "data/input/Votos/Eleições Municipais/votacao_secao_nom_part_mun.rds")

## Lendo os arquivos pré-processados

votos_mun <- readRDS("data/input/Votos/Eleições Municipais/votacao_secao_nom_part_mun.rds")

# 2. Clean ----------------------------------------------------------------

## 2.1. TSE ----------------------------------------------------------------

### 2.1.1. Resumo da Eleição ------------------------------------------------

#### 2.1.1.1. Eleições Gerais ------------------------------------------------

## Renomeando as colunas e adequando ao formato adotado
## no CEPESP DATA

resumo_gr <- resumo_gr %>% 
  rename("NUM_TURNO" = "NR_TURNO",
         "UF" = "SG_UF",
         "CODIGO_CARGO" = "CD_CARGO",
         "DESCRICAO_CARGO" = "DS_CARGO",
         "QTD_APTOS" = "QT_APTOS",
         "QTD_COMPARECIMENTO" = "QT_COMPARECIMENTO",
         "QTD_ABSTENCOES" = "QT_ABSTENCOES") %>% 
  mutate(DESCRICAO_CARGO = str_to_upper(DESCRICAO_CARGO)) %>% 
  arrange(UF,
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
  summarise(QTD_APTOS = sum(QTD_APTOS,
                            na.rm = TRUE),
            QTD_COMPARECIMENTO = sum(QTD_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTD_ABSTENCOES = sum(QTD_ABSTENCOES,
                                 na.rm = TRUE),
            QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS,
                                 na.rm = TRUE),
            QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS,
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
  summarise(QTD_APTOS = sum(QTD_APTOS,
                            na.rm = TRUE),
            QTD_COMPARECIMENTO = sum(QTD_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTD_ABSTENCOES = sum(QTD_ABSTENCOES,
                                 na.rm = TRUE),
            QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS,
                                 na.rm = TRUE),
            QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS,
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
  summarise(QTD_APTOS = sum(QTD_APTOS,
                            na.rm = TRUE),
            QTD_COMPARECIMENTO = sum(QTD_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTD_ABSTENCOES = sum(QTD_ABSTENCOES,
                                 na.rm = TRUE),
            QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS,
                                 na.rm = TRUE),
            QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS,
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
  summarise(QTD_APTOS = sum(QTD_APTOS,
                            na.rm = TRUE),
            QTD_COMPARECIMENTO = sum(QTD_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTD_ABSTENCOES = sum(QTD_ABSTENCOES,
                                 na.rm = TRUE),
            QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS,
                                 na.rm = TRUE),
            QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS,
                                    na.rm = TRUE))

gov_uf_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO == "GOVERNADOR")

### DEPUTADO ESTADUAL ###

de_br_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO %in% c("DEPUTADO ESTADUAL",
                                "DEPUTADO DISTRITAL")) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO) %>% 
  summarise(QTD_APTOS = sum(QTD_APTOS,
                            na.rm = TRUE),
            QTD_COMPARECIMENTO = sum(QTD_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTD_ABSTENCOES = sum(QTD_ABSTENCOES,
                                 na.rm = TRUE),
            QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS,
                                 na.rm = TRUE),
            QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS,
                                    na.rm = TRUE))

de_uf_cons <- resumo_gr %>% 
  filter(DESCRICAO_CARGO %in% c("DEPUTADO ESTADUAL",
                                "DEPUTADO DISTRITAL"))

#### 2.1.1.2. Eleições Municipais --------------------------------------------

## Renomeando as colunas e adequando ao formato adotado
## no CEPESP DATA

resumo_mun <- resumo_mun %>% 
  rename("NUM_TURNO" = "NR_TURNO",
         "UF" = "SG_UF",
         "COD_MUN_TSE" = "CD_MUNICIPIO",
         "CODIGO_CARGO" = "CD_CARGO",
         "DESCRICAO_CARGO" = "DS_CARGO",
         "QTD_APTOS" = "QT_APTOS",
         "QTD_COMPARECIMENTO" = "QT_COMPARECIMENTO",
         "QTD_ABSTENCOES" = "QT_ABSTENCOES") %>% 
  mutate(DESCRICAO_CARGO = str_to_upper(DESCRICAO_CARGO)) %>% 
  arrange(UF,
          CODIGO_CARGO)

## Separando os dados em bases por cargo e agregação regional

### PREFEITO ###

pf_mun_cons <- resumo_mun %>% 
  mutate(COD_MUN_TSE = str_pad(COD_MUN_TSE, 
                               width = 5,
                               side = "left",
                               pad = "0")) %>% 
  filter(DESCRICAO_CARGO == "PREFEITO") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           UF,
           COD_MUN_TSE) %>% 
  summarise(QTD_APTOS = sum(QTD_APTOS,
                            na.rm = TRUE),
            QTD_COMPARECIMENTO = sum(QTD_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTD_ABSTENCOES = sum(QTD_ABSTENCOES,
                                 na.rm = TRUE),
            QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS,
                                 na.rm = TRUE),
            QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS,
                                    na.rm = TRUE))

### VEREADOR ###

vr_mun_cons <- resumo_mun %>% 
  mutate(COD_MUN_TSE = str_pad(COD_MUN_TSE, 
                               width = 5,
                               side = "left",
                               pad = "0")) %>% 
  filter(DESCRICAO_CARGO == "VEREADOR") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           UF,
           COD_MUN_TSE) %>% 
  summarise(QTD_APTOS = sum(QTD_APTOS,
                            na.rm = TRUE),
            QTD_COMPARECIMENTO = sum(QTD_COMPARECIMENTO,
                                     na.rm = TRUE),
            QTD_ABSTENCOES = sum(QTD_ABSTENCOES,
                                 na.rm = TRUE),
            QT_VOTOS_VALIDOS = sum(QT_VOTOS_VALIDOS,
                                   na.rm = TRUE),
            QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS,
                                    na.rm = TRUE),
            QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA,
                                   na.rm = TRUE),
            QT_VOTOS_BRANCOS = sum(QT_VOTOS_BRANCOS,
                                   na.rm = TRUE),
            QT_VOTOS_NULOS = sum(QT_VOTOS_NULOS,
                                 na.rm = TRUE),
            QT_VOTOS_ANULADOS = sum(QT_VOTOS_ANULADOS,
                                    na.rm = TRUE))

### 2.1.2. Candidatos e Votos -----------------------------------------------

#### 2.1.2.1. Eleições Gerais ------------------------------------------------

## Renomeando as colunas e adequando ao formato adotado
## no CEPESP DATA

candidatos_gr <- candidatos_gr %>% 
  filter(DS_CARGO %in% c("SENADOR",
                         "DEPUTADO FEDERAL",
                         "DEPUTADO ESTADUAL",
                         "DEPUTADO DISTRITAL") &
         DS_SIT_TOT_TURNO != "#NULO#") %>% 
  rename("NUM_TURNO" = "NR_TURNO",
         "UF" = "SG_UF",
         "CODIGO_CARGO" = "CD_CARGO",
         "DESCRICAO_CARGO" = "DS_CARGO",
         "NUMERO_CANDIDATO" = "NR_CANDIDATO",
         "NOME_CANDIDATO" = "NM_CANDIDATO",
         "NOME_URNA_CANDIDATO" = "NM_URNA_CANDIDATO",
         "CPF_CANDIDATO" = "NR_CPF_CANDIDATO",
         "NUM_TITULO_ELEITORAL_CANDIDATO" = "NR_TITULO_ELEITORAL_CANDIDATO",
         "DATA_NASCIMENTO" = "DT_NASCIMENTO",
         "SIGLA_UF_NASCIMENTO" = "SG_UF_NASCIMENTO",
         "NOME_MUNICIPIO_NASCIMENTO" = "NM_MUNICIPIO_NASCIMENTO",
         "DESCRICAO_SEXO" = "DS_GENERO",
         "DESCRICAO_COR_RACA" = "DS_COR_RACA",
         "DESCRICAO_GRAU_INSTRUCAO" = "DS_GRAU_INSTRUCAO",
         "DESCRICAO_OCUPACAO" = "DS_OCUPACAO",
         "NUMERO_PARTIDO" = "NR_PARTIDO",
         "SIGLA_PARTIDO" = "SG_PARTIDO",
         "TIPO_LEGENDA" = "TP_AGREMIACAO",
         "CODIGO_LEGENDA" = "SQ_COLIGACAO",
         "SIGLA_LEGENDA" = "NM_COLIGACAO",
         "COMPOSICAO_LEGENDA" = "DS_COMPOSICAO_COLIGACAO",
         "DES_SITUACAO_CANDIDATURA" = "DS_DETALHE_SITUACAO_CAND",
         "DESC_SIT_TOT_TURNO" = "DS_SIT_TOT_TURNO") %>% 
  mutate(DESCRICAO_CARGO = str_to_upper(DESCRICAO_CARGO),
         SIGLA_LEGENDA = str_to_upper(SIGLA_LEGENDA)) %>% 
  arrange(ANO_ELEICAO,
          UF,
          CODIGO_CARGO)

votos_gr <- votos_gr %>%
  filter(!NR_VOTAVEL %in% c(95, 96, 97)) %>% 
  group_by(ANO_ELEICAO,
           NR_TURNO,
           SG_UF,
           CD_CARGO,
           DS_CARGO,
           NR_VOTAVEL) %>% 
  summarise(QT_VOTOS = sum(QT_VOTOS,
                           na.rm = TRUE)) %>% 
  rename("NUM_TURNO" = "NR_TURNO",
         "UF" = "SG_UF",
         "CODIGO_CARGO" = "CD_CARGO",
         "DESCRICAO_CARGO" = "DS_CARGO",
         "NUMERO_CANDIDATO" = "NR_VOTAVEL",
         "QTDE_VOTOS" = "QT_VOTOS") %>% 
  mutate(DESCRICAO_CARGO = str_to_upper(DESCRICAO_CARGO)) %>% 
  arrange(ANO_ELEICAO,
          UF,
          CODIGO_CARGO,
          NUMERO_CANDIDATO)

## Realizando o join entre as duas bases e
## organizando as informações

candidatos_gr <- right_join(candidatos_gr,
                            votos_gr) %>% 
  filter(is.na(NOME_CANDIDATO) &
         nchar(NUMERO_CANDIDATO) == 2 |
         !is.na(NOME_CANDIDATO)) %>% 
  filter(!DESCRICAO_CARGO %in% c("CONSELHEIRO DISTRITAL",
                                 "GOVERNADOR")) %>% 
  mutate(NUMERO_PARTIDO = str_sub(NUMERO_CANDIDATO,
                                  start = 1,
                                  end = 2),
         CPF_CANDIDATO = ifelse(CPF_CANDIDATO == "-4",
                                "00000000000",
                                CPF_CANDIDATO),
         NUM_TITULO_ELEITORAL_CANDIDATO = ifelse(NUM_TITULO_ELEITORAL_CANDIDATO == "-4",
                                                 "00000000000",
                                                 NUM_TITULO_ELEITORAL_CANDIDATO),
         DATA_NASCIMENTO = as.Date(DATA_NASCIMENTO,
                                   format = "%d/%m/%Y")) %>% 
  mutate(CPF_CANDIDATO = cpp_cpf(CPF_CANDIDATO),
         NUM_TITULO_ELEITORAL_CANDIDATO = cpp_titulo(NUM_TITULO_ELEITORAL_CANDIDATO))

## Separando os dados em bases por cargo, agregação regional e
## agregação política

### SENADOR ###

sen_uf_cand <- candidatos_gr %>% 
  filter(DESCRICAO_CARGO == "SENADOR")

sen_uf_part <- candidatos_gr %>% 
  filter(DESCRICAO_CARGO == "SENADOR") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           UF,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_UF = sum(QTDE_VOTOS,
                             na.rm = FALSE)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           NUMERO_PARTIDO) %>% 
  mutate(VOT_PART_BR = sum(VOT_PART_UF,
                          na.rm = TRUE))

### DEPUTADO FEDERAL ###

df_uf_cand <- candidatos_gr %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL" & 
         nchar(NUMERO_CANDIDATO) > 2)

df_uf_part <- candidatos_gr %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           UF,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_UF = sum(QTDE_VOTOS,
                              na.rm = FALSE)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           NUMERO_PARTIDO) %>% 
  mutate(VOT_PART_BR = sum(VOT_PART_UF,
                           na.rm = TRUE))

### DEPUTADO ESTADUAL ###

de_uf_cand <- candidatos_gr %>% 
  filter(DESCRICAO_CARGO %in% c("DEPUTADO ESTADUAL",
                                "DEPUTADO DISTRITAL") & 
         nchar(NUMERO_CANDIDATO) > 2)

de_uf_part <- candidatos_gr %>% 
  filter(DESCRICAO_CARGO %in% c("DEPUTADO ESTADUAL",
                                "DEPUTADO DISTRITAL")) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           UF,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_UF = sum(QTDE_VOTOS,
                              na.rm = FALSE)) %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           NUMERO_PARTIDO) %>% 
  mutate(VOT_PART_BR = sum(VOT_PART_UF,
                           na.rm = TRUE))

#### 2.1.2.2. Eleições Municipais --------------------------------------------

## Renomeando as colunas e adequando ao formato adotado
## no CEPESP DATA

candidatos_mun <- candidatos_mun %>% 
  filter(DS_CARGO %in% c("PREFEITO",
                         "VEREADOR") &
         DS_SIT_TOT_TURNO != "#NULO#" &
         DS_SITUACAO_CANDIDATURA %in% c("APTO",
                                        "DEFERIDO")) %>% 
  rename("NUM_TURNO" = "NR_TURNO",
         "UF" = "SG_UF",
         "COD_MUN_TSE" = "SG_UE",
         "CODIGO_CARGO" = "CD_CARGO",
         "DESCRICAO_CARGO" = "DS_CARGO",
         "NUMERO_CANDIDATO" = "NR_CANDIDATO",
         "NOME_CANDIDATO" = "NM_CANDIDATO",
         "NOME_URNA_CANDIDATO" = "NM_URNA_CANDIDATO",
         "CPF_CANDIDATO" = "NR_CPF_CANDIDATO",
         "NUM_TITULO_ELEITORAL_CANDIDATO" = "NR_TITULO_ELEITORAL_CANDIDATO",
         "DATA_NASCIMENTO" = "DT_NASCIMENTO",
         "SIGLA_UF_NASCIMENTO" = "SG_UF_NASCIMENTO",
         "NOME_MUNICIPIO_NASCIMENTO" = "NM_MUNICIPIO_NASCIMENTO",
         "DESCRICAO_SEXO" = "DS_GENERO",
         "DESCRICAO_COR_RACA" = "DS_COR_RACA",
         "DESCRICAO_GRAU_INSTRUCAO" = "DS_GRAU_INSTRUCAO",
         "DESCRICAO_OCUPACAO" = "DS_OCUPACAO",
         "NUMERO_PARTIDO" = "NR_PARTIDO",
         "SIGLA_PARTIDO" = "SG_PARTIDO",
         "TIPO_LEGENDA" = "TP_AGREMIACAO",
         "CODIGO_LEGENDA" = "SQ_COLIGACAO",
         "SIGLA_LEGENDA" = "NM_COLIGACAO",
         "COMPOSICAO_LEGENDA" = "DS_COMPOSICAO_COLIGACAO",
         "DES_SITUACAO_CANDIDATURA" = "DS_DETALHE_SITUACAO_CAND",
         "DESC_SIT_TOT_TURNO" = "DS_SIT_TOT_TURNO") %>%  
  mutate(DESCRICAO_CARGO = str_to_upper(DESCRICAO_CARGO),
         SIGLA_LEGENDA = str_to_upper(SIGLA_LEGENDA),
         COD_MUN_TSE = str_pad(COD_MUN_TSE, 
                               width = 5,
                               side = "left",
                               pad = "0")) %>% 
  arrange(ANO_ELEICAO,
          UF,
          COD_MUN_TSE,
          CODIGO_CARGO) 

votos_mun <- votos_mun %>%
  filter(!NR_VOTAVEL %in% c(95, 96, 97)) %>% 
  group_by(ANO_ELEICAO,
           NR_TURNO,
           SG_UF,
           CD_MUNICIPIO,
           CD_CARGO,
           DS_CARGO,
           NR_VOTAVEL) %>% 
  summarise(QT_VOTOS = sum(QT_VOTOS,
                           na.rm = TRUE)) %>% 
  rename("NUM_TURNO" = "NR_TURNO",
         "UF" = "SG_UF",
         "COD_MUN_TSE" = "CD_MUNICIPIO",
         "CODIGO_CARGO" = "CD_CARGO",
         "DESCRICAO_CARGO" = "DS_CARGO",
         "NUMERO_CANDIDATO" = "NR_VOTAVEL",
         "QTDE_VOTOS" = "QT_VOTOS") %>% 
  mutate(DESCRICAO_CARGO = str_to_upper(DESCRICAO_CARGO),
         COD_MUN_TSE = str_pad(COD_MUN_TSE, 
                               width = 5,
                               side = "left",
                               pad = "0")) %>% 
  arrange(ANO_ELEICAO,
          UF,
          COD_MUN_TSE,
          CODIGO_CARGO,
          NUMERO_CANDIDATO)

## Realizando o join entre as duas bases e
## organizando as informações

candidatos_mun <- right_join(candidatos_mun,
                             votos_mun) %>% 
  filter((is.na(NOME_CANDIDATO) &
           nchar(NUMERO_CANDIDATO) == 2) |
           !is.na(NOME_CANDIDATO)) %>% 
  mutate(NUMERO_PARTIDO = str_sub(NUMERO_CANDIDATO,
                                  start = 1,
                                  end = 2),
         CPF_CANDIDATO = ifelse(CPF_CANDIDATO == "-4",
                                "00000000000",
                                CPF_CANDIDATO),
         NUM_TITULO_ELEITORAL_CANDIDATO = ifelse(NUM_TITULO_ELEITORAL_CANDIDATO == "-4",
                                                 "00000000000",
                                                 NUM_TITULO_ELEITORAL_CANDIDATO),
         DATA_NASCIMENTO = as.Date(DATA_NASCIMENTO,
                                 format = "%d/%m/%Y")) %>% 
  mutate(CPF_CANDIDATO = cpp_cpf(CPF_CANDIDATO),
         NUM_TITULO_ELEITORAL_CANDIDATO = cpp_titulo(NUM_TITULO_ELEITORAL_CANDIDATO))

## Separando os dados em bases por cargo, agregação regional e
## agregação política

### PREFEITO ###

pf_mun_cand <- candidatos_mun %>% 
  filter(DESCRICAO_CARGO == "PREFEITO")

pf_mun_part <- candidatos_mun %>% 
  filter(DESCRICAO_CARGO == "PREFEITO") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           UF,
           COD_MUN_TSE,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_MUN = sum(QTDE_VOTOS,
                               na.rm = FALSE))

### VEREADOR ###

vr_mun_cand <- candidatos_mun %>% 
  filter(DESCRICAO_CARGO == "VEREADOR" & 
         nchar(NUMERO_CANDIDATO) > 2)

vr_mun_part <- candidatos_mun %>% 
  filter(DESCRICAO_CARGO == "VEREADOR") %>% 
  group_by(ANO_ELEICAO,
           NUM_TURNO,
           CODIGO_CARGO,
           DESCRICAO_CARGO,
           UF,
           COD_MUN_TSE,
           NUMERO_PARTIDO) %>% 
  summarise(VOT_PART_MUN = sum(QTDE_VOTOS,
                               na.rm = FALSE))

## Remove os arquivos que não serão mais utilizados

rm(resumo_gr, resumo_mun,
   candidatos_gr, candidatos_mun,
   votos_gr, votos_mun)

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
  pivot_longer(cols = `1998`:`2022`, 
               names_to = "ANO_ELEICAO",
               values_to = "QT_VAGAS") %>%
  mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) %>%
  select(ANO_ELEICAO,
         UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QT_VAGAS) %>% 
  arrange(ANO_ELEICAO,
          UF)

vagas_dep <- vagas_dep %>% 
  pivot_longer(cols = `1998`:`2022`, 
               names_to = "ANO_ELEICAO",
               values_to = "QT_VAGAS") %>%
  mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) %>% 
  select(ANO_ELEICAO,
         UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QT_VAGAS) %>% 
  arrange(ANO_ELEICAO,
          UF)

## Realizando o join com o resumo das eleições

### SENADOR ###

sen_uf_cons <- left_join(sen_uf_cons,
                         vagas_sen) %>% 
  select(ANO_ELEICAO:DESCRICAO_CARGO,
         QT_VAGAS,
         QTD_APTOS:QT_VOTOS_ANULADOS)

### DEPUTADO FEDERAL ###

df_uf_cons <- left_join(df_uf_cons,
                        vagas_dep) %>% 
  select(ANO_ELEICAO:DESCRICAO_CARGO,
         QT_VAGAS,
         QTD_APTOS:QT_VOTOS_ANULADOS) %>% 
  arrange(ANO_ELEICAO,
          UF,
          CODIGO_CARGO)

### DEPUTADO ESTADUAL ###

de_uf_cons <- left_join(de_uf_cons,
                        vagas_dep) %>% 
  select(ANO_ELEICAO:DESCRICAO_CARGO,
         QT_VAGAS,
         QTD_APTOS:QT_VOTOS_ANULADOS) %>% 
  arrange(ANO_ELEICAO,
          UF,
          CODIGO_CARGO)

### 3.2.2. Eleições Municipais ----------------------------------------------

## Alterando os arquivos de vagas para o formato long

vagas_ver <- vagas_ver %>% 
  pivot_longer(cols = `2000`:`2020`, 
               names_to = "ANO_ELEICAO",
               values_to = "QT_VAGAS") %>% 
  mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) %>%
  select(ANO_ELEICAO,
         UF,
         COD_MUN_TSE,
         COD_MUN_IBGE,
         NOME_MUNICIPIO,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QT_VAGAS) %>% 
  arrange(ANO_ELEICAO,
          UF,
          NOME_MUNICIPIO)

## Realizando o join com o resumo das eleições

### VEREADOR ###

vr_mun_cons <- left_join(vr_mun_cons,
                         vagas_ver) %>% 
  ungroup() %>% 
  select(ANO_ELEICAO:COD_MUN_TSE,
         QT_VAGAS,
         QTD_APTOS:QT_VOTOS_ANULADOS) %>% 
  arrange(ANO_ELEICAO,
          UF,
          COD_MUN_TSE,
          CODIGO_CARGO)

## 3.3. Candidatos ---------------------------------------------------------

### 3.3.1. Eleições Gerais --------------------------------------------------

## Realizando o join com os candidatos

### SENADOR ###

sen_uf_cand <- right_join(sen_br_cons,
                          sen_uf_cand) %>% 
  filter(NOME_CANDIDATO != "")

### DEPUTADO FEDERAL ###

df_uf_cand <- right_join(df_br_cons,
                         df_uf_cand) %>% 
  filter(NOME_CANDIDATO != "") %>% 
  select(ANO_ELEICAO,
         NUM_TURNO,
         UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QT_VOTOS_VALIDOS,
         NUMERO_CANDIDATO:VOT_PART_BR) %>% 
  rename("QT_VOTOS_VALIDOS_BR" = "QT_VOTOS_VALIDOS")

df_uf_cand <- right_join(df_uf_cons,
                         df_uf_cand) %>% 
  filter(NOME_CANDIDATO != "")

### DEPUTADO ESTADUAL ###

de_uf_cand <- right_join(de_br_cons,
                         de_uf_cand) %>% 
  filter(NOME_CANDIDATO != "") %>% 
  select(ANO_ELEICAO,
         NUM_TURNO,
         UF,
         CODIGO_CARGO,
         DESCRICAO_CARGO,
         QT_VOTOS_VALIDOS,
         NUMERO_CANDIDATO:VOT_PART_BR) %>% 
  rename("QT_VOTOS_VALIDOS_BR" = "QT_VOTOS_VALIDOS")

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
         QT_VAGAS,
         QTD_APTOS:QT_VOTOS_ANULADOS) %>% 
  mutate(QT_VAGAS = ifelse(is.na(QT_VAGAS),
                           1,
                           QT_VAGAS))

## 4.3. Município ----------------------------------------------------------

## Consolida os resumo das eleições com agregação 'Município' em um único
## banco de dados 

cons_mun <- rbind.fill(pf_mun_cons, 
                       vr_mun_cons) %>% 
  select(ANO_ELEICAO:DESCRICAO_CARGO,
         QT_VAGAS,
         QTD_APTOS:QT_VOTOS_ANULADOS) %>% 
  mutate(QT_VAGAS = ifelse(is.na(QT_VAGAS),
                           1,
                           QT_VAGAS))

## Remove da área de trabalho os bancos que não serão mais utilizados

rm(de_br_cons, df_br_cons,
   sen_br_cons, sen_uf_cons,
   gov_br_cons, gov_uf_cons, 
   pr_br_cons, pr_uf_cons)

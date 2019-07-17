

# Pacotes utilizados

library(dplyr)
library(lubridate)
library(tidyverse)


# Objetivo
#'        - Calcular os indicadores de fragmentacao legislativa:
#'        - Numero de cadeiras,Fracionalizacao,Fragmentacao, Fragmentacao máxima,
#'        - Desproporcionalidade de Gallagher, Numero efetivo 
#'              de partidos por votos e por cadeiras;
#'        - Limpeza e padronizacao dos dados.             
        


# 1. Numero de cadeiras ---------------------------------------------------------


## Filtra os candidatos que foram eleitos

### Deputado Federal

df <- df %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

### Deputado Estadual

de <- de %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
         DESC_SIT_TOT_TURNO == "ELEITO POR QP"|
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

## Soma as cadeiras conquistadas pelos partidos em cada UF

### Deputado Federal

df <- df %>% 
  dplyr::group_by(ANO_ELEICAO,
                  DESCRICAO_CARGO, 
                  SIGLA_PARTIDO, 
                  UF) %>% 
  dplyr::summarise("Cadeiras conquistadas por UF" = n())

### Deputado Estadual

de <- de %>% 
  dplyr::group_by(ANO_ELEICAO, 
                  DESCRICAO_CARGO, 
                  SIGLA_PARTIDO, 
                  UF) %>% 
  dplyr::summarise("Cadeiras conquistadas" = n())

## Soma o total de cadeiras conquistadas pelos partidos em cada eleicao

### Deputado Federal

df1 <- df %>% 
  dplyr::group_by(ANO_ELEICAO,
                  DESCRICAO_CARGO,
                  SIGLA_PARTIDO) %>% 
  dplyr::summarise(
    "Total de cadeiras conquistadas" = 
      sum(`Cadeiras conquistadas por UF`))

## Junta os bancos de cadeiras conquistadas por UF com o total de cadeiras conquistadas

### Deputado Federal

df <- left_join(df, df1, 
                by = c("ANO_ELEICAO", 
                       "DESCRICAO_CARGO",
                       "SIGLA_PARTIDO"))

## Percentual de cadeiras conquistas pelos partidos

### Deputado Federal

df1$`Percentual de cadeiras conquistadas` <- df1$`Total de cadeiras conquistadas`/513

## Elimina colunas desnecessarias, renomeia as colunas restantes e padroniza-as

### Deputado Federal

df1 <- df1 %>% 
   dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "DESCRICAO_CARGO", 
                "Sigla do partido" = "SIGLA_PARTIDO")

df1$Cargo <- str_to_title(df1$Cargo)

### Deputado Estadual

de <- de %>% 
    dplyr::rename("Ano da eleição" = "ANO_ELEICAO", 
                "Cargo" = "DESCRICAO_CARGO", 
                "Sigla do partido" = "SIGLA_PARTIDO") 

de$Cargo <- str_to_title(de$Cargo)


# 2. Fracionalizacao ------------------------------------------------------

## Funcao para o calculo da fracionalizacao

fracio <- function(x){
  
  1-(sum(x^2))
}

## Calculo do indice de fracionalizacao em cada eleicao

t98df <- df1 %>% 
  filter(`Ano da eleição` == 1998) 

t98df$Fracionalização <- fracio(t98df$`Percentual de cadeiras conquistadas`)

t02df <- df1 %>% 
  filter(`Ano da eleição` == 2002) 

t02df$Fracionalização <- fracio(t02df$`Percentual de cadeiras conquistadas`)

t06df <- df1 %>% 
  filter(`Ano da eleição` == 2006) 

t06df$Fracionalização <- fracio(t06df$`Percentual de cadeiras conquistadas`)

t10df <- df1 %>% 
  filter(`Ano da eleição` == 2010) 

t10df$Fracionalização <- fracio(t10df$`Percentual de cadeiras conquistadas`)

t14df <- df1 %>% 
  filter(`Ano da eleição` == 2014)

t14df$Fracionalização <- fracio(t14df$`Percentual de cadeiras conquistadas`)

t18df <- df1 %>% 
  filter(`Ano da eleição` == 2018) 

t18df$Fracionalização <- fracio(t18df$`Percentual de cadeiras conquistadas`)


# 3. Fracionalizacao maxima -----------------------------------------------

## Funcao para o calculo da fracionalizacao maxima

fracio_max <- function(N, n){
  
  (N*(n-1))/(n*(N-1))
  
}

## Calculo do indice de fracionalizacao maxima em cada eleicao

t98df$`Fracionalização máxima`<- fracio_max(513,18)

t02df$`Fracionalização máxima`<- fracio_max(513,19) 

t06df$`Fracionalização máxima`<- fracio_max(513,21)

t10df$`Fracionalização máxima`<- fracio_max(513,22)

t14df$`Fracionalização máxima`<- fracio_max(513,28)

t18df$`Fracionalização máxima`<- fracio_max(513,30)


# 4. Fragmentacao ---------------------------------------------------------

## Funcao para o calculo da fragmentacao

frag <- function(fracio, fracio_max){
  
  fracio/fracio_max
}

## Calculo do indice de fragmentacao em cada eleicao

t98df$Fragmentação <- frag(t98df$Fracionalização, t98df$`Fracionalização máxima`)

t02df$Fragmentação <- frag(t02df$Fracionalização, t02df$`Fracionalização máxima`)

t06df$Fragmentação <- frag(t06df$Fracionalização, t06df$`Fracionalização máxima`)

t10df$Fragmentação <- frag(t10df$Fracionalização, t10df$`Fracionalização máxima`)

t14df$Fragmentação <- frag(t14df$Fracionalização, t14df$`Fracionalização máxima`)

t18df$Fragmentação <- frag(t18df$Fracionalização, t18df$`Fracionalização máxima`)



# 5. Desproporcionalidade de Gallagher ------------------------------------




# 6. Numero efetivo de partidos ---------------------------------


# 6.1. Por votos ----------------------------------------------------------


# 6.2. Por cadeiras -------------------------------------------------------

## Funcao para o calculo do numero efetivo de partidos (cadeiras)

options(scipen=999)

NEP<-NA

NEPC <- function(p){
  for(i in 1:length(p)){
    NEP[[i]]<-(p[[i]]*p[[i]])
  }
  1/sum(NEP)}

## Calculo do numero efetivo de partidos por cadeiras

t98df$`Número efetivo de partidos por cadeiras` <- NEPC(t98df$`Percentual de cadeiras conquistadas`)

t02df$`Número efetivo de partidos por cadeiras` <- NEPC(t02df$`Percentual de cadeiras conquistadas`)

t06df$`Número efetivo de partidos por cadeiras` <- NEPC(t06df$`Percentual de cadeiras conquistadas`)

t10df$`Número efetivo de partidos por cadeiras` <- NEPC(t10df$`Percentual de cadeiras conquistadas`)

t14df$`Número efetivo de partidos por cadeiras` <- NEPC(t14df$`Percentual de cadeiras conquistadas`)

t18df$`Número efetivo de partidos por cadeiras` <- NEPC(t18df$`Percentual de cadeiras conquistadas`)



# 7. Padronizacao dos dados -----------------------------------------------

## Junta todos bancos de fragmentacao partidaria em um único

frag_part_fed <- bind_rows(t98df, t02df, t06df, t10df, t14df, t18df) 

## Arredonda em duas casas decimas os indices calculados

frag_part_fed$Fracionalização <- 
  format(round(frag_part_fed$Fracionalização, 
               digits = 2),  
         nsmall = 2)

frag_part_fed$`Fracionalização máxima` <- 
  format(round(frag_part_fed$`Fracionalização máxima`, 
               digits = 2), 
         nsmall = 2)

frag_part_fed$Fragmentação <- 
  format(round(frag_part_fed$Fragmentação, 
               digits = 2),
         nsmall = 2)

frag_part_fed$`Número efetivo de partidos por cadeiras` <- 
  format(round(frag_part_fed$`Número efetivo de partidos por cadeiras`, 
               digits = 2), 
         nsmall = 2)


# 8. Salva o arquivo ------------------------------------------------------

## Salva os arquivos referentes aos indicadores de fragmentacao
## partidaria em .csv

## Deputado Federal

write.csv(frag_part_fed, "data/output/frag_part_fed.csv")

## Remove da area de trabalho os bancos que nao serao mais utilizados

rm(t98df,t02df,t06df,t10df,t14df,t18df,df1,frag_part_fed,df,de)

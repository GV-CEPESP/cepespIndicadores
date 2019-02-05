

# Dados dos indicadores

rm(list = ls())


# Pacotes utilizados

library(cepespR)
library(dplyr)
library(tidyverse)
library(lubridate)


# 1. Dados ----------------------------------------------------------------

# Deputado Federal

df <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Federal",
                    regional_aggregation = "Brasil", political_aggregation = "Partido")

dfc <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Federal",
                     regional_aggregation = "Brasil", political_aggregation = "Consolidado")

# Deputado Estadual

de <- get_elections(year = "1998, 2002, 2006, 2010, 2014, 2018", position = "Deputado Estadual",
                    regional_aggregation = "Estado", political_aggregation = "Partido")

dec <- get_elections(year = "1998,2002, 2006, 2010, 2014, 2018", position = "Deputado Estadual",
                     regional_aggregation = "Estado", political_aggregation = "Consolidado")

# Vereador


vr <- get_elections(year = "2000, 2004, 2008, 2012, 2016", position = "Vereador",
                    regional_aggregation = "Municipio", political_aggregation = "Partido")

vrc <- get_elections(year = "2000, 2004, 2008, 2012, 2016", position = "Vereador",
                     regional_aggregation = "Municipio", political_aggregation = "Consolidado")




cidades <- count(vr, c("UF", "NOME_MUNICIPIO"))

cidades <- select(cidades, "UF", "NOME_MUNICIPIO")


# 2. Tranformacoes primarias ----------------------------------------------

 
df <- rename(df, UF = SIGLA_UE)

df$AGREGACAO_REGIONAL <- "BRASIL"

df <- df %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS)

de$AGREGACAO_REGIONAL <- "UF"

de <- de %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS) 

vr$AGREGACAO_REGIONAL <- "MUNICIPIO"

vr <- vr %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL,NOME_MUNICIPIO,DESCRICAO_CARGO,NUMERO_PARTIDO, SIGLA_PARTIDO, QTDE_VOTOS)

# Votação total e por UF dos partidos

df1 <- df %>%  
  group_by(ANO_ELEICAO, UF,SIGLA_PARTIDO) %>% 
  summarise(
    VOT_PART_UF = sum(QTDE_VOTOS))


df2 <- df1 %>% 
  group_by(ANO_ELEICAO,SIGLA_PARTIDO) %>% 
  summarise(
    TOT_VOT_PART = sum(VOT_PART_UF)
  )

de1 <- de %>% 
  group_by(ANO_ELEICAO, UF,SIGLA_PARTIDO) %>% 
  summarise(
    VOT_PART_UF = sum(QTDE_VOTOS)
  ) 

de2 <- de1 %>% 
  group_by(ANO_ELEICAO,SIGLA_PARTIDO) %>% 
  summarise(
    TOT_VOT_PART = sum(VOT_PART_UF)
  )

vr1 <- vr %>% 
  group_by(ANO_ELEICAO, NOME_MUNICIPIO,SIGLA_PARTIDO) %>% 
  summarise(
    VOT_PART_MUN = sum(QTDE_VOTOS)
  ) 

vr2 <- vr1 %>% 
  group_by(ANO_ELEICAO,SIGLA_PARTIDO) %>% 
  summarise(
    TOT_VOT_PART = sum(VOT_PART_MUN)
  )

# Votos validos de cada eleicao

# Deputado Federal

dfc1 <- dfc %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(
    VOTOS_VALIDOS = sum(QT_VOTOS_NOMINAIS,QT_VOTOS_LEGENDA)
  )

# Deputado Estadual

dec1 <- dec %>% 
  group_by(ANO_ELEICAO,UF) %>% 
  summarise(
    VOTOS_VALIDOS_UF = sum(QT_VOTOS_NOMINAIS,QT_VOTOS_LEGENDA)
  )

# Vereador

vrc1 <- vrc %>% 
  group_by(ANO_ELEICAO)



# 3. Join -----------------------------------------------------------------

# Deputado Federal

df <- left_join(df, dfc, by = c("ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_CARGO")) 

df <- df %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF,AGREGACAO_REGIONAL, DESCRICAO_CARGO, NUMERO_PARTIDO,SIGLA_PARTIDO,
         QTDE_VOTOS, QTD_APTOS,QTD_COMPARECIMENTO,QTD_ABSTENCOES,QT_VOTOS_NOMINAIS,
         QT_VOTOS_BRANCOS, QT_VOTOS_NULOS,QT_VOTOS_LEGENDA, QT_VOTOS_ANULADOS_APU_SEP)


df <- left_join(df, df1, by = c("ANO_ELEICAO", "UF", "SIGLA_PARTIDO"))

df <- left_join(df, df2, by = c("ANO_ELEICAO", "SIGLA_PARTIDO"))

df <- left_join(df,dfc1, by = "ANO_ELEICAO")

# Deputado Estadual

de <- left_join(de, dec, by = c("ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_CARGO","UF")) 

de <- de %>% 
  select(ANO_ELEICAO, NUM_TURNO, UF, AGREGACAO_REGIONAL,DESCRICAO_CARGO, NUMERO_PARTIDO,SIGLA_PARTIDO,
         QTDE_VOTOS, QTD_APTOS,QTD_COMPARECIMENTO,QTD_ABSTENCOES,QT_VOTOS_NOMINAIS,
         QT_VOTOS_BRANCOS, QT_VOTOS_NULOS,QT_VOTOS_LEGENDA, QT_VOTOS_ANULADOS_APU_SEP)


de <- left_join(de, de1, by = c("ANO_ELEICAO", "UF", "SIGLA_PARTIDO"))

de <- left_join(de, de2, by = c("ANO_ELEICAO", "SIGLA_PARTIDO"))


de <- left_join(de,dec1, by = c("ANO_ELEICAO","UF"))

# Vereador

vr <- left_join(vr, vr1, by = c("ANO_ELEICAO", "NOME_MUNICIPIO", "SIGLA_PARTIDO"))



# 4. Salvando os bancos ---------------------------------------------------

write.csv(df, "df.csv")

write.csv(de, "de.csv")

write.csv(cidades, "cidades.csv")

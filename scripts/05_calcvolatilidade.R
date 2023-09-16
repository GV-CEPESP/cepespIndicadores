
## OBJETIVOS

#'        - Calcular os indicadores de Volatilidade:
#'        
#'        - 1. Volatilidade Eleitoral; e
#'        - 2. Volatilidade Parlamentar.

#'        - Limpeza e padronização dos dados gerados.

# 1. Indicadores ----------------------------------------------------------

## 1.1. Eleições Gerais ----------------------------------------------------

### 1.1.1. Brasil -----------------------------------------------------------

#### 1.1.1.1. Deputado Federal -----------------------------------------------


#### 1.1.1.2. Deputado Estadual ----------------------------------------------


### 1.1.2. Estado -----------------------------------------------------------


#### 1.1.2.1. Deputado Federal -----------------------------------------------



#### 1.1.2.2. Deputado Estadual ----------------------------------------------




# 2.1. Deputado Federal ---------------------------------------------------


# 2.1.1. Brasil -----------------------------------------------------------

## Vector com os anos das eleicoes gerais

anos <- c(1998, 2002, 2006, 2010, 2014)

## Cria o banco onde os indicadores serao armazenados

ind_eleicoes_fed_br <- list()

## For loop que calcula os indicadores de volatilidade 

for(ano in anos){
  
  cat("Lendo",ano,"\n")
  
  ## Cria o banco com as informacoes da proxima
  ## eleicao em relacao ao ano corrente
  
  indicadores1 <- filter(df_br,
                         `Ano da eleição` == ano + 4)
  
  ## Filtra as estatisticas gerais ao ano corrente
  
  estatisticas_ano1 <- filter(df_br,
                              `Ano da eleição` == ano)
  
  ## Filtra as estatisticas gerais do proximo ano
  ## em relacao ao ano corrente
  
  estatisticas_ano2 <- filter(df_br,
                              `Ano da eleição` == ano + 4)
  
  ## Condicao para que se comece a calcular os indicadores
  
  if(nrow(estatisticas_ano1) > 0 &
     nrow(estatisticas_ano2) > 0){
    
    ## Ordena as linhas do banco estatisticas_ano1
    
    estatisticas_ano1 <- estatisticas_ano1 %>% 
      arrange(`Ano da eleição`, 
              `Sigla do partido`)
    
    ## Ordena as linhas do banco estatisticas_ano2
    
    estatisticas_ano2 <- estatisticas_ano2 %>% 
      arrange(`Ano da eleição`, 
              `Sigla do partido`)
    
    
    ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
    
    for(i in seq_along(unique(estatisticas_ano1$`Sigla do partido`))) {
      
      ## Remove duplicacoes
      
      part1 <- unique(estatisticas_ano1$`Sigla do partido`)
      
      part2 <- unique(estatisticas_ano2$`Sigla do partido`)
      
      ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 2
      
      if(!part1[i] %in% part2){
        
        ## Adiciona uma nova linha ao banco
        
        estatisticas_ano2[nrow(estatisticas_ano2)+1,] <- NA
        
        ## Atribui a variavel 'Sigla do partido' o partido que 
        ## esta faltando neste banco
        
        estatisticas_ano2[nrow(estatisticas_ano2),4] <- part1[i]
        
        ## Atribui o valor '0.00' a variavel 'Percentual de
        ## votos conquistados'
        
        estatisticas_ano2[nrow(estatisticas_ano2),7] <- 0.00
        
        ## Atribui o valor '0.00' a variavel 'Percentual de
        ## cadeiras conquistadas'
        
        estatisticas_ano2[nrow(estatisticas_ano2),8] <- 0.00
        
      }
    }
    
    ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
    
    for(i in seq_along(unique(estatisticas_ano2$`Sigla do partido`))) {
      
      ## Remove duplicacoes
      
      part1 <- unique(estatisticas_ano1$`Sigla do partido`)
      
      part2 <- unique(estatisticas_ano2$`Sigla do partido`)
      
      ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 1
      
      if(!part2[i] %in% part1){
        
        ## Adiciona uma nova linha ao banco
        
        estatisticas_ano1[nrow(estatisticas_ano1)+1,] <- NA
        
        ## Atribui a variavel 'Sigla do partido' o partido que 
        ## esta faltando neste banco
        
        estatisticas_ano1[nrow(estatisticas_ano1),4] <- part2[i]
        
        ## Atribui o valor '0.00' a variavel 'Percentual de
        ## votos conquistados'
        
        estatisticas_ano1[nrow(estatisticas_ano1),7] <- 0.00
        
        ## Atribui o valor '0.00' a variavel 'Percentual de
        ## cadeiras conquistadas'
        
        estatisticas_ano1[nrow(estatisticas_ano1),8] <- 0.00
      }
    }
    
    ## Atribui valor '0' para todas as linhas NA do banco
    
    for(i in 1:ncol(estatisticas_ano1)) {
      for(j in 1:nrow(estatisticas_ano1)){
        
        ## Se a variavel em analise for da classe 'character',
        ## adicione o valor '0' no formato de texto
        
        if(is.character(estatisticas_ano1[[i]]) &
           is.na(estatisticas_ano1[j,i])){
          
          ## Adiciona o valor '0' a celula em analise
          
          estatisticas_ano1[j,i] <- "0"
          
          ## Se a variavel em analise for diferente da classe 'character',
          ## adicione o valor '0' no formato numerico
          
        } else if(!is.character(estatisticas_ano1[j,i]) &
                  is.na(estatisticas_ano1[j,i]))
          
          ## Adiciona o valor '0' a celula em analise
          
          estatisticas_ano1[j,i] <- 0
      }
    }
    
    ## Atribui valor '0' para todas as linhas NA do banco
    
    for(i in 1:ncol(estatisticas_ano2)) {
      for(j in 1:nrow(estatisticas_ano2)){
        
        ## Se a variavel em analise for da classe 'character',
        ## adicione o valor '0' no formato de texto
        
        if(is_character(estatisticas_ano2[[i]]) &
           is.na(estatisticas_ano2[j,i])){
          
          ## Adiciona o valor '0' a celula em analise
          
          estatisticas_ano2[j,i] <- "0"
          
          ## Se a variavel em analise for diferente da classe 'character',
          ## adicione o valor '0' no formato numerico
          
        } else if(!is.character(estatisticas_ano2[j,i]) &
                  is.na(estatisticas_ano2[j,i]))
          
          ## Adiciona o valor '0' a celula em analise
          
          estatisticas_ano2[j,i] <- 0
      }
    }
    
    
    ## Ordena as linhas do banco estatisticas_ano1
    
    estatisticas_ano1 <- estatisticas_ano1 %>% 
      arrange(`Sigla do partido`)
    
    ## Ordena as linhas do banco estatisticas_ano2
    
    estatisticas_ano2 <- estatisticas_ano2 %>% 
      arrange(`Sigla do partido`)
    
    ## Calculo do indicador de 'Volatilidade eleitoral'
    
    indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                        estatisticas_ano2$`Percentual de votos conquistados`)
    
    ## Calculo do indicador de 'Volatilidade parlamentar'
    
    indicadores1$`Volatilidade parlamentar` <- volat_elet(estatisticas_ano1$`Percentual de cadeiras conquistadas`,
                                                          estatisticas_ano2$`Percentual de cadeiras conquistadas`)
    
    ## Empilha os indicadores calculados em um unico banco
    
    ind_eleicoes_fed_br <- bind_rows(ind_eleicoes_fed_br, indicadores1)
    
  }
}


# 2.1.2. Estado -----------------------------------------------------------

## Cria o banco onde os indicadores serao armazenados

ind_eleicoes_fed_uf <- list()

## For loop que calcula os indicadores de volatilidade
## para cada ano e uf

for(ano in anos){
  for(uf in sort(unique(df_uf$UF))){
    
    cat("Lendo", ano, uf, "\n")
    
    ## Cria o banco com as informacoes da proxima
    ## eleicao em relacao ao ano corrente
    
    indicadores1 <- filter(df_uf,
                           `Ano da eleição` == ano+4,
                           UF == uf)
    
    ## Filtra as estatisticas gerais ao ano corrente
    
    estatisticas_ano1 <- filter(df_uf,
                                `Ano da eleição` == ano,
                                UF == uf)
    
    ## Filtra as estatisticas gerais do proximo ano
    ## em relacao ao ano corrente
    
    estatisticas_ano2 <- filter(df_uf,
                                `Ano da eleição` == ano + 4,
                                UF == uf)
    
    ## Condicao para que se comece a calcular os indicadores
    
    if(nrow(estatisticas_ano1) > 0 &
       nrow(estatisticas_ano2) > 0){
      
      ## Ordena as linhas do banco estatisticas_ano1
      
      estatisticas_ano1 <- estatisticas_ano1 %>% 
        arrange(`Ano da eleição`, 
                `Sigla do partido`)
      
      ## Ordena as linhas do banco estatisticas_ano2
      
      estatisticas_ano2 <- estatisticas_ano2 %>% 
        arrange(`Ano da eleição`, 
                `Sigla do partido`)
      
      
      ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
      
      for(i in seq_along(unique(estatisticas_ano1$`Sigla do partido`))) {
        
        ## Remove duplicacoes
        
        part1 <- unique(estatisticas_ano1$`Sigla do partido`)
        
        part2 <- unique(estatisticas_ano2$`Sigla do partido`)
        
        ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 2
        
        if(!part1[i] %in% part2){
          
          ## Adiciona uma nova linha ao banco
          
          estatisticas_ano2[nrow(estatisticas_ano2)+1,] <- NA
          
          ## Atribui a variavel 'Sigla do partido' o partido que 
          ## esta faltando neste banco
          
          estatisticas_ano2[nrow(estatisticas_ano2),6] <- part1[i]
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## votos conquistados'
          
          estatisticas_ano2[nrow(estatisticas_ano2),9] <- 0.00
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## cadeiras conquistadas'
          
          estatisticas_ano2[nrow(estatisticas_ano2),10] <- 0.00
          
        }
      }
      
      ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
      
      for(i in seq_along(unique(estatisticas_ano2$`Sigla do partido`))) {
        
        ## Remove duplicacoes
        
        part1 <- unique(estatisticas_ano1$`Sigla do partido`)
        
        part2 <- unique(estatisticas_ano2$`Sigla do partido`)
        
        ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 1
        
        if(!part2[i] %in% part1){
          
          ## Adiciona uma nova linha ao banco
          
          estatisticas_ano1[nrow(estatisticas_ano1)+1,] <- NA
          
          ## Atribui a variavel 'Sigla do partido' o partido que 
          ## esta faltando neste banco
          
          estatisticas_ano1[nrow(estatisticas_ano1),6] <- part2[i]
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## votos conquistados'
          
          estatisticas_ano1[nrow(estatisticas_ano1),9] <- 0.00
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## cadeiras conquistadas'
          
          estatisticas_ano1[nrow(estatisticas_ano1),10] <- 0.00
        }
      }
      
      ## Atribui valor '0' para todas as linhas NA do banco
      
      for(i in 1:ncol(estatisticas_ano1)) {
        for(j in 1:nrow(estatisticas_ano1)){
          
          ## Se a variavel em analise for da classe 'character',
          ## adicione o valor '0' no formato de texto
          
          if(is.character(estatisticas_ano1[[i]]) &
             is.na(estatisticas_ano1[j,i])){
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano1[j,i] <- "0"
            
            ## Se a variavel em analise for diferente da classe 'character',
            ## adicione o valor '0' no formato numerico
            
          } else if(!is.character(estatisticas_ano1[j,i]) &
                    is.na(estatisticas_ano1[j,i]))
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano1[j,i] <- 0
        }
      }
      
      ## Atribui valor '0' para todas as linhas NA do banco
      
      for(i in 1:ncol(estatisticas_ano2)) {
        for(j in 1:nrow(estatisticas_ano2)){
          
          ## Se a variavel em analise for da classe 'character',
          ## adicione o valor '0' no formato de texto
          
          if(is_character(estatisticas_ano2[[i]]) &
             is.na(estatisticas_ano2[j,i])){
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano2[j,i] <- "0"
            
            ## Se a variavel em analise for diferente da classe 'character',
            ## adicione o valor '0' no formato numerico
            
          } else if(!is.character(estatisticas_ano2[j,i]) &
                    is.na(estatisticas_ano2[j,i]))
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano2[j,i] <- 0
        }
      }
      
      
      ## Ordena as linhas do banco estatisticas_ano1
      
      estatisticas_ano1 <- estatisticas_ano1 %>% 
        arrange(`Sigla do partido`)
      
      ## Ordena as linhas do banco estatisticas_ano2
      
      estatisticas_ano2 <- estatisticas_ano2 %>% 
        arrange(`Sigla do partido`)
    
    ## Calculo do indicador de 'Volatilidade eleitoral'
    indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                        estatisticas_ano2$`Percentual de votos conquistados`)
    
    ## Calculo do indicador de 'Volatilidade parlamentar'
    
    indicadores1$`Volatilidade parlamentar` <- volat_elet(estatisticas_ano1$`Percentual de cadeiras conquistadas`,
                                                          estatisticas_ano2$`Percentual de cadeiras conquistadas`)
    
    ## Empilha os indicadores calculados em um unico banco
    
    ind_eleicoes_fed_uf <- bind_rows(ind_eleicoes_fed_uf, indicadores1)
  }
  }
}


# 2.2. Deputado Estadual --------------------------------------------------

## Cria o banco onde os indicadores serao armazenados

ind_eleicoes_est <- list()

## For loop que calcula os indicadores de volatilidade
## para cada ano e uf

for(ano in anos){
  for(uf in sort(unique(de_uf$UF))){
    
    cat("Lendo", ano, uf, "\n")
    
    ## Cria o banco com as informacoes da proxima
    ## eleicao em relacao ao ano corrente
    
    indicadores1 <- filter(de_uf,
                           `Ano da eleição` == ano + 4,
                           UF == uf)
    
    ## Filtra as estatisticas gerais ao ano corrente
    
    estatisticas_ano1 <- filter(de_uf,
                                `Ano da eleição` == ano,
                                UF == uf)
    
    ## Filtra as estatisticas gerais do proximo ano
    ## em relacao ao ano corrente
    
    estatisticas_ano2 <- filter(de_uf,
                                `Ano da eleição` == ano + 4,
                                UF == uf)
    
    ## Condicao para que se comece a calcular os indicadores
    
    if(nrow(estatisticas_ano1) > 0 &
       nrow(estatisticas_ano2) > 0){
      
      ## Ordena as linhas do banco estatisticas_ano1
      
      estatisticas_ano1 <- estatisticas_ano1 %>% 
        arrange(`Ano da eleição`, 
                `Sigla do partido`)
      
      ## Ordena as linhas do banco estatisticas_ano2
      
      estatisticas_ano2 <- estatisticas_ano2 %>% 
        arrange(`Ano da eleição`, 
                `Sigla do partido`)
      
      
      ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
      
      for(i in seq_along(unique(estatisticas_ano1$`Sigla do partido`))) {
        
        ## Remove duplicacoes
        
        part1 <- unique(estatisticas_ano1$`Sigla do partido`)
        
        part2 <- unique(estatisticas_ano2$`Sigla do partido`)
        
        ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 2
        
        if(!part1[i] %in% part2){
          
          ## Adiciona uma nova linha ao banco
          
          estatisticas_ano2[nrow(estatisticas_ano2)+1,] <- NA
          
          ## Atribui a variavel 'Sigla do partido' o partido que 
          ## esta faltando neste banco
          
          estatisticas_ano2[nrow(estatisticas_ano2),6] <- part1[i]
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## votos conquistados'
          
          estatisticas_ano2[nrow(estatisticas_ano2),9] <- 0.00
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## cadeiras conquistadas'
          
          estatisticas_ano2[nrow(estatisticas_ano2),10] <- 0.00
          
        }
      }
      
      ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
      
      for(i in seq_along(unique(estatisticas_ano2$`Sigla do partido`))) {
        
        ## Remove duplicacoes
        
        part1 <- unique(estatisticas_ano1$`Sigla do partido`)
        
        part2 <- unique(estatisticas_ano2$`Sigla do partido`)
        
        ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 1
        
        if(!part2[i] %in% part1){
          
          ## Adiciona uma nova linha ao banco
          
          estatisticas_ano1[nrow(estatisticas_ano1)+1,] <- NA
          
          ## Atribui a variavel 'Sigla do partido' o partido que 
          ## esta faltando neste banco
          
          estatisticas_ano1[nrow(estatisticas_ano1),6] <- part2[i]
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## votos conquistados'
          
          estatisticas_ano1[nrow(estatisticas_ano1),9] <- 0.00
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## cadeiras conquistadas'
          
          estatisticas_ano1[nrow(estatisticas_ano1),10] <- 0.00
        }
      }
      
      ## Atribui valor '0' para todas as linhas NA do banco
      
      for(i in 1:ncol(estatisticas_ano1)) {
        for(j in 1:nrow(estatisticas_ano1)){
          
          ## Se a variavel em analise for da classe 'character',
          ## adicione o valor '0' no formato de texto
          
          if(is.character(estatisticas_ano1[[i]]) &
             is.na(estatisticas_ano1[j,i])){
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano1[j,i] <- "0"
            
            ## Se a variavel em analise for diferente da classe 'character',
            ## adicione o valor '0' no formato numerico
            
          } else if(!is.character(estatisticas_ano1[j,i]) &
                    is.na(estatisticas_ano1[j,i]))
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano1[j,i] <- 0
        }
      }
      
      ## Atribui valor '0' para todas as linhas NA do banco
      
      for(i in 1:ncol(estatisticas_ano2)) {
        for(j in 1:nrow(estatisticas_ano2)){
          
          ## Se a variavel em analise for da classe 'character',
          ## adicione o valor '0' no formato de texto
          
          if(is_character(estatisticas_ano2[[i]]) &
             is.na(estatisticas_ano2[j,i])){
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano2[j,i] <- "0"
            
            ## Se a variavel em analise for diferente da classe 'character',
            ## adicione o valor '0' no formato numerico
            
          } else if(!is.character(estatisticas_ano2[j,i]) &
                    is.na(estatisticas_ano2[j,i]))
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano2[j,i] <- 0
        }
      }
      
      
      ## Ordena as linhas do banco estatisticas_ano1
      
      estatisticas_ano1 <- estatisticas_ano1 %>% 
        arrange(`Sigla do partido`)
      
      ## Ordena as linhas do banco estatisticas_ano2
      
      estatisticas_ano2 <- estatisticas_ano2 %>% 
        arrange(`Sigla do partido`)
    
    ## Calculo do indicador de 'Volatilidade eleitoral'
    indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                        estatisticas_ano2$`Percentual de votos conquistados`)
    
    ## Calculo do indicador de 'Volatilidade parlamentar'
    
    indicadores1$`Volatilidade parlamentar` <- volat_elet(estatisticas_ano1$`Percentual de cadeiras conquistadas`,
                                                          estatisticas_ano2$`Percentual de cadeiras conquistadas`)
    ## Empilha todas as eleicoes 
    
    ind_eleicoes_est <- bind_rows(ind_eleicoes_est, indicadores1)
  }
  }
}



# 2.3. Prefeito -----------------------------------------------------------

## Vector com os anos das eleicoes municipais

anos_mun <- c(2000,2004,2008,2012,2016)

## Cria o banco onde os indicadores serao armazenados

ind_eleicoes_pf <- list()

## For loop que calcula os indicadores de volatilidade 
## para cada ano, municipio e turno

for(ano in anos_mun){
  for(municipio in sort(unique(pf_mun$`Código do município`))){
    for(turno in sort(unique(pf_mun$Turno))){
      
      cat("Lendo", ano, municipio, turno, "\n")
      
      ## Cria o banco com as informacoes da proxima
      ## eleicao em relacao ao ano corrente
      
      indicadores1 <- filter(pf_mun,
                             `Ano da eleição` == ano + 4,
                             `Código do município` == municipio,
                             Turno == turno)
      
      ## Filtra as estatisticas gerais ao ano corrente
      
      estatisticas_ano1 <- filter(pf_mun,
                                  `Ano da eleição` == ano,
                                  `Código do município` == municipio,
                                  Turno == turno)
      
      ## Filtra as estatisticas gerais do proximo ano
      ## em relacao ao ano corrente
      
      estatisticas_ano2 <- filter(pf_mun,
                                  `Ano da eleição` == ano + 4,
                                  `Código do município` == municipio,
                                  Turno == turno)
      
      ## Condicao para que se calcule os indicadores
      
      if(nrow(estatisticas_ano1) > 0 &
         nrow(estatisticas_ano2) > 0){
        
        ## Ordena as linhas do banco estatisticas_ano1
        
        estatisticas_ano1 <- estatisticas_ano1 %>% 
          arrange(`Ano da eleição`, 
                  UF, 
                  `Código do município`, 
                  `Sigla do partido`)
        
        ## Ordena as linhas do banco estatisticas_ano2
        
        estatisticas_ano2 <- estatisticas_ano2 %>% 
          arrange(`Ano da eleição`, 
                  UF, 
                  `Código do município`, 
                  `Sigla do partido`)
        
        ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
        
        for(i in seq_along(unique(estatisticas_ano1$`Sigla do partido`))) {
          
          ## Remove duplicacoes
          
          part1 <- unique(estatisticas_ano1$`Sigla do partido`)
          
          part2 <- unique(estatisticas_ano2$`Sigla do partido`)
          
          ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 2
          
          if(!part1[i] %in% part2){
            
            ## Adiciona uma nova linha ao banco
            
            estatisticas_ano2[nrow(estatisticas_ano2)+1,] <- NA
            
            ## Atribui a variavel 'Sigla do partido' o partido que 
            ## esta faltando neste banco
            
            estatisticas_ano2[nrow(estatisticas_ano2),7] <- part1[i]
            
            ## Atribui o valor '0.00' a variavel 'Percentual de
            ## votos conquistados'
            
            estatisticas_ano2[nrow(estatisticas_ano2),9] <- 0.00
          }
        }
        
        ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
        
        for(i in seq_along(unique(estatisticas_ano2$`Sigla do partido`))) {
          
          ## Remove duplicacoes
          
          part1 <- unique(estatisticas_ano1$`Sigla do partido`)
          
          part2 <- unique(estatisticas_ano2$`Sigla do partido`)
          
          ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 1
          
          if(!part2[i] %in% part1){
            
            ## Adiciona uma nova linha ao banco
            
            estatisticas_ano1[nrow(estatisticas_ano1)+1,] <- NA
            
            ## Atribui a variavel 'Sigla do partido' o partido que 
            ## esta faltando neste banco
            
            estatisticas_ano1[nrow(estatisticas_ano1),7] <- part2[i]
            
            ## Atribui o valor '0.00' a variavel 'Percentual de
            ## votos conquistados'
            
            estatisticas_ano1[nrow(estatisticas_ano1),9] <- 0.00
          }
        }
        
        ## Atribui valor '0' para todas as linhas NA do banco
        
        for(i in 1:ncol(estatisticas_ano1)) {
          for(j in 1:nrow(estatisticas_ano1)){
            
            ## Se a variavel em analise for da classe 'character',
            ## adicione o valor '0' no formato de texto
            
            if(is.character(estatisticas_ano1[[i]]) &
               is.na(estatisticas_ano1[j,i])){
              
              ## Adiciona o valor '0' a celula em analise
              
              estatisticas_ano1[j,i] <- "0"
              
              ## Se a variavel em analise for diferente da classe 'character',
              ## adicione o valor '0' no formato numerico
              
            } else if(!is.character(estatisticas_ano1[j,i]) &
                      is.na(estatisticas_ano1[j,i]))
              
              ## Adiciona o valor '0' a celula em analise
              
              estatisticas_ano1[j,i] <- 0
          }
        }
        
        ## Atribui valor '0' para todas as linhas NA do banco
        
        for(i in 1:ncol(estatisticas_ano2)) {
          for(j in 1:nrow(estatisticas_ano2)){
            
            ## Se a variavel em analise for da classe 'character',
            ## adicione o valor '0' no formato de texto
            
            if(is_character(estatisticas_ano2[[i]]) &
               is.na(estatisticas_ano2[j,i])){
              
              ## Adiciona o valor '0' a celula em analise
              
              estatisticas_ano2[j,i] <- "0"
              
              ## Se a variavel em analise for diferente da classe 'character',
              ## adicione o valor '0' no formato numerico
              
            } else if(!is.character(estatisticas_ano2[j,i]) &
                      is.na(estatisticas_ano2[j,i]))
              
              ## Adiciona o valor '0' a celula em analise
              
              estatisticas_ano2[j,i] <- 0
          }
        }
        
        
        ## Ordena as linhas do banco estatisticas_ano1
        
        estatisticas_ano1 <- estatisticas_ano1 %>% 
          arrange(`Sigla do partido`)
        
        ## Ordena as linhas do banco estatisticas_ano2
        
        estatisticas_ano2 <- estatisticas_ano2 %>% 
          arrange(`Sigla do partido`)
        
        ## Condicao para que se calcule os indicadores
        
        if(nrow(estatisticas_ano1 > 0)){
          
          
          ## Calculo do indicador de 'Volatilidade eleitoral'
          
          indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                              estatisticas_ano2$`Percentual de votos conquistados`)
          
          ## Empilha os indicadores calculados em um unico banco
          
          ind_eleicoes_pf <- bind_rows(ind_eleicoes_pf, indicadores1)
          
        }
      }
    }
  }
  
}



# 2.4. Vereador -----------------------------------------------------------

## Cria o banco onde os indicadores serao armazenados

ind_eleicoes_vr <- list()

## For loop que calcula os indicadores de volatilidade 
## para cada ano e municipio

for(ano in anos_mun){
  for(municipio in sort(unique(vr_mun$`Código do município`))){
    
    cat("Lendo",ano,municipio,"\n")
    
    ## Cria o banco com as informacoes da proxima
    ## eleicao em relacao ao ano corrente
    
    indicadores1 <- filter(vr_mun,
                           `Ano da eleição` == ano + 4,
                           `Código do município` == municipio)
    
    ## Filtra as estatisticas gerais ao ano corrente
    
    estatisticas_ano1 <- filter(vr_mun,
                                `Ano da eleição` == ano,
                                `Código do município` == municipio)
    estatisticas_ano2 <- filter(vr_mun,
                                `Ano da eleição` == ano + 4,
                                `Código do município` == municipio)
    
    ## Condicao para que se comece a calcular os indicadores
    
    if(nrow(estatisticas_ano1) > 0 &
       nrow(estatisticas_ano2) > 0){
      
      ## Ordena as linhas do banco estatisticas_ano1
      
      estatisticas_ano1 <- estatisticas_ano1 %>% 
        arrange(`Ano da eleição`,
                UF,
                `Código do município`,
                `Sigla do partido`)
      
      ## Ordena as linhas do banco estatisticas_ano2
      
      estatisticas_ano2 <- estatisticas_ano2 %>% 
        arrange(`Ano da eleição`, 
                UF,
                `Código do município`,
                `Sigla do partido`)
      
      
      ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
      
      for(i in seq_along(unique(estatisticas_ano1$`Sigla do partido`))) {
        
        ## Remove duplicacoes
        
        part1 <- unique(estatisticas_ano1$`Sigla do partido`)
        
        part2 <- unique(estatisticas_ano2$`Sigla do partido`)
        
        ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 2
        
        if(!part1[i] %in% part2){
          
          ## Adiciona uma nova linha ao banco
          
          estatisticas_ano2[nrow(estatisticas_ano2)+1,] <- NA
          
          ## Atribui a variavel 'Sigla do partido' o partido que 
          ## esta faltando neste banco
          
          estatisticas_ano2[nrow(estatisticas_ano2),8] <- part1[i]
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## votos conquistados'
          
          estatisticas_ano2[nrow(estatisticas_ano2),11] <- 0.00
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## cadeiras conquistadas'
          
          estatisticas_ano2[nrow(estatisticas_ano2),12] <- 0.00
          
        }
      }
      
      ## Equipara o numero de partidos entre as estatisticas do ano 1 e ano 2
      
      for(i in seq_along(unique(estatisticas_ano2$`Sigla do partido`))) {
        
        ## Remove duplicacoes
        
        part1 <- unique(estatisticas_ano1$`Sigla do partido`)
        
        part2 <- unique(estatisticas_ano2$`Sigla do partido`)
        
        ## Condicao para que se 'crie' um novo partido nas estatiscas do ano 1
        
        if(!part2[i] %in% part1){
          
          ## Adiciona uma nova linha ao banco
          
          estatisticas_ano1[nrow(estatisticas_ano1)+1,] <- NA
          
          ## Atribui a variavel 'Sigla do partido' o partido que 
          ## esta faltando neste banco
          
          estatisticas_ano1[nrow(estatisticas_ano1),8] <- part2[i]
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## votos conquistados'
          
          estatisticas_ano1[nrow(estatisticas_ano1),11] <- 0.00
          
          ## Atribui o valor '0.00' a variavel 'Percentual de
          ## cadeiras conquistadas'
          
          estatisticas_ano1[nrow(estatisticas_ano1),12] <- 0.00
        }
      }
      
      ## Atribui valor '0' para todas as linhas NA do banco
      
      for(i in 1:ncol(estatisticas_ano1)) {
        for(j in 1:nrow(estatisticas_ano1)){
          
          ## Se a variavel em analise for da classe 'character',
          ## adicione o valor '0' no formato de texto
          
          if(is.character(estatisticas_ano1[[i]]) &
             is.na(estatisticas_ano1[j,i])){
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano1[j,i] <- "0"
            
            ## Se a variavel em analise for diferente da classe 'character',
            ## adicione o valor '0' no formato numerico
            
          } else if(!is.character(estatisticas_ano1[j,i]) &
                    is.na(estatisticas_ano1[j,i]))
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano1[j,i] <- 0
        }
      }
      
      ## Atribui valor '0' para todas as linhas NA do banco
      
      for(i in 1:ncol(estatisticas_ano2)) {
        for(j in 1:nrow(estatisticas_ano2)){
          
          ## Se a variavel em analise for da classe 'character',
          ## adicione o valor '0' no formato de texto
          
          if(is_character(estatisticas_ano2[[i]]) &
             is.na(estatisticas_ano2[j,i])){
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano2[j,i] <- "0"
            
            ## Se a variavel em analise for diferente da classe 'character',
            ## adicione o valor '0' no formato numerico
            
          } else if(!is.character(estatisticas_ano2[j,i]) &
                    is.na(estatisticas_ano2[j,i]))
            
            ## Adiciona o valor '0' a celula em analise
            
            estatisticas_ano2[j,i] <- 0
        }
      }
      
      
      ## Ordena as linhas do banco estatisticas_ano1
      
      estatisticas_ano1 <- estatisticas_ano1 %>% 
        arrange(`Sigla do partido`)
      
      ## Ordena as linhas do banco estatisticas_ano2
      
      estatisticas_ano2 <- estatisticas_ano2 %>% 
        arrange(`Sigla do partido`)
    
      ## Calculo do indicador de 'Volatilidade eleitoral'
      
      indicadores1$`Volatilidade eleitoral` <- volat_elet(estatisticas_ano1$`Percentual de votos conquistados`,
                                                          estatisticas_ano2$`Percentual de votos conquistados`)
      
      ## Calculo do indicador de 'Volatilidade parlamentar'
      
      indicadores1$`Volatilidade parlamentar` <- volat_elet(estatisticas_ano1$`Percentual de cadeiras conquistadas`,
                                                            estatisticas_ano2$`Percentual de cadeiras conquistadas`)
      
      ## Empilha os indicadores calculados em um unico banco 
      
      ind_eleicoes_vr <- bind_rows(ind_eleicoes_vr, indicadores1)
  }
}
}

# 3. Rbind ----------------------------------------------------------------


# 3.1. Estado -------------------------------------------------------------

### Empilha os bancos em um unico

ind_eleicoes_est <- bind_rows(ind_eleicoes_fed_uf, ind_eleicoes_est)

# 4. Padronizacao dos dados -----------------------------------------------


# 4.1. Brasil -------------------------------------------------------------

## Atribui o valor 'Deputado Federal' a variavel 'Cargo'

ind_eleicoes_fed_br$Cargo <- "Deputado Federal"

## Reorganiza a tabela

ind_eleicoes_fed_br <- ind_eleicoes_fed_br %>% 
  select(`Ano da eleição`, 
         Cargo,
         `Volatilidade eleitoral`, 
         `Volatilidade parlamentar`
  ) %>% 
  unique()

## Padroniza o formato numerico da variavel
## 'Volatilidade eleitoral'

ind_eleicoes_fed_br$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes_fed_br$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_fed_br$`Volatilidade eleitoral` <- gsub("\\.", ",", 
                                                     ind_eleicoes_fed_br$`Volatilidade eleitoral`)

## Padroniza o formato numerico da variavel
## 'Volatilidade parlamentar'

ind_eleicoes_fed_br$`Volatilidade parlamentar` <- 
  format(round(ind_eleicoes_fed_br$`Volatilidade parlamentar`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_fed_br$`Volatilidade parlamentar` <- gsub("\\.", ",", 
                                                     ind_eleicoes_fed_br$`Volatilidade parlamentar`)

# 4.2. Estado -------------------------------------------------------------

## Reorganiza a tabela

ind_eleicoes_est <- ind_eleicoes_est %>% 
  select(`Ano da eleição`,
         UF,
         Cargo,
         `Volatilidade eleitoral`, 
         `Volatilidade parlamentar`
  ) %>% 
  unique()

## Padroniza o formato numerico da variavel
## 'Volatilidade eleitoral'

ind_eleicoes_est$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes_est$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_est$`Volatilidade eleitoral` <- gsub("\\.", ",", 
                                                 ind_eleicoes_est$`Volatilidade eleitoral`)

## Padroniza o formato numerico da variavel
## 'Volatilidade parlamentar'

ind_eleicoes_est$`Volatilidade parlamentar` <- 
  format(round(ind_eleicoes_est$`Volatilidade parlamentar`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_est$`Volatilidade parlamentar` <- gsub("\\.", ",", 
                                                 ind_eleicoes_est$`Volatilidade parlamentar`)

## Ordena o banco

ind_eleicoes_est <- ind_eleicoes_est %>% 
  arrange(`Ano da eleição`, UF)

# 4.3. Municipio ----------------------------------------------------------

# 4.3.1. Prefeito -----------------------------------------------------------


## Reorganiza a tabela

ind_eleicoes_pf <- ind_eleicoes_pf %>% 
  select(`Ano da eleição`,
         Turno,
         UF,
         `Código do município`,
         Cargo,
         `Volatilidade eleitoral`) %>% 
  unique()

## Padroniza o formato numerico da variavel
## 'Volatilidade eleitoral'

ind_eleicoes_pf$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes_pf$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_pf$`Volatilidade eleitoral` <- gsub("\\.", ",", 
                                                 ind_eleicoes_pf$`Volatilidade eleitoral`)

## Ordena a tabela em funcao do 'Ano da eleicao',
## 'UF', 'Nome do municipio' e 'Turno'

ind_eleicoes_pf <- ind_eleicoes_pf %>% 
  arrange(`Ano da eleição`,Turno,UF)


# 4.3.2. Vereador ---------------------------------------------------------

## Remove valores estranhos

ind_eleicoes_vr <- ind_eleicoes_vr %>% 
  filter(`Volatilidade eleitoral` < 100.01)

## Reorganiza a tabela

ind_eleicoes_vr <- ind_eleicoes_vr %>% 
  select(`Ano da eleição`,
         UF,
         `Código do município`,
         Cargo,
         `Volatilidade eleitoral`, 
         `Volatilidade parlamentar`
  ) %>% 
  unique()

## Ordena a tabela em funcao do 'Ano da eleicao',
## 'UF' e 'Nome do municipio'

ind_eleicoes_vr <- ind_eleicoes_vr %>% 
  arrange(`Ano da eleição`,UF)

## Padroniza o formato numerico da variavel
## 'Volatilidade eleitoral'

ind_eleicoes_vr$`Volatilidade eleitoral` <- 
  format(round(ind_eleicoes_vr$`Volatilidade eleitoral`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_vr$`Volatilidade eleitoral` <- gsub("\\.", ",", 
                                                 ind_eleicoes_vr$`Volatilidade eleitoral`)

## Padroniza o formato numerico da variavel
## 'Volatilidade parlamentar'

ind_eleicoes_vr$`Volatilidade parlamentar` <- 
  format(round(ind_eleicoes_vr$`Volatilidade parlamentar`, 
               digits = 2),  
         nsmall = 2)

ind_eleicoes_vr$`Volatilidade parlamentar` <- gsub("\\.", ",", 
                                                 ind_eleicoes_vr$`Volatilidade parlamentar`)


# 4. Salva os arquivos ----------------------------------------------------


# 4.1. Brasil -------------------------------------------------------------

## Salva os indicadores de volatilidade em .rds

saveRDS(ind_eleicoes_fed_br, "data/output/vol_br.rds")


# 4.2. Estado -------------------------------------------------------------

## Salva os indicadores de volatilidade em .rds

saveRDS(ind_eleicoes_est, "data/output/vol_uf.rds")


# 4.3. Municipio ----------------------------------------------------------

## Salva os indicadores de volatilidade em .rds

saveRDS(ind_eleicoes_vr, "data/output/vol_mun.rds")


# 4.4. Prefeito -----------------------------------------------------------

## Salva os indicadores de volatilidade em .rds

saveRDS(ind_eleicoes_pf, "data/output/vol_pf.rds")

## Remove os arquivos que nao serao mais utilizados

rm(list = ls())

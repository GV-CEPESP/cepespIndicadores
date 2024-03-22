
## Função para calcular os indicadores de "Fragmentação" para cada 
## cargo e agregação regional

indic_frag <- function(data, 
                       agregacao = c("BR", "UF", "MUN_PF", "MUN_VR")){
  
  if(agregacao == "BR"){
    
    ## Calcula os indicadores de fragmentação em cada ano
    
    data <- data %>% 
      group_by(ANO_ELEICAO) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS),
             `Número efetivo de partidos legislativo` = num_efetivo_part(PERC_CADEIRAS),
             `Fracionalização` = fracionalizacao(PERC_CADEIRAS),
             `Fracionalização máxima` = fracionalizacao_max(QT_VAGAS, 
                                                            NUM_PART_PARLAMENT),
             `Fragmentação` = fragmentacao(`Fracionalização`,
                                           `Fracionalização máxima`),
             `Desproporcionalidade` = desp_gallagher(PERC_VOTOS,
                                                     PERC_CADEIRAS))
    
  } else if(agregacao == "UF"){
    
    ## Calcula os indicadores de fragmentação em cada ano e UF
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               UF) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS),
             `Número efetivo de partidos legislativo` = num_efetivo_part(PERC_CADEIRAS),
             `Fracionalização` = fracionalizacao(PERC_CADEIRAS),
             `Fracionalização máxima` = fracionalizacao_max(QT_VAGAS, 
                                                            NUM_PART_PARLAMENT),
             `Fragmentação` = fragmentacao(`Fracionalização`,
                                           `Fracionalização máxima`),
             `Desproporcionalidade` = desp_gallagher(PERC_VOTOS,
                                                     PERC_CADEIRAS))
    
  } else if(agregacao == "MUN_PF"){
    
    ## Calcula os indicadores de fragmentação em cada ano, turno e município
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               NUM_TURNO, 
               COD_MUN_TSE) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS))
    
    
  } else if(agregacao == "MUN_VR"){
    
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
             `Fragmentação` = fragmentacao(`Fracionalização`,
                                           `Fracionalização máxima`),
             `Desproporcionalidade` = desp_gallagher(PERC_VOTOS,
                                                     PERC_CADEIRAS))
  }
  
  return(data)
  
}

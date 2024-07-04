
## Função para calcular os indicadores de "Fragmentação" para cada 
## cargo e agregação regional

indic_frag <- function(data, 
                       agregacao = c("BR", "UF", 
                                     "PF_MUN", "VR_MUN")){
  
############################### BR ########################################
  
  if(agregacao == "BR"){
    
    ## Calcula os indicadores de fragmentação em cada ano
    
    data <- data %>% 
      group_by(ANO_ELEICAO) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS),
             `Número efetivo de partidos legislativo` = num_efetivo_part(PERC_CADEIRAS),
             `Fracionalização` = fracionalizacao(PERC_CADEIRAS),
             `Fracionalização máxima` = fracionalizacao_max(INFORMACAO_DISPONIVEL, 
                                                            NUM_PART_PARLAMENT),
             `Fragmentação` = fragmentacao(`Fracionalização`,
                                           `Fracionalização máxima`),
             `Desproporcionalidade` = desp_gallagher(PERC_VOTOS,
                                                     PERC_CADEIRAS))
    
##################################### UF ###################################
    
  } else if(agregacao == "UF"){
    
    ## Calcula os indicadores de fragmentação em cada ano e UF
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               SIGLA_UF) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS),
             `Número efetivo de partidos legislativo` = num_efetivo_part(PERC_CADEIRAS),
             `Fracionalização` = fracionalizacao(PERC_CADEIRAS),
             `Fracionalização máxima` = fracionalizacao_max(INFORMACAO_DISPONIVEL, 
                                                            NUM_PART_PARLAMENT),
             `Fragmentação` = fragmentacao(`Fracionalização`,
                                           `Fracionalização máxima`),
             `Desproporcionalidade` = desp_gallagher(PERC_VOTOS,
                                                     PERC_CADEIRAS))
    
#################################### PF_MUN ###############################
    
  } else if(agregacao == "PF_MUN"){
    
    ## Calcula os indicadores de fragmentação em cada ano, turno e município
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               NUM_TURNO, 
               COD_MUN_TSE,
               COD_MUN_IBGE) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS))
    
##################################### VR_MUN ################################
    
  } else if(agregacao == "VR_MUN"){
    
    ## Calcula os indicadores de fragmentação em cada ano e município
    
    data <- data %>% 
      group_by(ANO_ELEICAO,
               NUM_TURNO, 
               COD_MUN_TSE,
               COD_MUN_IBGE) %>% 
      mutate(`Número efetivo de partidos eleitoral` =  num_efetivo_part(PERC_VOTOS),
             `Número efetivo de partidos legislativo` = num_efetivo_part(PERC_CADEIRAS),
             `Fracionalização` = fracionalizacao(PERC_CADEIRAS),
             `Fracionalização máxima` = fracionalizacao_max(INFORMACAO_DISPONIVEL, 
                                                            NUM_PART_PARLAMENT),
             `Fragmentação` = fragmentacao(`Fracionalização`,
                                           `Fracionalização máxima`),
             `Desproporcionalidade` = desp_gallagher(PERC_VOTOS,
                                                     PERC_CADEIRAS))
    
    ## Salva os municípios com informação incompleta em um novo data frame
    
    com_erro <- data %>% 
      filter(INFORMACAO_DISPONIVEL != QTDE_VAGAS)
    
    ## Exporta o resultado para conferência posterior
    
    saveRDS(com_erro,
            "data/output/fragmentacao_vereadores_municipos_com_erro.rds")
    
    ## Remove esses casos da base final
    
    data <- data %>% 
      filter(INFORMACAO_DISPONIVEL == QTDE_VAGAS)
    
  }
  
  return(data)
  
}

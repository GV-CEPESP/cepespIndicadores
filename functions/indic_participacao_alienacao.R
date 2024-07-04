
## Função para calcular os indicadores de 'Participação e Alienação'

indic_particip_alien <- function(data){
  
  data <- data %>% 
    mutate(ABSTENCAO_PERCENTUAL = abstencao_percent(QTDE_ABSTENCOES,
                                                    QTDE_APTOS),
           VOTOS_BRANCOS_PERCENTUAIS = votosbrancos_percent(QTDE_VOTOS_BRANCOS,
                                                            QTDE_APTOS),
           VOTOS_NULOS_PERCENTUAIS = votosnulos_percent(QTDE_VOTOS_NULOS,
                                                        QTDE_APTOS),
           ALIENACAO_ABSOLUTA = alienacao_absoluta(QTDE_ABSTENCOES,
                                                   QTDE_VOTOS_NULOS,
                                                   QTDE_VOTOS_BRANCOS),
           ALIENACAO_PERCENTUAL = alienacao_percentual(QTDE_ABSTENCOES,
                                                       QTDE_VOTOS_NULOS,
                                                       QTDE_VOTOS_BRANCOS,
                                                       QTDE_APTOS))
  
}

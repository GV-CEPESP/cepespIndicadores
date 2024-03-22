
## Função para calcular os indicadores de 'Participação e Alienação'

indic_particip_alien <- function(data){
  
  data <- data %>% 
    mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                    "DEPUTADO ESTADUAL",
                                    DESCRICAO_CARGO),
           ABSTENCAO_PERCENTUAL = abstencao_percent(QT_ABSTENCOES,
                                                    QT_APTOS),
           VOTOS_BRANCOS_PERCENTUAIS = votosbrancos_percent(QT_VOTOS_BRANCOS,
                                                            QT_APTOS),
           VOTOS_NULOS_PERCENTUAIS = votosnulos_percent(QT_VOTOS_NULOS,
                                                        QT_APTOS),
           ALIENACAO_ABSOLUTA = alienacao_absoluta(QT_ABSTENCOES,
                                                   QT_VOTOS_NULOS,
                                                   QT_VOTOS_BRANCOS),
           ALIENACAO_PERCENTUAL = alienacao_percentual(QT_ABSTENCOES,
                                                       QT_VOTOS_NULOS,
                                                       QT_VOTOS_BRANCOS,
                                                       QT_APTOS))
  
}

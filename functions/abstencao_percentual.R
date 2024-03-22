
## Função para o cálculo da 'Abstenção Percentual'

abstencao_percent <- function(quant_abstencoes, quant_eleitores_apt){
  
  (quant_abstencoes / quant_eleitores_apt) * 100
  
}
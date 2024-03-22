
## Função para o cálculo da 'Alienação Percentual'

alienacao_percentual <- function(quant_abstencoes, quant_votosnulos, 
                                 quant_votosbrancos, quant_eleitores_apt){
  
  
  ((quant_abstencoes + quant_votosnulos + quant_votosbrancos) / quant_eleitores_apt) * 100
  
}
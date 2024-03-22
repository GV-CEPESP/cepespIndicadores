
## Função para o cálculo dos 'Votos Brancos Percentuais'

votosbrancos_percent <- function(quant_votosbrancos, quant_eleitores_apt){
  
  (quant_votosbrancos / quant_eleitores_apt) * 100
  
}
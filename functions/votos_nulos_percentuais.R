
## Função para o cálculo dos 'Votos Nulos Percentuais'

votosnulos_percent <- function(quant_votosnulos, quant_eleitores_apt){
  
  (quant_votosnulos / quant_eleitores_apt) * 100
  
}
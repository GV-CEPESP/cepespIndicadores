
## Função para o cálculo da 'Volatilidade'

volatilidade <- function(prop_votos_cadeiras_T, prop_votos_cadeiras_T4) {
  
  abs((prop_votos_cadeiras_T * 100) - (prop_votos_cadeiras_T4 * 100))
  
}
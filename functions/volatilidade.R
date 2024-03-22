
## Função para o cálculo da 'Volatilidade'

volatilidade <- function(prop_votos_cadeiras_T, prop_votos_cadeiras_T1) {
  
  sum(abs((prop_votos_cadeiras_T * 100) - (prop_votos_cadeiras_T1 * 100))) / 2 
  
}
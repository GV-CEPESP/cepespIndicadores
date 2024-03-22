
## Cálculo do 'Número Efetivo de Partidos' 

num_efetivo_part <- function(prop_votos_cadeiras){
  
  1 / sum(prop_votos_cadeiras ^ 2)
  
}
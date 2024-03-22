
## Cálculo da 'Fracionalização'

fracionalizacao <- function(perc_cadeiras_part){
  
  1 - (sum(perc_cadeiras_part ^ 2))
  
}
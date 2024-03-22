
## Cálculo da 'Fracionalização Máxima'

fracionalizacao_max <- function(num_cadeiras, num_partidos_repres){
  
  (num_cadeiras * (num_partidos_repres - 1)) / 
    
    (num_partidos_repres * (num_cadeiras - 1))
  
}
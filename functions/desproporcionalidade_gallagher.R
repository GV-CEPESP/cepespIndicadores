
## Cálculo da 'Desproporcionalidade de Gallagher'

desp_gallagher <- function(perc_votos, perc_cadeiras){
  
  sqrt(sum((perc_votos * 100 - perc_cadeiras * 100) ^ 2, 
           na.rm = TRUE) / 2)
  
}
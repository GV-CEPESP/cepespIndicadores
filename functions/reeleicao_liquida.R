
## Cálculo da 'Reeleição Líquida' 

reeleicao_liq <- function(num_reeleitos, num_derrotados){
  
  (num_reeleitos / (num_reeleitos + num_derrotados)) * 100
  
}
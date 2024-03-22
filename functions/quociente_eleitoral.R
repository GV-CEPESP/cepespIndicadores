
## Cálculo do 'Quociente Eleitoral'

quoc_eleitoral <- function(votos_validos, qt_vagas){
  
  as.numeric(votos_validos) / as.numeric(qt_vagas)
  
}
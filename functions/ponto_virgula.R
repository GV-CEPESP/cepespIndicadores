
## Função para inserir pontuação e vírgula em números grandes
## e pequenos

pont_virg <- function(string,
                      tipo = c("inteiro", "decimal")){
  
  if(tipo == "inteiro"){
    
    suppressWarnings(
      
      formatC(string, 
              digits = 0,
              format = "f",
              big.mark = ".",
              small.mark = ",",
              decimal.mark = ",")
      
    )
    
  } else if(tipo == "decimal"){
    
    suppressWarnings(
      
      formatC(string, 
              digits = 4,
              format = "f",
              big.mark = ".",
              small.mark = ",",
              decimal.mark = ",",
              flag = "#")
      
    )
    
  }
  
}
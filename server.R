

# Objetivo
#'        - Criar graficos e tabelas para a exibicao dos indicadores.



# 1. Server ---------------------------------------------------------------


server <- function(input, output, session){
  
  
 
# 1.1. Sobre --------------------------------------------------------------

  
 ## Funcao para descricao do sobre
  
  output$sobre <- renderUI({
    note <- paste0("
                   <h2 align = 'center'>
                   <font size ='6' color = 'black'><strong>
                   
                   SOBRE </font></h2>
                   
                   <font size = '1' color = 'black'>


                   <h4 align = 'justify'><br />
                   <p style='line-height:150%'>Os indicadores eleitorais são uma iniciativa 
                   de disseminar análise de dados eleitorais. 
                   Os indicadores aqui calculados foram inspirados pelo livro 'Votos e Partidos - Almanaque 
                   de Dados Eleitorais' de Wanderley Guilherme dos Santos e pelo antigo site de Jairo Nicolau. 
                   Todos os indicadores foram calculados 
                   a partir dos dados do <a href='http://www.cepesp.io/cepesp-data/'> CepespData </a>. 
                   Desenvolvido 
                   por Rebeca Carvalho e Gabriela Campos com orientação de George Avelino e 
                   apoio da <a href='http://cepespdata.io/sobre'> 
                   equipe CEPESP</a>. </p></h4></font>")
    HTML(note)
  })
  
# 1.2. Cargo e agregacao regional -------------------------------------------------  
  
## Funcao que retorna os cargos disponiveis para o indicador selecionado  
  
### Fragmentacao legislativa
  
  indicadores1 <- reactive({
    cargo <- input$DESCRICAO_CARGO1
    if(length(cargo) > 0){
      return(input$INDICADORES_FRAG)
    } 
  })
  
  
  output$INDICADORES_FRAG <- renderUI({
    cargo <- input$DESCRICAO_CARGO1
    if(length(cargo) > 0 &
       cargo == "Senador"){
  selectizeInput(inputId = "INDICADORES_FRAG",
                 label = NULL, 
                 choices = c("", "Número efetivo de partidos eleitoral",
                             "Número efetivo de partidos legislativo","Fracionalização", 
                             "Fracionalização máxima",            
                             "Fragmentação", "Desproporcionalidade"),
                 selected = NULL,
                 options = list(placeholder = 'Escolha um indicador'))
    } else if(length(cargo) > 0 &
              cargo == "Prefeito"){
      selectizeInput(inputId = "INDICADORES_FRAG",
                     label = NULL, 
                     choices = c("", "Número efetivo de partidos eleitoral"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um indicador'))
    } else{
      selectizeInput(inputId = "INDICADORES_FRAG",
                     label = NULL, 
                     choices = c("", "Número efetivo de partidos eleitoral",
                                 "Número efetivo de partidos legislativo","Fracionalização", 
                                 "Fracionalização máxima",            
                                 "Fragmentação", "Desproporcionalidade",
                                 "Quociente eleitoral", "Quociente partidário"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um indicador'))
    }
    
  })
  
 
## Funcao que retorna uma nova caixa de selecao quando o usuario seleciona "UF" na agregacao regional    
  
### Fragmentacao legislativa
  
  
  agregacao1 <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO1
    if(length(indicador) > 0){
      return(input$AGREGACAO_REGIONAL1)
    } 
  })
  
  
  output$AGREGACAO_REGIONAL1 <- renderUI({
    indicador <- req(input$INDICADORES_FRAG)
    cargo <- req(input$DESCRICAO_CARGO1)
    if(cargo == "Deputado Estadual"){
    selectizeInput("AGREGACAO_REGIONAL1",
                   label = NULL,
                   choices = c("", "UF"),
                   selected = NULL,
                   options = list(placeholder = 'Escolha uma agregação regional')) 
   } else if(cargo == "Deputado Federal"){
      selectizeInput("AGREGACAO_REGIONAL1",
                     label = NULL,
                     choices = 
                       c("","Brasil", "UF"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
    }else if(cargo == "Deputado Estadual"){
      selectizeInput("AGREGACAO_REGIONAL1",
                     label = NULL,
                     choices = 
                       c("","UF"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
    } else if(cargo == "Senador"){
      selectizeInput("AGREGACAO_REGIONAL1",
                     label = NULL,
                     choices = 
                       c("","Brasil"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
      
    } else if(cargo == "Prefeito" |
              cargo == "Vereador"){
      selectizeInput("AGREGACAO_REGIONAL1",
                     label = NULL,
                     choices = 
                       c("", "Município", "Quantidade de eleitores aptos"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação'))
    } else if(is.null(indicador) | is.na(indicador) &
              is.null(cargo) | is.na(cargo)){
      return()
    }
    })
  
  
  
    uf1 <- reactive({
    indicador <- req(input$INDICADORES_FRAG)
    cargo <- req(input$DESCRICAO_CARGO1)
    agregacao <- req(input$AGREGACAO_REGIONAL1)
    if(length(agregacao == "UF") > 0){
      return(input$UF1)
    } 
  })
  
  
  output$UF1 <- renderUI({
    indicador <- req(input$INDICADORES_FRAG)
    cargo <- req(input$DESCRICAO_CARGO1)
    agregacao <- req(input$AGREGACAO_REGIONAL1)
    if(cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return()
    }else if(cargo != "Senador" &
             cargo != "Prefeito" &
             cargo != "Vereador" &
      length(agregacao == "UF") > 0){
      selectizeInput("UF1",
                     label = NULL,
                     choices = 
                       c("","Todas UFs", "AC", "AL", "AM", "AP", "BA",
                         "CE", "DF", "ES","GO", "MA", "MG",
                         "MS", "MT", "PA", "PB", "PE", "PI",
                         "PR", "RJ", "RN", "RO", "RR","RS", 
                         "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
     } else{
      return()
    }
  })
  
  
  ### Municipio
 
  frag_leg_mun$`Nome do município2` <- paste(frag_leg_mun$`Nome do município`, "-",
                                             frag_leg_mun$UF)
  
  frag_leg_pf$`Nome do município2` <- paste(frag_leg_pf$`Nome do município`, "-",
                                             frag_leg_pf$UF)
  
  distcad_mun$`Nome do município2` <- paste(distcad_mun$`Nome do município`, "-",
                                             distcad_mun$UF)
  
  
  municipio1 <- reactive({
    indicador <- req(input$INDICADORES_FRAG)
    cargo <- req(input$DESCRICAO_CARGO1)
    agregacao <- req(input$AGREGACAO_REGIONAL1)
    if(length(agregacao == "Município") > 0){
      return(input$MUN1)
    } 
  })
  
  
  output$MUN1 <- renderUI({
    indicador <- req(input$INDICADORES_FRAG)
    cargo <- req(input$DESCRICAO_CARGO1)
    agregacao <- req(input$AGREGACAO_REGIONAL1)
    if(cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              agregacao == "Quantidade de eleitores aptos"){
      return()
    } else if((cargo == "Prefeito" | 
               cargo == "Vereador") &
              length(agregacao == "Município") > 0){
      selectizeInput("MUN1",
                     label = NULL,
                     choices = 
                       c("","Todos os municípios", municipios),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um município'))
    } else{
      return()
    }
  })
  
  
  ### Intervalos de eleitores aptos
  
  intervalos1 <- reactive({
    indicador <- req(input$INDICADORES_FRAG)
    cargo <- req(input$DESCRICAO_CARGO1)
    agregacao <- req(input$AGREGACAO_REGIONAL1)
    if(length(agregacao == "Quantidade de eleitores aptos") > 0){
      return(input$INT1)
    } 
  })
  
  
  output$INT1 <- renderUI({
    indicador <- req(input$INDICADORES_FRAG)
    cargo <- req(input$DESCRICAO_CARGO1)
    agregacao <- req(input$AGREGACAO_REGIONAL1)
    if(cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador" )&
              agregacao == "Município"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              length(agregacao == "Quantidade de eleitores aptos") > 0){
      selectizeInput("INT1",
                     label = NULL,
                     choices = 
                       c("",
                         "Até 5 mil eleitores",
                         "De 5 a 10 mil eleitores",
                         "De 10 a 20 mil eleitores",
                         "De 20 a 50 mil eleitores",
                         "De 50 a 100 mil eleitores",
                         "De 100 a 200 mil eleitores",
                         "Acima de 200 mil eleitores"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um intervalo de eleitores'))
    } else{
      return()
    }
  })
  
  
  
  ## Funcao que permite que o menu seja ocultado
  
  observeEvent(input$showpanel1, {
    if(input$showpanel1 == TRUE) {
      removeCssClass("Main1", "col-sm-12")
      addCssClass("Main1", "col-sm-8")
      shinyjs::show(id = "Sidebar1")
      shinyjs::enable(id = "Sidebar1")
    }
    else {
      removeCssClass("Main1", "col-sm-8")
      addCssClass("Main1", "col-sm-12")
      shinyjs::hide(id = "Sidebar1")
    }
  })
  
  
### Renovacao parlamentar
  
  
  indicadores2 <- reactive({
    cargo <- input$DESCRICAO_CARGO2
    if(length(cargo) > 0){
      return(input$INDICADORES_RENOV)
    } 
  })
  
  
  output$INDICADORES_RENOV <- renderUI({
    cargo <- input$DESCRICAO_CARGO2
    if(length(cargo) > 0){
      selectizeInput(inputId = "INDICADORES_RENOV",
                     label = NULL, 
                     choices = c("","Reeleição", "Reeleição líquida",
                                 "Renovação", "Renovação líquida",
                                 "Recandidaturas"), ## Indicadores disponiveis
                     selected = NULL,
                     options = list(placeholder = 'Escolha um indicador'))
    }
  })
  
  
  agregacao2 <- reactive({
    indicador <- input$INDICADORES_RENOV
    if(length(indicador) > 0){
      return(input$AGREGACAO_REGIONAL2)
    } 
  })
  
  
  output$AGREGACAO_REGIONAL2 <- renderUI({
    cargo <- req(input$DESCRICAO_CARGO2)
    if(cargo == "Deputado Federal"){
      selectizeInput("AGREGACAO_REGIONAL2",
                     label = NULL,
                     choices = 
                       c("", "Brasil", "UF"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
    } else if(cargo == "Deputado Estadual"){
      selectizeInput("AGREGACAO_REGIONAL2",
                     label = NULL,
                     choices = 
                       c("", "Brasil", "UF"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
    } else if(cargo == "Prefeito" |
              cargo == "Vereador"){
      selectizeInput("AGREGACAO_REGIONAL2",
                     label = NULL,
                     choices = c("","Município", "Quantidade de eleitores aptos"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação'))
    }
  })
  
  
  
   uf2 <- reactive({
    agregacao <- req(input$AGREGACAO_REGIONAL2)
    if(length(agregacao == "UF") > 0){
      return(input$UF2)
    } 
  })
  
  
  output$UF2 <- renderUI({
    agregacao <- req(input$AGREGACAO_REGIONAL2)
    cargo <- req(input$DESCRICAO_CARGO2)
    if((cargo == "Deputado Federal" |
        cargo == "Deputado Estadual") & 
       agregacao == "Brasil"){
      return()
    }else if(cargo == "Deputado Estadual" | 
       cargo == "Deputado Federal" &
      length(agregacao == "UF") > 0){
      selectizeInput("UF2",
                     label = NULL,
                     choices = 
                       c("","Todas UFs","AC", "AL", "AM", 
                         "AP", "BA", "CE", "DF", "ES","GO",
                         "MA", "MG","MS", "MT", "PA", "PB", 
                         "PE", "PI","PR", "RJ", "RN", "RO", 
                         "RR","RS", "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
    } else{
      return()
    }
  })
  
  
  ### Municipio
  
  renov_parl_mun$`Nome do município2` <- paste(renov_parl_mun$`Nome do município`, "-",
                                             renov_parl_mun$UF)
  
  #renov_parl_pf$`Nome do município2` <- paste(renov_parl_pf$`Nome do município`, "-",
   #                                            renov_parl_pf$UF)
  
  municipio2 <- reactive({
    indicador <- req(input$INDICADORES_RENOV)
    cargo <- req(input$DESCRICAO_CARGO2)
    agregacao <- req(input$AGREGACAO_REGIONAL2)
    if(length(agregacao == "Município") > 0){
      return(input$MUN2)
    } 
  })
  
  
  output$MUN2 <- renderUI({
    indicador <- req(input$INDICADORES_RENOV)
    cargo <- req(input$DESCRICAO_CARGO2)
    agregacao <- req(input$AGREGACAO_REGIONAL2)
    if((cargo == "Deputado Federal" |
        cargo == "Deputado Estadual") & 
        agregacao == "Brasil"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              agregacao == "Quantidade de eleitores aptos"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              length(agregacao == "Município") > 0){
      selectizeInput("MUN2",
                     label = NULL,
                     choices = 
                       c("","Todos os municípios", municipios),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um município'))
    } else{
      return()
    }
  })
  
  ### Intervalos de eleitores aptos
  
  intervalos2 <- reactive({
    indicador <- req(input$INDICADORES_RENOV)
    cargo <- req(input$DESCRICAO_CARGO2)
    agregacao <- req(input$AGREGACAO_REGIONAL2)
    if(length(agregacao == "Quantidade de eleitores aptos") > 0){
      return(input$INT2)
    } 
  })
  
  
  output$INT2 <- renderUI({
    indicador <- req(input$INDICADORES_RENOV)
    cargo <- req(input$DESCRICAO_CARGO2)
    agregacao <- req(input$AGREGACAO_REGIONAL2)
    if((cargo == "Deputado Federal" |
        cargo == "Deputado Estadual") & 
       agregacao == "Brasil"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              agregacao == "Município"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              length(agregacao == "Quantidade de eleitores aptos") > 0){
      selectizeInput("INT2",
                     label = NULL,
                     choices = 
                       c("",
                         "Até 5 mil eleitores",
                         "De 5 a 10 mil eleitores",
                         "De 10 a 20 mil eleitores",
                         "De 20 a 50 mil eleitores",
                         "De 50 a 100 mil eleitores",
                         "De 100 a 200 mil eleitores",
                         "Acima de 200 mil eleitores"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um intervalo de eleitores'))
    } else{
      return()
    }
  })
  
  
  
  ## Funcao que permite que o menu seja ocultado
  
  observeEvent(input$showpanel2, {
    if(input$showpanel2 == TRUE) {
      removeCssClass("Main2", "col-sm-12")
      addCssClass("Main2", "col-sm-8")
      shinyjs::show(id = "Sidebar2")
      shinyjs::enable(id = "Sidebar2")
    }
    else {
      removeCssClass("Main2", "col-sm-8")
      addCssClass("Main2", "col-sm-12")
      shinyjs::hide(id = "Sidebar2")
    }
  })
  
  
  ## Alienacao
  
  indicadores3 <- reactive({
    cargo <- input$DESCRICAO_CARGO3
    if(length(cargo) > 0){
      return(input$INDICADORES_ALIE)
    } 
  })
  
  output$INDICADORES_ALIE <- renderUI({
    cargo <- input$DESCRICAO_CARGO3
    if(length(cargo) > 0){
  selectizeInput(inputId = "INDICADORES_ALIE",
                 label = NULL, 
                 choices = list("", 
                                `Indicadores percentuais` = c("Abstenção percentual", "Votos nulos percentuais", 
                             "Votos brancos percentuais", "Alienação percentual"),
                             `Indicadores absolutos` = c("Abstenção absoluta", "Votos nulos absolutos", 
                             "Votos brancos absolutos", "Alienação absoluta")), ## Indicadores disponiveis
                 selected = NULL,
                 options = list(placeholder = 'Escolha um indicador'))
    }
  })
  
  
  agregacao3 <- reactive({
    indicador <- input$INDICADORES_ALIE
    if(length(indicador) > 0){
      return(input$AGREGACAO_REGIONAL3)
    } 
  })
  
  output$AGREGACAO_REGIONAL3 <- renderUI({
    cargo <- req(input$DESCRICAO_CARGO3)
    if(cargo != "Vereador" &
       cargo != "Prefeito"){
      selectizeInput("AGREGACAO_REGIONAL3",
                     label = NULL,
                     choices = 
                       c("", "Brasil", "UF"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
    } else if(cargo == "Prefeito" |
              cargo == "Vereador"){
      selectizeInput("AGREGACAO_REGIONAL3",
                     label = NULL,
                     choices = c("","Município", "Quantidade de eleitores aptos"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação'))
    }
  })
  
  
  uf3 <- reactive({
    cargo <- req(input$DESCRICAO_CARGO3)
    agregacao <- req(input$AGREGACAO_REGIONAL3)
    if(cargo != "Vereador" &
      agregacao == "UF"){
      return(input$UF3)
    } 
  })
  
  
  output$UF3 <- renderUI({
    cargo <- req(input$DESCRICAO_CARGO3)
    agregacao <- req(input$AGREGACAO_REGIONAL3)
    if((cargo != "Prefeito" | 
      cargo != "Vereador") &
      agregacao == "UF"){
      selectizeInput("UF3",
                     label = NULL,
                     choices = 
                       c("","Todas UFs","AC", "AL", "AM", 
                         "AP", "BA", "CE", "DF", "ES","GO",
                         "MA", "MG","MS", "MT", "PA", "PB", 
                         "PE", "PI","PR", "RJ", "RN", "RO", 
                         "RR","RS", "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
    } else{
      return()
    }
  })
  
  ### Municipio
  
  alien_mun$`Nome do município2` <- paste(alien_mun$`Nome do município`, "-",
                                             alien_mun$UF)
  
  municipio3 <- reactive({
    indicador <- req(input$INDICADORES_ALIE)
    cargo <- req(input$DESCRICAO_CARGO3)
    agregacao <- req(input$AGREGACAO_REGIONAL3)
    if(length(agregacao == "Município") > 0){
      return(input$MUN3)
    } 
  })
  
  
  output$MUN3 <- renderUI({
    indicador <- req(input$INDICADORES_ALIE)
    cargo <- req(input$DESCRICAO_CARGO3)
    agregacao <- req(input$AGREGACAO_REGIONAL3)
    if(cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return()
    } else if((cargo == "Prefeito" |
             cargo == "Vereador") &
              agregacao == "Quantidade de eleitores aptos"){
      return()
    } else if((cargo == "Prefeito" |
              cargo == "Vereador") &
              length(agregacao == "Município") > 0){
      selectizeInput("MUN3",
                     label = NULL,
                     choices = 
                       c("","Todos os municípios", municipios),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um município'))
    } else{
      return()
    }
  })
  
  
  ### Intervalos de eleitores aptos
  
  intervalos3 <- reactive({
    indicador <- req(input$INDICADORES_ALIE)
    cargo <- req(input$DESCRICAO_CARGO3)
    agregacao <- req(input$AGREGACAO_REGIONAL3)
    if(length(agregacao == "Quantidade de eleitores aptos") > 0){
      return(input$INT3)
    } 
  })
  
  
  output$INT3 <- renderUI({
    indicador <- req(input$INDICADORES_ALIE)
    cargo <- req(input$DESCRICAO_CARGO3)
    agregacao <- req(input$AGREGACAO_REGIONAL3)
    if(cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return()
    } else if((cargo == "Prefeito" | 
               cargo == "Vereador") &
              agregacao == "Município"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              length(agregacao == "Quantidade de eleitores aptos") > 0){
      selectizeInput("INT3",
                     label = NULL,
                     choices = 
                       c("",
                         "Até 5 mil eleitores",
                         "De 5 a 10 mil eleitores",
                         "De 10 a 20 mil eleitores",
                         "De 20 a 50 mil eleitores",
                         "De 50 a 100 mil eleitores",
                         "De 100 a 200 mil eleitores",
                         "Acima de 200 mil eleitores"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um intervalo de eleitores'))
    } else{
      return()
    }
  })
  
  
  ## Funcao que permite que o menu seja ocultado
  
  observeEvent(input$showpanel3, {
    if(input$showpanel3 == TRUE) {
      removeCssClass("Main3", "col-sm-12")
      addCssClass("Main3", "col-sm-8")
      shinyjs::show(id = "Sidebar3")
      shinyjs::enable(id = "Sidebar3")
    }
    else {
      removeCssClass("Main3", "col-sm-8")
      addCssClass("Main3", "col-sm-12")
      shinyjs::hide(id = "Sidebar3")
    }
  })
  
  
  ## Volatilidade
  
  ### Agregacao regional
  
  indicadores4 <- reactive({
    cargo <- input$DESCRICAO_CARGO4
    if(length(cargo) > 0){
      return(input$INDICADORES_VOL)
    } 
  })
  
  output$INDICADORES_VOL <- renderUI({
    cargo <- input$DESCRICAO_CARGO4
    if(cargo == "Prefeito"){
  selectizeInput(inputId = "INDICADORES_VOL",
                 label = NULL, 
                 choices = c("","Volatilidade eleitoral"), ## Indicadores disponiveis
                 selected = NULL,
                 options = list(placeholder = 'Escolha um indicador'))
    } else if(cargo != "Prefeito"){
    selectizeInput(inputId = "INDICADORES_VOL",
                   label = NULL, 
                   choices = c("","Volatilidade eleitoral",
                               "Volatilidade parlamentar"), ## Indicadores disponiveis
                   selected = NULL,
                   options = list(placeholder = 'Escolha um indicador'))
    } else{
      return()
    }
  })
  
  
  agregacao4 <- reactive({
    indicador <- input$INDICADORES_VOL
    if(length(indicador) > 0){
      return(input$AGREGACAO_REGIONAL4)
    } 
  })
  
  
  output$AGREGACAO_REGIONAL4 <- renderUI({
    cargo <- req(input$DESCRICAO_CARGO4)
    if(cargo == "Deputado Federal"){
      selectizeInput("AGREGACAO_REGIONAL4",
                     label = NULL,
                     choices = 
                       c("", "Brasil", "UF"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
    } else if(cargo == "Deputado Estadual"){
      selectizeInput("AGREGACAO_REGIONAL4",
                     label = NULL,
                     choices = 
                       c("","UF"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
      
    } else if(cargo == "Prefeito" |
              cargo == "Vereador"){
      selectizeInput("AGREGACAO_REGIONAL4",
                     label = NULL,
                     choices = c("","Município", "Quantidade de eleitores aptos"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação'))
    } else{
      return()
    }
  })
  
  ### UF
  
  uf4 <- reactive({
    agregacao <- req(input$AGREGACAO_REGIONAL4)
    if(length(agregacao == "UF") > 0){
      return(input$UF4)
    } 
  })
  
  
  output$UF4 <- renderUI({
    agregacao <- req(input$AGREGACAO_REGIONAL4)
    cargo <- req(input$DESCRICAO_CARGO4)
    if(cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return()
    }else if(cargo == "Deputado Estadual" | 
             cargo == "Deputado Federal" &
             length(agregacao == "UF") > 0){
      selectizeInput("UF4",
                     label = NULL,
                     choices = 
                       c("","Todas UFs","AC", "AL", "AM", 
                         "AP", "BA", "CE", "DF", "ES","GO",
                         "MA", "MG","MS", "MT", "PA", "PB", 
                         "PE", "PI","PR", "RJ", "RN", "RO", 
                         "RR","RS", "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
    } else{
      return()
    }
  })
  
  ### Municipio
  
  vol_mun$`Nome do município2` <- paste(vol_mun$`Nome do município`, "-",
                                             vol_mun$UF)
  
  vol_pf$`Nome do município2` <- paste(vol_pf$`Nome do município`, "-",
                                        vol_pf$UF)
  
  municipio4 <- reactive({
    indicador <- req(input$INDICADORES_VOL)
    cargo <- req(input$DESCRICAO_CARGO4)
    agregacao <- req(input$AGREGACAO_REGIONAL4)
    if(length(agregacao == "Município") > 0){
      return(input$MUN4)
    } 
  })
  
  
  output$MUN4 <- renderUI({
    indicador <- req(input$INDICADORES_VOL)
    cargo <- req(input$DESCRICAO_CARGO4)
    agregacao <- req(input$AGREGACAO_REGIONAL4)
    if(cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
             agregacao == "Quantidade de eleitores aptos"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              length(agregacao == "Município") > 0){
      selectizeInput("MUN4",
                     label = NULL,
                     choices = 
                       c("","Todos os municípios", municipios),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um município'))
    } else{
      return()
    }
  })
  
  ### Intervalos de eleitores aptos
  
  intervalos4 <- reactive({
    indicador <- req(input$INDICADORES_VOL)
    cargo <- req(input$DESCRICAO_CARGO4)
    agregacao <- req(input$AGREGACAO_REGIONAL4)
    if(length(agregacao == "Quantidade de eleitores aptos") > 0){
      return(input$INT4)
    } 
  })
  
  
  output$INT4 <- renderUI({
    indicador <- req(input$INDICADORES_VOL)
    cargo <- req(input$DESCRICAO_CARGO4)
    agregacao <- req(input$AGREGACAO_REGIONAL4)
    if(cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              agregacao == "Município"){
      return()
    } else if((cargo == "Prefeito" |
               cargo == "Vereador") &
              length(agregacao == "Quantidade de eleitores aptos") > 0){
      selectizeInput("INT4",
                     label = NULL,
                     choices = 
                       c("",
                         "Até 5 mil eleitores",
                         "De 5 a 10 mil eleitores",
                         "De 10 a 20 mil eleitores",
                         "De 20 a 50 mil eleitores",
                         "De 50 a 100 mil eleitores",
                         "De 100 a 200 mil eleitores",
                         "Acima de 200 mil eleitores"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um intervalo de eleitores'))
    } else{
      return()
    }
  })
  
  
  ## Funcao que permite que o menu seja ocultado
  
  observeEvent(input$showpanel4, {
    if(input$showpanel4 == TRUE) {
      removeCssClass("Main4", "col-sm-12")
      addCssClass("Main4", "col-sm-8")
      shinyjs::show(id = "Sidebar4")
      shinyjs::enable(id = "Sidebar4")
    }
    else {
      removeCssClass("Main4", "col-sm-8")
      addCssClass("Main4", "col-sm-12")
      shinyjs::hide(id = "Sidebar4")
    }
  })
  
  
# 2. Indicadores ------------------------------------------------------------
  
  ## Definicao das atribuicoes das tabela dos indicadores e seus respectivos botoes de acao
  ## Definicao de cada indicador
  
# 2.1. Fragmentacao legislativa -------------------------------------------- 
  
  ## Interpretacao do indicador
  
 selected <-  reactive({
    
    indicador <- input$INDICADORES_FRAG
    
    ano <- input[["column_clicked"]]
    
    cargo <- input$DESCRICAO_CARGO1
   
    cell <- input$dpg_br_cell_clicked
    
    value <- cell$value
    
    if(length(ano) > 0){
    
    texto <- paste0("A ", indicador,
                   " existente entre os votos recebidos pelos partidos e 
                   as cadeiras conquistadas pelos mesmos no ano de ", ano, 
                   " para o cargo de ", cargo, ", foi igual a ", value, ".")
    }   
  })
  
  output$info <- renderText({
    selected()
  })
 

  ## Modal para ajuda
  
  ### Resumo
  
  observeEvent(input$modal_frag,{
    showModal(modalDialog(
                          title = h4(class = "h4 titulo",
                                          "AJUDA"),
                          footer = modalButton("FECHAR"), 
                          size = "m",
                          htmlOutput("def_frag"),
                          easyClose = TRUE,
                          style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "))
  })
  
  ### Dados desagregados
  
  observeEvent(input$modal_frag_ag,{
    showModal(modalDialog(
                          title = h4(class = "h4 titulo",
                                          "AJUDA"),
                          footer = modalButton("FECHAR"), 
                          size = "m",
                          htmlOutput("def_frag"),
                          easyClose = TRUE,
                          style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "
                          ))
  })
  
  ## Funcao para descricao dos indicadores de fragmentacao partidaria
  
  output$def_frag <- renderUI({
    note <- paste0("
                   <font color = 'black'>
                   <h4>Número efetivo de partidos</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>O conceito de número efetivo de partidos define o grau de fragmentação do sistema partidário, 
                   através da ponderação da força relativa das legendas que compõem o parlamento. O valor calculado 
                   aponta a quantidade de partidos com alguma relevância em um dado sistema político. O NEP é 
                   calculado dividindo-se 1 pelo somatório do quadrado das proporções de <b>votos</b> ou de 
                   <b>cadeiras</b> obtidos pelos partidos em uma dada eleição.</p>
                   <p style='line-height:150%'>Quando calculado utilizando-se votos, o NEP exprime a fragmentação eleitoral do sistema partidário,
                   isto é a quantidade de partidos que contam efetivamente para a competição em eleições. O NEP calculado a partir das cadeiras 
                   exprime a fragmentação de uma casa legislativa em termos dos partidos com alguma força substantiva dentro da instituição. 
                   O primeiro, o NEP eleitoral, é frequentemente utilizado para mensurar o grau de dispersão da competição política em um país, 
                   isto é, para saber se a disputa por cargos envolve poucos ou muitos partidos. O segundo, o NEP legislativo, indica o grau de 
                   dispersão do poder legislativo entre os partidos que compõem um órgão legislativo. Através dele pode-se saber quantos partidos 
                   estão em condições de influenciar de forma efetiva o processo legislativo</p></h5>
                   <p><h5 align = 'justify'>
                   <strong>Fórmula: </strong>
                   <p>
                   <p><i>Número efetivo de partidos eleitoral </i></p>
                   NEP = 1/ &sum;(pv<sup>2</sup>),
                   <p>onde pv = proporção de votos obtidos pelos partidos.</p>
                   <p><i>Número efetivo de partidos legislativo </i></p>
                   NEP = 1/ &sum;(pc<sup>2</sup>),
                   <p>onde pc = proporção de cadeiras obtidas pelos partidos.</p>
                   <p><br /></h5>
                   <h4>Fracionalização </h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Este indicador tem por objetivo medir a dispersão partidária de um parlamento. 
                   Ele indica qual a probabilidade de dois parlamentares desse parlamento, 
                   tomados ao acaso, pertecerem a partidos diferentes.</p></h5>
                   <p>
                   <p><h5 align = 'justify'>
                   <strong>Fórmula: </strong>
                   <p>
                   <p>
                   F = 1 - &sum;(pe<sup>2</sup>), 
                   <p>onde pe = percentual de cadeiras ocupadas por partido.</p></h5>
                   <p><br />                 
                   <h4>Fracionalização máxima</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>A 'fracionalização máxima' não depende da votação dos partidos, mas da quantidade 
                   de cadeiras e partidos com representação parlamentar.<p></h5>
                   <p>
                   <p><h5 align = 'justify'>
                   <strong>Fórmula: </strong>
                   <p>
                   <p>
                   FM = N*(n-1)/n*(N-1), 
                   <p> onde N = número de cadeiras e n = número de partidos com representação parlamentar.</p></h5>
                   <p><br />      
                   <h4>Fragmentação</h4>
                   <h5 align = 'justify'><br />
                   A fragmentação mede quanto o índice de fracionalização se aproxima da fracionalização
                   máxima.</h5>
                   <p>
                   <p><h5 align = 'justify'>
                   <strong>Fórmula: </strong>
                   <p>
                   <p>
                   FG = (Índice de fracionalização)/(Índice de fracionalização máxima)</h5>
                   <p>
                   <h4><br />Desproporcionalidade</h4>
                   <h5 align = 'justify'><br />
                   O índice proposto por Gallagher consiste na diferença dos percentuais de votos e de cadeiras obtidas por cada partido.</h5>
                   <p>
                   <h5 align = 'justify'><br />
                   <strong>Fórmula: </strong>
                   <p>
                   <p>
                   D = &radic;&sum;(vi - si)<sup>2</sup>/2,
                   <p>onde vi = percentual de votos e si = percentual de cadeiras.</p></h5>
                   <p>
                   <h4><br />Quociente Eleitoral</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>É o número mínimo de votos que um partido ou coligação deve atingir 
                   em determinada UF e eleição para garantir uma vaga.</p></h5>
                   <p>
                   <p><h5 align = 'justify'>
                   <strong>Fórmula: </strong>
                   <p>
                   <p>
                   QE = (Votos válidos)/(Número de vagas existentes)</h5>
                   <p>
                   <h4><br/ > Quociente Partidário</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>O indicador representa o número de vagas que o partido ou coligação obteve, 
                   excluindo as vagas distribuídas por média.</p></h5>
                   <p>
                   <p><h5 align = 'justify'>
                   <strong>Fórmula: </strong>
                   <p>
                   <p>
                   QP = Número de votos válidos do partido ou coligação/Quociente eleitoral</h5>
                   <p>
                   <p>
                   <p><h5 align = 'justify'><strong>Fonte:</strong>
                   <p>
                   <p>1. Votos e partidos: almanaque de dados eleitorais: Brasil e outros 
                   países/ Organização de Wanderley Guilherme dos Santos, com a colaboração de Fabrícia Guimarães. -
                   Rio de Janeiro: Editora FGV, 2002); 
                   <p>2. <a href='http://datapolitica.com.br/eleicao/metodologia.html'>Data Politica</a></p>
                   <p>3. <a href='http://www.tse.jus.br/eleitor/glossario/termos/quociente-eleitoral'>
                   Quociente eleitoral - TSE </a></p>
                   <p>4.<a href='http://www.tse.jus.br/eleitor/glossario/termos/quociente-partidario'>
                   Quociente partidário - TSE </h5></a></p></font>")
    HTML(note)
  }) 
  
# 2.1.1. Desproporcionalidade --------------------------------  
  
  ## Resumo
  
  ### Fragmentacao legislativa (Brasil)
  
  
  dpgbr <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Desproporcionalidade" & 
      agregacao == "Brasil"){
      return(input$dpg_br)
    }
  })
  
  output$dpg_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bdpg_br()
  })
  
  bdpg_br <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'dpg_br',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        if(indicador == "Desproporcionalidade" & 
            agregacao == "Brasil"){
          frag_leg_br %>% 
            ungroup() %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          `Desproporcionalidade`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Desproporcionalidade`)
        }
      })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (Brasil)
  
  ag_dpg_br <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Desproporcionalidade" & 
       agregacao == "Brasil"){
      return(input$agreg_dpg_br)
    }
  })
  
  output$agreg_dpg_br <- DT::renderDataTable(server = FALSE,{
    bagreg_dpg_br()
  })
  
  bagreg_dpg_br <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'dpg_br_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
        class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        if(indicador == "Desproporcionalidade" & 
           agregacao == "Brasil"){
          data = frag_leg_br %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            select(`Ano da eleição`,
                   Cargo,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   Desproporcionalidade) %>% 
            unique()
        } else{
          return()
        }
      })
  })
  
  
  ## Resumo
  
  ### Fragmentacao legislativa (UF)
  
  
  dpguf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Desproporcionalidade" & 
       agregacao == "UF"){
      return(input$dpg_uf)
    }
  })
  
  output$dpg_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bdpg_uf()
  })
  
  bdpg_uf <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'dpg_uf',
        bom = TRUE))), 
        class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        uf <- req(input$UF1)
        if(indicador == "Desproporcionalidade" & 
          agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            frag_leg_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Desproporcionalidade`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Desproporcionalidade`)
          } else{
            frag_leg_uf %>% 
            dplyr::filter(UF == input$UF1 & 
                            Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Desproporcionalidade`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Desproporcionalidade`)
          }
        } else{
          return()
        }
      })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (UF)  
  
  ag_dpg_uf <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Desproporcionalidade" & 
       agregacao == "UF"){
      return(input$agreg_dpg_uf)
    } else{
      return()
    }
  })
  
  output$agreg_dpg_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_dpg_uf()
  })
  
  bagreg_dpg_uf <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'dpg_uf_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
        class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        uf <- req(input$UF1)
        if(indicador == "Desproporcionalidade" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = frag_leg_uf %>%
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
              select(`Ano da eleição`,
                     UF,
                     Cargo,
                     `Número efetivo de partidos eleitoral`,
                     `Número efetivo de partidos legislativo`,
                     `Fracionalização`,
                     `Fracionalização máxima`,
                     `Fragmentação`,
                     Desproporcionalidade) %>% 
            unique()
          } else{
            data = frag_leg_uf %>% 
            dplyr::filter(UF == input$UF1 & 
                          Cargo == input$DESCRICAO_CARGO1) %>% 
              select(`Ano da eleição`,
                     UF,
                     Cargo,
                     `Número efetivo de partidos eleitoral`,
                     `Número efetivo de partidos legislativo`,
                     `Fracionalização`,
                     `Fracionalização máxima`,
                     `Fragmentação`,
                     Desproporcionalidade) %>% 
              unique()
        }
        } else{
          return()
        }
      })
  })
  
  
  ## Resumo
  
  ### Fragmentacao legislativa (MUN)
  
  
  dpgmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Desproporcionalidade" & 
       agregacao == "Município"){
      return(input$dpg_mun)
    }
  })
  
  output$dpg_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bdpg_mun()
  })
  
  bdpg_mun <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'dpg_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Desproporcionalidade" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           frag_leg_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Desproporcionalidade`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Desproporcionalidade`)
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 &
                                            Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Desproporcionalidade`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Desproporcionalidade`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_dpg_mun <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Desproporcionalidade" & 
       agregacao == "Município"){
      return(input$agreg_dpg_mun)
    } else{
      return()
    }
  })
  
  output$agreg_dpg_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_dpg_mun()
  })
  
  bagreg_dpg_mun <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'dpg_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Desproporcionalidade" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = frag_leg_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 & 
                                            Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Fragmentacao legislativa (Intervalo de eleitores aptos)
  
  
  
  dpgmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Desproporcionalidade" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$dpg_med)
    }
  })
  
  output$dpg_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bdpg_med()
  })
  
  bdpg_med <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE,
      searching = FALSE,
      paging = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Desproporcionalidade" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                            Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional da desproporcionalidade`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da desproporcionalidade`)%>% 
                             mutate("media" = "Média nacional da desproporcionalidade") %>% 
                             column_to_rownames("media")
                           
                           media2 <- frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média da desproporcionalidade`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da desproporcionalidade`)%>% 
                             mutate("media" = "Média da desproporcionalidade do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                         }
                       } else{
                         return()
                       }
                     })
  })
  
  
  dpgint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Desproporcionalidade" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$dpg_int)
    }
  })
  
  output$dpg_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bdpg_int()
  })
  
  bdpg_int <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'dpg_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Desproporcionalidade" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Desproporcionalidade`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Desproporcionalidade`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_dpg_int <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Desproporcionalidade" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_dpg_int)
    } else{
      return()
    }
  })
  
  output$agreg_dpg_int <- DT::renderDataTable(server = FALSE,{
    bagreg_dpg_int()
  })
  
  bagreg_dpg_int <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'dpg_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Desproporcionalidade" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(indicador != "Desproporcionalidade"){
                           return()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 & 
                                           Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
# 1.1.1. Fracionalizacao -------------------------------------------------- 


## Resumo
  
### Fragmentacao legislativa (Brasil)
  
  
  fraciobr <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização" & 
       cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return(input$fracio_br)
    } else{
      return()
    }
  })
  
  output$fracio_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfracio_br()
  })
  
  bfracio_br <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'fracio_br',
                  bom = TRUE))), 
                class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                            'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      uf <- input$UF1
      if(indicador == "Fracionalização" & 
         agregacao == "Brasil"){
        frag_leg_br %>% 
          ungroup() %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
          dplyr::select(`Ano da eleição`,
                        `Fracionalização`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Fracionalização`)
      } else{
        return()
      }
    })
  })  
  
## Dados desagregados
  
### Fragmentacao legislativa (Brasil)
  
  ag_fracio_br <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização" & 
       agregacao == "Brasil"){
      return(input$agreg_fracio_br)
    } else{
      return()
    }
  })
  
  output$agreg_fracio_br <- DT::renderDataTable(server = FALSE,{
    bagreg_fracio_br()
  })
  
  bagreg_fracio_br <- eventReactive(input$BCALC1, {
    datatable(options = list(
                scrollX = TRUE,
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 1
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'fracio_br_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
                class = "display",
                rownames = FALSE,
                extensions = c('Buttons', 
                               'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      if(indicador == "Fracionalização" & 
         agregacao == "Brasil"){
        data = frag_leg_br %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
          select(`Ano da eleição`,
                 Cargo,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 Desproporcionalidade) %>% 
          unique() 
      } else{
        return()
      }
    })
  })
  
  ## Resumo
  
  ### Fragmentacao legislativa (UF)
  
  
  fraciouf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização" & 
       agregacao == "UF"){
      return(input$fracio_uf)
    } else{
      return()
    }
  })
  
  output$fracio_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfracio_uf()
  })
  
  bfracio_uf <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_uf',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        uf <- req(input$UF1)
        if(indicador == "Fracionalização" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            frag_leg_uf %>% 
            ungroup() %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fracionalização`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fracionalização`)
          } else{
            frag_leg_uf %>% 
            ungroup() %>% 
            dplyr::filter(UF == input$UF1 & 
                          Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          `Fracionalização`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fracionalização`)
           }
        } else{
          return()
        }
      })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (UF)  
  
  ag_fracio_uf <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização" & 
       agregacao == "UF"){
      return(input$agreg_fracio_uf)
    }
  })
  
  output$agreg_fracio_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_fracio_uf()
  })
  
  bagreg_fracio_uf <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'fracio_uf_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        uf <- req(input$UF1)
        if(indicador == "Fracionalização" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
          data = frag_leg_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>%
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   Desproporcionalidade) %>% 
            unique() 
          } else{
          data = frag_leg_uf %>% 
            dplyr::filter(UF == input$UF1 & 
                          Cargo == input$DESCRICAO_CARGO1) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   Desproporcionalidade) %>% 
            unique()
          }
        } else{
          return()
        }
      })
  })  
  
  
  ## Resumo
  
  ### Fragmentacao legislativa (MUN)
  
  
  fraciomun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fracionalização" & 
       agregacao == "Município"){
      return(input$fracio_mun)
    } else{
      return()
    }
  })
  
  output$fracio_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bfracio_mun()
  })
  
  bfracio_mun <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Fracionalização" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           frag_leg_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Fracionalização`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Fracionalização`)
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Fracionalização`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Fracionalização`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_fracio_mun <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização" & 
       agregacao == "Município"){
      return(input$agreg_fracio_mun)
    } else{
      return()
    }
  })
  
  output$agreg_fracio_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_fracio_mun()
  })
  
  bagreg_fracio_mun <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'fracio_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Fracionalização" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = frag_leg_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Fragmentacao legislativa (Intervalo de eleitores aptos)
  
  
  
  fraciomed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fracionalização" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$fracio_med)
    } else{
      return()
    }
  })
  
  output$fracio_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    fracio_med()
  })
  
  fracio_med <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Fracionalização" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                          media1 <- frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional da fracionalização`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da fracionalização`) %>% 
                             mutate("media" = "Média nacional da fracionalização") %>% 
                             column_to_rownames("media")
                          
                          media2 <- frag_leg_mun %>% 
                            dplyr::filter(`Eleitores aptos` == input$INT1 &
                                            Cargo == input$DESCRICAO_CARGO1) %>% 
                            dplyr::select(`Ano da eleição`,
                                          `Média da fracionalização`) %>% 
                            unique() %>% 
                            spread(`Ano da eleição`,
                                   `Média da fracionalização`) %>% 
                            mutate("media" = "Média da fracionalização do grupo") %>% 
                            column_to_rownames("media")
                          
                          media1 <- rbind(media1, media2)
                          
                          media1
                         }
                       } else{
                         return()
                       }
                     })
  }) 
  
  
  fracioint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fracionalização" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$fracio_int)
    } else{
      return()
    }
  })
  
  output$fracio_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    fracio_int()
  })
  
  fracio_int <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Fracionalização" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Fracionalização`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Fracionalização`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_fracio_int <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_fracio_int)
    } else{
      return()
    }
  })
  
  output$agreg_fracio_int <- DT::renderDataTable(server = FALSE,{
    bagreg_fracio_int()
  })
  
  bagreg_fracio_int <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'fracio_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Fracionalização" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
# 1.1.3. Fracionalizacao maxima -------------------------------------------
  
## Resumo
  
### Fragmentacao legislativa (Brasil
  
  fraciomaxbr <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização máxima" 
       & agregacao == "Brasil"){
      return(input$fracio_max_br)
    } else{
      return()
    }
  })
  
  output$fracio_max_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfracio_max_br()
  })
  
  bfracio_max_br <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'fracio_max_br',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons', 
                             'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      if(indicador == "Fracionalização máxima" & 
         agregacao == "Brasil"){
        frag_leg_br %>% 
          ungroup() %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
          dplyr::select(`Ano da eleição`,
                        `Fracionalização máxima`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Fracionalização máxima`)
      } else{
        return()
      }
    })
  })
  
## Dados desagregados
  
### Fragmentacao legislativa (Brasil)
  
  ag_fracio_max_br <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização máxima" & 
       agregacao == "Brasil"){
      return(input$agreg_fracio_max_br)
    } else{
      return()
    }
  })
  
  output$agreg_fracio_max_br <- DT::renderDataTable(server = FALSE,{
    bagreg_fracio_max_br()
  })
  
  bagreg_fracio_max_br <- eventReactive(input$BCALC1, {
    datatable(options = list(
                scrollX = TRUE,
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 1
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'fracio_max_br_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons', 
                             'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      if(indicador == "Fracionalização máxima" & 
         agregacao == "Brasil"){
        data = frag_leg_br %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
          select(`Ano da eleição`,
                 Cargo,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 Desproporcionalidade) %>% 
          unique() 
      } else{
        return()
      }
    })
  })
  
  ## Resumo
  
  ### Fragmentacao legislativa (UF)
  
  fraciomaxuf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fracionalização máxima" 
       & agregacao == "UF"){
      return(input$fracio_max_uf)
    } else{
      return()
    } 
  })
  
  output$fracio_max_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfracio_max_uf()
  })
  
  bfracio_max_uf <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_max_uf',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- req(input$INDICADORES_FRAG)
        agregacao <- req(input$AGREGACAO_REGIONAL1)
        uf <- req(input$UF1)
        if(indicador == "Fracionalização máxima" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = frag_leg_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fracionalização máxima`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                  `Fracionalização máxima`)
          } else{
            data = frag_leg_uf %>% 
            dplyr::filter(UF == input$UF1 & 
                          Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fracionalização máxima`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fracionalização máxima`)
        }
        } else{
          return()
        }
      })
  })
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (UF) 
  
  ag_fracio_max_uf <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização máxima" & 
       agregacao == "UF"){
      return(input$agreg_fracio_max_uf)
    } else{
      return()
    }
  })
  
  output$agreg_fracio_max_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_fracio_max_uf()
  })
  
  bagreg_fracio_max_uf <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'fracio_max_uf_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        uf <- req(input$UF1)
        if(indicador == "Fracionalização máxima" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          }else if(uf == "Todas UFs"){
            data = frag_leg_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>%
              select(`Ano da eleição`,
                     UF,
                     Cargo,
                     `Número efetivo de partidos eleitoral`,
                     `Número efetivo de partidos legislativo`,
                     `Fracionalização`,
                     `Fracionalização máxima`,
                     `Fragmentação`,
                     Desproporcionalidade) %>% 
            unique()
          } else{
            data = frag_leg_uf %>% 
            dplyr::filter(UF == input$UF1 & 
                          Cargo == input$DESCRICAO_CARGO1) %>% 
              select(`Ano da eleição`,
                     UF,
                     Cargo,
                     `Número efetivo de partidos eleitoral`,
                     `Número efetivo de partidos legislativo`,
                     `Fracionalização`,
                     `Fracionalização máxima`,
                     `Fragmentação`,
                     Desproporcionalidade) %>% 
              unique()
        }
        } else{
          return()
        }
      })
  })
  
  
  ## Resumo
  
  ### Fragmentacao legislativa (MUN)
  
  
  fraciomaxmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fracionalização máxima" & 
       agregacao == "Município"){
      return(input$fracio_max_mun)
    } else{
      return()
    }
  })
  
  output$fracio_max_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bfracio_max_mun()
  })
  
  bfracio_max_mun <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_max_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Fracionalização máxima" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           frag_leg_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Fracionalização máxima`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Fracionalização máxima`)
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Fracionalização máxima`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Fracionalização máxima`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_fracio_max_mun <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização máxima" & 
       agregacao == "Município"){
      return(input$agreg_fracio_max_mun)
    } else{
      return()
    }
  })
  
  output$agreg_fracio_max_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_fracio_max_mun()
  })
  
  bagreg_fracio_max_mun <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'fracio_max_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Fracionalização máxima" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = frag_leg_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
  ## Resumo
  
  ### Fragmentacao legislativa (Intervalo de eleitores aptos)
  
  
  fraciomaxmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fracionalização máxima" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$fracio_max_med)
    } else{
      return()
    }
  })
  
  output$fracio_max_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfracio_max_med()
  })
  
  bfracio_max_med <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Fracionalização máxima" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional da fracionalização máxima`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da fracionalização máxima`) %>% 
                             mutate("media" = "Média nacional da fracionalização máxima") %>% 
                             column_to_rownames("media")
                           
                           media2 <- frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média da fracionalização máxima`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da fracionalização máxima`) %>% 
                             mutate("media" = "Média da fracionalização máxima do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                         }
                       } else{
                         return()
                       }
                     })
  }) 
  
  
  fraciomaxint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fracionalização máxima" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$fracio_max_int)
    } else{
      return()
    }
  })
  
  output$fracio_max_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfracio_max_int()
  })
  
  bfracio_max_int <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_max_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Fracionalização máxima" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Fracionalização máxima`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Fracionalização máxima`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (INT)  
  
  ag_fracio_max_int <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fracionalização máxima" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_fracio_max_int)
    } else{
      return()
    }
  })
  
  output$agreg_fracio_max_int <- DT::renderDataTable(server = FALSE,{
    bagreg_fracio_max_int()
  })
  
  bagreg_fracio_max_int <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'fracio_max_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Fracionalização máxima" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
# 1.1.4. Fragmentacao -----------------------------------------------------

## Resumo 
    
### Fragmentacao legislativa (Brasil
  
  fragbr <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fragmentação" & 
       agregacao == "Brasil"){
      return(input$frag_br)
    } else{
      return()
    }
  })
  
  output$frag_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfrag_br()
  })
  
  
  bfrag_br <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'frag_br',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                             'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      if(indicador == "Fragmentação" &
         agregacao == "Brasil"){
        frag_leg_br %>% 
          ungroup() %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
          dplyr::select(`Ano da eleição`,
                        `Fragmentação`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Fragmentação`)
        
      } else{
        return()
      }
    })
  })
  
## Dados desagregados
  
### Fragmentacao legislativa (Brasil  
  
  ag_frag_br <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fragmentação" & 
       agregacao == "Brasil"){
      return(input$agreg_frag_br)
    } else{
      return()
    }
  })
  
  output$agreg_frag_br <- DT::renderDataTable(server = FALSE,{
    bagreg_frag_br()
  })
  
  bagreg_frag_br <- eventReactive(input$BCALC1, {
    datatable(options = list(
                scrollX = TRUE,
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 1
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'frag_br_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                            'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      if(indicador == "Fragmentação" & 
         agregacao == "Brasil"){
        if(indicador == ""){
          return()
        } else{
        data = frag_leg_br %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
          select(`Ano da eleição`,
                 Cargo,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 Desproporcionalidade) %>% 
          unique() 
        }
      } else{
        return()
      } 
    })
  })  
  
  
  ## Resumo 
  
  ### Fragmentacao legislativa (UF)
  
  fraguf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fragmentação" & 
       agregacao == "UF"){
      return(input$frag_uf)
    } else{
      return()
    }
  })
  
  output$frag_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfrag_uf()
  })
  
  
  bfrag_uf <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'frag_uf',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        uf <- req(input$UF1)
        if(indicador == "Fragmentação" & 
           agregacao == "UF"){
           if(uf == ""){
             return()
           } else if(uf == "Todas UFs"){
            frag_leg_uf %>% 
            ungroup() %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fragmentação`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fragmentação`)
          
          } else{
            frag_leg_uf %>% 
            ungroup() %>% 
            dplyr::filter(UF == input$UF1 & 
                          Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fragmentação`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fragmentação`)
          }
        } else{
          return()
        }
      })
  })
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (UF) 
  
  ag_frag_uf <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fragmentação" &
       agregacao == "UF"){
      return(input$agreg_frag_uf)
    } else{
      return()
    }
  })
  
  output$agreg_frag_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_frag_uf()
  })
  
  bagreg_frag_uf <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'frag_uf_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        uf <- req(input$UF1)
        if(indicador == "Fragmentação" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          }else if(uf == "Todas UFs"){
          data = frag_leg_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   Desproporcionalidade) %>% 
            unique()
          } else{
          data = frag_leg_uf %>% 
            dplyr::filter(UF == input$UF1 & 
                          Cargo == input$DESCRICAO_CARGO1) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   Desproporcionalidade) %>% 
            unique()
        }
        } else{
          return()
        }
      })
  })
  
  
  ## Resumo
  
  ### Fragmentacao legislativa (MUN)
  
  
  fragmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fragmentação" & 
       agregacao == "Município"){
      return(input$frag_mun)
    } else{
      return()
    }
  })
  
  output$frag_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bfrag_mun()
  })
  
  bfrag_mun <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'frag_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Fragmentação" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           frag_leg_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Fragmentação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Fragmentação`)
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Fragmentação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Fragmentação`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_frag_mun <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fragmentação" & 
       agregacao == "Município"){
      return(input$agreg_frag_mun)
    } else{
      return()
    }
  })
  
  output$agreg_frag_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_frag_mun()
  })
  
  bagreg_frag_mun <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'frag_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Fragmentação" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = frag_leg_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Fragmentacao legislativa (Intervalo de eleitores aptos)
  
  
  
  fragmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fragmentação" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$frag_med)
    } else{
      return()
    }
  })
  
  output$frag_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    frag_med()
  })
  
  frag_med <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Fragmentação" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           media1 <- frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional da fragmentação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da fragmentação`) %>% 
                             mutate("media" = "Média nacional da fragmentação") %>% 
                             column_to_rownames("media")
                           
                           media2 <- frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média da fragmentação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da fragmentação`) %>% 
                             mutate("media" = "Média da fragmentação do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                         }
                       } else{
                         return()
                       }
                     })
  }) 
  
  
  
  fragint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Fragmentação" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$frag_int)
    } else{
      return()
    }
  })
  
  output$frag_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    frag_int()
  })
  
  frag_int <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'frag_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Fragmentação" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Fragmentação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Fragmentação`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_frag_int <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Fragmentação" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_frag_int)
    } else{
      return()
    }
  })
  
  output$agreg_frag_int <- DT::renderDataTable(server = FALSE,{
    bagreg_frag_int()
  })
  
  bagreg_frag_int <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'frag_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Fragmentação" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
# 1.1.5. Numero efetivo de partidos legislativo -----------------------------------------------------
  
## Resumo 
  
### Fragmentacao legislativa (Brasil
  
  neplbr <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "Brasil"){
      return(input$nepl_br)
    } else{
      return()
    }
  })
  
  output$nepl_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepl_br()
  })
  
  bnepl_br <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'nepl_br',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',  
                             'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      if(indicador == "Número efetivo de partidos legislativo" & 
         agregacao == "Brasil"){
        frag_leg_br %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
          dplyr::select(`Ano da eleição`,
                        `Número efetivo de partidos legislativo`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Número efetivo de partidos legislativo`)
      } else{
        return()
      }
    })
  })
  
## Dados desagregados
  
### Fragmentacao legislativa (Brasil  
  
  ag_nepl_br <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "Brasil"){
      return(input$agreg_nepl_br)
    } else{
      return()
    }
  })
  
  output$agreg_nepl_br <- DT::renderDataTable(server = FALSE,{
    bagreg_nepl_br()
  })
  
  bagreg_nepl_br <- eventReactive(input$BCALC1, {
    datatable(options = list(
                scrollX = TRUE,
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 1
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'nepl_br_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',   
                             'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      if(indicador == "Número efetivo de partidos legislativo" & 
          agregacao == "Brasil"){
        data = frag_leg_br %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>%
          select(`Ano da eleição`,
                 Cargo,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 Desproporcionalidade) %>% 
          unique() 
      } else{
        return()
      }
    })
  })  
  
## Resumo
  
### Fragmentacao legislativa (UF)
  
  nepluf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "UF"){
      return(input$nepl_uf)
    } else{
      return()
    }
  })
  
  output$nepl_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepl_uf()
  })   
  
  bnepl_uf <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 1
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'nepl_uf',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons', 
                             'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      uf <- req(input$UF1)
      if(indicador == "Número efetivo de partidos legislativo" & 
         agregacao == "UF"){
        if(uf == ""){
          return()
        } else if(uf == "Todas UFs"){
          frag_leg_uf %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
          dplyr::select(`Ano da eleição`,
                        UF,
                        `Número efetivo de partidos legislativo`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Número efetivo de partidos legislativo`)
        
        } else{
          frag_leg_uf %>% 
          dplyr::filter(UF == input$UF1 & 
                        Cargo == input$DESCRICAO_CARGO1) %>% 
          dplyr::select(`Ano da eleição`,
                        UF,
                        `Número efetivo de partidos legislativo`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Número efetivo de partidos legislativo`)
        }
      } else{
        return()
      }
    })
  })  
 
## Dados desagregados
  
### Fragmentacao legislativa (UF) 
  
  ag_nepl_uf <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- req(input$UF1)
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "UF"){
      return(input$agreg_nepl_uf)
    } else{
      return()
    }
  })
  
  output$agreg_nepl_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_nepl_uf()
  })
  
  bagreg_nepl_uf <- eventReactive(input$BCALC1, {
    datatable(options = list(
                scrollX = TRUE,
                autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 3
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'nepl_uf_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
      extensions = c('Buttons',      
                     'FixedColumns'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL1
      uf <- req(input$UF1)
      if(indicador == "Número efetivo de partidos legislativo" & 
         agregacao == "UF"){
        if(uf == ""){
          return()
        } else if(uf == "Todas UFs"){
          data = frag_leg_uf %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   Desproporcionalidade) %>% 
          unique()
        } else{
          data = frag_leg_uf %>% 
          dplyr::filter(UF == input$UF1 &
                        Cargo == input$DESCRICAO_CARGO1) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   Desproporcionalidade) %>% 
          unique() 
      }
      } else{
        return()
      }
    })
  })
  
  ## Resumo
  
  ### Fragmentacao legislativa (MUN)
  
  
  neplmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    cargo <- input$DESCRICAO_CARGO1
    if(cargo == "Vereador" &
       indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "Município"){
      return(input$nepl_mun)
    } else{
      return()
    }
  })
  
  output$nepl_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bnepl_mun()
  })
  
  bnepl_mun <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'nepl_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(cargo == "Vereador" &
                          indicador == "Número efetivo de partidos legislativo" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           frag_leg_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Número efetivo de partidos legislativo`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Número efetivo de partidos legislativo`)
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Número efetivo de partidos legislativo`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Número efetivo de partidos legislativo`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_nepl_mun <- reactive({
    cargo <- input$DESCRICAO_CARGO1
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(cargo == "Vereador" &
       indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "Município"){
      return(input$agreg_nepl_mun)
    } else{
      return()
    }
  })
  
  output$agreg_nepl_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_nepl_mun()
  })
  
  bagreg_nepl_mun<- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'nepl_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(cargo == "Vereador" &
                          indicador == "Número efetivo de partidos legislativo" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = frag_leg_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
  ## Resumo
  
  ### Fragmentacao legislativa (Intervalo de eleitores aptos)
  
  
  neplmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$nepl_med)
    } else{
      return()
    }
  })
  
  output$nepl_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    nepl_med()
  })
  
  nepl_med <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(cargo == "Vereador" &
                          indicador == "Número efetivo de partidos legislativo" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <-  frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional do número efetivo de partidos legislativo`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional do número efetivo de partidos legislativo`) %>% 
                             mutate("media" = "Média nacional do número efetivo de partidos legislativo") %>% 
                             column_to_rownames("media")
                           
                           media2 <-  frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média do número efetivo de partidos legislativo`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média do número efetivo de partidos legislativo`) %>% 
                             mutate("media" = "Média do número efetivo de partidos legislativo do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  
  neplint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$nepl_int)
    } else{
      return()
    }
  })
  
  output$nepl_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    nepl_int()
  })
  
  nepl_int <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'nepl_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(cargo == "Vereador" &
                          indicador == "Número efetivo de partidos legislativo" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Número efetivo de partidos legislativo`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Número efetivo de partidos legislativo`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_nepl_int <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_nepl_int)
    } else{
      return()
    }
  })
  
  output$agreg_nepl_int <- DT::renderDataTable(server = FALSE,{
    bagreg_nepl_int()
  })
  
  bagreg_nepl_int <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'nepl_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(cargo == "Vereador" &
                         indicador == "Número efetivo de partidos legislativo" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
# 1.1.6. Numero efetivo de partidos eleitoral -----------------------------  
  
  ## Tabela para visualizacao  
  
  ### Fragmentacao legislativa (Brasil
  
  nepelbr <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "Brasil"){
      return(input$nepel_br)
    } else{
      return()
    }
  })
  
  output$nepel_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepel_br()
  })
  
  bnepel_br <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
       columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'nepel_br',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',   
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        if(indicador == "Número efetivo de partidos eleitoral" & 
           agregacao == "Brasil"){
          frag_leg_br %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          `Número efetivo de partidos eleitoral`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Número efetivo de partidos eleitoral`)
        } else{
          return()
        }
      })
  })
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (Brasil  
  
  ag_nepel_br <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "Brasil"){
      return(input$agreg_nepel_br)
    } else{
      return()
    }
  })
  
  output$agreg_nepel_br <- DT::renderDataTable(server = FALSE,{
    bagreg_nepel_br()
  })
  
  bagreg_nepel_br <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'nepel_br_agreg',
        bom = TRUE),
        list(                    
          extend = 'colvis',                   
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',  
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        if(indicador == "Número efetivo de partidos eleitoral" & 
           agregacao == "Brasil"){
          data = frag_leg_br %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            select(`Ano da eleição`,
                   Cargo,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   Desproporcionalidade) %>% 
            unique() 
        } else{
          return()
        }
      })
  })  
  
  ## Tabela para visualizacao  
  
  ### Fragmentacao legislativa (UF)
  
  nepeluf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "UF"){
      return(input$nepel_uf)
    } else{
      return()
    }
  })
  
  output$nepel_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepel_uf()
  })
  
  bnepel_uf <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'nepel_uf',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',   
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        uf <- req(input$UF1)
        if(indicador == "Número efetivo de partidos eleitoral" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if( uf == "Todas UFs"){
            frag_leg_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Número efetivo de partidos eleitoral`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                  `Número efetivo de partidos eleitoral`)
          } else{
            frag_leg_uf %>% 
            dplyr::filter(UF == input$UF1 &
                          Cargo == input$DESCRICAO_CARGO1) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Número efetivo de partidos eleitoral`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Número efetivo de partidos eleitoral`)
        }
        } else{
          return()
        }
      })
  })
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (UF)  
  
  ag_nepel_uf <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos eleitoral" &
       agregacao == "UF"){
      return(input$agreg_nepel_uf)
    } else{
      return()
    }
  })
  
  output$agreg_nepel_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_nepel_uf()
  })
  
  bagreg_nepel_uf <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'nepel_uf_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL1
        uf <- req(input$UF1)
        if(indicador == "Número efetivo de partidos eleitoral" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = frag_leg_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
              select(`Ano da eleição`,
                     UF,
                     Cargo,
                     `Número efetivo de partidos eleitoral`,
                     `Número efetivo de partidos legislativo`,
                     `Fracionalização`,
                     `Fracionalização máxima`,
                     `Fragmentação`,
                     Desproporcionalidade) %>% 
            unique()
          } else{
            data = frag_leg_uf %>% 
            dplyr::filter(UF == input$UF1 &
                          Cargo == input$DESCRICAO_CARGO1) %>% 
              select(`Ano da eleição`,
                     UF,
                     Cargo,
                     `Número efetivo de partidos eleitoral`,
                     `Número efetivo de partidos legislativo`,
                     `Fracionalização`,
                     `Fracionalização máxima`,
                     `Fragmentação`,
                     Desproporcionalidade) %>% 
              unique()  
        }
        } else{
          return()
        }
      })
  })
  
  ## Resumo
  
  ### Fragmentacao legislativa (MUN)
  
  
  nepelmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "Município"){
      return(input$nepel_mun)
    } else{
      return()
    }
  })
  
  output$nepel_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bnepel_mun()
  })
  
  bnepel_mun <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'nepel_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(cargo == "Vereador" &
                          indicador == "Número efetivo de partidos eleitoral" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           frag_leg_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Número efetivo de partidos eleitoral`)
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Número efetivo de partidos eleitoral`)
                           
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Número efetivo de partidos eleitoral" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           frag_leg_pf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Número efetivo de partidos eleitoral`)
                         } else{
                           frag_leg_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Número efetivo de partidos eleitoral`)
                           
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_nepel_mun <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "Município"){
      return(input$agreg_nepel_mun)
    } else{
      return()
    }
  })
  
  output$agreg_nepel_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_nepel_mun()
  })
  
  bagreg_nepel_mun <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'nepel_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                    'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(cargo == "Vereador" &
                          indicador == "Número efetivo de partidos eleitoral" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = frag_leg_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                         } else if(cargo == "Prefeito" &
                                   indicador == "Número efetivo de partidos eleitoral" & 
                                   agregacao == "Município"){
                           if(municipio == ""){
                             return()
                           } else if(municipio == "Todos os municípios"){
                             data = frag_leg_pf %>%
                               dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                               select(`Ano da eleição`,
                                      UF,
                                      `Nome do município`,
                                      Cargo,
                                      `Número efetivo de partidos eleitoral`) %>% 
                               unique()
                           } else{
                             data = frag_leg_pf %>% 
                               dplyr::filter(`Nome do município2` == input$MUN1 & 
                                               Cargo == input$DESCRICAO_CARGO1) %>% 
                               select(`Ano da eleição`,
                                      UF,
                                      `Nome do município`,
                                      Cargo,
                                      `Número efetivo de partidos eleitoral`) %>% 
                               unique()
                           }
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Fragmentacao legislativa (Intervalo de eleitores aptos)
  
  
  nepelmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$nepel_med)
    } else{
      return()
    }
  })
  
  output$nepel_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    nepel_med()
  })
  
  nepel_med <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(cargo == "Vereador" &
                          indicador == "Número efetivo de partidos eleitoral" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional do número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional do número efetivo de partidos eleitoral`) %>% 
                             mutate("media" = "Média nacional do número efetivo de partidos eleitoral") %>% 
                             column_to_rownames("media")
                           
                           media2 <- frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média do número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média do número efetivo de partidos eleitoral`) %>% 
                             mutate("media" = "Média do número efetivo de partidos eleitoral do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Número efetivo de partidos eleitoral" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- frag_leg_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           Turno,
                                           `Média nacional do número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional do número efetivo de partidos eleitoral`) %>% 
                             mutate("media" = "Média nacional do número efetivo de partidos eleitoral") 
                           
                           media1 <- media1 %>% 
                             mutate("media" = ifelse(media1$Turno == 2,
                                                     "Média nacional do número efetivo de partidos eleitoral.",
                                                     media1$media)) %>% 
                             column_to_rownames("media")
                           
                           media2 <- frag_leg_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           Turno,
                                           `Média do número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média do número efetivo de partidos eleitoral`) %>% 
                             mutate("media" = "Média do número efetivo de partidos eleitoral do grupo") 
                           
                           media2 <- media2 %>% 
                             mutate("media" = ifelse(media2$Turno == 2,
                                                     "Média do número efetivo de partidos eleitoral do grupo.",
                                                     media2$media)) %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       }
                     })
  })
  
  
  nepelint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$nepel_int)
    } else{
      return()
    }
  })
  
  output$nepel_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    nepel_int()
  })
  
  nepel_int <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'nepel_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(cargo == "Vereador" &
                          indicador == "Número efetivo de partidos eleitoral" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Número efetivo de partidos eleitoral`)
                           
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Número efetivo de partidos eleitoral" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           frag_leg_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                            Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Número efetivo de partidos eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Número efetivo de partidos eleitoral`)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (INT)  
  
  ag_nepel_int <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_nepel_int)
    } else{
      return()
    }
  })
  
  output$agreg_nepel_int <- DT::renderDataTable(server = FALSE,{
    bagreg_nepel_int()
  })
  
  bagreg_nepel_int <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'nepel_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(cargo == "Vereador" &
                          indicador == "Número efetivo de partidos eleitoral" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = frag_leg_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`,
                                    `Número efetivo de partidos legislativo`,
                                    `Fracionalização`,
                                    `Fracionalização máxima`,
                                    `Fragmentação`,
                                    Desproporcionalidade) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Número efetivo de partidos eleitoral" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = frag_leg_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(`Ano da eleição`,
                                    UF,
                                    `Nome do município`,
                                    Cargo,
                                    `Número efetivo de partidos eleitoral`) %>% 
                             unique()
                         }
                       }
                     })
  })


# 1.1.7. Quociente eleitoral -----------------------------------------------
  
  ## Resumo
  
  ### Deputado Federal
  
  quocebr <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO1
    uf <- input$UF1
    if(indicador == "Quociente eleitoral" & 
       cargo == "Deputado Federal"){
      return(input$quoce_br)
    } else{
      return()
    }
  })
  
  output$quoce_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquoce_br()
  })
  
  
  
  bquoce_br <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_elei_br',
        bom = TRUE)
      )),
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF1
                       if(indicador == "Quociente eleitoral" & 
                          cargo == "Deputado Federal"){
                         if(input$UF1 == "Todas UFs"){
                           distcad_br %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Quociente eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Quociente eleitoral`)
                         }else{
                           distcad_br %>% 
                             dplyr::filter(UF == input$UF1) %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Quociente eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Quociente eleitoral`)
                           }
                       } else{
                         return()
                       }
                     })
    
  })
  
  
  
  ## Dados desagregados
  
  ### Deputado Federal
  
  ag_quoce_br <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO1
    uf <- input$UF1
    if(indicador == "Quociente eleitoral" &
       cargo == "Deputado Federal"){
      return(input$agreg_quoce_br) 
    } else{
      return()
    }
  })
  
  output$agreg_quoce_br <- DT::renderDataTable(server = FALSE,{
    bagreg_quoce_br()
  })
  
  bagreg_quoce_br <- eventReactive(input$BCALC1, {
    datatable(options = list(
     autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'quoc_elei_br_agreg',
        bom = TRUE),
        list(
          extend = 'colvis',
          text = 'Colunas')
      )), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF1
                       if(indicador == "Quociente eleitoral" &
                          cargo == "Deputado Federal"){
                         if(input$UF1 == "Todas UFs"){
                           data = distcad_br %>% 
                             unique() 
                         }
                         else{
                           data = distcad_br %>% 
                             dplyr::filter(UF == input$UF1) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  
  ## Resumo
  
  ### Deputado Estadual
  
  quoceuf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente eleitoral" & 
       cargo == "Deputado Estadual"){
      return(input$quoce_uf)
    } else{
      return()
    }
  })
  
  output$quoce_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquoce_uf()
  })
  
  
  bquoce_uf <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_elei_uf',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF1
                       if(indicador == "Quociente eleitoral" & 
                          cargo == "Deputado Estadual"){
                         if(input$UF1=="Todas UFs"){
                           expr = distcad_uf %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Quociente eleitoral`) %>% 
                             unique %>% 
                             spread(`Ano da eleição`,
                                    `Quociente eleitoral`)
                           
                         }else{
                           expr = distcad_uf %>% 
                             dplyr::filter(UF == input$UF1) %>% 
                             select(`Ano da eleição`, 
                                    UF, 
                                    `Quociente eleitoral`) %>% 
                             unique %>% 
                             spread(`Ano da eleição`, 
                                    `Quociente eleitoral`)
                           }
                       } else{
                         return()
                       }
                     })
  }) 
  
  ## Dados desagregados
  
  ## Deputado Estadual  
  
  ag_quoce_uf <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente eleitoral" & 
       cargo == "Deputado Estadual"){
      return(input$agreg_quoce_uf) 
    } else{
      return()
    }
  })
  
  output$agreg_quoce_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_quoce_uf()
  })
  
  bagreg_quoce_uf <- eventReactive(input$BCALC1, {
    datatable(options = list(
     autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_elei_uf_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF1
                       if(indicador == "Quociente eleitoral" & 
                          cargo == "Deputado Estadual"){
                         if(input$UF1 == "Todas UFs"){
                           expr = distcad_uf %>% 
                             unique()
                         } else {
                           expr = distcad_uf %>% 
                             dplyr::filter(UF == input$UF1) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  
  ## Resumo
  
  ### Vereador
  
  
  quocemun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Quociente eleitoral" & 
       agregacao == "Município"){
      return(input$quoce_mun)
    } else{
      return()
    }
  })
  
  output$quoce_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bquoce_mun()
  })
  
  bquoce_mun <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_elei_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(cargo == "Vereador" &
                         indicador == "Quociente eleitoral" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           distcad_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Quociente eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Quociente eleitoral`)
                         } else{
                           distcad_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 &
                                           Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Quociente eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Quociente eleitoral`)
                         }
                       } else{
                         return()
                       }
                       
                     })
  })  
  
  ## Dados desagregados
  
  ### Vereador
  
  ag_quoce_mun <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Quociente eleitoral" & 
       agregacao == "Município"){
      return(input$agreg_quoce_mun)
    } else{
      return()
    }
  })
  
  output$agreg_quoce_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_quoce_mun()
  })
  
  bagreg_quoce_mun <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_elei_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- input$MUN1
                       if(cargo == "Vereador" &
                          indicador == "Quociente eleitoral" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return(NULL)
                         } else if(municipio == "Todos os municípios"){
                           data = distcad_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média do quociente eleitoral`,
                                    -`Média do quociente partidário`,
                                    -`Média nacional do quociente eleitoral`,
                                    -`Média nacional do quociente partidário`) %>% 
                             unique()
                         } else{
                           data = distcad_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média do quociente eleitoral`,
                                    -`Média do quociente partidário`,
                                    -`Média nacional do quociente eleitoral`,
                                    -`Média nacional do quociente partidário`) %>% 
                             unique()
                         }
                       } else {
                         return(NULL)
                       }
                     })
  })
  
  ## Resumo
  
  ### Vereador - Eleitores aptos
  
  
  quocemed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Quociente eleitoral" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$quoce_med)
    } else{
      return()
    }
  })
  
  output$quoce_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquoce_med()
  })
  
  bquoce_med <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(cargo == "Vereador" &
                          indicador == "Quociente eleitoral" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- distcad_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional do quociente eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional do quociente eleitoral`) %>% 
                             mutate("media" = "Média nacional do quociente eleitoral") %>% 
                             column_to_rownames("media")
                           
                           media2 <- distcad_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média do quociente eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média do quociente eleitoral`) %>% 
                             mutate("media" = "Média do quociente eleitoral do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                         }
                       } else{
                         return()
                       }
                       
                     })
  }) 
  
  
  quoceint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Quociente eleitoral" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$quoce_int)
    } else{
      return()
    }
  })
  
  output$quoce_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquoce_int()
  })
  
  bquoce_int <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_elei_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(cargo == "Vereador" &
                         indicador == "Quociente eleitoral" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           distcad_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Quociente eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Quociente eleitoral`)
                         }
                       } else{
                         return()
                       }
                       
                     })
  })  
  
  ## Dados desagregados
  
  ### Vereador - Eleitores aptos
  
  ag_quoce_int <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Quociente eleitoral" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_quoce_int)
    } else{
      return()
    }
  })
  
  output$agreg_quoce_int <- DT::renderDataTable(server = TRUE,{
    bagreg_quoce_int()
  })
  
  bagreg_quoce_int <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_elei_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO1
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(cargo == "Vereador" &
                         indicador == "Quociente eleitoral" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = distcad_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média do quociente eleitoral`,
                                    -`Média do quociente partidário`,
                                    -`Média nacional do quociente eleitoral`,
                                    -`Média nacional do quociente partidário`) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
# 1.1.8. Quociente partidario ---------------------------------------------
  
  ## Resumo
  
  ### Deputado Federal
  
  quocpbr <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & 
       cargo == "Deputado Federal"){
      return(input$quocp_br)
    } else{
      return()
    }
  })
  
  output$quocp_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquocp_br()
  })
  
  bquocp_br <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
     autoWidth = FALSE,
      ordering = TRUE,
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_part_br',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF1
                       if(indicador == "Quociente partidário" & 
                          cargo == "Deputado Federal"){
                         if(input$UF1=="Todas UFs"){
                           expr = distcad_br %>% 
                             select(`Ano da eleição`, 
                                    UF, 
                                    `Sigla do partido`, 
                                    `Quociente partidário`)
                         }else{
                           expr = distcad_br %>% 
                             dplyr::filter(UF == input$UF1) %>% 
                             select(`Ano da eleição`, 
                                    UF, 
                                    `Sigla do partido`, 
                                    `Quociente partidário`)
                         }
                       } else{
                         return()
                       }
                     })
  })
  
  ## Dados desagregados
  
  # Deputado Federal
  
  ag_quocp_br <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & 
       cargo == "Deputado Federal"){
      return(input$agreg_quocp_br)
    } else{
      return()
    }
  })
  
  output$agreg_quocp_br <- DT::renderDataTable(server = FALSE,{
    bagreg_quocp_br()
  })
  
  bagreg_quocp_br <- eventReactive(input$BCALC1, {
    datatable(options = list(
     autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_part_br_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF1
                       if(indicador == "Quociente partidário" & 
                          cargo == "Deputado Federal"){
                         if(input$UF1 == "Todas UFs"){
                           expr = distcad_br %>% 
                             unique()
                         }else{
                           expr = distcad_br %>% 
                             dplyr::filter(UF == input$UF1) %>% 
                             unique()
                           }
                       } else{
                         return()
                       }
                     })
  })
  
  ## Resumo
  
  ### Deputado estadual
  
  quocpuf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & 
       cargo == "Deputado Estadual"){
      return(input$quocp_uf)
    } else{
      return()
    }
  })
  
  output$quocp_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquocp_uf()
  })
  
  bquocp_uf <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_part_uf',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF1
                       if(indicador == "Quociente partidário" & 
                          cargo == "Deputado Estadual"){
                         if(input$UF1=="Todas UFs"){
                           expr = distcad_uf %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Sigla do partido`,
                                    `Quociente partidário`)
                           
                         }else{
                           expr = distcad_uf %>% 
                             dplyr::filter(UF == input$UF1) %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Sigla do partido`, 
                                    `Quociente partidário`)
                         }
                       } else{
                         return()
                       }
                     })
  })
  
  ## Dados desagregados
  
  ### Deputado Estadual
  
  ag_quocp_uf <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & 
       cargo == "Deputado Estadual"){
      return(input$agreg_quocp_uf)
    } else{
      return()
    }
  })
  
  output$agreg_quocp_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_quocp_uf()
  })
  
  bagreg_quocp_uf <- eventReactive(input$BCALC1,{
    datatable(options = list(
     autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_part_uf_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF1
                       if(indicador == "Quociente partidário" & 
                          cargo == "Deputado Estadual"){
                         if(input$UF1 == "Todas UFs"){
                           expr = distcad_uf %>% 
                             unique()
                         }else{          
                           expr = distcad_uf %>% 
                             dplyr::filter(UF == input$UF1) %>% 
                             unique()
                           }
                       } else{
                         return()
                       }
                     })
  })
  
  
  
  ## Resumo
  
  ### Vereador
  
  
  quocpmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Quociente partidário" & 
       agregacao == "Município"){
      return(input$quocp_mun)
    } else{
      return()
    }
  })
  
  output$quocp_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bquocp_mun()
  })
  
  bquocp_mun <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_part_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Quociente partidário" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           distcad_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Sigla do partido`,
                                           `Quociente partidário`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Quociente partidário`)
                         } else{
                           distcad_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 &
                                           Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Sigla do partido`,
                                           `Quociente partidário`) %>% 
                             unique %>% 
                             spread(`Ano da eleição`,
                                    `Quociente partidário`)
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Fragmentacao legislativa (MUN)  
  
  ag_quocp_mun <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Quociente partidário" & 
       agregacao == "Município"){
      return(input$agreg_quocp_mun)
    } else{
      return()
    }
  })
  
  output$agreg_quocp_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_quocp_mun()
  })
  
  bagreg_quocp_mun <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_part_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       municipio <- req(input$MUN1)
                       if(indicador == "Quociente partidário" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = distcad_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO1) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média do quociente eleitoral`,
                                    -`Média do quociente partidário`,
                                    -`Média nacional do quociente eleitoral`,
                                    -`Média nacional do quociente partidário`) %>% 
                             unique()
                         } else{
                           data = distcad_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média do quociente eleitoral`,
                                    -`Média do quociente partidário`,
                                    -`Média nacional do quociente eleitoral`,
                                    -`Média nacional do quociente partidário`) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })
  
  ## Resumo
  
  ### Vereador - Eleitores aptos
  
  
  quocpmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Quociente partidário" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$quocp_med)
    } else{
      return()
    }
  })
  
  output$quocp_med <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bquocp_med()
  })
  
  bquocp_med <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Quociente partidário" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- distcad_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional do quociente partidário`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional do quociente partidário`) %>% 
                             mutate("media" = "Média nacional do quociente partidário") %>% 
                             column_to_rownames("media")
                           
                           media2 <- distcad_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média do quociente partidário`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média do quociente partidário`) %>% 
                             mutate("media" = "Média do quociente partidário do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                         }
                       } else{
                         return()
                       }
                       
                     })
  }) 
  
  
  quocpint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    uf <- input$UF1
    if(indicador == "Quociente partidário" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$quocp_int)
    } else{
      return()
    }
  })
  
  output$quocp_int <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    bquocp_int()
  })
  
  bquocp_int <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_part_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Quociente partidário" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           distcad_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 &
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Quociente partidário`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Quociente partidário`)
                         }
                       } else{
                         return()
                       }
                       
                     })
  })  
  
  ## Dados desagregados
  
  ### Vereador
  
  ag_quocp_int <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL1
    if(indicador == "Quociente partidário" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_quocp_int)
    } else{
      return()
    }
  })
  
  output$agreg_quocp_int <- DT::renderDataTable(server = TRUE,{
    bagreg_quocp_int()
  })
  
  bagreg_quocp_int <- eventReactive(input$BCALC1, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_part_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_FRAG
                       agregacao <- input$AGREGACAO_REGIONAL1
                       intervalo <- req(input$INT1)
                       if(indicador == "Quociente partidário" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = distcad_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT1 & 
                                             Cargo == input$DESCRICAO_CARGO1) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média do quociente eleitoral`,
                                    -`Média do quociente partidário`,
                                    -`Média nacional do quociente eleitoral`,
                                    -`Média nacional do quociente partidário`) %>% 
                             unique()
                         }
                       } else{
                         return()
                       }
                     })
  })  
  
# 2.2. Reeleicao ---------------------------------------------- 
  
  ## Modal para ajuda
  
  ### Dados Resumo
  
  observeEvent(input$modal_renovp,{
    showModal(modalDialog(
                          title = h4(class = "h4 titulo",
                            "AJUDA"),
                          footer = modalButton("FECHAR"), 
                          size = "m",
                          htmlOutput("def_renovp"),
                          easyClose = TRUE,
                          style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "))
  })
  
  ### Dados desagregados
  
  observeEvent(input$modal_renovp_ag,{
    showModal(modalDialog(
                          title = h4(class = "h4 titulo",
                                          "AJUDA"),
                          footer = modalButton("FECHAR"), 
                          size = "m",
                          htmlOutput("def_renovp"),
                          easyClose = TRUE,
                          style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "
    ))
  })
  
  ## Funcao para descricao dos indicadores de renovacao parlamentar
  
  output$def_renovp <- renderUI({
    note <- paste0("
                   <font color = 'black'>
                   <h4> Reeleição </h4>
                   <h5 align = 'justify'><br />
                   Exprime a percentagem dos reeleitos em relação ao total de vagas em disputa.</h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   REEL = (REELEITOS / TOTAL DE VAGAS) * 100
                   <p>
                   <h4><br /> Reeleição líquida </h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Exprime a percentagem dos reeleitos em relação ao 
                   total de recandidatos.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   REEL LIQ = (REELEITOS / (DERROTADOS + REELEITOS)) * 100
                    <h4><br /> Renovação </h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'> Esta fórmula computa o número total de 
                   representantes novos em uma legislatura, comparado à composição da 
                   legislatura anterior.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   RENOV = (1 - INDICADOR DE REELEIÇÃO) * 100
                  <p>
                   <h4><br /> Renovação líquida </h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>A 'Renovação líquida' .</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   RENOV LIQ = (1 - INDICADOR DE REELEIÇÃO LÍQUIDA) * 100
                   <h4><br /> Recandidaturas </h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>O indicador de 'Recandidaturas' calcula o percentual de candidatos 
                   que se recandidataram em função dos candidatos eleitos no pleito anterior.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   RECAND = (RECANDIDATOS/ELEITOS NA ELEIÇÃO PASSADA) * 100
                  <p><br /> 
                  <strong>Fonte:</strong> 
                  <p>1. Votos e partidos: almanaque de dados eleitorais: Brasil e outros 
                  países/ Organização de Wanderley Guilherme dos Santos, com a colaboração de Fabrícia Guimarães. -
                  Rio de Janeiro: Editora FGV, 2002).</p></a></font>

                   ")
    HTML(note)
  }) 
  
# 2.2.1. Reeleicao ------------------------------------------------------
  
  ## Resumo
  
  ### Renovacao parlamentar (Brasil) 
  
  reelbr <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Reeleição" &
       agregacao == "Brasil"){
      return(input$reel_br)
    }
  })
  
  
  output$reel_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_br()
  })
  
  breel_br <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
     autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'reel_br',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',       
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO2
        agregacao <- input$AGREGACAO_REGIONAL2
        if(indicador == "Reeleição" &
           agregacao == "Brasil"){
          renov_parl_br %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          `Reeleição`) %>% 
            spread(`Ano da eleição`,
                   `Reeleição`)
          
        }
      })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (Brasil) 
  
  ag_reel_br<- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Reeleição" & 
       agregacao == "Brasil"){
      return(input$agreg_reel_br)
    }
  })
  
  output$agreg_reel_br <- DT::renderDataTable(server = FALSE,{
    bagreg_reel_br()
  })
  
  bagreg_reel_br <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
     autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'reel_br_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO2
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- input$UF2
        if(indicador == "Reeleição" &
           agregacao == "Brasil"){
          data = renov_parl_br %>%
            dplyr::filter(Cargo==input$DESCRICAO_CARGO2) 
          
        }
      })
  })
  
  ## Resumo
  
  ### Renovacao parlamentar (UF)  
  
  
  reelmed_uf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Reeleição" & 
       agregacao == "UF"){
      return(input$reel_med_uf)
    }
  })
  
  output$reel_med_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_med_uf()
  })
  
  breel_med_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
        cargo <- input$DESCRICAO_CARGO2
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if((cargo == "Deputado Federal" |
            cargo == "Deputado Estadual") &
           indicador == "Reeleição" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional da reeleição`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional da reeleição`) %>% 
              mutate("media" = "Média nacional da reeleição") %>% 
              column_to_rownames("media")
            
            media1
            
          } else{
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                            UF == input$UF2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional da reeleição`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional da reeleição`) %>% 
              mutate("media" = "Média nacional da reeleição") %>% 
              column_to_rownames("media")
            
            media1
            
          }
        } 
      })
  })
  
  reeluf <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Reeleição" & 
       agregacao == "UF"){
      return(input$reel_uf)
    }
  })
  
  
  output$reel_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_uf()
  })
  
  breel_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
     autoWidth = FALSE,
      
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'reel_uf',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO2
        uf <- req(input$UF2)
        agregacao <- input$AGREGACAO_REGIONAL2
        if(indicador == "Reeleição" & 
           agregacao == "UF")
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            renov_parl_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Reeleição`) %>% 
            spread(`Ano da eleição`,
                  `Reeleição`)
          
          } else{
            renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                            UF == input$UF2) %>% 
              dplyr::select(`Ano da eleição`,
                            UF,
                            `Reeleição`) %>% 
              spread(`Ano da eleição`,
                     `Reeleição`)
        }
      })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (UF)
  
  ag_reel_uf<- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Reeleição" &
       agregacao == "UF"){
      return(input$agreg_reel_uf)
    }
  })
  
  output$agreg_reel_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_reel_uf()
  })
  
  bagreg_reel_uf <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
     autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'reel_uf_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                    
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Reeleição" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = renov_parl_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
              select( -`Média nacional da reeleição`,
                      -`Média nacional da reeleição líquida`,
                      -`Média nacional da renovação`,
                      -`Média nacional da renovação líquida`,
                      -`Média nacional das recandidaturas`) %>% 
            unique()
          } else{
            data = renov_parl_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                     UF == input$UF2) %>% 
              select( -`Média nacional da reeleição`,
                      -`Média nacional da reeleição líquida`,
                      -`Média nacional da renovação`,
                      -`Média nacional da renovação líquida`,
                      -`Média nacional das recandidaturas`) %>% 
            unique()
          }
        }
      })
  })
  
  
  ## Resumo
  
  ### Vereador
  
  reelmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Reeleição" & 
       agregacao == "Município"){
      return(input$reel_mun)
    }
  })
  
  output$reel_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    breel_mun()
  })
  
  breel_mun <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'reel_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição`)
                         } else{
                           renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição`)
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Reeleição" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_pf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição`)
                         } else{
                           renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição`)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (MUN)  
  
  ag_reel_mun <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Reeleição" & 
       agregacao == "Município"){
      return(input$agreg_reel_mun)
    }
  })
  
  output$agreg_reel_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_reel_mun()
  })
  
  bagreg_reel_mun <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'reel_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Reeleição" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_pf %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Vereador
  
  
  reelmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Reeleição" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$reel_med)
    }
  })
  
  output$reel_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_med()
  })
  
  breel_med <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional da reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da reeleição`) %>% 
                             mutate("media" = "Média nacional da reeleição") %>% 
                             column_to_rownames("media")
                           
                           media2 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média da reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da reeleição`) %>% 
                             mutate("media" = "Média da reeleição do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Reeleição" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média nacional da reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da reeleição`) %>% 
                             mutate("media" = "Média nacional da reeleição") %>% 
                             column_to_rownames("media") 
                           
                           #media1 <- media1 %>% 
                            # mutate("media" = ifelse(media1$Turno == 2,
                             #                        "Média nacional da Reeleição.",
                              #                       media1$media)) %>% 
                             #column_to_rownames("media")
                           
                           media2 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média da reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da reeleição`) %>% 
                             mutate("media" = "Média da reeleição do grupo") %>% 
                             column_to_rownames("media")
                           
                           #media2 <- media2 %>% 
                            # mutate("media" = ifelse(media2$Turno == 2,
                             #                        "Média da Reeleição.",
                              #                       media2$media)) %>% 
                          #   column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       }
                     })
  })
  
  
  
  
  
  reelint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Reeleição" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$reel_int)
    }
  })
  
  output$reel_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_int()
  })
  
  breel_int <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'reel_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição`)
                           
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Reeleição" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição`)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (Eleitores aptos)  
  
  ag_reel_int <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Reeleição" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_reel_int)
    }
  })
  
  output$agreg_reel_int <- DT::renderDataTable(server = FALSE,{
    bagreg_reel_int()
  })
  
  bagreg_reel_int <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'reel_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if (cargo == "Prefeito" &
                                  indicador == "Reeleição" & 
                                  agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
# 2.2.2. Reeleicao liquida --------------------------------------------------
  
  ## Resumo
  
  ### Renovacao parlamentar (Brasil)  
  
  reeliqbr <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Reeleição líquida" & 
       agregacao == "Brasil"){
      return(input$reel_liq_br)
    }
  })
  
  
  output$reel_liq_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_liq_br()
  })
  
  breel_liq_br <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
     autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'reel_liq_br',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO2
        agregacao <- input$AGREGACAO_REGIONAL2
        if(indicador == "Reeleição líquida" & 
           agregacao == "Brasil"){
          renov_parl_br %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          `Reeleição líquida`) %>% 
            spread(`Ano da eleição`,
                   `Reeleição líquida`) %>% 
            unique()
          
        }
      })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (Brasil)  
  
  ag_reeliq_br <- reactive({
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Reeleição líquida" & 
       agregacao == "Brasil"){
      return(input$agreg_reel_liq_br)
    }
  })
  
  output$agreg_reel_liq_br <- DT::renderDataTable(server = FALSE,{
    bagreg_reel_liq_br()
  })
  
  bagreg_reel_liq_br <- eventReactive(input$BCALC2, {
    datatable(options = list(
     autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'reel_liq_br_agreg',
        bom = TRUE),
        list(                    
          extend = 'colvis',                   
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO2
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- input$UF2
        if(indicador == "Reeleição líquida" & 
           agregacao == "Brasil"){
            data = renov_parl_br %>%
            dplyr::filter(Cargo==input$DESCRICAO_CARGO2)
        }
      })
  })
  
  ## Resumo
  
  ### Renovacao parlamentar (UF) 
  
  
  reeliqmed_uf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Reeleição líquida" & 
       agregacao == "UF"){
      return(input$reel_liq_med_uf)
    }
  })
  
  output$reel_liq_med_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_liq_med_uf()
  })
  
  breel_liq_med_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
        cargo <- input$DESCRICAO_CARGO2
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if((cargo == "Deputado Federal" |
            cargo == "Deputado Estadual") &
           indicador == "Reeleição líquida" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional da reeleição líquida`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional da reeleição líquida`) %>% 
              mutate("media" = "Média nacional da reeleição líquida") %>% 
              column_to_rownames("media")
            
            media1
            
          } else{
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                              UF == input$UF2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional da reeleição líquida`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional da reeleição líquida`) %>% 
              mutate("media" = "Média nacional da reeleição líquida") %>% 
              column_to_rownames("media")
            
            media1
            
          }
        } 
      })
  })
  
  reeliquf <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Reeleição líquida" & 
       agregacao == "UF"){
      return(input$reel_liq_uf)
    }
  })
  
  
  output$reel_liq_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_liq_uf()
  })
  
  breel_liq_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
     autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'reel_liq_uf',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        cargo <- input$DESCRICAO_CARGO2
        if(indicador == "Reeleição líquida" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            renov_parl_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Reeleição líquida`) %>% 
            spread(`Ano da eleição`,
                   `Reeleição líquida`) %>% 
            unique()
          } else{
            renov_parl_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                          UF == input$UF2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Reeleição líquida`) %>% 
            spread(`Ano da eleição`,
                   `Reeleição líquida`) %>% 
            unique()
          }
        }
      })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (UF) 
  
  ag_reeliq_uf <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Reeleição líquida" & 
       agregacao == "UF"){
      return(input$agreg_reel_liq_uf)
    }
  })
  
  output$agreg_reel_liq_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_reel_liq_uf()
  })
  
  bagreg_reel_liq_uf <- eventReactive(input$BCALC2, {
    datatable(options = list(
     autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'reel_liq_uf_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Reeleição líquida" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = renov_parl_uf %>%
            dplyr::filter(Cargo==input$DESCRICAO_CARGO2) %>% 
              select( -`Média nacional da reeleição`,
                      -`Média nacional da reeleição líquida`,
                      -`Média nacional da renovação`,
                      -`Média nacional da renovação líquida`,
                      -`Média nacional das recandidaturas`) 
          } else{
            data = renov_parl_uf %>%
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                            UF == input$UF2) %>% 
              select( -`Média nacional da reeleição`,
                      -`Média nacional da reeleição líquida`,
                      -`Média nacional da renovação`,
                      -`Média nacional da renovação líquida`,
                      -`Média nacional das recandidaturas`) %>% 
              unique()
          }
        }
      })
  })
  
  
  ## Resumo
  
  ### Renovacao parlamentar (MUN)
  
  reeliqmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Reeleição líquida" & 
       agregacao == "Município"){
      return(input$reel_liq_mun)
    }
  })
  
  output$reel_liq_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    breel_liq_mun()
  })
  
  breel_liq_mun <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'reel_liq_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                    
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição líquida" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição líquida`)
                         } else{
                           renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição líquida`)
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Reeleição líquida" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_pf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição líquida`)
                         } else{
                           renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição líquida`)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (MUN)  
  
  ag_reeliq_mun <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Reeleição líquida" & 
       agregacao == "Município"){
      return(input$agreg_reel_liq_mun)
    }
  })
  
  output$agreg_reel_liq_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_reel_liq_mun()
  })
  
  bagreg_reel_liq_mun <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'reel_liq_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição líquida" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Reeleição líquida" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_pf %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                            Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Vereador
  
  
  reeliqmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Reeleição líquida" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$reel_liq_med)
    }
  })
  
  output$reel_liq_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_liq_med()
  })
  
  breel_liq_med <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição líquida" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional da reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da reeleição líquida`)%>% 
                             mutate("media" = "Média nacional da reeleição líquida") %>% 
                             column_to_rownames("media")
                           
                           media2 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média da reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da reeleição líquida`)%>% 
                             mutate("media" = "Média da reeleição líquida do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Reeleição líquida" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média nacional da reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da reeleição líquida`)%>% 
                             mutate("media" = "Média nacional da reeleição líquida") %>% 
                             column_to_rownames("media") 
                           
                           #media1 <- media1 %>% 
                            # mutate("media" = ifelse(media1$Turno == 2,
                             #                        "Média nacional da Reeleição líquida.",
                              #                       media1$media)) %>% 
                             #column_to_rownames("media")
                           
                           media2 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média da reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da reeleição líquida`)%>% 
                             mutate("media" = "Média da reeleição líquida do grupo") %>% 
                             column_to_rownames("media")
                           
                           #media2 <- media2 %>% 
                            # mutate("media" = ifelse(media2$Turno == 2,
                             #                        "Média da Reeleição líquida.",
                              #                       media2$media)) %>% 
                             #column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       }
                     })
  })  
  
  
  
  reeliqint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Reeleição líquida" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$reel_liq_int)
    }
  })
  
  output$reel_liq_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    breel_liq_int()
  })
  
  breel_liq_int <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'reel_liq_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição líquida" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           reel_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição líquida`)
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Reeleição líquida" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           reel_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Reeleição líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Reeleição líquida`)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### reelacao parlamentar (Eleitores aptos)  
  
  ag_reel_liq_int <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Reeleição" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_reel_liq_int)
    }
  })
  
  output$agreg_reel_liq_int <- DT::renderDataTable(server = FALSE,{
    bagreg_reel_liq_int()
  })
  
  bagreg_reel_liq_int <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'reel_liq_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Reeleição líquida" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Reeleição líquida" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                            Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
# 2.2.3. Renovacao ------------------------------------------------
  
  ## Resumo
  
  ### Renovacao parlamentar (Brasil)  
  
  renovbr <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Renovação" &
       agregacao == "Brasil"){
      return(input$renov_br)
    }
  })
  
  
  output$renov_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_br()
  })
  
  brenov_br <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
     autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_br',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO2
        agregacao <- input$AGREGACAO_REGIONAL2
        if(indicador == "Renovação" &
           agregacao == "Brasil"){
          renov_parl_br %>% 
            dplyr::filter(Cargo==input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          `Renovação`) %>% 
            spread(`Ano da eleição`,
                   `Renovação`)
          
        }
      })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (Brasil)  
  
  ag_renov_br <- reactive({
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Renovação" & 
       agregacao == "Brasil"){
      return(input$agreg_renov_br)
    }
  })
  
  output$agreg_renov_br <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_br()
  })
  
  bagreg_renov_br <- eventReactive(input$BCALC2, {
    datatable(options = list(
     autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'renov_br_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO2
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- input$UF2
        if(indicador == "Renovação" & 
           agregacao == "Brasil"){
          data = renov_parl_br %>% 
            dplyr::filter(Cargo==input$DESCRICAO_CARGO2)
          
        }
      })
  })
  
  ## Resumo
  
  ### Renovacao parlamentar (UF) 
  
  
  renovmed_uf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Renovação" & 
       agregacao == "UF"){
      return(input$renov_med_uf)
    }
  })
  
  output$renov_med_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_med_uf()
  })
  
  brenov_med_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
        cargo <- input$DESCRICAO_CARGO2
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if((cargo == "Deputado Federal" |
            cargo == "Deputado Estadual") &
           indicador == "Renovação" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional da renovação`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional da renovação`) %>% 
              mutate("media" = "Média nacional da renovação") %>% 
              column_to_rownames("media")
            
            media1
            
          } else{
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                              UF == input$UF2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional da renovação`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional da renovação`) %>% 
              mutate("media" = "Média nacional da renovação") %>% 
              column_to_rownames("media")
            
            media1
            
          }
        } 
      })
  })
  
  renovuf <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Renovação" &
       agregacao == "UF"){
      return(input$renov_uf)
    }
  })
  
  
  output$renov_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_uf()
  })
  
  brenov_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
     autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_uf',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        cargo <- input$DESCRICAO_CARGO2
        if(indicador == "Renovação" &
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if (uf == "Todas UFs"){
            renov_parl_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Renovação`) %>% 
            spread(`Ano da eleição`,
                  `Renovação`) %>% 
            unique()
          
          } else{
            renov_parl_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                          UF == input$UF2) %>%
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Renovação`) %>% 
            spread(`Ano da eleição`,
                   `Renovação`) %>% 
            unique()
          }
        }
      })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (UF) 
  
  ag_renov_uf <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Renovação" & 
       agregacao == "UF"){
      return(input$agreg_renov_uf)
    }
  })
  
  output$agreg_renov_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_uf()
  })
  
  bagreg_renov_uf <- eventReactive(input$BCALC2, {
    datatable(options = list(
     autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'renov_uf_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Renovação" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = renov_parl_uf %>% 
            filter(Cargo == input$DESCRICAO_CARGO2) %>% 
              select( -`Média nacional da reeleição`,
                      -`Média nacional da reeleição líquida`,
                      -`Média nacional da renovação`,
                      -`Média nacional da renovação líquida`,
                      -`Média nacional das recandidaturas`) %>%  
            unique()
          } else{
            data = renov_parl_uf %>% 
            filter(Cargo == input$DESCRICAO_CARGO2 &
                   UF == input$UF2) %>% 
              select( -`Média nacional da reeleição`,
                      -`Média nacional da reeleição líquida`,
                      -`Média nacional da renovação`,
                      -`Média nacional da renovação líquida`,
                      -`Média nacional das recandidaturas`) %>% 
            unique()
          }
        }
      })
  })
  
  
  ## Resumo
  
  ### Renovacao parlamentar (MUN)
  
  renovmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Renovação" & 
       agregacao == "Município"){
      return(input$renov_mun)
    }
  })
  
  output$renov_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    brenov_mun()
  })
  
  brenov_mun <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação`)
                         } else{
                           renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação`)
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_pf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação`)
                         } else{
                           renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                            Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação`)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (MUN)  
  
  ag_renov_mun <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Renovação" & 
       agregacao == "Município"){
      return(input$agreg_renov_mun)
    }
  })
  
  output$agreg_renov_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_renov_mun()
  })
  
  bagreg_renov_mun <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'renov_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_pf %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                            Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Vereador
  
  
  renovmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Renovação" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$renov_med)
    }
  })
  
  output$renov_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_med()
  })
  
  brenov_med <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional da renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da renovação`)%>% 
                             mutate("media" = "Média nacional da renovação") %>% 
                             column_to_rownames("media")
                           
                           media2 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média da renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da renovação`)%>% 
                             mutate("media" = "Média da renovação do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média nacional da renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da renovação`)%>% 
                             mutate("media" = "Média nacional da renovação")  %>% 
                             column_to_rownames("media")
                           
                           #media1 <- media1 %>% 
                            # mutate("media" = ifelse(media1$Turno == 2,
                             #                        "Média nacional da renovação líquida.",
                              #                       media1$media)) %>% 
                             #column_to_rownames("media")
                           
                           
                           media2 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média da renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da renovação`)%>% 
                             mutate("media" = "Média da renovação do grupo") %>% 
                             column_to_rownames("media") 
                           
                           #media2 <- media2 %>% 
                            # mutate("media" = ifelse(media2$Turno == 2,
                             #                        "Média da Renovação.",
                              #                       media2$media)) %>% 
                             # column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       }
                     })
  })  
  
  
  
  renovint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Renovação" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$renov_int)
    }
  })
  
  output$renov_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_int()
  })
  
  brenov_int <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação`)
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                            Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação`)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (Eleitores aptos)  
  
  ag_renov_int <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Renovação" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_renov_int)
    }
  })
  
  output$agreg_renov_int <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_int()
  })
  
  bagreg_renov_int <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'renov_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                            Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
 
  # 2.2.4. Renovacao liquida ------------------------------------------------
  
  ## Resumo
  
  ### Renovacao parlamentar (Brasil)  
  
  renovliqbr <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Renovação líquida" &
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$renov_liq_br)
    }
  })
  
  
  output$renov_liq_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_liq_br()
  })
  
  brenov_liq_br <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_liq_br',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_RENOV
                       cargo <- input$DESCRICAO_CARGO2
                       agregacao <- input$AGREGACAO_REGIONAL2
                       if(indicador == "Renovação líquida" &
                          agregacao == "Brasil"){
                         renov_parl_br %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO2) %>% 
                           dplyr::select(`Ano da eleição`,
                                         `Renovação líquida`) %>% 
                           spread(`Ano da eleição`,
                                  `Renovação líquida`)
                         
                       }
                     })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (Brasil)  
  
  ag_renovliq_br <- reactive({
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Renovação líquida" & 
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$agreg_renov_liq_br)
    }
  })
  
  output$agreg_renov_liq_br <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_liq_br()
  })
  
  bagreg_renov_liq_br <- eventReactive(input$BCALC2, {
    datatable(options = list(
      autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'renov_liq_br_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_RENOV
                       cargo <- input$DESCRICAO_CARGO2
                       agregacao <- input$AGREGACAO_REGIONAL2
                       uf <- input$UF2
                       if(indicador == "Renovação líquida" & 
                          agregacao == "Brasil"){
                         data = renov_parl_br %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO2) 
                         
                       }
                     })
  })
  
  ## Resumo
  
  ### Renovacao parlamentar (UF) 
  
  renovliqmed_uf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Renovação" & 
       agregacao == "UF"){
      return(input$renov_liq_med_uf)
    }
  })
  
  output$renov_liq_med_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_liq_med_uf()
  })
  
  brenov_liq_med_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
        cargo <- input$DESCRICAO_CARGO2
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if((cargo == "Deputado Federal" |
            cargo == "Deputado Estadual") &
           indicador == "Renovação líquida" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional da renovação líquida`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional da renovação líquida`) %>% 
              mutate("media" = "Média nacional da renovação líquida") %>% 
              column_to_rownames("media")
            
            media1
            
          } else{
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                              UF == input$UF2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional da renovação líquida`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional da renovação líquida`) %>% 
              mutate("media" = "Média nacional da renovação líquida") %>% 
              column_to_rownames("media")
            
            media1
            
          }
        } 
      })
  })
  
  renovliquf <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Renovação líquida" &
       agregacao == "UF"){
      return(input$renov_liq_uf)
    }
  })
  
  
  output$renov_liq_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_liq_uf()
  })
  
  brenov_liq_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_liq_uf',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       uf <- req(input$UF2)
                       cargo <- input$DESCRICAO_CARGO2
                       if(indicador == "Renovação líquida" &
                          agregacao == "UF"){
                         if(uf == ""){
                           return()
                         } else if (uf == "Todas UFs"){
                           renov_parl_uf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Renovação líquida`) %>% 
                             spread(`Ano da eleição`,
                                    `Renovação líquida`) %>% 
                             unique()
                           
                         } else{
                           renov_parl_uf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                                             UF == input$UF2) %>%
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Renovação líquida`) %>% 
                             spread(`Ano da eleição`,
                                    `Renovação líquida`) %>% 
                             unique()
                         }
                       }
                     })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (UF) 
  
  ag_renovliq_uf <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Renovação líquida" & 
       agregacao == "UF"){
      return(input$agreg_renov_liq_uf)
    }
  })
  
  output$agreg_renov_liq_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_liq_uf()
  })
  
  bagreg_renov_liq_uf <- eventReactive(input$BCALC2, {
    datatable(options = list(
      autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'renov_liq_uf_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       uf <- req(input$UF2)
                       if(indicador == "Renovação líquida" & 
                          agregacao == "UF"){
                         if(uf == ""){
                           return()
                         } else if(uf == "Todas UFs"){
                           data = renov_parl_uf %>% 
                             filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             select( -`Média nacional da reeleição`,
                                     -`Média nacional da reeleição líquida`,
                                     -`Média nacional da renovação`,
                                     -`Média nacional da renovação líquida`,
                                     -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_uf %>% 
                             filter(Cargo == input$DESCRICAO_CARGO2 &
                                      UF == input$UF2) %>% 
                             select( -`Média nacional da reeleição`,
                                     -`Média nacional da reeleição líquida`,
                                     -`Média nacional da renovação`,
                                     -`Média nacional da renovação líquida`,
                                     -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Renovacao parlamentar (MUN)
  
  renovliqmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Renovação líquida" & 
       agregacao == "Município"){
      return(input$renov_liq_mun)
    }
  })
  
  output$renov_liq_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    brenov_liq_mun()
  })
  
  brenov_liq_mun <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_liq_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação líquida" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação líquida`)
                         } else{
                           renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação líquida`)
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação líquida" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_pf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação líquida`)
                         } else{
                           renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação líquida`)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (MUN)  
  
  ag_renovliq_mun <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Renovação líquida" & 
       agregacao == "Município"){
      return(input$agreg_renov_liq_mun)
    }
  })
  
  output$agreg_renov_liq_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_renov_liq_mun()
  })
  
  bagreg_renov_liq_mun <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'renov_liq_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação líquida" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação líquida" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_pf %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Vereador
  
  
  renovliqmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Renovação líquida" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$renov_liq_med)
    }
  })
  
  output$renov_liq_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_liq_med()
  })
  
  brenov_liq_med <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação líquida" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional da renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da renovação líquida`)%>% 
                             mutate("media" = "Média nacional da renovação líquida") %>% 
                             column_to_rownames("media")
                           
                           media2 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média da renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da renovação líquida`)%>% 
                             mutate("media" = "Média da renovação líquida do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação líquida" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média nacional da renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da renovação líquida`)%>% 
                             mutate("media" = "Média nacional da renovação líquida")  %>% 
                             column_to_rownames("media")
                           
                           #media1 <- media1 %>% 
                           # mutate("media" = ifelse(media1$Turno == 2,
                           #                        "Média nacional da renovação líquida.",
                           #                       media1$media)) %>% 
                           #column_to_rownames("media")
                           
                           
                           media2 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média da renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da renovação líquida`)%>% 
                             mutate("media" = "Média da renovação líquida do grupo") %>% 
                             column_to_rownames("media") 
                           
                           #media2 <- media2 %>% 
                           # mutate("media" = ifelse(media2$Turno == 2,
                           #                        "Média da renovação líquida.",
                           #                       media2$media)) %>% 
                           # column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       }
                     })
  })  
  
  
  
  renovliqint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Renovação líquida" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$renov_liq_int)
    }
  })
  
  output$renov_liq_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_liq_int()
  })
  
  brenov_liq_int <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_liq_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação líquida" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação líquida`)
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação líquida" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           `Renovação líquida`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Renovação líquida`)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (Eleitores aptos)  
  
  ag_renov_liq_int <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Renovação líquida" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_renov_liq_int)
    }
  })
  
  output$agreg_renov_liq_int <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_liq_int()
  })
  
  bagreg_renov_liq_int <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'renov_liq_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       if(cargo == "Vereador" &
                          indicador == "Renovação líquida" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Renovação líquida" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  

# 2.2.5. Recandidaturas ---------------------------------------------------


  ## Resumo
  
  ### Renovacao parlamentar (Brasil)  
  
  recandbr <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Recandidaturas" &
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$recand_br)
    }
  })
  
  
  output$recand_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brecand_br()
  })
  
  brecand_br <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'recand_br',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       
                       indicador <- input$INDICADORES_RENOV
                       cargo <- input$DESCRICAO_CARGO2
                       agregacao <- input$AGREGACAO_REGIONAL2
                       
                       if(indicador == "Recandidaturas" &
                          agregacao == "Brasil"){
                         
                         renov_parl_br %>% 
                           dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Recandidaturas) %>% 
                           spread(`Ano da eleição`,
                                  Recandidaturas)
                         
                       }
                     })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (Brasil)  
  
  ag_recand_br <- reactive({
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Recandidaturas" & 
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$agreg_recand_br)
    }
  })
  
  output$agreg_recand_br <- DT::renderDataTable(server = FALSE,{
    bagreg_recand_br()
  })
  
  bagreg_recand_br <- eventReactive(input$BCALC2, {
    datatable(options = list(
      autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'recand_br_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       
                       indicador <- input$INDICADORES_RENOV
                       cargo <- input$DESCRICAO_CARGO2
                       agregacao <- input$AGREGACAO_REGIONAL2
                       uf <- input$UF2
                       
                       if(indicador == "Recandidaturas" & 
                          agregacao == "Brasil"){
                         
                         data = renov_parl_br %>% 
                           dplyr::filter(Cargo == input$DESCRICAO_CARGO2) 
                         
                       }
                     })
  })
  
  ## Resumo
  
  ### Renovacao parlamentar (UF) 
  
  recandmed_uf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Recandidaturas" & 
       agregacao == "UF"){
      return(input$recand_med_uf)
    }
  })
  
  output$recand_med_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brecand_med_uf()
  })
  
  brecand_med_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
        
        cargo <- input$DESCRICAO_CARGO2
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        
        if((cargo == "Deputado Federal" |
            cargo == "Deputado Estadual") &
           indicador == "Recandidaturas" & 
           agregacao == "UF"){
          
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional das recandidaturas`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional das recandidaturas`) %>% 
              mutate("media" = "Média nacional das recandidaturas") %>% 
              column_to_rownames("media")
            
            media1
            
          } else{
            
            media1 <- renov_parl_uf %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                              UF == input$UF2) %>% 
              dplyr::select(`Ano da eleição`,
                            `Média nacional das recandidaturas`) %>% 
              unique() %>% 
              spread(`Ano da eleição`,
                     `Média nacional das recandidaturas`) %>% 
              mutate("media" = "Média nacional das recandidaturas") %>% 
              column_to_rownames("media")
            
            media1
            
          }
        } 
      })
  })
  
  recanduf <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO2
    if(indicador == "Recandidaturas" &
       agregacao == "UF"){
      return(input$recand_uf)
    }
  })
  
  
  output$recand_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brecand_uf()
  })
  
  brecand_uf <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'recand_uf',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       uf <- req(input$UF2)
                       cargo <- input$DESCRICAO_CARGO2
                       
                       if(indicador == "Recandidaturas" &
                          agregacao == "UF"){
                         if(uf == ""){
                           return()
                         } else if (uf == "Todas UFs"){
                           renov_parl_uf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           Recandidaturas) %>% 
                             spread(`Ano da eleição`,
                                    Recandidaturas) %>% 
                             unique()
                           
                         } else{
                           
                           renov_parl_uf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2 &
                                             UF == input$UF2) %>%
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           Recandidaturas) %>% 
                             spread(`Ano da eleição`,
                                    Recandidaturas) %>% 
                             unique()
                         }
                       }
                     })
  }) 
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (UF) 
  
  ag_recand_uf <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Recandidaturas" & 
       agregacao == "UF"){
      return(input$agreg_recand_uf)
    }
  })
  
  output$agreg_recand_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_recand_uf()
  })
  
  bagreg_recand_uf <- eventReactive(input$BCALC2, {
    datatable(options = list(
      autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'recand_uf_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       uf <- req(input$UF2)
                       
                       if(indicador == "Recandidaturas" & 
                          agregacao == "UF"){
                         if(uf == ""){
                           return()
                         } else if(uf == "Todas UFs"){
                           data = renov_parl_uf %>% 
                             filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             select( -`Média nacional da reeleição`,
                                     -`Média nacional da reeleição líquida`,
                                     -`Média nacional da renovação`,
                                     -`Média nacional da renovação líquida`,
                                     -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_uf %>% 
                             filter(Cargo == input$DESCRICAO_CARGO2 &
                                      UF == input$UF2) %>% 
                             select( -`Média nacional da reeleição`,
                                     -`Média nacional da reeleição líquida`,
                                     -`Média nacional da renovação`,
                                     -`Média nacional da renovação líquida`,
                                     -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Renovacao parlamentar (MUN)
  
  recandmun <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Recandidaturas" & 
       agregacao == "Município"){
      return(input$recand_mun)
    }
  })
  
  output$recand_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
    brecand_mun()
  })
  
  brecand_mun <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'recand_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'FixedColumns'),{
                       
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       
                       if(cargo == "Vereador" &
                          indicador == "Recandidaturas" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           Recandidaturas) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    Recandidaturas)
                         } else{
                           renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           Recandidaturas) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    Recandidaturas)
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Recandidaturas" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           renov_parl_pf %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           Recandidaturas) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    Recandidaturas)
                         } else{
                           renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           Recandidaturas) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    Recandidaturas)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (MUN)  
  
  ag_recand_mun <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Recandidaturas" & 
       agregacao == "Município"){
      return(input$agreg_recand_mun)
    }
  })
  
  output$agreg_recand_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_recand_mun()
  })
  
  bagreg_recand_mun <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'recand_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       municipio <- req(input$MUN2)
                       
                       if(cargo == "Vereador" &
                          indicador == "Recandidaturas" & 
                          agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_mun %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Recandidaturas" & 
                                 agregacao == "Município"){
                         if(municipio == ""){
                           return()
                         } else if(municipio == "Todos os municípios"){
                           data = renov_parl_pf %>%
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Nome do município2` == input$MUN2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>%
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
  ## Resumo
  
  ### Vereador
  
  
  recandmed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Recandidaturas" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$recand_med)
    }
  })
  
  output$recand_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brecand_med()
  })
  
  brecand_med <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('Buttons',
                     'FixedColumns'),{
                       
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       
                       if(cargo == "Vereador" &
                          indicador == "Recandidaturas" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média nacional das recandidaturas`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional das recandidaturas`)%>% 
                             mutate("media" = "Média nacional das recandidaturas") %>% 
                             column_to_rownames("media")
                           
                           media2 <- renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           `Média das recandidaturas`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média das recandidaturas`)%>% 
                             mutate("media" = "Média das recandidaturas do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Recandidaturas" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           
                           media1 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média nacional das recandidaturas`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional das recandidaturas`)%>% 
                             mutate("media" = "Média nacional das recandidaturas")  %>% 
                             column_to_rownames("media")
                           
                           #media1 <- media1 %>% 
                           # mutate("media" = ifelse(media1$Turno == 2,
                           #                        "Média nacional da renovação líquida.",
                           #                       media1$media)) %>% 
                           #column_to_rownames("media")
                           
                           
                           media2 <- renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           #Turno,
                                           `Média das recandidaturas`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média das recandidaturas`)%>% 
                             mutate("media" = "Média das recandidaturas do grupo") %>% 
                             column_to_rownames("media") 
                           
                           #media2 <- media2 %>% 
                           # mutate("media" = ifelse(media2$Turno == 2,
                           #                        "Média da renovação líquida.",
                           #                       media2$media)) %>% 
                           # column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         }
                       }
                     })
  })  
  
  
  
  recandint <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Recandidaturas" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$recand_int)
    }
  })
  
  output$recand_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brecand_int()
  })
  
  brecand_int <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 1
      ),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'recand_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       
                       if(cargo == "Vereador" &
                          indicador == "Recandidaturas" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           Recandidaturas) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    Recandidaturas)
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Recandidaturas" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 &
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           Recandidaturas) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    Recandidaturas)
                         }
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Renovacao parlamentar (Eleitores aptos)  
  
  ag_recand_int <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Recandidaturas" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_recand_int)
    }
  })
  
  output$agreg_recand_int <- DT::renderDataTable(server = FALSE,{
    bagreg_recand_int()
  })
  
  bagreg_recand_int <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(list(
        leftColumns = 3
      )),
      columnDefs = list(list(
        className = 'dt-center', 
        targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'recand_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       
                       cargo <- input$DESCRICAO_CARGO2
                       indicador <- input$INDICADORES_RENOV
                       agregacao <- input$AGREGACAO_REGIONAL2
                       intervalo <- req(input$INT2)
                       
                       if(cargo == "Vereador" &
                          indicador == "Recandidaturas" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       } else if(cargo == "Prefeito" &
                                 indicador == "Recandidaturas" & 
                                 agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{
                           data = renov_parl_pf %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT2 & 
                                             Cargo == input$DESCRICAO_CARGO2) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da reeleição`,
                                    -`Média da reeleição líquida`,
                                    -`Média da renovação`,
                                    -`Média da renovação líquida`,
                                    -`Média das recandidaturas`,
                                    -`Média nacional da reeleição`,
                                    -`Média nacional da reeleição líquida`,
                                    -`Média nacional da renovação`,
                                    -`Média nacional da renovação líquida`,
                                    -`Média nacional das recandidaturas`) %>% 
                             unique()
                         }
                       }
                     })
  })
  
  
# 2.3. Alienacao ----------------------------------------------------------  
  
 
  ## Modal para ajuda
  
  ### Dados desagregados
  
  observeEvent(input$modal_alien,{
    showModal(modalDialog(
                         title = h4(class = "h4 titulo",
                                        "AJUDA"),
                          footer = modalButton("FECHAR"), 
                          size = "m",
                          htmlOutput("def_alien"),
                          easyClose = TRUE,
                          style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "))
  })
  
  ### Dados agregados
  
  observeEvent(input$modal_alien_ag,{
    showModal(modalDialog(
                          title = h4(class = "h4 titulo",
                                         "AJUDA"),
                          footer = modalButton("FECHAR"), 
                          size = "m",
                          htmlOutput("def_alien"),
                          easyClose = TRUE,
                          style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "
                          ))
  })
  
  ## Funcao para descricao dos indicadores de alienacao
  
  output$def_alien <- renderUI({
    note <- paste0("
                   <font color = 'black'>
                   <h4>Alienação absoluta</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Indicadores de alienação medem a participação nas eleições, por unidade eleitoral.
                   A 'alienação absoluta' é a soma da quantidade de abstenções, votos brancos e votos nulos
                   de determinada eleição.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   AA = (Abstenções + Votos brancos + Votos nulos)
                   <p>
                   <h4><br />Alienação percentual</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'> A 'alienação percentual' é o índice de 'alienação absoluta' 
                   dividido pelo total de eleitores 
                   aptos da unidade eleitoral.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   AP = (Índice de alienação absoluta)/(Total de eleitores aptos)
                   <p>
                   <h4><br />Abstenção absoluta</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Quantidade de eleitores que não compareceram às eleições 
                   naquela agregação regional.</p></h5>
                   <p>
                   <h4><br />Abstenção percentual</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>A 'abstenção percentual' é o índice de 'abstenção absoluta' 
                   dividido pelo total de eleitores 
                   aptos da unidade eleitoral.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   ABP = (Índice de abstenção absoluta)/(Total de eleitores aptos)
                   <p>
                   <h4><br />Votos brancos absolutos</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Quantidade de votos brancos totalizados naquela 
                   agregação regional.</p></h5>
                   <p>
                   <h4><br />Votos brancos percentuais</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Os 'votos brancos percentuais' são calculados através
                   da divisão do índice de 'votos brancos absolutos' pelo total de eleitores 
                   aptos da unidade eleitoral.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   VBP = (Índice de votos brancos absolutos)/(Total de eleitores aptos)
                   <p>
                   <h4><br />Votos nulos absolutos</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Quantidade de votos nulos totalizados naquela 
                   agregação regional.</p></h5>
                   <p>
                   <h4><br />Votos nulos percentuais</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Os 'votos nulos percentuais' são calculados através
                   da divisão do índice de 'votos nulos absolutos' pelo total de eleitores 
                   aptos da unidade eleitoral.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   VNP = (Índice de votos nulos absolutos)/(Total de eleitores aptos)
                   <p>
                   <p><br />
                   <strong>Fonte:</strong> 
                   <p>1. Votos e partidos: almanaque de dados eleitorais: Brasil e outros 
                   países/ Organização de Wanderley Guilherme dos Santos, com a colaboração de Fabrícia Guimarães. -
                   Rio de Janeiro: Editora FGV, 2002).</p></font>")
    HTML(note)
  })
   
  
 
# 2.3.1. Alienacao absoluta --------------------------------------------------------


## Resumo
  
### Cargos BR
  
  alienabsbr <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Alienação absoluta" & 
       agregacao == "Brasil"){
      return(input$alien_abs_br)
  }
  })
  

  output$alien_abs_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_abs_br()
  })
  
  balien_abs_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
    datatable(options = list(
               autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 1
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'alien_abs_br',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                             'FixedColumns'),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      if(indicador == "Alienação absoluta" & 
         agregacao == "Brasil"){
        alien_br %>% 
          dplyr::filter(Cargo ==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,
                        Turno,
                        `Alienação absoluta`) %>% 
          spread(`Ano da eleição`,
                 `Alienação absoluta`)
        
      }
    })
  }) 
  
## Dados desagregados
  
### Cargos BR  
  
  ag_alienabs_br <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Alienação absoluta" & 
       agregacao == "Brasil"){
      return(input$agreg_alien_abs_br)
    }
  })
  
  output$agreg_alien_abs_br <- DT::renderDataTable(server = FALSE,{
    bagreg_alien_abs_br()
  })
  
  bagreg_alien_abs_br <- eventReactive(input$BCALC3, {
    datatable(options = list(
               autoWidth = FALSE,
                scrollX = TRUE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 3
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'alien_abs_br_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                             'FixedColumns'),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação absoluta" & 
         agregacao == "Brasil"){
        data = alien_br %>%
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) 
        
      }
    })
  })
  
## Tabela para visualizacao  
  
### Cargos UF
  
  alienabsuf <- reactive({ ## Atributos das tabelas de alienacao absoluta 
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    uf <- input$UF3
    if(indicador == "Alienação absoluta" & 
       agregacao == "UF"){
      return(input$alien_abs_uf)
  }    
  })
  
  output$alien_abs_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_abs_uf()
  })
  
  balien_abs_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
    datatable(options = list(
               autoWidth = FALSE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 2
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'alien_abs_uf',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                             'FixedColumns'),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação absoluta" & agregacao == "UF"){
        if(uf=="Todas UFs"){
          alien_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          Turno, 
                          `Alienação absoluta`) %>% 
            spread(`Ano da eleição`,
                  `Alienação absoluta`) 
        }else{
          alien_uf %>% 
            dplyr::filter(UF == input$UF3 & 
                          Cargo==input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          Turno,
                          `Alienação absoluta`) %>% 
            spread(`Ano da eleição`,
                   `Alienação absoluta`)}
        
      }
    })
  })  
  
## Dados desagregados
  
### Cargos UF  
  
  ag_alienabs_uf <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação absoluta" & 
       agregacao == "UF"){
      return(input$agreg_alien_abs_uf)
    }
  })
  
  output$agreg_alien_abs_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_alien_abs_uf()
  })
  
  bagreg_alien_abs_uf <- eventReactive(input$BCALC3, {
    datatable(options = list(
               autoWidth = FALSE,
                scrollX = TRUE,
                ordering = TRUE, 
                searching = FALSE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                fixedColumns = list(
                  leftColumns = 3
                ),
                columnDefs = list(list(
                  className = 'dt-center', targets = '_all')),
                dom = 'Bflrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'alien_abs_uf_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                             'FixedColumns'),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação absoluta" & 
         agregacao == "UF"){
        if(input$UF3 == "Todas UFs"){
          data = alien_uf %>% 
            dplyr::filter(Cargo==input$DESCRICAO_CARGO3)
          } else{ 
              data = alien_uf %>% 
                dplyr::filter(Cargo==input$DESCRICAO_CARGO3 & 
                              UF == input$UF3) 
            }}
    })
  })
  
  
  ## Resumo
  
  ### Cargos MUN
  
  alienabsmun <- reactive({ ## Atributos das tabelas de alienacao absoluta 
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    uf <- input$UF3
    if(indicador == "Alienação absoluta" & 
       agregacao == "Município"){
      return(input$alien_abs_mun)
    }    
  })
  
  output$alien_abs_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_abs_mun()
  })
  
  balien_abs_mun <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'alien_abs_mun',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_ALIE
                       cargo <- input$DESCRICAO_CARGO3
                       agregacao <- input$AGREGACAO_REGIONAL3
                       municipio <- input$MUN3
                       if(indicador == "Alienação absoluta" & 
                          agregacao == "Município"){
                         if(municipio == "Todos os municípios"){
                           data = alien_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO3) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           Turno, 
                                           `Alienação absoluta`) %>% 
                             spread(`Ano da eleição`,
                                    `Alienação absoluta`) 
                         }else{
                           data = alien_mun %>% 
                             dplyr::filter(`Nome do município2` == input$MUN3 & 
                                            Cargo == input$DESCRICAO_CARGO3) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           Turno,
                                           `Alienação absoluta`) %>% 
                             spread(`Ano da eleição`,
                                    `Alienação absoluta`)}
                         
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Cargos MUN 
  
  ag_alienabs_mun <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação absoluta" & 
       agregacao == "Município"){
      return(input$agreg_alien_abs_mun)
    }
  })
  
  output$agreg_alien_abs_mun <- DT::renderDataTable(server = TRUE,{
    bagreg_alien_abs_mun()
  })
  
  bagreg_alien_abs_mun <- eventReactive(input$BCALC3, {
    datatable(options = list(
      autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'alien_abs_mun_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_ALIE
                       cargo <- input$DESCRICAO_CARGO3
                       agregacao <- input$AGREGACAO_REGIONAL3
                       municipio <- input$MUN3
                       if(indicador == "Alienação absoluta" & 
                          agregacao == "Município"){
                         if(municipio == "Todos os municípios"){
                           data = alien_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO3) %>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da quantidade de abstenções`,
                                    -`Média do percentual de abstenções`,
                                    -`Média da quantidade de votos brancos`,
                                    -`Média do percentual de votos brancos`,
                                    -`Média da quantidade de votos nulos`,
                                    -`Média do percentual de votos nulos`,
                                    -`Média da alienação absoluta`,
                                    -`Média da alienação percentual`,
                                    -`Média nacional da quantidade de abstenções`,
                                    -`Média nacional do percentual de abstenções`,
                                    -`Média nacional da quantidade de votos brancos`,
                                    -`Média nacional do percentual de votos brancos`,
                                    -`Média nacional da quantidade de votos nulos`,
                                    -`Média nacional do percentual de votos nulos`,
                                    -`Média nacional da alienação absoluta`,
                                    -`Média nacional da alienação percentual`)
                         } else{ 
                           data = alien_mun %>% 
                             dplyr::filter(Cargo==input$DESCRICAO_CARGO3 & 
                                             `Nome do município2` == input$MUN3)%>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da quantidade de abstenções`,
                                    -`Média do percentual de abstenções`,
                                    -`Média da quantidade de votos brancos`,
                                    -`Média do percentual de votos brancos`,
                                    -`Média da quantidade de votos nulos`,
                                    -`Média do percentual de votos nulos`,
                                    -`Média da alienação absoluta`,
                                    -`Média da alienação percentual`,
                                    -`Média nacional da quantidade de abstenções`,
                                    -`Média nacional do percentual de abstenções`,
                                    -`Média nacional da quantidade de votos brancos`,
                                    -`Média nacional do percentual de votos brancos`,
                                    -`Média nacional da quantidade de votos nulos`,
                                    -`Média nacional do percentual de votos nulos`,
                                    -`Média nacional da alienação absoluta`,
                                    -`Média nacional da alienação percentual`)
                         }}
                     })
  })
  
  
  ## Resumo
  
  ### Eleitores aptos
  
  
  alienabsmed <- reactive({ ## Atributos das tabelas de alienacao absoluta 
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    uf <- input$UF3
    if(indicador == "Alienação absoluta" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$alien_abs_med)
    }    
  })
  
  output$alien_abs_med <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_abs_med()
  })
  
  balien_abs_med <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 't'), 
      class = "display",
      extensions = c('FixedColumns'),{
                       indicador <- input$INDICADORES_ALIE
                       cargo <- input$DESCRICAO_CARGO3
                       agregacao <- input$AGREGACAO_REGIONAL3
                       intervalo <- input$INT3
                       if(indicador == "Alienação absoluta" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return() 
                         } else if(cargo == "Prefeito" &
                                   intervalo == "Acima de 200 mil eleitores"){
                           
                           media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo == input$DESCRICAO_CARGO3) %>% 
                             dplyr::select(`Ano da eleição`,
                                           Turno,
                                           `Média nacional da alienação absoluta`) %>%
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da alienação absoluta`) %>% 
                             mutate("media" = "Média nacional da alienação absoluta") 
                           
                           media1 <- media1 %>% 
                             mutate("media" = ifelse(media1$Turno == 2,
                                                     "Média nacional da alienação absoluta.",
                                                     media1$media)) %>% 
                             column_to_rownames("media")
                           
                           
                           media2 <- alien_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                             Cargo == input$DESCRICAO_CARGO3) %>% 
                             dplyr::select(`Ano da eleição`,
                                           Turno,
                                           `Média da alienação absoluta`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da alienação absoluta`) %>% 
                             mutate("media" = "Média da alienação absoluta do grupo")
                           
                           media2 <- media2 %>% 
                             mutate("media" = ifelse(media2$Turno == 2,
                                                     "Média da alienação absoluta do grupo.",
                                                     media2$media)) %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                         } else{
                           
                           media1 <- alien_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                             Cargo == input$DESCRICAO_CARGO3) %>% 
                             dplyr::select(`Ano da eleição`,
                                           Turno,
                                           `Média nacional da alienação absoluta`) %>%
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média nacional da alienação absoluta`) %>% 
                             mutate("media" = "Média nacional da alienação absoluta") %>% 
                             column_to_rownames("media")
                           
                           media2 <- alien_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                             Cargo == input$DESCRICAO_CARGO3) %>% 
                             dplyr::select(`Ano da eleição`,
                                           Turno,
                                           `Média da alienação absoluta`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Média da alienação absoluta`) %>% 
                             mutate("media" = "Média da alienação absoluta do grupo") %>% 
                             column_to_rownames("media")
                           
                           media1 <- rbind(media1, media2)
                           
                           media1
                           
                           }
                       }
                     })
  })  
  
  
  
  alienabsint <- reactive({ ## Atributos das tabelas de alienacao absoluta 
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    uf <- input$UF3
    if(indicador == "Alienação absoluta" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$alien_abs_int)
    }    
  })
  
  output$alien_abs_int <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_abs_int()
  })
  
  balien_abs_int <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
    datatable(options = list(
      autoWidth = FALSE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 2
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'alien_abs_int',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_ALIE
                       cargo <- input$DESCRICAO_CARGO3
                       agregacao <- input$AGREGACAO_REGIONAL3
                       intervalo <- input$INT3
                       if(indicador == "Alienação absoluta" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                          return() 
                         } else{
                           data = alien_mun %>% 
                             dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo == input$DESCRICAO_CARGO3) %>% 
                             dplyr::select(`Ano da eleição`,
                                           UF,
                                           `Nome do município`,
                                           Turno,
                                           `Alienação absoluta`) %>% 
                             spread(`Ano da eleição`,
                                    `Alienação absoluta`)}
                         
                       }
                     })
  })  
  
  ## Dados desagregados
  
  ### Eleitores aptos
  
  ag_alienabs_int <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação absoluta" & 
       agregacao == "Quantidade de eleitores aptos"){
      return(input$agreg_alien_abs_int)
    }
  })
  
  output$agreg_alien_abs_int <- DT::renderDataTable(server = FALSE,{
    bagreg_alien_abs_int()
  })
  
  bagreg_alien_abs_int <- eventReactive(input$BCALC3, {
    datatable(options = list(
      autoWidth = FALSE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = FALSE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      fixedColumns = list(
        leftColumns = 3
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = '_all')),
      dom = 'Bflrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'alien_abs_int_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'FixedColumns'),{
                       indicador <- input$INDICADORES_ALIE
                       cargo <- input$DESCRICAO_CARGO3
                       agregacao <- input$AGREGACAO_REGIONAL3
                       intervalo <- input$INT3
                       if(indicador == "Alienação absoluta" & 
                          agregacao == "Quantidade de eleitores aptos"){
                         if(intervalo == ""){
                           return()
                         } else{ 
                           data = alien_mun %>% 
                             dplyr::filter(Cargo == input$DESCRICAO_CARGO3 & 
                                           `Eleitores aptos` == input$INT3)%>% 
                             select(-`Nome do município2`, -`Eleitores aptos`,
                                    -`Média da quantidade de abstenções`,
                                    -`Média do percentual de abstenções`,
                                    -`Média da quantidade de votos brancos`,
                                    -`Média do percentual de votos brancos`,
                                    -`Média da quantidade de votos nulos`,
                                    -`Média do percentual de votos nulos`,
                                    -`Média da alienação absoluta`,
                                    -`Média da alienação percentual`,
                                    -`Média nacional da quantidade de abstenções`,
                                    -`Média nacional do percentual de abstenções`,
                                    -`Média nacional da quantidade de votos brancos`,
                                    -`Média nacional do percentual de votos brancos`,
                                    -`Média nacional da quantidade de votos nulos`,
                                    -`Média nacional do percentual de votos nulos`,
                                    -`Média nacional da alienação absoluta`,
                                    -`Média nacional da alienação percentual`)
                         }}
                     })
  })
  
  
# 2.3.2. Alienacao percentual ---------------------------------------------

## Tabela para visualizacao
    
### Cargos BR

alienpercbr <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  if(indicador == "Alienação percentual" & 
     agregacao == "Brasil"){
    return(input$alien_perc_br)
  }
})


output$alien_perc_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  balien_perc_br()
})

balien_perc_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
             autoWidth = FALSE,
              
              ordering = TRUE, 
              searching = FALSE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              fixedColumns = list(
                leftColumns = 1
              ),
              columnDefs = list(list(
                className = 'dt-center', targets = '_all')),
              dom = 'Bflrtip',
              buttons = list(list(
                extend = 'csv',
                title = 'alien_perc_br',
                bom = TRUE))), 
             class = "display",
            rownames = FALSE,
            extensions = c('Buttons',   
                          
                           'FixedColumns'),{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Alienação percentual" & 
       agregacao == "Brasil"){
      alien_br %>% 
        dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
        dplyr::select(`Ano da eleição`,
                      Turno, 
                      `Alienação percentual`) %>% 
        spread(`Ano da eleição`,
               `Alienação percentual`)
      
    }
  })
}) 

## Dados desagregados

### Cargos BR

ag_alienperc_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  if(indicador == "Alienação percentual" & 
     agregacao == "Brasil"){
    return(input$agreg_alien_perc_br)
  }
})

output$agreg_alien_perc_br <- DT::renderDataTable(server = FALSE,{
  bagreg_alien_perc_br()
})

bagreg_alien_perc_br <- eventReactive(input$BCALC3, {
  datatable(options = list(
             autoWidth = FALSE,
              scrollX = TRUE,
              
              ordering = TRUE, 
              searching = FALSE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              fixedColumns = list(
                leftColumns = 3
              ),
              columnDefs = list(list(
                className = 'dt-center', targets = '_all')),
              dom = 'Bflrtip',
              buttons = list(
                             list(
                extend = 'csv',
                exportOptions = list(
                  columns = ':visible'),
                title = 'alien_perc_br_agreg',
                bom = TRUE),
                list(                     
                  extend = 'colvis',                     
                  text = 'Colunas'))), 
             class = "display",
            rownames = FALSE,
            extensions = c('Buttons',
                          
                           'FixedColumns'),{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação percentual" & 
       agregacao == "Brasil"){
      alien_br %>%
        dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
        unique()
      
      
    }
  })
})


## Tabela para visualizacao

### Cargos UF

alienpercuf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Alienação percentual" & 
     agregacao == "UF"){
    return(input$alien_perc_uf)
  }
})


output$alien_perc_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  balien_perc_uf()
})

balien_perc_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
             autoWidth = FALSE,
              
              ordering = TRUE, 
              searching = FALSE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              fixedColumns = list(
                leftColumns = 2
              ),
              columnDefs = list(list(
                className = 'dt-center', targets = '_all')),
              dom = 'Bflrtip',
              buttons = list(list(
                extend = 'csv',
                title = 'alien_perc_uf',
                bom = TRUE))), 
             class = "display",
            rownames = FALSE,
            extensions = c('Buttons',                              
                          
                           'FixedColumns'),{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação percentual" & 
       agregacao == "UF"){
      if(uf=="Todas UFs"){
        alien_uf %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,
                        UF,
                        Cargo,
                        Turno,
                        `Alienação percentual`) %>% 
          spread(`Ano da eleição`,
                 `Alienação percentual`)
      }
      else{
        alien_uf %>% 
          dplyr::filter(UF == input$UF3 & 
                        Cargo==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,
                        UF,
                        Turno,
                        `Alienação percentual`) %>% 
          spread(`Ano da eleição`,
                 `Alienação percentual`)}
      
    }
  })
})

## Dados desagregados

### Cargos UF

ag_alienperc_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Alienação percentual" & 
     agregacao == "UF"){
    return(input$agreg_alien_perc_uf)
  }
})

output$agreg_alien_perc_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_alien_perc_uf()
})

bagreg_alien_perc_uf <- eventReactive(input$BCALC3, {
  datatable(options = list(
             autoWidth = FALSE,
              scrollX = TRUE,
              
              ordering = TRUE, 
              searching = FALSE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              fixedColumns = list(
                leftColumns = 3
              ),
              columnDefs = list(list(
                className = 'dt-center', targets = '_all')),
              dom = 'Bflrtip',
              buttons = list(
                             list(
                extend = 'csv',
                exportOptions = list(
                  columns = ':visible'),
                title = 'alien_perc_uf_agreg',
                bom = TRUE),
                list(                     
                  extend = 'colvis',                     
                  text = 'Colunas'))), 
             class = "display",
            rownames = FALSE,
            extensions = c('Buttons',
                          
                           'FixedColumns'),{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação percentual" & 
       agregacao == "UF"){
      if(input$UF3 == "Todas UFs"){
        data =alien_uf %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
         unique()
      } else{ 
        data = alien_uf %>% 
          dplyr::filter(UF == input$UF3 &
                        Cargo==input$DESCRICAO_CARGO3) %>% 
          unique()
      }}
  })
})


## Resumo

### Cargos MUN

alienpercmun <- reactive({ ## Atributos das tabelas de alienacao absoluta 
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Alienação percentual" & 
     agregacao == "Município"){
    return(input$alien_perc_mun)
  }    
})

output$alien_perc_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
  balien_perc_mun()
})

balien_perc_mun <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'alien_perc_mun',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons', 
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Alienação percentual" & 
                        agregacao == "Município"){
                       if(municipio =="Todos os municípios"){
                         alien_mun %>% 
                           dplyr::filter(Cargo == input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno, 
                                         `Alienação percentual`) %>% 
                           spread(`Ano da eleição`,
                                  `Alienação percentual`) 
                       }else{
                         alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Alienação percentual`) %>% 
                           spread(`Ano da eleição`,
                                  `Alienação percentual`)}
                       
                     }
                   })
})  

## Dados desagregados

### Vereador 

ag_alienperc_mun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Alienação percentual" & 
     agregacao == "Município"){
    return(input$agreg_alien_perc_mun)
  }
})

output$agreg_alien_perc_mun <- DT::renderDataTable(server = TRUE,{
  bagreg_alien_perc_mun()
})

bagreg_alien_perc_mun <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'alien_perc_mun_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',  
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Alienação percentual" & 
                        agregacao == "Município"){
                       if(municipio == "Todos os municípios"){
                         data = alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`)
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3 & 
                                           `Nome do município2` == input$MUN3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`)
                       }}
                   })
})


## Resumo

### Eleitores aptos



alienpercmed <- reactive({ ## Atributos das tabelas de alienacao absoluta 
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Alienação percentual" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$alien_perc_med)
  }    
})

output$alien_perc_med <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
  balien_perc_med()
})

balien_perc_med <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', 
      targets = '_all')),
    dom = 't'), 
    class = "display",
    extensions = c('FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Alienação percentual" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       } else if(cargo == "Prefeito" &
                                 intervalo == "Acima de 200 mil eleitores"){
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional da alienação percentual`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da alienação percentual`) %>% 
                           mutate("media" = "Média nacional da alienação percentual") 
                         
                         media1 <- media1 %>% 
                           mutate("media" = ifelse(media1$Turno == 2,
                                                   "Média nacional da alienação percentual.",
                                                   media1$media)) %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da alienação percentual`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média da alienação percentual`) %>% 
                           mutate("media" = "Média da alienação percentual do grupo")
                         
                         media2 <- media2 %>% 
                           mutate("media" = ifelse(media2$Turno == 2,
                                                   "Média da alienação percentual do grupo.",
                                                   media2$media)) %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                       
                         } else{
                           
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo == input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional da alienação percentual`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da alienação percentual`) %>% 
                           mutate("media" = "Média nacional da alienação percentual") %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo == input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da alienação percentual`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média da alienação percentual`) %>% 
                           mutate("media" = "Média da alienação percentual do grupo") %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                         }
                     }
                   })
})


alienpercint <- reactive({ ## Atributos das tabelas de alienacao absoluta 
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Alienação percentual" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$alien_perc_int)
  }    
})

output$alien_perc_int <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
  balien_perc_int()
})

balien_perc_int <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'alien_perc_int',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Alienação percentual" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       }else{
                         alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Alienação percentual`) %>% 
                           spread(`Ano da eleição`,
                                  `Alienação percentual`)}
                       
                     }
                   })
})  

## Dados desagregados

### Eleitores aptos

ag_alienperc_int <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Alienação percentual" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$agreg_alien_perc_int)
  }
})

output$agreg_alien_perc_int <- DT::renderDataTable(server = FALSE,{
  bagreg_alien_perc_int()
})

bagreg_alien_perc_int <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'alien_perc_int_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Alienação percentual" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3 & 
                                         `Eleitores aptos` == input$INT3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`)
                       }}
                   })
})





# 2.3.3. Abstencao absoluta -------------------------------------------------------

## Tabela para visualizacao

### Cargos BR

abstabsbr <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  if(indicador == "Abstenção absoluta" & 
     agregacao == "Brasil"){
    return(input$abst_abs_br)
  }
})


output$abst_abs_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_abs_br()
})

babst_abs_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'abst_abs_br',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',   
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     if(indicador == "Abstenção absoluta" & 
                        agregacao == "Brasil"){
                       alien_br %>% 
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         dplyr::select(`Ano da eleição`,
                                       Turno, 
                                       `Quantidade de abstenções`) %>% 
                         spread(`Ano da eleição`,
                                `Quantidade de abstenções`)
                     }
                   })
}) 

## Dados desagregados

### Cargos BR

ag_abstabs_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  if(indicador == "Abstenção absoluta" & 
     agregacao == "Brasil"){
    return(input$agreg_abst_abs_br)
  }
})

output$agreg_abst_abs_br <- DT::renderDataTable(server = FALSE,{
  bagreg_abst_abs_br()
})

bagreg_abst_abs_br <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'abst_abs_br_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Abstenção absoluta" & 
                        agregacao == "Brasil"){
                       alien_br %>%
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         unique()
                       
                       
                     }
                   })
})


## Tabela para visualizacao

### Cargos UF

abstabsuf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Abstenção absoluta" & 
     agregacao == "UF"){
    return(input$abst_abs_uf)
  }
})


output$abst_abs_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_abs_uf()
})

babst_abs_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'abst_abs_uf',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Abstenção absoluta" & 
                        agregacao == "UF"){
                       if(uf=="Todas UFs"){
                         alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Cargo,
                                         Turno,
                                         `Quantidade de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de abstenções`)
                       }
                       else{
                         alien_uf %>% 
                           dplyr::filter(UF == input$UF3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Turno,
                                         `Quantidade de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de abstenções`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos UF

ag_abstabs_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Abstenção absoluta" & 
     agregacao == "UF"){
    return(input$agreg_abst_abs_uf)
  }
})

output$agreg_abst_abs_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_abst_abs_uf()
})

bagreg_abst_abs_uf <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'abst_abs_uf_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Abstenção absoluta" & 
                        agregacao == "UF"){
                       if(input$UF3 == "Todas UFs"){
                         data =alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       } else{ 
                         data = alien_uf %>% 
                           dplyr::filter(UF == input$UF3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       }}
                   })
})



## Tabela para visualizacao

### Cargos MUN

abstabsmun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Abstenção absoluta" & 
     agregacao == "Município"){
    return(input$abst_abs_mun)
  }
})


output$abst_abs_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_abs_mun()
})

babst_abs_mun <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'abst_abs_mun',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Abstenção absoluta" & 
                        agregacao == "Município"){
                       if(municipio =="Todos os municípios"){
                         alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Cargo,
                                         Turno,
                                         `Quantidade de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de abstenções`)
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Quantidade de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de abstenções`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos MUN

ag_abstabs_mun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Abstenção absoluta" & 
     agregacao == "Município"){
    return(input$agreg_abst_abs_mun)
  }
})

output$agreg_abst_abs_mun <- DT::renderDataTable(server = TRUE,{
  bagreg_abst_abs_mun()
})

bagreg_abst_abs_mun <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'abst_abs_mun_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Abstenção absoluta" & 
                        agregacao == "Município"){
                       if(municipio == "Todos os municípios"){
                         data = alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})


## Tabela para visualizacao

### Eleitores aptos


abstabsmed <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Abstenção absoluta" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$abst_abs_med)
  }
})


output$abst_abs_med <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_abs_med()
})

babst_abs_med <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 't'), 
    class = "display",
    extensions = c('FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Abstenção absoluta" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       } else if(cargo == "Prefeito" &
                                 intervalo == "Acima de 200 mil eleitores"){
                         alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo == input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da quantidade de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Média da quantidade de abstenções`)
                       } else if(cargo == "Prefeito" &
                                 intervalo == "Acima de 200 mil eleitores"){ 
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo == input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional da quantidade de abstenções`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da quantidade de abstenções`) %>% 
                           mutate("media" = "Média nacional da quantidade de abstenções") 
                         
                         media1 <- media1 %>% 
                           mutate("media" = ifelse(media1$Turno == 2,
                                                   "Média nacional da quantidade de abstenções.",
                                                   media1$media)) %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo == input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da quantidade de abstenções`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média da quantidade de abstenções`) %>% 
                           mutate("media" = "Média da quantidade de abstenções do grupo") 
                         
                         media2 <- media2 %>% 
                           mutate("media" = ifelse(media2$Turno == 2,
                                                   "Média da quantidade de abstenções do grupo.",
                                                   media2$media)) %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                       } else{
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo == input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional da quantidade de abstenções`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da quantidade de abstenções`) %>% 
                           mutate("media" = "Média nacional da quantidade de abstenções") %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo == input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da quantidade de abstenções`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média da quantidade de abstenções`) %>% 
                           mutate("media" = "Média da quantidade de abstenções do grupo") %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                         }
                     }
                   })
})


abstabsint <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Abstenção absoluta" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$abst_abs_int)
  }
})


output$abst_abs_int <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_abs_int()
})

babst_abs_int <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'abst_abs_int',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Abstenção absoluta" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                         Cargo == input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Quantidade de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de abstenções`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos MUN

ag_abstabs_int <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Abstenção absoluta" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$agreg_abst_abs_int)
  }
})

output$agreg_abst_abs_int <- DT::renderDataTable(server = FALSE,{
  bagreg_abst_abs_int()
})

bagreg_abst_abs_int <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'abst_abs_int_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Abstenção absoluta" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})



# 2.3.4. Abstencao percentual ---------------------------------------------


## Tabela para visualizacao

### Cargos BR

abstpercbr <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  if(indicador == "Abstenção percentual" & 
     agregacao == "Brasil"){
    return(input$abst_perc_br)
  }
})


output$abst_perc_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_perc_br()
})

babst_perc_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'abst_perc_br',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',   
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     if(indicador == "Abstenção percentual" & 
                        agregacao == "Brasil"){
                       alien_br %>% 
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         dplyr::select(`Ano da eleição`,
                                       Turno, 
                                       `Percentual de abstenções`) %>% 
                         spread(`Ano da eleição`,
                                `Percentual de abstenções`)
                     }
                   })
}) 

## Dados desagregados

### Cargos BR

ag_abstperc_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  if(indicador == "Abstenção percentual" & 
     agregacao == "Brasil"){
    return(input$agreg_abst_perc_br)
  }
})

output$agreg_abst_perc_br <- DT::renderDataTable(server = FALSE,{
  bagreg_abst_perc_br()
})

bagreg_abst_perc_br <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'abst_perc_br_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Abstenção percentual" & 
                        agregacao == "Brasil"){
                       alien_br %>%
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         unique()
                     }
                   })
})


## Tabela para visualizacao

### Cargos UF

abstpercuf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Abstenção percentual" & 
     agregacao == "UF"){
    return(input$abst_perc_uf)
  }
})


output$abst_perc_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_perc_uf()
})

babst_perc_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'abst_perc_uf',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Abstenção percentual" & 
                        agregacao == "UF"){
                       if(uf=="Todas UFs"){
                         alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Cargo,
                                         Turno,
                                         `Percentual de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de abstenções`)
                       }
                       else{
                         alien_uf %>% 
                           dplyr::filter(UF == input$UF3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Turno,
                                         `Percentual de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de abstenções`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos UF

ag_abstperc_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Abstenção percentual" & 
     agregacao == "UF"){
    return(input$agreg_abst_perc_uf)
  }
})

output$agreg_abst_perc_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_abst_perc_uf()
})

bagreg_abst_perc_uf <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'abst_perc_uf_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Abstenção percentual" & 
                        agregacao == "UF"){
                       if(input$UF3 == "Todas UFs"){
                         data =alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       } else{ 
                         data = alien_uf %>% 
                           dplyr::filter(UF == input$UF3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       }}
                   })
})


## Tabela para visualizacao

### Cargos MUN

abstpercmun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Abstenção percentual" & 
     agregacao == "Município"){
    return(input$abst_perc_mun)
  }
})


output$abst_perc_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_perc_mun()
})

babst_perc_mun <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'abst_perc_mun',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Abstenção percentual" & 
                        agregacao == "Município"){
                       if(municipio =="Todos os municípios"){
                         alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Cargo,
                                         Turno,
                                         `Percentual de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de abstenções`)
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Percentual de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de abstenções`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos MUN

ag_abstperc_mun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Abstenção percentual" & 
     agregacao == "Município"){
    return(input$agreg_abst_perc_mun)
  }
})

output$agreg_abst_perc_mun <- DT::renderDataTable(server = TRUE,{
  bagreg_abst_perc_mun()
})

bagreg_abst_perc_mun <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'abst_perc_mun_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Abstenção percentual" & 
                        agregacao == "Município"){
                       if(municipio == "Todos os municípios"){
                         data = alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})



## Tabela para visualizacao

### Eleitores aptos

abstpercmed <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Abstenção percentual" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$abst_perc_med)
  }
})


output$abst_perc_med <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_perc_med()
})

babst_perc_med <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 't'), 
    class = "display",
    extensions = c('FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Abstenção percentual" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       } else if(cargo == "Prefeito" &
                                 intervalo == "Acima de 200 mil eleitores"){ 
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional do percentual de abstenções`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional do percentual de abstenções`) %>% 
                           mutate("media" = "Média nacional do percentual de abstenções")
                         
                         media1 <- media1 %>% 
                           mutate("media" = ifelse(media1$Turno == 2,
                                                   "Média nacional do percentual de abstenções.",
                                                   media1$media)) %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média do percentual de abstenções`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média do percentual de abstenções`) %>% 
                           mutate("media" = "Média do percentual de abstenções do grupo")
                         
                         media2 <- media2 %>% 
                           mutate("media" = ifelse(media2$Turno == 2,
                                                   "Média do percentual de abstenções do grupo.",
                                                   media2$media)) %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                       } else{
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional do percentual de abstenções`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional do percentual de abstenções`) %>% 
                           mutate("media" = "Média nacional do percentual de abstenções") %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média do percentual de abstenções`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média do percentual de abstenções`) %>% 
                           mutate("media" = "Média do percentual de abstenções do grupo") %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                         }
                     }
                   })
})



abstpercint <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Abstenção percentual" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$abst_perc_int)
  }
})


output$abst_perc_int <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  babst_perc_int()
})

babst_perc_int <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'abst_perc_int',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                   
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Abstenção percentual" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Percentual de abstenções`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de abstenções`)}
                       
                     }
                   })
})

## Dados desagregados

### Eleitores aptos

ag_abstperc_int <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Abstenção percentual" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$agreg_abst_perc_int)
  }
})

output$agreg_abst_perc_int <- DT::renderDataTable(server = FALSE,{
  bagreg_abst_perc_int()
})

bagreg_abst_perc_int <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'abst_perc_int_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Abstenção percentual" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 &
                                          Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})

# 2.3.5. Votos brancos absolutos ----------------------------------------------------

## Tabela para visualizacao

### Cargos BR

vtbrabsbr <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  if(indicador == "Votos brancos absolutos" & 
     agregacao == "Brasil"){
    return(input$vtbr_abs_br)
  }
})


output$vtbr_abs_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_abs_br()
})

bvtbr_abs_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtbr_abs_br',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',   
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     if(indicador == "Votos brancos absolutos" & 
                        agregacao == "Brasil"){
                       alien_br %>% 
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         dplyr::select(`Ano da eleição`,
                                       Turno, 
                                       `Quantidade de votos brancos`) %>% 
                         spread(`Ano da eleição`,
                                `Quantidade de votos brancos`)
                       
                     }
                   })
}) 

## Dados desagregados

### Cargos BR

ag_vtbrabs_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  if(indicador == "Votos brancos absolutos" & 
     agregacao == "Brasil"){
    return(input$agreg_vtbr_abs_br)
  }
})

output$agreg_vtbr_abs_br <- DT::renderDataTable(server = FALSE,{
  bagreg_vtbr_abs_br()
})

bagreg_vtbr_abs_br <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtbr_abs_br_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos brancos absolutos" & 
                        agregacao == "Brasil"){
                       alien_br %>%
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         unique()
                       
                       
                     }
                   })
})


## Tabela para visualizacao

### Cargos UF

vtbrabsuf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos brancos absolutos" & 
     agregacao == "UF"){
    return(input$vtbr_abs_uf)
  }
})


output$vtbr_abs_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_abs_uf()
})

bvtbr_abs_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtbr_abs_uf',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos brancos absolutos" & 
                        agregacao == "UF"){
                       if(uf=="Todas UFs"){
                         alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Cargo,
                                         Turno,
                                         `Quantidade de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos brancos`)
                       }
                       else{
                         alien_uf %>% 
                           dplyr::filter(UF == input$UF3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Turno,
                                         `Quantidade de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos brancos`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos UF

ag_vtbrabs_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos brancos absolutos" & 
     agregacao == "UF"){
    return(input$agreg_vtbr_abs_uf)
  }
})

output$agreg_vtbr_abs_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_vtbr_abs_uf()
})

bagreg_vtbr_abs_uf <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtbr_abs_uf_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos brancos absolutos" & 
                        agregacao == "UF"){
                       if(input$UF3 == "Todas UFs"){
                         data =alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                          unique()
                       } else{ 
                         data = alien_uf %>% 
                           dplyr::filter(UF == input$UF3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       }}
                   })
})

## Tabela para visualizacao

### Cargos MUN

vtbrabsmun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos brancos absolutos" & 
     agregacao == "Município"){
    return(input$vtbr_abs_mun)
  }
})


output$vtbr_abs_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_abs_mun()
})

bvtbr_abs_mun <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtbr_abs_mun',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',           
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Votos brancos absolutos" & 
                        agregacao == "Município"){
                       if(municipio =="Todos os municípios"){
                         alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Cargo,
                                         Turno,
                                         `Quantidade de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos brancos`)
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Quantidade de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos brancos`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos MUN

ag_vtbrabs_mun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos brancos absolutos" & 
     agregacao == "Município"){
    return(input$agreg_vtbr_abs_mun)
  }
})

output$agreg_vtbr_abs_mun <- DT::renderDataTable(server = TRUE,{
  bagreg_vtbr_abs_mun()
})

bagreg_vtbr_abs_mun <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtbr_abs_mun_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Votos brancos absolutos" & 
                        agregacao == "Município"){
                       if(municipio == "Todos os municípios"){
                         data = alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})



## Tabela para visualizacao

### Eleitores aptos


vtbrabsmed <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos brancos absolutos" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vtbr_abs_med)
  }
})


output$vtbr_abs_med <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_abs_med()
})

bvtbr_abs_med <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 't'), 
    class = "display",
    rownames = FALSE,
    extensions = c('FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos brancos absolutos" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       } else if(cargo == "Prefeito" &
                                 intervalo == "Acima de 200 mil eleitores"){ 
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional da quantidade de votos brancos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da quantidade de votos brancos`) %>% 
                           mutate("media" = "Média nacional da quantidade de votos brancos") 
                         
                         media1 <- media1 %>% 
                           mutate("media" = ifelse(media1$Turno == 2,
                                                   "Média nacional da quantidade de votos brancos.",
                                                   media1$media)) %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da quantidade de votos brancos`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média da quantidade de votos brancos`) %>% 
                           mutate("media" = "Média da quantidade de votos brancos do grupo")
                         
                         media2 <- media2 %>% 
                           mutate("media" = ifelse(media2$Turno == 2,
                                                   "Média da quantidade de votos brancos do grupo.",
                                                   media2$media)) %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                       } else{
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional da quantidade de votos brancos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da quantidade de votos brancos`) %>% 
                           mutate("media" = "Média nacional da quantidade de votos brancos") %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da quantidade de votos brancos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média da quantidade de votos brancos`) %>% 
                           mutate("media" = "Média da quantidade de votos brancos do grupo") %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                         }
                     }
                   })
})


vtbrabsint <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos brancos absolutos" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vtbr_abs_int)
  }
})


output$vtbr_abs_int <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_abs_int()
})

bvtbr_abs_int <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtbr_abs_int',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',           
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos brancos absolutos" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                        return()
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                         Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Quantidade de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos brancos`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos INT

ag_vtbrabs_int <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos brancos absolutos" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$agreg_vtbr_abs_int)
  }
})

output$agreg_vtbr_abs_int <- DT::renderDataTable(server = FALSE,{
  bagreg_vtbr_abs_int()
})

bagreg_vtbr_abs_int <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtbr_abs_int_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos brancos absolutos" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})






# 2.3.6. Votos brancos percentuais ----------------------------------------

## Tabela para visualizacao

### Cargos BR

vtbrpercbr <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  if(indicador == "Votos brancos percentuais" & 
     agregacao == "Brasil"){
    return(input$vtbr_perc_br)
  }
})


output$vtbr_perc_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_perc_br()
})

bvtbr_perc_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtbr_perc_br',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',   
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     if(indicador == "Votos brancos percentuais" & 
                        agregacao == "Brasil"){
                       alien_br %>% 
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         dplyr::select(`Ano da eleição`,
                                       Turno, 
                                       `Percentual de votos brancos`) %>% 
                         spread(`Ano da eleição`,
                                `Percentual de votos brancos`)
                       
                     }
                   })
}) 

## Dados desagregados

### Cargos BR

ag_vtbrperc_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  if(indicador == "Votos brancos percentuais" & 
     agregacao == "Brasil"){
    return(input$agreg_vtbr_perc_br)
  }
})

output$agreg_vtbr_perc_br <- DT::renderDataTable(server = FALSE,{
  bagreg_vtbr_perc_br()
})

bagreg_vtbr_perc_br <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtbr_perc_br_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos brancos percentuais" & 
                        agregacao == "Brasil"){
                       alien_br %>%
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         unique()
                       
                       
                     }
                   })
})


## Tabela para visualizacao

### Cargos UF

vtbrpercuf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos brancos percentuais" & 
     agregacao == "UF"){
    return(input$vtbr_perc_uf)
  }
})


output$vtbr_perc_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_perc_uf()
})

bvtbr_perc_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtbr_perc_uf',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos brancos percentuais" & 
                        agregacao == "UF"){
                       if(uf=="Todas UFs"){
                         alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Cargo,
                                         Turno,
                                         `Percentual de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos brancos`)
                       }
                       else{
                         alien_uf %>% 
                           dplyr::filter(UF == input$UF3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Turno,
                                         `Percentual de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos brancos`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos UF

ag_vtbrperc_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos brancos percentuais" & 
     agregacao == "UF"){
    return(input$agreg_vtbr_perc_uf)
  }
})

output$agreg_vtbr_perc_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_vtbr_perc_uf()
})

bagreg_vtbr_perc_uf <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtbr_perc_uf_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos brancos percentuais" & 
                        agregacao == "UF"){
                       if(input$UF3 == "Todas UFs"){
                         data =alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       } else{ 
                         data = alien_uf %>% 
                           dplyr::filter(UF == input$UF3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       }}
                   })
})


## Tabela para visualizacao

### Cargos MUN

vtbrpercmun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos brancos percentuais" & 
     agregacao == "Município"){
    return(input$vtbr_perc_mun)
  }
})


output$vtbr_perc_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_perc_mun()
})

bvtbr_perc_mun <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtbr_perc_mun',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Votos brancos percentuais" & 
                        agregacao == "Município"){
                       if(municipio =="Todos os municípios"){
                         alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Cargo,
                                         Turno,
                                         `Percentual de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos brancos`)
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Percentual de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos brancos`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos MUN

ag_vtbrperc_mun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos brancos percentuais" & 
     agregacao == "Município"){
    return(input$agreg_vtbr_perc_mun)
  }
})

output$agreg_vtbr_perc_mun <- DT::renderDataTable(server = TRUE,{
  bagreg_vtbr_perc_mun()
})

bagreg_vtbr_perc_mun <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtbr_perc_mun_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Votos brancos percentuais" & 
                        agregacao == "Município"){
                       if(municipio == "Todos os municípios"){
                         data = alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})


## Tabela para visualizacao

### Eleitores aptos


vtbrpercmed <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos brancos percentuais" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vtbr_perc_med)
  }
})


output$vtbr_perc_med <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_perc_med()
})

bvtbr_perc_med <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 't'), 
    class = "display",
    extensions = c('FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos brancos percentuais" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else if(cargo == "Prefeito" &
                                 intervalo == "Acima de 200 mil eleitores"){ 
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional do percentual de votos brancos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional do percentual de votos brancos`) %>% 
                           mutate("media" = "Média nacional do percentual de votos brancos") 
                         
                         media1 <- media1 %>% 
                           mutate("media" = ifelse(media1$Turno == 2,
                                                   "Média nacional do percentual de votos brancos.",
                                                   media1$media)) %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média do percentual de votos brancos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média do percentual de votos brancos`) %>% 
                           mutate("media" = "Média do percentual de votos brancos do grupo") 
                         
                         media2 <- media2 %>% 
                           mutate("media" = ifelse(media2$Turno == 2,
                                                   "Média do percentual de votos brancos do grupo.",
                                                   media2$media)) %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                         
                       } else{
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional do percentual de votos brancos`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional do percentual de votos brancos`) %>% 
                           mutate("media" = "Média nacional do percentual de votos brancos") %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média do percentual de votos brancos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média do percentual de votos brancos`) %>% 
                           mutate("media" = "Média do percentual de votos brancos do grupo") %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                         }
                     }
                   })
})



vtbrpercint <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos brancos percentuais" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vtbr_perc_int)
  }
})


output$vtbr_perc_int <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtbr_perc_int()
})

bvtbr_perc_int <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtbr_perc_int',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos brancos percentuais" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Percentual de votos brancos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos brancos`)}
                       
                     }
                   })
})

## Dados desagregados

### Eleitores aptos

ag_vtbrperc_int <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos brancos percentuais" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$agreg_vtbr_perc_int)
  }
})

output$agreg_vtbr_perc_int <- DT::renderDataTable(server = FALSE,{
  bagreg_vtbr_perc_int()
})

bagreg_vtbr_perc_int <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtbr_perc_int_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos brancos percentuais" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})



# 2.3.7. Votos nulos absolutos ------------------------------------------------------


## Tabela para visualizacao

### Cargos BR

vtnlabsbr <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  if(indicador == "Votos nulos absolutos" & 
     agregacao == "Brasil"){
    return(input$vtnl_abs_br)
  }
})


output$vtnl_abs_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_abs_br()
})

bvtnl_abs_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtnl_abs_br',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',   
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     if(indicador == "Votos nulos absolutos" & 
                        agregacao == "Brasil"){
                       alien_br %>% 
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         dplyr::select(`Ano da eleição`,
                                       Turno, 
                                       `Quantidade de votos nulos`) %>% 
                         spread(`Ano da eleição`,
                                `Quantidade de votos nulos`)
                       
                     }
                   })
}) 

## Dados desagregados

### Cargos BR

ag_vtnlabs_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  if(indicador == "Votos nulos absolutos" & 
     agregacao == "Brasil"){
    return(input$agreg_vtnl_abs_br)
  }
})

output$agreg_vtnl_abs_br <- DT::renderDataTable(server = FALSE,{
  bagreg_vtnl_abs_br()
})

bagreg_vtnl_abs_br <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtnl_abs_br_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos nulos absolutos" & 
                        agregacao == "Brasil"){
                       alien_br %>%
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         unique()
                       
                       
                     }
                   })
})


## Tabela para visualizacao

### Cargos UF

vtnlabsuf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos nulos absolutos" & 
     agregacao == "UF"){
    return(input$vtnl_abs_uf)
  }
})


output$vtnl_abs_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_abs_uf()
})

bvtnl_abs_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtnl_abs_uf',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos nulos absolutos" & 
                        agregacao == "UF"){
                       if(uf=="Todas UFs"){
                         alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Cargo,
                                         Turno,
                                         `Quantidade de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos nulos`)
                       }
                       else{
                         alien_uf %>% 
                           dplyr::filter(UF == input$UF3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Turno,
                                         `Quantidade de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos nulos`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos UF

ag_vtnlabs_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos nulos absolutos" & 
     agregacao == "UF"){
    return(input$agreg_vtnl_abs_uf)
  }
})

output$agreg_vtnl_abs_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_vtnl_abs_uf()
})

bagreg_vtnl_abs_uf <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtnl_abs_uf_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos nulos absolutos" & 
                        agregacao == "UF"){
                       if(input$UF3 == "Todas UFs"){
                         alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                          unique()
                       } else{ 
                         data = alien_uf %>% 
                           dplyr::filter(UF == input$UF3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       }}
                   })
})


## Tabela para visualizacao

### Cargos MUN

vtnlabsmun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos nulos absolutos" & 
     agregacao == "Município"){
    return(input$vtnl_abs_mun)
  }
})


output$vtnl_abs_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_abs_mun()
})

bvtnl_abs_mun <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtnl_abs_mun',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Votos nulos absolutos" & 
                        agregacao == "Município"){
                       if(municipio =="Todos os municípios"){
                         alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Cargo,
                                         Turno,
                                         `Quantidade de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos nulos`)
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Quantidade de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos nulos`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos MUN

ag_vtnlabs_mun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos nulos absolutos" & 
     agregacao == "Município"){
    return(input$agreg_vtnl_abs_mun)
  }
})

output$agreg_vtnl_abs_mun <- DT::renderDataTable(server = TRUE,{
  bagreg_vtnl_abs_mun()
})

bagreg_vtnl_abs_mun <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtnl_abs_mun_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Votos nulos absolutos" & 
                        agregacao == "Município"){
                       if(municipio == "Todos os municípios"){
                         alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 &
                                          Cargo==input$DESCRICAO_CARGO3) %>%
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})


## Tabela para visualizacao

### Eleitores aptos

vtnlabsmed <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos nulos absolutos" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vtnl_abs_med)
  }
})


output$vtnl_abs_med <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_abs_med()
})

bvtnl_abs_med <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 't'), 
    class = "display",
    extensions = c('FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos nulos absolutos" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       } else if(cargo == "Prefeito" &
                                 intervalo == "Acima de 200 mil eleitores"){ 
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional da quantidade de votos nulos`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da quantidade de votos nulos`) %>% 
                           mutate("media" = "Média nacional da quantidade de votos nulos") 
                         
                         media1 <- media1 %>% 
                           mutate("media" = ifelse(media1$Turno == 2,
                                                   "Média nacional da quantidade de votos nulos.",
                                                   media1$media)) %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da quantidade de votos nulos`) %>% 
                           unique()
                           spread(`Ano da eleição`,
                                  `Média da quantidade de votos nulos`) %>% 
                           mutate("media" = "Média da quantidade de votos nulos do grupo") 
                         
                         media2 <- media2 %>% 
                           mutate("media" = ifelse(media2$Turno == 2,
                                                   "Média da quantidade de votos nulos do grupo.",
                                                   media2$media)) %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                         
                       } else{
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional da quantidade de votos nulos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da quantidade de votos nulos`) %>% 
                           mutate("media" = "Média nacional da quantidade de votos nulos") %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da quantidade de votos nulos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média da quantidade de votos nulos`) %>% 
                           mutate("media" = "Média da quantidade de votos nulos do grupo") %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                         }
                     }
                   })
})


vtnlabsint <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos nulos absolutos" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vtnl_abs_int)
  }
})


output$vtnl_abs_int <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_abs_int()
})

bvtnl_abs_int <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtnl_abs_int',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos nulos absolutos" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                        return()
                       } else{
                         alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Quantidade de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Quantidade de votos nulos`)}
                       
                     }
                   })
})

## Dados desagregados

### Eleitores aptos

ag_vtnlabs_int <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos nulos absolutos" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$agreg_vtnl_abs_int)
  }
})

output$agreg_vtnl_abs_int <- DT::renderDataTable(server = FALSE,{
  bagreg_vtnl_abs_int()
})

bagreg_vtnl_abs_int <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtnl_abs_int_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos nulos absolutos" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>%
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})




# 2.3.8. Votos nulos percentuais ------------------------------------------

## Tabela para visualizacao

### Cargos BR

vtnlpercbr <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  if(indicador == "Votos nulos percentuais" & 
     agregacao == "Brasil"){
    return(input$vtnl_perc_br)
  }
})


output$vtnl_perc_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_perc_br()
})

bvtnl_perc_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtnl_perc_br',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',   
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     if(indicador == "Votos nulos percentuais" & 
                        agregacao == "Brasil"){
                       alien_br %>% 
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         dplyr::select(`Ano da eleição`,
                                       Turno, 
                                       `Percentual de votos nulos`) %>% 
                         spread(`Ano da eleição`,
                                `Percentual de votos nulos`)
                       
                     }
                   })
}) 

## Dados desagregados

### Cargos BR

ag_vtnlperc_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  if(indicador == "Votos nulos percentuais" & 
     agregacao == "Brasil"){
    return(input$agreg_vtnl_perc_br)
  }
})

output$agreg_vtnl_perc_br <- DT::renderDataTable(server = FALSE,{
  bagreg_vtnl_perc_br()
})

bagreg_vtnl_perc_br <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtnl_perc_br_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos nulos percentuais" & 
                        agregacao == "Brasil"){
                       alien_br %>%
                         dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                         unique()
                       
                       
                     }
                   })
})


## Tabela para visualizacao

### Cargos UF

vtnlpercuf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos nulos percentuais" & 
     agregacao == "UF"){
    return(input$vtnl_perc_uf)
  }
})


output$vtnl_perc_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_perc_uf()
})

bvtnl_perc_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtnl_perc_uf',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos nulos percentuais" & 
                        agregacao == "UF"){
                       if(uf=="Todas UFs"){
                         alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Cargo,
                                         Turno,
                                         `Percentual de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos nulos`)
                       }
                       else{
                         alien_uf %>% 
                           dplyr::filter(UF == input$UF3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         Turno,
                                         `Percentual de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos nulos`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos UF

ag_vtnlperc_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos nulos percentuais" & 
     agregacao == "UF"){
    return(input$agreg_vtnl_perc_uf)
  }
})

output$agreg_vtnl_perc_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_vtnl_perc_uf()
})

bagreg_vtnl_perc_uf <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtnl_perc_uf_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     uf <- input$UF3
                     if(indicador == "Votos nulos percentuais" & 
                        agregacao == "UF"){
                       if(input$UF3 == "Todas UFs"){
                         alien_uf %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       } else{ 
                         data = alien_uf %>% 
                           dplyr::filter(UF == input$UF3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           unique()
                       }}
                   })
})

## Tabela para visualizacao

### Cargos MUN

vtnlpercmun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos nulos percentuais" & 
     agregacao == "Município"){
    return(input$vtnl_perc_mun)
  }
})


output$vtnl_perc_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_perc_mun()
})

bvtnl_perc_mun <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtnl_perc_mun',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Votos nulos percentuais" & 
                        agregacao == "Município"){
                       if(municipio =="Todos os municípios"){
                         alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Cargo,
                                         Turno,
                                         `Percentual de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos nulos`)
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Percentual de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos nulos`)}
                       
                     }
                   })
})

## Dados desagregados

### Cargos MUN

ag_vtnlperc_mun <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos nulos percentuais" & 
     agregacao == "Município"){
    return(input$agreg_vtnl_perc_mun)
  }
})

output$agreg_vtnl_perc_mun <- DT::renderDataTable(server = TRUE,{
  bagreg_vtnl_perc_mun()
})

bagreg_vtnl_perc_mun <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtnl_perc_mun_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     municipio <- input$MUN3
                     if(indicador == "Votos nulos percentuais" & 
                        agregacao == "Município"){
                       if(municipio == "Todos os municípios"){
                         alien_mun %>% 
                           dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Nome do município2` == input$MUN3 &
                                         Cargo==input$DESCRICAO_CARGO3) %>%
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})


## Tabela para visualizacao

### Eleitores aptos

vtnlpercmed <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos nulos percentuais" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vtnl_perc_med)
  }
})


output$vtnl_perc_med <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_perc_med()
})

bvtnl_perc_med <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 't'), 
    class = "display",
    extensions = c('FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos nulos percentuais" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       } else if(cargo == "Prefeito" &
                                intervalo == "Acima de 200 mil eleitores"){
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional do percentual de votos nulos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional do percentual de votos nulos`) %>% 
                           mutate("media" = "Média nacional do percentual de votos nulos")
                         
                         media1 <- media1 %>% 
                           mutate("media" = ifelse(media1$Turno == 2,
                                                   "Média nacional do percentual de votos nulos.",
                                                   media1$media)) %>% 
                           column_to_rownames("media")
                         
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média do percentual de votos nulos`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média do percentual de votos nulos`) %>% 
                           mutate("media" = "Média do percentual de votos nulos do grupo")
                         
                         media2 <- media2 %>% 
                           mutate("media" = ifelse(media2$Turno == 2,
                                                   "Média do percentual de votos nulos do grupo.",
                                                   media2$media)) %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                       } else{
                         
                         media1 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional do percentual de votos nulos`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional do percentual de votos nulos`) %>% 
                           mutate("media" = "Média nacional do percentual de votos nulos") %>% 
                           column_to_rownames("media")
                         
                         media2 <- alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média do percentual de votos nulos`) %>%
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média do percentual de votos nulos`) %>% 
                           mutate("media" = "Média do percentual de votos nulos do grupo") %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                         }
                     }
                   })
})


vtnlpercint <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Votos nulos percentuais" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vtnl_perc_int)
  }
})


output$vtnl_perc_int <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  bvtnl_perc_int()
})

bvtnl_perc_int <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 2
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vtnl_perc_int',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',                              
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos nulos percentuais" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo ==""){
                         return()
                       }
                       else{
                         alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 & 
                                           Cargo==input$DESCRICAO_CARGO3) %>% 
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         Turno,
                                         `Percentual de votos nulos`) %>% 
                           spread(`Ano da eleição`,
                                  `Percentual de votos nulos`)}
                       
                     }
                   })
})

## Dados desagregados

### Eleitores aptos

ag_vtnlperc_int <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Votos nulos percentuais" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$agreg_vtnl_perc_int)
  }
})

output$agreg_vtnl_perc_int <- DT::renderDataTable(server = FALSE,{
  bagreg_vtnl_perc_int()
})

bagreg_vtnl_perc_int <- eventReactive(input$BCALC3, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vtnl_perc_int_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_ALIE
                     cargo <- input$DESCRICAO_CARGO3
                     agregacao <- input$AGREGACAO_REGIONAL3
                     intervalo <- input$INT3
                     if(indicador == "Votos nulos percentuais" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                        return()
                       } else{ 
                         data = alien_mun %>% 
                           dplyr::filter(`Eleitores aptos` == input$INT3 &
                                           Cargo==input$DESCRICAO_CARGO3) %>%
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da quantidade de abstenções`,
                                  -`Média do percentual de abstenções`,
                                  -`Média da quantidade de votos brancos`,
                                  -`Média do percentual de votos brancos`,
                                  -`Média da quantidade de votos nulos`,
                                  -`Média do percentual de votos nulos`,
                                  -`Média da alienação absoluta`,
                                  -`Média da alienação percentual`,
                                  -`Média nacional da quantidade de abstenções`,
                                  -`Média nacional do percentual de abstenções`,
                                  -`Média nacional da quantidade de votos brancos`,
                                  -`Média nacional do percentual de votos brancos`,
                                  -`Média nacional da quantidade de votos nulos`,
                                  -`Média nacional do percentual de votos nulos`,
                                  -`Média nacional da alienação absoluta`,
                                  -`Média nacional da alienação percentual`) %>% 
                           unique()
                       }}
                   })
})




# 2.3. Volatilidade -------------------------------------------------------


## Modal para ajuda

### Resumo

observeEvent(input$modal_vol,{
  showModal(modalDialog(
    title = h4(class = "h4 titulo",
                    "AJUDA"),
    footer = modalButton("FECHAR"), 
    size = "m",
    htmlOutput("def_vol"),
    easyClose = TRUE,
    style = "
    overflow: hidden;
    overflow-y: scroll;
    flex: 1 1 auto;
    padding: 1rem;
    max-width: 850px;
    margin: 1.75rem auto;
    max-height: 500px;
    display: flex;
    width: auto;
    "))
  })
  
  ### Dados desagregados
  
  observeEvent(input$modal_vol_ag,{
    showModal(modalDialog(
                          title = h4(class = "h4 titulo",
                                          "AJUDA"),
                          footer = modalButton("FECHAR"), 
                          size = "m",
                          htmlOutput("def_vol"),
                          easyClose = TRUE,
                          style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "
                          ))
  })

  
  output$def_vol <- renderUI({
    note <- paste0("
                   <font color = 'black'>
                   <h4><br /> Volatilidade </h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>A volatilidade é uma medida agregada que resulta do
                   somatório das perdas e ganhos dos partidos entre duas eleições, dividido por dois.
                   As perdas e ganhos dos partidos tanto podem ser expressas em proporções de
                   votos ou cadeiras no parlamento.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   <p><i>Volatilidade eleitoral </i></p>
                   V = &sum;|(%Vp (t) - %Vp (t-1))|/2,
                   <p>onde Vp = proporções de votos obtidos por cada partido entre as eleições 't' e
                   't-1'.</p>
                   <p><i>Volatilidade parlamentar </i></p>
                   V = &sum;|(%Cp (t) - %Cp (t-1))|/2,
                   <p>onde Cp = proporções de cadeiras obtidas por cada partido entre as eleições
                   't' e 't-1'.</p>
                   <p><br /></h5>
                   <strong>Fonte:</strong> 
                   <p>1. Votos e partidos: almanaque de dados eleitorais: Brasil e outros 
                   países/ Organização de Wanderley Guilherme dos Santos, com a colaboração de Fabrícia Guimarães. -
                   Rio de Janeiro: Editora FGV, 2002);  
                   <p>2. FIGUEIREDO, M. Volatilidade eleitoral em eleições parlamentares, 1950-1978.
                   Opinião Pública, Campinas, vol. III, nº 3, Dezembro, 1995, p.186-196.
                   <a href= 'https://www.cesop.unicamp.br/vw/1IEjOMDM_MDA_3e2e0_/v3n3a03.pdf'></a></font>
                   
                   ")
    HTML(note)
  }) 

# 2.4.1. Volatilidade eleitoral -------------------------------------------

## Resumo

### Volatilidade (Brasil)  

volelebr <- reactive({ ## Atributos das tabelas 
  indicador <- input$INDICADORES_VOL
  cargo <- input$DESCRICAO_CARGO4
  agregacao <- input$DESCRICAO_CARGO4
  if(indicador == "Volatilidade eleitoral" & 
     cargo == "Deputado Federal" &
     agregacao == "Brasil"){
    return(input$vol_ele_br)
  }
})


output$vol_ele_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
  bvol_ele_br()
})

bvol_ele_br <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vol_ele_br',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_VOL
                     cargo <- input$DESCRICAO_CARGO4
                     agregacao <- input$AGREGACAO_REGIONAL4
                     if(indicador == "Volatilidade eleitoral" &
                        cargo == "Deputado Federal" &
                        agregacao == "Brasil"){
                      vol_br %>% 
                         dplyr::select(`Ano da eleição`,
                                       `Volatilidade eleitoral`) %>% 
                         spread(`Ano da eleição`,
                                `Volatilidade eleitoral`) %>% 
                         unique()
                       
                     }
                   })
}) 

## Dados desagregados

### Volatilidade (Brasil)  

ag_volele_br <- reactive({
  indicador <- input$INDICADORES_VOL
  cargo <- input$DESCRICAO_CARGO4
  agregacao <- input$AGREGACAO_REGIONAL4
  if(indicador == "Volatilidade eleitoral" &
     cargo == "Deputado Federal" &
     agregacao == "Brasil"){
    return(input$agreg_vol_ele_br)
  }
})

output$agreg_vol_ele_br <- DT::renderDataTable(server = FALSE,{
  bagreg_vol_ele_br()
})

bagreg_vol_ele_br <- eventReactive(input$BCALC4, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_ele_br_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',        
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_VOL
                     cargo <- input$DESCRICAO_CARGO4
                     agregacao <- input$AGREGACAO_REGIONAL4
                     uf <- input$UF3
                     if(indicador == "Volatilidade eleitoral" & 
                        cargo == "Deputado Federal" &
                        agregacao == "Brasil"){
                       data = vol_br 
                       
                     }
                   })
})


## Resumo

### Volatilidade (UF) 

voleleuf <- reactive({ ## Atributos das tabelas 
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$DESCRICAO_CARGO4)
  if(indicador == "Volatilidade eleitoral" & 
     agregacao == "UF"){
    return(input$vol_ele_uf)
  }
})


output$vol_ele_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
  bvol_ele_uf()
})

bvol_ele_uf <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vol_ele_uf',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',              
                  
                   'FixedColumns'),{
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     uf <- req(input$UF4)
                     if(indicador == "Volatilidade eleitoral" &
                        agregacao == "UF"){
                       if(uf == ""){
                         return()
                       } else if(uf == "Todas UFs"){
                         vol_uf %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Volatilidade eleitoral`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade eleitoral`) %>% 
                           unique()
                       } else{
                         vol_uf %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                           UF == req(input$UF4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Volatilidade eleitoral`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade eleitoral`) %>% 
                           unique()
                       }
                     }
                   })
}) 

## Dados desagregados

### Volatilidade (UF) 

ag_volele_uf <- reactive({
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$AGREGACAO_REGIONAL4)
  if(indicador == "Volatilidade eleitoral" &
     agregacao == "UF"){
    return(input$agreg_vol_ele_uf)
  }
})

output$agreg_vol_ele_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_vol_ele_uf()
})

bagreg_vol_ele_uf <- eventReactive(input$BCALC4, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_ele_uf_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',        
                  
                   'FixedColumns'),{
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     uf <- req(input$UF4)
                     if(indicador == "Volatilidade eleitoral" & 
                        agregacao == "UF"){
                       if(uf == ""){
                         return()
                       } else if(uf == "Todas UFs"){
                         data = vol_uf %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4)) %>% 
                           unique()
                       } else{
                         data = vol_uf %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                    UF == req(input$UF4)) %>% 
                           unique()
                       }
                     }
                   })
})


## Resumo

### Volatilidade (MUN) 

volelemun <- reactive({ ## Atributos das tabelas 
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$DESCRICAO_CARGO4)
  if(indicador == "Volatilidade eleitoral" & 
     agregacao == "Município"){
    return(input$vol_ele_mun)
  }
})


output$vol_ele_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
  bvol_ele_mun()
})

bvol_ele_mun <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vol_ele_mun',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     cargo <- input$DESCRICAO_CARGO4
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     municipio <- req(input$MUN4)
                     if(cargo == "Vereador" &
                        indicador == "Volatilidade eleitoral" &
                        agregacao == "Município"){
                       if(municipio == ""){
                         return()
                       } else if(municipio == "Todos os municípios"){
                         vol_mun %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         `Volatilidade eleitoral`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade eleitoral`) %>% 
                           unique()
                       } else{
                         vol_mun %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                         `Nome do município2` == req(input$MUN4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         `Volatilidade eleitoral`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade eleitoral`) %>% 
                           unique()
                       }
                     } else if(cargo == "Prefeito" &
                               indicador == "Volatilidade eleitoral" &
                               agregacao == "Município"){
                       if(municipio == ""){
                         return()
                       } else if(municipio == "Todos os municípios"){
                         vol_pf %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         `Volatilidade eleitoral`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade eleitoral`) %>% 
                           unique()
                       } else{
                         vol_pf %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                         `Nome do município2` == req(input$MUN4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         `Volatilidade eleitoral`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade eleitoral`) %>% 
                           unique()
                       }
                     }
                   })
}) 

## Dados desagregados

### Volatilidade (MUN) 

ag_volele_mun <- reactive({
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$AGREGACAO_REGIONAL4)
  if(indicador == "Volatilidade eleitoral" &
     agregacao == "Município"){
    return(input$agreg_vol_ele_mun)
  }
})

output$agreg_vol_ele_mun <- DT::renderDataTable(server = TRUE,{
  bagreg_vol_ele_mun()
})

bagreg_vol_ele_mun <- eventReactive(input$BCALC4, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_ele_mun_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     cargo <- input$DESCRICAO_CARGO4
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     municipio <- req(input$MUN4)
                     if(cargo == "Vereador" &
                        indicador == "Volatilidade eleitoral" & 
                        agregacao == "Município"){
                       if(municipio == ""){
                         return()
                       } else if(municipio == "Todos os municípios"){
                         data = vol_mun %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4)) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da volatilidade eleitoral`,
                                  -`Média da volatilidade parlamentar`,
                                  -`Média nacional da volatilidade eleitoral`,
                                  -`Média nacional da volatilidade parlamentar`) %>% 
                           unique()
                       } else{
                         data = vol_mun %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                  `Nome do município2` == req(input$MUN4)) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da volatilidade eleitoral`,
                                  -`Média da volatilidade parlamentar`,
                                  -`Média nacional da volatilidade eleitoral`,
                                  -`Média nacional da volatilidade parlamentar`) %>% 
                           unique()
                       }
                     } else if(cargo == "Prefeito" &
                               indicador == "Volatilidade eleitoral" & 
                               agregacao == "Município"){
                       if(municipio == ""){
                         return()
                       } else if(municipio == "Todos os municípios"){
                         data = vol_pf %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4)) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da volatilidade eleitoral`,
                                  -`Média da volatilidade parlamentar`,
                                  -`Média nacional da volatilidade eleitoral`) %>% 
                           unique()
                       } else{
                         data = vol_pf %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                    `Nome do município2` == req(input$MUN4)) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da volatilidade eleitoral`,
                                  -`Média nacional da volatilidade eleitoral`) %>% 
                           unique()
                       }
                     }
                   })
})



## Resumo

### Volatilidade (Eleitores aptos) 


volelemed <- reactive({ ## Atributos das tabelas 
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$DESCRICAO_CARGO4)
  if(indicador == "Volatilidade eleitoral" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vol_ele_med)
  }
})


output$vol_ele_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
  bvol_ele_med()
})

bvol_ele_med <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 't'), 
    class = "display",
    extensions = c('FixedColumns'),{
                     cargo <- input$DESCRICAO_CARGO4
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     intervalo <- req(input$INT4)
                     if(cargo == "Vereador" &
                        indicador == "Volatilidade eleitoral" &
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{
                         
                         media1 <- vol_mun %>% 
                           ungroup() %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                           `Eleitores aptos` == req(input$INT4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         `Média nacional da volatilidade eleitoral`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da volatilidade eleitoral`) %>% 
                           mutate("media" = "Média nacional da volatilidade eleitoral") %>% 
                           column_to_rownames("media")
                         
                         media2 <- vol_mun %>% 
                           ungroup() %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                           `Eleitores aptos` == req(input$INT4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         `Média da volatilidade eleitoral`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média da volatilidade eleitoral`) %>% 
                           mutate("media" = "Média da volatilidade eleitoral do grupo") %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                       }
                     } else if(cargo == "Prefeito" &
                               indicador == "Volatilidade eleitoral" &
                               agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{
                         
                         media1 <- vol_pf %>% 
                           ungroup() %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                         `Eleitores aptos` == req(input$INT4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média nacional da volatilidade eleitoral`) %>% 
                           unique() %>%
                           spread(`Ano da eleição`,
                                  `Média nacional da volatilidade eleitoral`) %>% 
                           mutate("media" = "Média nacional da volatilidade eleitoral")
                         
                         media1 <- media1 %>% 
                           mutate("media" = ifelse(media1$Turno == 2,
                                                   "Média nacional da volatilidade eleitoral.",
                                                   media1$media)) %>% 
                           column_to_rownames("media")
                         
                         media2 <- vol_pf %>% 
                           ungroup() %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                           `Eleitores aptos` == req(input$INT4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         `Média da volatilidade eleitoral`) %>% 
                           unique() %>%
                           spread(`Ano da eleição`,
                                  `Média da volatilidade eleitoral`) %>% 
                           mutate("media" = "Média da volatilidade eleitoral do grupo") 
                         
                         media2 <- media2 %>% 
                           mutate("media" = ifelse(media2$Turno == 2,
                                                   "Média da volatilidade eleitoral do grupo.",
                                                   media2$media)) %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                       }
                     }
                   })
}) 

voleleint <- reactive({ ## Atributos das tabelas 
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$DESCRICAO_CARGO4)
  if(indicador == "Volatilidade eleitoral" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vol_ele_int)
  }
})


output$vol_ele_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
  bvol_ele_int()
})

bvol_ele_int <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vol_ele_int',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     cargo <- input$DESCRICAO_CARGO4
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     intervalo <- req(input$INT4)
                     if(cargo == "Vereador" &
                        indicador == "Volatilidade eleitoral" &
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{
                         vol_mun %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                           `Eleitores aptos` == req(input$INT4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         `Volatilidade eleitoral`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade eleitoral`) %>% 
                           unique()
                       }
                     } else if(cargo == "Prefeito" &
                               indicador == "Volatilidade eleitoral" &
                               agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{
                         vol_pf %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                           `Eleitores aptos` == req(input$INT4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         Turno,
                                         UF,
                                         `Nome do município`,
                                         `Volatilidade eleitoral`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade eleitoral`) %>% 
                           unique()
                       }
                     }
                   })
}) 

## Dados desagregados

### Volatilidade (Eleitores aptos) 

ag_volele_int <- reactive({
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$AGREGACAO_REGIONAL4)
  if(indicador == "Volatilidade eleitoral" &
     agregacao == "Quantidade de eleitores aptos"){
    return(input$agreg_vol_ele_int)
  }
})

output$agreg_vol_ele_int <- DT::renderDataTable(server = FALSE,{
  bagreg_vol_ele_int()
})

bagreg_vol_ele_int <- eventReactive(input$BCALC4, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_ele_int_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                   'FixedColumns'),{
                     cargo <- input$DESCRICAO_CARGO4
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     intervalo <- req(input$INT4)
                     if(cargo == "Vereador" &
                        indicador == "Volatilidade eleitoral" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{
                         data = vol_mun %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                    `Eleitores aptos` == req(input$INT4)) %>% 
                           select(-`Nome do município2`,`Eleitores aptos`) %>% 
                           unique()
                       }
                     } else if(cargo == "Prefeito" &
                               indicador == "Volatilidade eleitoral" & 
                               agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{
                         data = vol_pf %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                    `Eleitores aptos` == req(input$INT4)) %>% 
                           select(-`Nome do município2`,`Eleitores aptos`,
                                  -`Média da volatilidade eleitoral`,
                                  -`Média nacional da volatilidade eleitoral`) %>% 
                           unique()
                       }
                     }
                   })
})



# 2.4.2. Volatilidade parlamentar -----------------------------------------

## Resumo

### Volatilidade (Brasil)  

volparlbr <- reactive({ ## Atributos das tabelas 
  indicador <- input$INDICADORES_VOL
  cargo <- input$DESCRICAO_CARGO4
  agregacao <- input$DESCRICAO_CARGO4
  if(indicador == "Volatilidade parlamentar" & 
     cargo == "Deputado Federal" &
     agregacao == "Brasil"){
    return(input$vol_parl_br)
  }
})


output$vol_parl_br <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
  bvol_parl_br()
})

bvol_parl_br <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vol_parl_br',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_VOL
                     cargo <- input$DESCRICAO_CARGO4
                     agregacao <- input$AGREGACAO_REGIONAL4
                     if(indicador == "Volatilidade parlamentar" &
                        cargo == "Deputado Federal" &
                        agregacao == "Brasil"){
                       vol_br %>% 
                         dplyr::select(`Ano da eleição`,
                                       `Volatilidade parlamentar`) %>% 
                         spread(`Ano da eleição`,
                                `Volatilidade parlamentar`)
                       
                     }
                   })
}) 

## Dados desagregados

### Volatilidade (Brasil)  

ag_volparl_br <- reactive({
  indicador <- input$INDICADORES_VOL
  cargo <- input$DESCRICAO_CARGO4
  agregacao <- input$AGREGACAO_REGIONAL4
  if(indicador == "Volatilidade parlamentar" &
     cargo == "Deputado Federal" &
     agregacao == "Brasil"){
    return(input$agreg_vol_parl_br)
  }
})

output$agreg_vol_parl_br <- DT::renderDataTable(server = FALSE,{
  bagreg_vol_parl_br()
})

bagreg_vol_parl_br <- eventReactive(input$BCALC4, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_parl_br_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',        
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_VOL
                     cargo <- input$DESCRICAO_CARGO4
                     agregacao <- input$AGREGACAO_REGIONAL4
                     uf <- input$UF3
                     if(indicador == "Volatilidade parlamentar" & 
                        cargo == "Deputado Federal" &
                        agregacao == "Brasil"){
                       data = vol_br 
                       
                     }
                   })
})


## Resumo

### Volatilidade (UF) 

volparluf <- reactive({ ## Atributos das tabelas 
  indicador <- input$INDICADORES_VOL
  agregacao <- input$DESCRICAO_CARGO4
  if(indicador == "Volatilidade parlamentar" & 
     agregacao == "UF"){
    return(input$vol_parl_uf)
  }
})


output$vol_parl_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
  bvol_parl_uf()
})

bvol_parl_uf <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vol_parl_uf',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',              
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_VOL
                     agregacao <- input$AGREGACAO_REGIONAL4
                     uf <- req(input$UF4)
                     if(indicador == "Volatilidade parlamentar" &
                        agregacao == "UF"){
                       if(uf == ""){
                         return()
                       } else if(uf == "Todas UFs"){
                         vol_uf %>% 
                           dplyr::filter(Cargo == input$DESCRICAO_CARGO4) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Volatilidade parlamentar`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade parlamentar`) %>% 
                           unique()
                       } else{
                         vol_uf %>% 
                           dplyr::filter(Cargo == input$DESCRICAO_CARGO4 &
                                           UF == input$UF4) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Volatilidade parlamentar`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade parlamentar`) %>% 
                           unique()
                       }
                     }
                   })
}) 

## Dados desagregados

### Volatilidade (UF) 

ag_volparl_uf <- reactive({
  indicador <- input$INDICADORES_VOL
  agregacao <- input$AGREGACAO_REGIONAL4
  if(indicador == "Volatilidade parlamentar" &
     agregacao == "UF"){
    return(input$agreg_vol_parl_uf)
  }
})

output$agreg_vol_parl_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_vol_parl_uf()
})

bagreg_vol_parl_uf <- eventReactive(input$BCALC4, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_parl_uf_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',        
                  
                   'FixedColumns'),{
                     indicador <- input$INDICADORES_VOL
                     agregacao <- input$AGREGACAO_REGIONAL4
                     uf <- req(input$UF4)
                     if(indicador == "Volatilidade parlamentar" & 
                        agregacao == "UF"){
                       if(uf == ""){
                         return()
                       } else if(uf == "Todas UFs"){
                         data = vol_uf %>% 
                           filter(Cargo == input$DESCRICAO_CARGO4) %>% 
                           unique()
                       } else{
                         data = vol_uf %>% 
                           filter(Cargo == input$DESCRICAO_CARGO4 &
                                    UF == input$UF4) %>% 
                           unique()
                       }
                     }
                   })
})


## Resumo

### Volatilidade (MUN) 

volparlmun <- reactive({ ## Atributos das tabelas 
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$DESCRICAO_CARGO4)
  if(indicador == "Volatilidade parlamentar" & 
     agregacao == "Município"){
    return(input$vol_parl_mun)
  }
})


output$vol_parl_mun <- DT::renderDataTable(server = TRUE,{ ## Tabela que devera ser chamada na ui
  bvol_parl_mun()
})

bvol_parl_mun <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vol_parl_mun',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',              
                  
                   'FixedColumns'),{
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     municipio <- req(input$MUN4)
                     if(indicador == "Volatilidade parlamentar" &
                        agregacao == "Município"){
                       if(municipio == ""){
                         return()
                       } else if(municipio == "Todos os municípios"){
                         vol_mun %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         `Volatilidade parlamentar`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade parlamentar`) %>% 
                           unique()
                       } else{
                         vol_mun %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                           `Nome do município2` == req(input$MUN4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         `Volatilidade parlamentar`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade parlamentar`) %>% 
                           unique()
                       }
                     }
                   })
}) 

## Dados desagregados

### Volatilidade (MUN) 

ag_volparl_mun <- reactive({
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$AGREGACAO_REGIONAL4)
  if(indicador == "Volatilidade parlamentar" &
     agregacao == "Município"){
    return(input$agreg_vol_parl_mun)
  }
})

output$agreg_vol_parl_mun <- DT::renderDataTable(server = TRUE,{
  bagreg_vol_parl_mun()
})

bagreg_vol_parl_mun <- eventReactive(input$BCALC4, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_parl_mun_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',        
                  
                   'FixedColumns'),{
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     municipio <- req(input$MUN4)
                     if(indicador == "Volatilidade parlamentar" & 
                        agregacao == "Município"){
                       if(municipio == ""){
                         return()
                       } else if(municipio == "Todos os municípios"){
                         data = vol_mun %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4)) %>% 
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da volatilidade eleitoral`,
                                  -`Média da volatilidade parlamentar`,
                                  -`Média nacional da volatilidade eleitoral`,
                                  -`Média nacional da volatilidade parlamentar`) %>% 
                           unique()
                       } else{
                         data = vol_mun %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                    `Nome do município2` == req(input$MUN4)) %>%
                           select(-`Nome do município2`, -`Eleitores aptos`,
                                  -`Média da volatilidade eleitoral`,
                                  -`Média da volatilidade parlamentar`,
                                  -`Média nacional da volatilidade eleitoral`,
                                  -`Média nacional da volatilidade parlamentar`) %>% 
                           unique()
                       }
                     }
                   })
})

## Resumo

### Volatilidade (Eleitores aptos) 


volparlmed <- reactive({ ## Atributos das tabelas 
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$DESCRICAO_CARGO4)
  if(indicador == "Volatilidade parlamentar" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vol_parl_med)
  }
})


output$vol_parl_med <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
  bvol_parl_med()
})

bvol_parl_med <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 't'), 
    class = "display",
    extensions = c('FixedColumns'),{
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     intervalo <- req(input$INT4)
                     if(indicador == "Volatilidade parlamentar" &
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{
                         
                         media1 <- vol_mun %>% 
                           ungroup() %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                           `Eleitores aptos` == req(input$INT4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         `Média nacional da volatilidade parlamentar`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média nacional da volatilidade parlamentar`) %>% 
                           mutate("media" = "Média nacional da volatilidade parlamentar") %>% 
                           column_to_rownames("media")
                         
                         media2 <- vol_mun %>% 
                           ungroup() %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                           `Eleitores aptos` == req(input$INT4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         `Média da volatilidade parlamentar`) %>% 
                           unique() %>% 
                           spread(`Ano da eleição`,
                                  `Média da volatilidade parlamentar`) %>% 
                           mutate("media" = "Média da volatilidade parlamentar do grupo") %>% 
                           column_to_rownames("media")
                         
                         media1 <- rbind(media1, media2)
                         
                         media1
                         
                       }
                     }
                   })
}) 

volparlint <- reactive({ ## Atributos das tabelas 
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$DESCRICAO_CARGO4)
  if(indicador == "Volatilidade parlamentar" & 
     agregacao == "Quantidade de eleitores aptos"){
    return(input$vol_parl_int)
  }
})


output$vol_parl_int <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
  bvol_parl_int()
})

bvol_parl_int <- eventReactive(input$BCALC4, { ## Botao de acao
  datatable(options = list(
    autoWidth = FALSE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 1
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(list(
      extend = 'csv',
      title = 'vol_parl_int',
      bom = TRUE))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',              
                   'FixedColumns'),{
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     intervalo <- req(input$INT4)
                     if(indicador == "Volatilidade parlamentar" &
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{
                         vol_mun %>% 
                           dplyr::filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                         `Eleitores aptos` == req(input$INT4)) %>%
                           dplyr::select(`Ano da eleição`,
                                         UF,
                                         `Nome do município`,
                                         `Volatilidade parlamentar`) %>% 
                           spread(`Ano da eleição`,
                                  `Volatilidade parlamentar`) %>% 
                           unique()
                       }
                     }
                   })
}) 

## Dados desagregados

### Volatilidade (Eleitores aptos) 

ag_volparl_int <- reactive({
  indicador <- req(input$INDICADORES_VOL)
  agregacao <- req(input$AGREGACAO_REGIONAL4)
  if(indicador == "Volatilidade parlamentar" &
     agregacao == "Quantidade de eleitores aptos"){
    return(input$agreg_vol_parl_int)
  }
})

output$agreg_vol_parl_int <- DT::renderDataTable(server = FALSE,{
  bagreg_vol_parl_int()
})

bagreg_vol_parl_int <- eventReactive(input$BCALC4, {
  datatable(options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    ordering = TRUE, 
    searching = FALSE,
    lengthChange = FALSE,
    lengthMenu = FALSE,
    fixedColumns = list(
      leftColumns = 3
    ),
    columnDefs = list(list(
      className = 'dt-center', targets = '_all')),
    dom = 'Bflrtip',
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_parl_int_agreg',
        bom = TRUE),
      list(                     
        extend = 'colvis',                     
        text = 'Colunas'))), 
    class = "display",
    rownames = FALSE,
    extensions = c('Buttons',        
                   'FixedColumns'),{
                     indicador <- req(input$INDICADORES_VOL)
                     agregacao <- req(input$AGREGACAO_REGIONAL4)
                     intervalo <- req(input$INT4)
                     if(indicador == "Volatilidade parlamentar" & 
                        agregacao == "Quantidade de eleitores aptos"){
                       if(intervalo == ""){
                         return()
                       } else{
                         data = vol_mun %>% 
                           filter(Cargo == req(input$DESCRICAO_CARGO4) &
                                  `Eleitores aptos` == req(input$INT4)) %>%
                           select(-`Nome do município2`, -`Eleitores aptos`, 
                                  -`Média da volatilidade eleitoral`,
                                  -`Média da volatilidade parlamentar`,
                                  -`Média nacional da volatilidade eleitoral`,
                                  -`Média nacional da volatilidade parlamentar`) %>% 
                           unique()
                       }
                     }
                   })
})



}



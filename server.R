

# Objetivo
#'        - Criar graficos e tabelas para a exibicao dos indicadores.



# 1. Server ---------------------------------------------------------------


server <- function(input, output,session){
  

# 1.1. Sobre --------------------------------------------------------------

  
 ## Funcao para descricao do sobre
  
  output$sobre <- renderUI({
    note <- paste0("
                   <h2 align = 'center'>
                   <font size ='6' color = 'black'><strong>
                   
                   Sobre </font></h2>
                   
                   <font size = '1' color = 'black'>


                   <h4 align = 'justify'><br />
                   <p style='line-height:150%'>Os indicadores eleitorais são uma iniciativa de disseminar análise de dados eleitorais. 
                   Os indicadores aqui calculados foram inspirados pelo livro 'Votos e Partidos - Almanaque 
                   de Dados Eleitorais' de Wanderley Guilherme dos Santos e pelo antigo site de Jairo Nicolau. Todos os indicadores foram calculados 
                   a partir dos dados do <a href='http://www.cepesp.io/cepesp-data/'> CepespData </a>. Desenvolvido 
                   por Rebeca Carvalho e Gabriela Campos com orientação de George Avelino e apoio da <a href='http://cepespdata.io/sobre'> 
                   equipe CEPESP</a>. </p></h4></font>")
    HTML(note)
  })
  
# 1.2. Agregacao regional -------------------------------------------------  
  
## Funcao que retorna uma nova caixa de selecao quando o usuario seleciona "UF" na agregacao regional    
  
### Fragmentacao partidaria
  
  
  carg <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    if(cargo == "Deputado Federal"){
      return(input$AGREGACAO_REGIONAL2)
    } 
  })
  
  
  output$AGREGACAO_REGIONAL2 <- renderUI({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    if(indicador != "Quociente eleitoral" &
       indicador != "Quociente partidário" &
       cargo == "Deputado Federal"){
      selectizeInput("AGREGACAO_REGIONAL2",
                     label = NULL,
                     choices = 
                       c("Brasil"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
    }else if(indicador != "Quociente eleitoral" &
             indicador != "Quociente partidário" &
             cargo == "Deputado Estadual"){
      selectizeInput("AGREGACAO_REGIONAL2",
                     label = NULL,
                     choices = 
                       c("UF"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
    } else if(indicador == "Quociente eleitoral" | 
              indicador == "Quociente partidário"){
      return()
      
    } else{
      return()
    }
    })
  
  
  
  agreg_q <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    if(indicador == "Quociente eleitoral" |
       indicador == "Quociente partidário"){
      return(input$UF1)
    } 
  })
  
  output$UF1 <- renderUI({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    if(indicador == "Quociente eleitoral" |
       indicador == "Quociente partidário"){
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
    }
  })
  
  
  
  agreg <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(agregacao == "UF"){
      return(input$UF2)
    } 
  })
  
  
  output$UF2 <- renderUI({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    cargo <- input$DESCRICAO_CARGO2
    if(indicador != "Quociente eleitoral" &
       indicador != "Quociente partidário" &
      cargo == "Deputado Estadual" & 
       length(agregacao == "UF") > 0){
      selectizeInput("UF2",
                     label = NULL,
                     choices = 
                       c("","Todas UFs", "AC", "AL", "AM", "AP", "BA",
                         "CE", "DF", "ES","GO", "MA", "MG",
                         "MS", "MT", "PA", "PB", "PE", "PI",
                         "PR", "RJ", "RN", "RO", "RR","RS", 
                         "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
    }
  })
  
  
### Renovacao parlamentar
  
  
  carg <- reactive({
    cargo <- input$DESCRICAO_CARGO3
    if(cargo == "Deputado Federal"){
      return(input$AGREGACAO_REGIONAL2)
    } 
  })
  
  
  output$AGREGACAO_REGIONAL3 <- renderUI({
    cargo <- input$DESCRICAO_CARGO3
    if(cargo == "Deputado Federal"){
      selectizeInput("AGREGACAO_REGIONAL3",
                     label = NULL,
                     choices = 
                       c("", "Brasil"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
    } else if(cargo == "Deputado Estadual"){
      selectizeInput("AGREGACAO_REGIONAL3",
                     label = NULL,
                     choices = 
                       c("", "UF"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma agregação regional'))
      
    }
  })
  
  
  
   agreg <- reactive({
    agregacao <- input$AGREGACAO_REGIONAL3
    if(agregacao == "UF"){
      return(input$UF3)
    } 
  })
  
  
  output$UF3 <- renderUI({
    agregacao <- input$AGREGACAO_REGIONAL3
    cargo <- input$DESCRICAO_CARGO3
    if(cargo == "Deputado Estadual" &
      length(agregacao == "UF") > 0){
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
    }
  })
  
  
  ## Alienacao
  
  agreg <- reactive({
    agregacao <- input$AGREGACAO_REGIONAL4
    if(agregacao == "UF"){
      return(input$UF4)
    } 
  })
  
  
  output$UF4 <- renderUI({
    agregacao <- input$AGREGACAO_REGIONAL4
    if(agregacao == "UF"){
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
    }
  })
  
  
  
# 2. Indicadores ------------------------------------------------------------
  
  ## Definicao das atribuicoes das tabela dos indicadores e seus respectivos botoes de acao
  ## Definicao de cada indicador
  
# 2.1. Fragmentacao legislativa --------------------------------------------    
  
## Modal inicial
    
 m_frag <- 
    observeEvent(input$CepespIndicadores, ignoreInit = TRUE, once = TRUE, {
    if(input$CepespIndicadores == "Fragmentação legislativa") {
       showModal(modalDialog(
        title = "Definição",
        footer = modalButton("Fechar"), 
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
    }else{
      return()
    }
    })
  
    
  ## Modal para ajuda
  
  ### Resumo
  
  observeEvent(input$modal_frag,{
    showModal(modalDialog(title = "Definição",
                          footer = modalButton("Fechar"), 
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
  
  ### Dados agregados
  
  observeEvent(input$modal_frag_ag,{
    showModal(modalDialog(title = "Definição",
                          footer = modalButton("Fechar"), 
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
                   <p>
                   <strong>Fórmula: </strong>
                   <p><i>Número efetivo de partidos eleitoral </i></p>
                   NEP = 1/ &sum;(pv<sup>2</sup>),
                   <p>onde pv = proporção de votos obtidos pelos partidos.</p>
                   <p><i>Número efetivo de partidos legislativo </i></p>
                   NEP = 1/ &sum;(pc<sup>2</sup>),
                   <p>onde pc = proporção de cadeiras obtidas pelos partidos.</p>
                   <p><br />
                   <h4>Fracionalização </h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Este indicador tem por objetivo medir a dispersão partidária de um parlamento. 
                   Ele indica qual a probabilidade de dois parlamentares desse parlamento, 
                   tomados ao acaso, pertecerem a partidos diferentes.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   F = 1 - &sum;(pe<sup>2</sup>), 
                   <p>onde pe = percentual de cadeiras ocupadas por partido.</p>
                   <p><br />                 
                   <h4>Fracionalização máxima</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>A 'fracionalização máxima' não depende da votação dos partidos, mas da quantidade 
                   de cadeiras e partidos com representação parlamentar.<p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   FM = N*(n-1)/n*(N-1), 
                   <p> onde N = número de cadeiras e n = número de partidos com representação parlamentar.</p>
                   <p><br />      
                   <h4>Fragmentação</h4>
                   <h5 align = 'justify'><br />
                   A fragmentação mede quanto o índice de fracionalização se aproxima da fracionalização
                   máxima.</h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   FG = (Índice de fracionalização)/(Índice de fracionalização máxima)
                   <p>
                   <h4><br />Desproporcionalidade</h4>
                   <h5 align = 'justify'><br />
                   O índice proposto por Gallagher consiste na diferença dos percentuais de votos e de cadeiras obtidas por cada partido.</h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   D = &radic;&sum;(vi - si)<sup>2</sup>/2,
                   <p>onde vi = percentual de votos e si = percentual de cadeiras.</p>
                   <p>
                   <h4><br />Quociente Eleitoral</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>É o número mínimo de votos que um partido ou coligação deve atingir 
                   em determinada UF e eleição para garantir uma vaga.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   QE = (Votos válidos)/(Número de vagas existentes)
                   <p>
                   <h4><br/ > Quociente Partidário</h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>O indicador representa o número de vagas que o partido ou coligação obteve, 
                   excluindo as vagas distribuídas por média.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   QP = Número de votos válidos do partido ou coligação/Quociente eleitoral
                   <p>
                   <strong>Fonte:</strong> 
                   <p>1. Votos e partidos: almanaque de dados eleitorais: Brasil e outros 
                   países/ Organização de Wanderley Guilherme dos Santos, com a colaboração de Fabrícia Guimarães. -
                   Rio de Janeiro: Editora FGV, 2002); 
                   <p>2. <a href='http://datapolitica.com.br/eleicao/metodologia.html'>Data Politica</a></p>
                   <p>3. <a href='http://www.tse.jus.br/eleitor/glossario/termos/quociente-eleitoral'>
                   Quociente eleitoral - TSE </a></p>
                   <p>4.<a href='http://www.tse.jus.br/eleitor/glossario/termos/quociente-partidario'>
                   Quociente partidário - TSE </a></p></font>")
    HTML(note)
  }) 
  
# 2.1.1. Desproporcionalidade de gallagher --------------------------------  
  
  ## Tabela para visualizacao    
  
  ### Deputado Federal
  
  
  depfedg <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Desproporcionalidade" & 
      agregacao == "Brasil"){
      return(input$dpg_fed)
    }
  })
  
  output$dpg_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bdpg_fed()
  })
  
  bdpg_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_fed',
        bom = TRUE))), 
        class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        if(indicador == "Desproporcionalidade" & 
            agregacao == "Brasil"){
          frag_part_fed %>% 
            ungroup() %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          `Desproporcionalidade`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Desproporcionalidade`)
          
          
        }
      })
  })  
  
  ## Resumo
  
  ### Deputado Federal  
  
  ag_dpgfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Desproporcionalidade" & 
       agregacao == "Brasil"){
      return(input$agreg_dpgfed)
    }
  })
  
  output$agreg_dpgfed <- DT::renderDataTable(server = FALSE,{
    bagreg_dpgfed()
  })
  
  bagreg_dpgfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      select = TRUE,
      autoWidth = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'fracio_fed_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
        class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        if(indicador == "Desproporcionalidade" & 
           agregacao == "Brasil"){
          data = frag_part_fed %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            select(`Ano da eleição`,
                   Cargo,
                   Desproporcionalidade,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`) %>% 
            unique()
        }
      })
  })
  
  
  ## Tabela para visualizacao    
  
  ### Deputado Estadual
  
  
  depestg <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Desproporcionalidade" & 
       agregacao == "UF"){
      return(input$dpg_est)
    }
  })
  
  output$dpg_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bdpg_est()
  })
  
  bdpg_est <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_est',
        bom = TRUE))), 
        class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Desproporcionalidade" & 
          agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            frag_part_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Desproporcionalidade`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Desproporcionalidade`)
          
          
          } else{
            frag_part_est %>% 
            dplyr::filter(UF == input$UF2 & 
                            Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Desproporcionalidade`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Desproporcionalidade`)
          
          
        }
        }
      })
  })  
  
  ## Resumo
  
  ### Deputado Estadual  
  
  ag_dpgest <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Desproporcionalidade" & 
       agregacao == "UF"){
      return(input$agreg_dpgest)
    }
  })
  
  output$agreg_dpgest <- DT::renderDataTable(server = FALSE,{
    bagreg_dpgest()
  })
  
  bagreg_dpgest <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'fracio_est_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
        class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Desproporcionalidade" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = frag_part_est %>%
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
              select(`Ano da eleição`,
                     UF,
                     Cargo,
                     Desproporcionalidade,
                     `Fracionalização`,
                     `Fracionalização máxima`,
                     `Fragmentação`,
                     `Número efetivo de partidos eleitoral`,
                     `Número efetivo de partidos legislativo`) %>% 
            unique()
          } else{
            data = frag_part_est %>% 
            dplyr::filter(UF == input$UF2 & 
                          Cargo == input$DESCRICAO_CARGO2) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   Desproporcionalidade,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`) %>% 
              unique()
        }
        }
      })
  })
  
  
# 2.1.2. Fracionalizacao -------------------------------------------------- 


## Tabela para visualizacao    
  
### Deputado Federal
  
  
  depfedf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fracionalização" & 
       cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return(input$fracio_fed)
    }
  })
  
  output$fracio_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfracio_fed()
  })
  
  bfracio_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
                autoWidth = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'fracio_fed',
                  bom = TRUE))), 
                class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                             'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização" & 
         agregacao == "Brasil"){
        frag_part_fed %>% 
          ungroup() %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
          dplyr::select(`Ano da eleição`,
                        `Fracionalização`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Fracionalização`)
        
        
      }
    })
  })  
  
## Resumo
  
### Deputado Federal  
  
  ag_fracfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fracionalização" & 
       agregacao == "Brasil"){
      return(input$agreg_fracfed)
    }
  })
  
  output$agreg_fracfed <- DT::renderDataTable(server = FALSE,{
    bagreg_fracfed()
  })
  
  bagreg_fracfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
                scrollX = TRUE,
                autoWidth = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'fracio_fed_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
                class = "display",
              rownames = FALSE,
              extensions = c('Buttons', 
                             'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Fracionalização" & 
         agregacao == "Brasil"){
        data = frag_part_fed %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
          select(`Ano da eleição`,
                 Cargo,
                 Desproporcionalidade,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`) %>% 
          unique() 
      }
    })
  })
  
  ## Tabela para visualizacao    
  
  ### Deputado Estadual
  
  
  depestf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fracionalização" & 
       agregacao == "UF"){
      return(input$fracio_est)
    }
  })
  
  output$fracio_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfracio_est()
  })
  
  bfracio_est <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_est',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Fracionalização" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            frag_part_est %>% 
            ungroup() %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fracionalização`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fracionalização`)
          } else{
            frag_part_est %>% 
            ungroup() %>% 
            dplyr::filter(UF == input$UF2 & 
                          Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          `Fracionalização`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fracionalização`)
        
           }
        }
      })
  })  
  
  ## Resumo
  
  ### Deputado Estadual  
  
  ag_fracest <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fracionalização" & 
       agregacao == "UF"){
      return(input$agreg_fracest)
    }
  })
  
  output$agreg_fracest <- DT::renderDataTable(server = FALSE,{
    bagreg_fracest()
  })
  
  bagreg_fracest <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      select = TRUE,
      autoWidth = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'fracio_est_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Fracionalização" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
          data = frag_part_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   Desproporcionalidade,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`) %>% 
            unique() 
          } else{
          data = frag_part_est %>% 
            dplyr::filter(UF == input$UF2 & 
                          Cargo == input$DESCRICAO_CARGO2) %>% 
          select(`Ano da eleição`,
                 UF,
                 Cargo,
                 Desproporcionalidade,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`) %>% 
            unique()
        }
           }
      })
  })  
  
# 2.1.3. Fracionalizacao maxima -------------------------------------------
  
## Tabela para visualizacao  
  
### Deputado Federal
  
  depfedfm <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fracionalização máxima" 
       & agregacao == "Brasil"){
      return(input$fraciomax_fed)
    }
  })
  
  output$fraciomax_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfraciomax_fed()
  })
  
  bfraciomax_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
                autoWidth = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'fracio_max_fed',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons', 
                             'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Fracionalização máxima" & 
         agregacao == "Brasil"){
        frag_part_fed %>% 
          ungroup() %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
          dplyr::select(`Ano da eleição`,
                        `Fracionalização máxima`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Fracionalização máxima`)
        
      }
    })
  })
  
## Resumo
  
### Deputado Federal  
  
  ag_fracmaxfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fracionalização máxima" & 
       agregacao == "Brasil"){
      return(input$agreg_fracmaxfed)
    }
  })
  
  output$agreg_fracmaxfed <- DT::renderDataTable(server = FALSE,{
    bagreg_fracmaxfed()
  })
  
  bagreg_fracmaxfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
                scrollX = TRUE,
                select = TRUE,
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'fracio_max_fed_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons', 
                               'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Fracionalização máxima" & 
         agregacao == "Brasil"){
        data = frag_part_fed %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
          select(`Ano da eleição`,
                 Cargo,
                 Desproporcionalidade,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`) %>% 
          unique() 
      }
    })
  })
  
  ## Tabela para visualizacao  
  
  ### Deputado Estadual
  
  depestfm <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização máxima" 
       & agregacao == "UF"){
      return(input$fraciomax_est)
    } 
  })
  
  output$fraciomax_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfraciomax_est()
  })
  
  bfraciomax_est <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'fracio_max_est',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- req(input$INDICADORES_FRAG)
        agregacao <- req(input$AGREGACAO_REGIONAL2)
        uf <- req(input$UF2)
        if(indicador == "Fracionalização máxima" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = frag_part_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fracionalização máxima`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                  `Fracionalização máxima`)
          } else{
            data = frag_part_est %>% 
            dplyr::filter(UF == input$UF2 & 
                          Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fracionalização máxima`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fracionalização máxima`)
        }
        }
      })
  })
  
  ## Resumo
  
  ### Deputado Estadual 
  
  ag_fracmaxest <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fracionalização máxima" & 
       agregacao == "UF"){
      return(input$agreg_fracmaxest)
    }
  })
  
  output$agreg_fracmaxest <- DT::renderDataTable(server = FALSE,{
    bagreg_fracmaxest()
  })
  
  bagreg_fracmaxest <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      select = TRUE,
      autoWidth = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'fracio_max_est_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Fracionalização máxima" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          }else if(uf == "Todas UFs"){
            data = frag_part_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
              select(`Ano da eleição`,
                     UF,
                     Cargo,
                     Desproporcionalidade,
                     `Fracionalização`,
                     `Fracionalização máxima`,
                     `Fragmentação`,
                     `Número efetivo de partidos eleitoral`,
                     `Número efetivo de partidos legislativo`) %>% 
            unique()
          } else{
            data = frag_part_est %>% 
            dplyr::filter(UF == input$UF2 & 
                          Cargo == input$DESCRICAO_CARGO2) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   Desproporcionalidade,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`) %>% 
              unique()
        }
      }
      })
  })  
  
# 2.1.4. Fragmentacao -----------------------------------------------------

## Tabela para visualizacao  
    
### Deputado Federal
  
  depfed_frag <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fragmentação" & 
       agregacao == "Brasil"){
      return(input$frag_fed)
    }
  })
  
  output$frag_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfrag_fed()
  })
  
  
  bfrag_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
                autoWidth = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'frag_fed',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                             'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Fragmentação" &
         agregacao == "Brasil"){
        frag_part_fed %>% 
          ungroup() %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
          dplyr::select(`Ano da eleição`,
                        `Fragmentação`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Fragmentação`)
        
      }
    })
  })
  
## Resumo
  
### Deputado Federal  
  
  ag_fragfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fragmentação" & 
       agregacao == "Brasil"){
      return(input$agreg_fragfed)
    }
  })
  
  output$agreg_fragfed <- DT::renderDataTable(server = FALSE,{
    bagreg_fragfed()
  })
  
  bagreg_fragfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
                scrollX = TRUE,
                select = TRUE,
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'frag_fed_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',
                               'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Fragmentação" & 
         agregacao == "Brasil"){
        data = frag_part_fed %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
          select(`Ano da eleição`,
                 Cargo,
                 Desproporcionalidade,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`) %>% 
          unique() 
      }
    })
  })  
  
  
  ## Tabela para visualizacao  
  
  ### Deputado Estadual
  
  depest_frag <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fragmentação" & 
       agregacao == "UF"){
      return(input$frag_est)
    }
  })
  
  output$frag_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfrag_est()
  })
  
  
  bfrag_est <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'frag_est',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Fragmentação" & 
           agregacao == "UF"){
           if(uf == ""){
             return()
           } else if(uf == "Todas UFs"){
            frag_part_est %>% 
            ungroup() %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fragmentação`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fragmentação`)
          
          } else{
            frag_part_est %>% 
            ungroup() %>% 
            dplyr::filter(UF == input$UF2 & 
                          Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Fragmentação`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Fragmentação`)
          }
        }
      })
  })
  
  ## Resumo
  
  ### Deputado Estadual 
  
  ag_fragest <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Fragmentação" &
       agregacao == "UF"){
      return(input$agreg_fragest)
    }
  })
  
  output$agreg_fragest <- DT::renderDataTable(server = FALSE,{
    bagreg_fragest()
  })
  
  bagreg_fragest <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      select = TRUE,
      autoWidth = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'frag_est_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Fragmentação" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          }else if(uf == "Todas UFs"){
          data = frag_part_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   Desproporcionalidade,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`) %>% 
            unique()
          } else{
          data = frag_part_est %>% 
            dplyr::filter(UF == input$UF2 & 
                          Cargo == input$DESCRICAO_CARGO2) %>% 
          select(`Ano da eleição`,
                 UF,
                 Cargo,
                 Desproporcionalidade,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`) %>% 
            unique()
        }
        }
      })
  })  
  
# 2.1.5. Numero efetivo de partidos por cadeiras -----------------------------------------------------
  
## Tabela para visualizacao  
  
### Deputado Federal
  
  depfedn <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "Brasil"){
      return(input$nepc_fed)
    }
  })
  
  output$nepc_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepc_fed()
  })
  
  bnepc_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
                autoWidth = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'nepc_fed',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',  
                              'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Número efetivo de partidos legislativo" & 
         agregacao == "Brasil"){
        frag_part_fed %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
          dplyr::select(`Ano da eleição`,
                        `Número efetivo de partidos legislativo`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Número efetivo de partidos legislativo`)
        
      }
    })
  })
  
## Resumo
  
### Deputado Federal  
  
  ag_nepfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "Brasil"){
      return(input$agreg_nepfed)
    }
  })
  
  output$agreg_nepfed <- DT::renderDataTable(server = FALSE,{
    bagreg_nepfed()
  })
  
  bagreg_nepfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
                scrollX = TRUE,
                select = TRUE,
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'nepc_fed_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',   
                              'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Número efetivo de partidos legislativo" & 
          agregacao == "Brasil"){
        data = frag_part_fed %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>%
          select(`Ano da eleição`,
                 Cargo,
                 Desproporcionalidade,
                 `Fracionalização`,
                 `Fracionalização máxima`,
                 `Fragmentação`,
                 `Número efetivo de partidos eleitoral`,
                 `Número efetivo de partidos legislativo`) %>% 
          unique() 
      }
    })
  })  
  
## Tabela para visualizacao  
  
### Deputado Estadual
  
  depest_nep <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "UF"){
      return(input$nepc_est)
    }
  })
  
  output$nepc_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepc_est()
  })   
  
  bnepc_est <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
                autoWidth = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'nepc_est',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons', 
                             'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- req(input$UF2)
      if(indicador == "Número efetivo de partidos legislativo" & 
         agregacao == "UF"){
        if(uf == ""){
          return()
        } else if(uf == "Todas UFs"){
          frag_part_est %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
          dplyr::select(`Ano da eleição`,
                        UF,
                        `Número efetivo de partidos legislativo`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Número efetivo de partidos legislativo`)
        
        } else{
          frag_part_est %>% 
          dplyr::filter(UF == input$UF2 & 
                        Cargo == input$DESCRICAO_CARGO2) %>% 
          dplyr::select(`Ano da eleição`,
                        UF,
                        `Número efetivo de partidos legislativo`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Número efetivo de partidos legislativo`)
        }
      }
    })
  })  
 
## Resumo
  
### Deputado Estadual 
  
  ag_nepest <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- req(input$UF2)
    if(indicador == "Número efetivo de partidos legislativo" & 
       agregacao == "UF"){
      return(input$agreg_nepest)
    }
  })
  
  output$agreg_nepest <- DT::renderDataTable(server = FALSE,{
    bagreg_nepest()
  })
  
  bagreg_nepest <- eventReactive(input$BCALC2, {
    datatable(options = list(
                scrollX = TRUE,
                select = TRUE,
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'nepc_est_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
      extensions = c('Buttons',      
                     'Select'),{
      indicador <- input$INDICADORES_FRAG
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- req(input$UF2)
      if(indicador == "Número efetivo de partidos legislativo" & 
         agregacao == "UF"){
        if(uf == ""){
          return()
        } else if(uf == "Todas UFs"){
          data = frag_part_est %>% 
          dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   Desproporcionalidade,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`) %>% 
          unique()
        } else{
          data = frag_part_est %>% 
          dplyr::filter(UF == input$UF2 &
                        Cargo == input$DESCRICAO_CARGO2) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   Desproporcionalidade,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`) %>% 
          unique() 
      }
      }
    })
  })
  
# 2.1.6. Numero efetivo de partidos por votos -----------------------------  
  
  ## Tabela para visualizacao  
  
  ### Deputado Federal
  
  depfedvn <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "Brasil"){
      return(input$nepv_fed)
    }
  })
  
  output$nepv_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepv_fed()
  })
  
  bnepv_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'nepc_fed',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',   
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        if(indicador == "Número efetivo de partidos eleitoral" & 
           agregacao == "Brasil"){
          frag_part_fed %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          `Número efetivo de partidos eleitoral`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Número efetivo de partidos eleitoral`)
          
        }
      })
  })
  
  ## Resumo
  
  ### Deputado Federal  
  
  ag_nepvfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "Brasil"){
      return(input$agreg_nepvfed)
    }
  })
  
  output$agreg_nepvfed <- DT::renderDataTable(server = FALSE,{
    bagreg_nepvfed()
  })
  
  bagreg_nepvfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      select = TRUE,
      autoWidth = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'nepc_fed_agreg',
        bom = TRUE),
        list(                    
          extend = 'colvis',                   
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',  
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        if(indicador == "Número efetivo de partidos eleitoral" & 
           agregacao == "Brasil"){
          data = frag_part_fed %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            select(`Ano da eleição`,
                   Cargo,
                   Desproporcionalidade,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`) %>% 
            unique() 
        }
      })
  })  
  
  ## Tabela para visualizacao  
  
  ### Deputado Estadual
  
  depestvn <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos eleitoral" & 
       agregacao == "UF"){
      return(input$nepv_est)
    }
  })
  
  output$nepv_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepv_est()
  })
  
  bnepv_est <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'nepc_est',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',   
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Número efetivo de partidos eleitoral" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if( uf == "Todas UFs"){
            frag_part_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Número efetivo de partidos eleitoral`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                  `Número efetivo de partidos eleitoral`)
          
          } else{
            frag_part_est %>% 
            dplyr::filter(UF == input$UF2 &
                          Cargo == input$DESCRICAO_CARGO2) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Número efetivo de partidos eleitoral`) %>% 
            unique() %>% 
            spread(`Ano da eleição`,
                   `Número efetivo de partidos eleitoral`)
        }
        }
      })
  })
  
  ## Resumo
  
  ### Deputado Estadual  
  
  ag_nepvest <- reactive({
    indicador <- input$INDICADORES_FRAG
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos eleitoral" &
       agregacao == "UF"){
      return(input$agreg_nepvest)
    }
  })
  
  output$agreg_nepvest <- DT::renderDataTable(server = FALSE,{
    bagreg_nepvest()
  })
  
  bagreg_nepvest <- eventReactive(input$BCALC2, {
    datatable(options = list(
      scrollX = TRUE,
      select = TRUE,
      autoWidth = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'nepc_est_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_FRAG
        agregacao <- input$AGREGACAO_REGIONAL2
        uf <- req(input$UF2)
        if(indicador == "Número efetivo de partidos eleitoral" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = frag_part_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO2) %>% 
              select(`Ano da eleição`,
                     UF,
                     Cargo,
                     Desproporcionalidade,
                     `Fracionalização`,
                     `Fracionalização máxima`,
                     `Fragmentação`,
                     `Número efetivo de partidos eleitoral`,
                     `Número efetivo de partidos legislativo`) %>% 
            unique()
          } else{
            data = frag_part_est %>% 
            dplyr::filter(UF == input$UF2 &
                          Cargo == input$DESCRICAO_CARGO2) %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   Desproporcionalidade,
                   `Fracionalização`,
                   `Fracionalização máxima`,
                   `Fragmentação`,
                   `Número efetivo de partidos eleitoral`,
                   `Número efetivo de partidos legislativo`) %>% 
              unique()  
            
        }
        }
      })
  })  
  

# 2.1.7. Quociente eleitoral -----------------------------------------------
  
  ## Tabela para visualizacao
  
  
  ### Deputado Federal
  
  depfed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    uf <- input$UF
    if(indicador == "Quociente eleitoral" & 
       cargo == "Deputado Federal"){
      return(input$quoce_fed)
    }
  })
  
  output$quoce_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquoce_fed()
  })
  
  
  
  bquoce_fed <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_elei_dep_fed',
        bom = TRUE)
        #list(
        #extend = 'collection',
        #text = 'Plot',
        #action =  DT::JS("function ( e, dt, node, config ) {
        #                           }
        
        #               ")) 
      )),
      class = "display",
      #selection = list(mode = 'multiple',
      #                target = 'row+column'), 
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
                       indicador <- input$INDICADORES_DISTR
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF
                       if(indicador == "Quociente eleitoral" & 
                          cargo == "Deputado Federal"){
                         if(input$UF == "Todas UFs"){
                           distcad_fed %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Quociente eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Quociente eleitoral`)
                         }else{
                           distcad_fed %>% 
                             dplyr::filter(UF == input$UF) %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Quociente eleitoral`) %>% 
                             unique() %>% 
                             spread(`Ano da eleição`,
                                    `Quociente eleitoral`)}
                         
                       }
                     })
    
  })
  
  
  
  output$x2 <-renderPlotly({
    s = input$quoce_fed_rows_selected
    if(length(s))
      plot_ly(
        data = distcad_fed2, 
        x = ~`Ano da eleição`,
        y = ~`Quociente eleitoral`,
        size = ~`Quociente eleitoral`,
        color = ~UF,
        type = "scatter",
        mode = "lines+markers") %>% 
      layout(
        title = "Quociente eleitoral: 1998-2018",
        showlegend = TRUE) 
  })
  
  
  
  ## Resumo
  
  ### Deputado Federal  
  
  ag_quocefed <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    uf <- input$UF
    if(indicador == "Quociente eleitoral" &
       cargo == "Deputado Federal"){
      return(input$agreg_quocefed) 
    }
  })
  
  output$agreg_quocefed<- DT::renderDataTable(server = FALSE,{
    bagreg_quocefed()
  })
  
  bagreg_quocefed <- eventReactive(input$BCALC1, {
    datatable(options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'quoc_elei_dep_fed_agreg',
        bom = TRUE),
        list(
          extend = 'colvis',
          text = 'Colunas')
      )), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
                       indicador <- input$INDICADORES_DISTR
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF
                       if(indicador == "Quociente eleitoral" &
                          cargo == "Deputado Federal"){
                         if(input$UF == "Todas UFs"){
                           data = distcad_fed %>% 
                             unique() 
                         }
                         else{
                           data = distcad_fed %>% 
                             dplyr::filter(UF == input$UF) %>% 
                             unique()
                         }}
                     })
  })  
  
  
  ## Tabela para visualizacao  
  
  ### Deputado Estadual
  
  depest <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente eleitoral" & 
       cargo == "Deputado Estadual"){
      return(input$quoce_est)
    }
  })
  
  output$quoce_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquoce_est()
  })
  
  
  bquoce_est <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_elei_dep_est',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
                       indicador <- input$INDICADORES_DISTR
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF
                       if(indicador == "Quociente eleitoral" & 
                          cargo == "Deputado Estadual"){
                         if(input$UF=="Todas UFs"){
                           expr = distcad_est %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Quociente eleitoral`) %>% 
                             unique %>% 
                             spread(`Ano da eleição`,
                                    `Quociente eleitoral`)
                           
                         }else{
                           expr = distcad_est %>% 
                             dplyr::filter(UF == input$UF) %>% 
                             select(`Ano da eleição`, 
                                    UF, 
                                    `Quociente eleitoral`) %>% 
                             unique %>% 
                             spread(`Ano da eleição`, 
                                    `Quociente eleitoral`)}
                       }
                     })
  }) 
  
  ## Resumo
  
  ## Deputado Estadual  
  
  ag_est <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente eleitoral" & 
       cargo == "Deputado Estadual"){
      return(input$agreg_quoceest) 
    }
  })
  
  output$agreg_quoceest <- DT::renderDataTable(server = FALSE,{
    bagreg_quoceest()
  })
  
  bagreg_quoceest <- eventReactive(input$BCALC1, {
    datatable(options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_elei_dep_est_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
                       indicador <- input$INDICADORES_DISTR
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF
                       if(indicador == "Quociente eleitoral" & 
                          cargo == "Deputado Estadual"){
                         if(input$UF == "Todas UFs"){
                           expr = distcad_est %>% 
                             unique()
                         } else {
                           expr = distcad_est %>% 
                             dplyr::filter(UF == input$UF) %>% 
                             unique()
                           
                         }}
                     })
  })  
  
# 2.1.8. Quociente partidario ---------------------------------------------
  
  ## Tabela para visualizacao    
  
  ### Deputado Federal
  
  depfedp <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & 
       cargo == "Deputado Federal"){
      return(input$quocp_fed)
    }
  })
  
  output$quocp_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquocp_fed()
  })
  
  bquocp_fed <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      ordering = TRUE,
      select = TRUE,
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_part_dep_fed',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
                       indicador <- input$INDICADORES_DISTR
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF
                       if(indicador == "Quociente partidário" & 
                          cargo == "Deputado Federal"){
                         if(input$UF=="Todas UFs"){
                           expr = distcad_fed %>% 
                             select(`Ano da eleição`, 
                                    UF, 
                                    `Sigla do partido`, 
                                    `Quociente partidário`)
                           
                         }else{
                           expr = distcad_fed %>% 
                             dplyr::filter(UF == input$UF) %>% 
                             select(`Ano da eleição`, 
                                    UF, 
                                    `Sigla do partido`, 
                                    `Quociente partidário`)
                         }
                         
                       }
                     })
  })
  
  ## Resumo
  
  # Deputado Federal
  
  ag_quocpfed <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & 
       cargo == "Deputado Federal"){
      return(input$agreg_quocpfed)
    }
  })
  
  output$agreg_quocpfed <- DT::renderDataTable(server = FALSE,{
    bagreg_quocpfed()
  })
  
  bagreg_quocpfed <- eventReactive(input$BCALC1, {
    datatable(options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_part_dep_fed_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
                       indicador <- input$INDICADORES_DISTR
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF
                       if(indicador == "Quociente partidário" & 
                          cargo == "Deputado Federal"){
                         if(input$UF == "Todas UFs"){
                           expr = distcad_fed %>% 
                             unique()
                           
                         }else{
                           expr = distcad_fed %>% 
                             dplyr::filter(UF == input$UF) %>% 
                             unique()}
                       }
                     })
  })
  
  ## Tabela para visualizacao  
  
  ### Deputado estadual
  
  depestp <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & 
       cargo == "Deputado Estadual"){
      return(input$quocp_est)
    }
  })
  
  output$quocp_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bquocp_est()
  })
  
  bquocp_est <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'quoc_part_dep_est',
        bom = TRUE))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
                       indicador <- input$INDICADORES_DISTR
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF
                       if(indicador == "Quociente partidário" & 
                          cargo == "Deputado Estadual"){
                         if(input$UF=="Todas UFs"){
                           expr = distcad_est %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Sigla do partido`,
                                    `Quociente partidário`)
                           
                         }else{
                           expr = distcad_est %>% 
                             dplyr::filter(UF == input$UF) %>% 
                             select(`Ano da eleição`, 
                                    UF,
                                    `Sigla do partido`, 
                                    `Quociente partidário`)
                         }}
                     })
  })
  
  ## Resumo
  
  ### Deputado Estadual  
  
  ag_qupcpest <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & 
       cargo == "Deputado Estadual"){
      return(input$agreg_quocpest)
    }
  })
  
  output$agreg_quocpest <- DT::renderDataTable(server = FALSE,{
    bagreg_quocpest()
  })
  
  bagreg_quocpest <- eventReactive(input$BCALC1,{
    datatable(options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          exportOptions = list(
            columns = ':visible'),
          title = 'quoc_part_dep_est_agreg',
          bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
      class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
                       indicador <- input$INDICADORES_DISTR
                       cargo <- input$DESCRICAO_CARGO1
                       uf <- input$UF
                       if(indicador == "Quociente partidário" & 
                          cargo == "Deputado Estadual"){
                         if(input$UF == "Todas UFs"){
                           expr = distcad_est %>% 
                             unique()
                         }else{          
                           expr = distcad_est %>% 
                             dplyr::filter(UF == input$UF) %>% 
                             unique()}
                       }
                     })
  })
  
  
# 2.2. Renovacao parlamentar ---------------------------------------------- 
  
  ## Modal inicial
  
  m_renov <- observeEvent(input$CepespIndicadores, ignoreInit = TRUE, once = TRUE, {
    if(input$CepespIndicadores == "Renovação parlamentar")  {
      showModal(modalDialog(
        title = "Definição",
        footer = modalButton("Fechar"), 
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
    }else{
      return()
    }
  })
  
  ## Modal para ajuda
  
  ### Resumo
  
  observeEvent(input$modal_renovp,{
    showModal(modalDialog(title = "Definição",
                          footer = modalButton("Fechar"), 
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
  
  ### Dados agregados
  
  observeEvent(input$modal_renovp_ag,{
    showModal(modalDialog(title = "Definição",
                          footer = modalButton("Fechar"), 
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
                   <h4> Conservação </h4>
                   <h5 align = 'justify'><br />
                   Exprime a percentagem dos reeleitos em relação ao total de recandidatos.</h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   CS = (REELEIT)/(DERROT +  REELEIT) * 100
                   <p>
                   <h4><br /> Renovação bruta </h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>Esta fórmula computa o número total de representantes novos em uma legislatura,
                   comparado à composição da legislatura anterior.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   RN = (DESIST + DERROT)/(TOT) * 100
                   <h4><br /> Renovação líquida </h4>
                   <h5 align = 'justify'><br />
                   <p style='line-height:150%'>A 'renovação líquida' é composta pelo número de candidatos à reeleição que foram
                   derrotados divido pelo total de recandidatos (derrotados e reeleitos).</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   RL = (DERROT)/(REELEIT + DERROT) * 100
                   <h4><br /> Volatilidade eleitoral </h4>
                   <h5 align = 'justify'><br />
                  <p style='line-height:150%'>O indicador 'volatilidade eleitoral' é uma medida agregada que resulta do
                  somatório das perdas e ganhos dos partidos entre duas eleições, dividido por dois.
                  As perdas e ganhos dos partidos tanto podem ser expressas em proporções de
                  votos ou cadeiras no parlamento.</p></h5>
                  <p>
                  <strong>Fórmula: </strong>
                  <p>
                  VT = &sum;(Vti - Vti-1)/2
                  <p><br /> 
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
  
# 2.2.1. Conservacao ------------------------------------------------------
  
  ## Tabela para visualizacao    
  
  ### Deputado Federal 
  
  depfedc <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Conservação" &
       agregacao == "Brasil"){
      return(input$conserv_fed)
    }
  })
  
  
  output$conserv_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bconserv_fed()
  })
  
  bconserv_fed <- eventReactive(input$BCALC3, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_parl_fed',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',       
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO3
        agregacao <- input$AGREGACAO_REGIONAL3
        if(indicador == "Conservação" &
           cargo == "Deputado Federal" &
           agregacao == "Brasil"){
          renov_parl_fed %>% 
            dplyr::filter(Cargo ==input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,
                          `Conservação`) %>% 
            spread(`Ano da eleição`,
                   `Conservação`)
          
        }
      })
  }) 
  
  ## Resumo
  
  ### Deputado Federal 
  
  ag_alifedc<- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Conservação" & 
       agregacao == "Brasil"){
      return(input$agreg_conserv_fed)
    }
  })
  
  output$agreg_conserv_fed <- DT::renderDataTable(server = FALSE,{
    bagreg_conserv_fed()
  })
  
  bagreg_conserv_fed <- eventReactive(input$BCALC3, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'renov_parl_fed_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO3
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- input$UF3
        if(indicador == "Conservação" &
           cargo == "Deputado Federal" &
           agregacao == "Brasil"){
          data = renov_parl_fed %>%
            dplyr::filter(Cargo==input$DESCRICAO_CARGO3) 
          
        }
      })
  })
  
  ## Tabela para visualizacao    
  
  ### Deputado Estadual 
  
  depestc <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Conservação" & 
       agregacao == "UF"){
      return(input$conserv_est)
    }
  })
  
  
  output$conserv_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bconserv_est()
  })
  
  bconserv_est <- eventReactive(input$BCALC3, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renov_parl_est',
        bom = TRUE))), 
       class = "display",rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO3
        uf <- req(input$UF3)
        agregacao <- input$AGREGACAO_REGIONAL3
        if(indicador == "Conservação" &
           cargo == "Deputado Estadual")
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            renov_parl_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Conservação`) %>% 
            spread(`Ano da eleição`,
                  `Conservação`)
          
          } else{
            renov_parl_est %>% 
              dplyr::filter(Cargo == input$DESCRICAO_CARGO3 &
                            UF == input$UF3) %>% 
              dplyr::select(`Ano da eleição`,
                            UF,
                            `Conservação`) %>% 
              spread(`Ano da eleição`,
                     `Conservação`)
        }
      })
  }) 
  
  ## Resumo
  
  ### Deputado Estadual 
  
  ag_aliestc<- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Conservação" &
       agregacao == "UF"){
      return(input$agreg_conserv_est)
    }
  })
  
  output$agreg_conserv_est <- DT::renderDataTable(server = FALSE,{
    bagreg_conserv_est()
  })
  
  bagreg_conserv_est <- eventReactive(input$BCALC3, {
    datatable(options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'renov_parl_est_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                    
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- req(input$UF3)
        if(indicador == "Conservação" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = renov_parl_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3) %>% 
            unique()
          } else{
            data = renov_parl_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3 &
                     UF == input$UF3) %>% 
            unique()
          }
        }
      })
  })
  
# 2.2.2. Renovacao bruta --------------------------------------------------
  
  ## Tabela para visualizacao    
  
  ### Deputado Federal 
  
  depfedrb <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Renovação bruta" & 
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$renov_br_fed)
    }
  })
  
  
  output$renov_br_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_br_fed()
  })
  
  brenov_br_fed <- eventReactive(input$BCALC3, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renovbr_parl_fed',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO3
        agregacao <- input$AGREGACAO_REGIONAL3
        if(indicador == "Renovação bruta" & 
           cargo == "Deputado Federal" &
           agregacao == "Brasil"){
          renov_parl_fed %>% 
            dplyr::filter(Cargo ==input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,
                          `Renovação bruta`) %>% 
            spread(`Ano da eleição`,
                   `Renovação bruta`) %>% 
            unique()
          
        }
      })
  }) 
  
  ## Resumo
  
  ### Deputado Federal 
  
  ag_alifedrb <- reactive({
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Renovação bruta" & 
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$agreg_renov_br_fed)
    }
  })
  
  output$agreg_renov_br_fed <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_br_fed()
  })
  
  bagreg_renov_br_fed <- eventReactive(input$BCALC3, {
    datatable(options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'renov_parl_fed_agreg',
        bom = TRUE),
        list(                    
          extend = 'colvis',                   
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO3
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- input$UF3
        if(indicador == "Renovação bruta" & 
           cargo == "Deputado Federal" &
           agregacao == "Brasil"){
            data = renov_parl_fed %>%
            dplyr::filter(Cargo==input$DESCRICAO_CARGO3)
        }
      })
  })
  
  ## Tabela para visualizacao    
  
  ### Deputado Estadual 
  
  depestrb <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Renovação bruta" & 
       agregacao == "UF"){
      return(input$renov_br_est)
    }
  })
  
  
  output$renov_br_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_br_est()
  })
  
  brenov_br_est <- eventReactive(input$BCALC3, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renovbr_parl_est',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',     
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- req(input$UF3)
        if(indicador == "Renovação bruta" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            renov_parl_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Renovação bruta`) %>% 
            spread(`Ano da eleição`,
                   `Renovação bruta`) %>% 
            unique()
          } else{
            renov_parl_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3 &
                          UF == input$UF3) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Renovação bruta`) %>% 
            spread(`Ano da eleição`,
                   `Renovação bruta`) %>% 
            unique()
          }
        }
      })
  }) 
  
  ## Resumo
  
  ### Deputado Estadual 
  
  ag_aliestrb <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Renovação bruta" & 
       agregacao == "UF"){
      return(input$agreg_renov_br_est)
    }
  })
  
  output$agreg_renov_br_est <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_br_est()
  })
  
  bagreg_renov_br_est <- eventReactive(input$BCALC3, {
    datatable(options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'renov_parl_est_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- req(input$UF3)
        if(indicador == "Renovação bruta" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = renov_parl_est %>%
            dplyr::filter(Cargo==input$DESCRICAO_CARGO3)
          } else{
            data = renov_parl_est %>%
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3 &
                            UF == input$UF3)
          }
        }
      })
  })
  
  
# 2.2.3. Renovacao liquida ------------------------------------------------
  
  ## Tabela para visualizacao    
  
  ### Deputado Federal 
  
  depfedrl <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Renovação líquida" &
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$renov_liq_fed)
    }
  })
  
  
  output$renov_liq_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_liq_fed()
  })
  
  brenov_liq_fed <- eventReactive(input$BCALC3, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renovliq_parl_fed',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',  
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO3
        agregacao <- input$AGREGACAO_REGIONAL3
        if(indicador == "Renovação líquida" &
           cargo == "Deputado Federal" &
           agregacao == "Brasil"){
          renov_parl_fed %>% 
            dplyr::select(`Ano da eleição`,
                          `Renovação líquida`) %>% 
            spread(`Ano da eleição`,
                   `Renovação líquida`)
          
        }
      })
  }) 
  
  ## Resumo
  
  ### Deputado Federal 
  
  ag_alifedrl <- reactive({
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Renovação líquida" & 
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$agreg_renov_liq_fed)
    }
  })
  
  output$agreg_renov_liq_fed <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_liq_fed()
  })
  
  bagreg_renov_liq_fed <- eventReactive(input$BCALC3, {
    datatable(options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'renov_parl_fed_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO3
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- input$UF3
        if(indicador == "Renovação líquida" & 
           cargo == "Deputado Federal" &
           agregacao == "Brasil"){
          data = renov_parl_fed  
          
        }
      })
  })
  
  ## Tabela para visualizacao    
  
  ### Deputado Estadual 
  
  depestrl <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Renovação líquida" &
       agregacao == "UF"){
      return(input$renov_liq_est)
    }
  })
  
  
  output$renov_liq_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    brenov_liq_est()
  })
  
  brenov_liq_est <- eventReactive(input$BCALC3, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'renovliq_parl_est',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- req(input$UF3)
        if(indicador == "Renovação líquida" &
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if (uf == "Todas UFs"){
            renov_parl_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3) %>%
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Renovação líquida`) %>% 
            spread(`Ano da eleição`,
                  `Renovação líquida`) %>% 
            unique()
          
          } else{
            renov_parl_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3 &
                          UF == input$UF3) %>%
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
  
  ## Resumo
  
  ### Deputado Estadual 
  
  ag_aliestrl <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Renovação líquida" & 
       agregacao == "UF"){
      return(input$agreg_renov_liq_est)
    }
  })
  
  output$agreg_renov_liq_est <- DT::renderDataTable(server = FALSE,{
    bagreg_renov_liq_est()
  })
  
  bagreg_renov_liq_est <- eventReactive(input$BCALC3, {
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      scrollX = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'renov_parl_est_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons', 
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- req(input$UF3)
        if(indicador == "Renovação líquida" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = renov_parl_est %>% 
            filter(Cargo == input$DESCRICAO_CARGO3) %>% 
            unique()
          } else{
            data = renov_parl_est %>% 
            filter(Cargo == input$DESCRICAO_CARGO3 &
                   UF == input$UF3) %>% 
            unique()
          }
        }
      })
  })
  
  
  
# 2.2.4. Volatilidade eleitoral -------------------------------------------
  
  ## Tabela para visualizacao    
  
  ### Deputado Federal 
  
  depfedve <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Volatilidade eleitoral" & 
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$vol_ele_fed)
    }
  })
  
  
  output$vol_ele_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bvol_ele_fed()
  })
  
  bvol_ele_fed <- eventReactive(input$BCALC3, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'vol_ele_fed',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO3
        agregacao <- input$AGREGACAO_REGIONAL3
        if(indicador == "Volatilidade eleitoral" &
           cargo == "Deputado Federal" &
           agregacao == "Brasil"){
          renov_parl_fed %>% 
            dplyr::select(`Ano da eleição`,
                          `Volatilidade eleitoral`) %>% 
            spread(`Ano da eleição`,
                   `Volatilidade eleitoral`)
          
        }
      })
  }) 
  
  ## Resumo
  
  ### Deputado Federal 
  
  ag_alifedve <- reactive({
    indicador <- input$INDICADORES_RENOV
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Volatilidade eleitoral" &
       cargo == "Deputado Federal" &
       agregacao == "Brasil"){
      return(input$agreg_vol_ele_fed)
    }
  })
  
  output$agreg_vol_ele_fed <- DT::renderDataTable(server = FALSE,{
    bagreg_vol_ele_fed()
  })
  
  bagreg_vol_ele_fed <- eventReactive(input$BCALC3, {
    datatable(options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_ele_fed_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',        
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        cargo <- input$DESCRICAO_CARGO3
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- input$UF3
        if(indicador == "Volatilidade eleitoral" & 
           cargo == "Deputado Federal" &
           agregacao == "Brasil"){
          data = renov_parl_fed 
          
        }
      })
  })
  
  
  ## Tabela para visualizacao    
  
  ### Deputado Estadual
  
  depestve <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Volatilidade eleitoral" & 
       agregacao == "UF"){
      return(input$vol_ele_est)
    }
  })
  
  
  output$vol_ele_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bvol_ele_est()
  })
  
  bvol_ele_est <- eventReactive(input$BCALC3, { ## Botao de acao
    datatable(options = list(
      autoWidth = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(list(
        extend = 'csv',
        title = 'vol_ele_est',
        bom = TRUE))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',              
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- req(input$UF3)
        if(indicador == "Volatilidade eleitoral" &
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            renov_parl_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3) %>%
            dplyr::select(`Ano da eleição`,
                          UF,
                          `Volatilidade eleitoral`) %>% 
            spread(`Ano da eleição`,
                   `Volatilidade eleitoral`) %>% 
              unique()
          } else{
            renov_parl_est %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO3 &
                          UF == input$UF3) %>%
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
  
  ## Resumo
  
  ### Deputado Estadual
  
  ag_aliestve <- reactive({
    indicador <- input$INDICADORES_RENOV
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Volatilidade eleitoral" &
       agregacao == "UF"){
      return(input$agreg_vol_ele_est)
    }
  })
  
  output$agreg_vol_ele_est <- DT::renderDataTable(server = FALSE,{
    bagreg_vol_ele_est()
  })
  
  bagreg_vol_ele_est <- eventReactive(input$BCALC3, {
    datatable(options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      select = TRUE,
      ordering = TRUE, 
      searching = TRUE,
      lengthChange = FALSE,
      lengthMenu = FALSE,
      columnDefs = list(list(
        className = 'dt-right', targets = '_all')),
      dom = 'Bfrtip',
      buttons = list(
                     list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible'),
        title = 'vol_ele_est_agreg',
        bom = TRUE),
        list(                     
          extend = 'colvis',                     
          text = 'Colunas'))), 
       class = "display",
      rownames = FALSE,
      extensions = c('Buttons',        
                     'Select'),{
        indicador <- input$INDICADORES_RENOV
        agregacao <- input$AGREGACAO_REGIONAL3
        uf <- req(input$UF3)
        if(indicador == "Volatilidade eleitoral" & 
           agregacao == "UF"){
          if(uf == ""){
            return()
          } else if(uf == "Todas UFs"){
            data = renov_parl_est %>% 
            filter(Cargo == input$DESCRICAO_CARGO3) %>% 
            unique()
          } else{
            data = renov_parl_est %>% 
            filter(Cargo == input$DESCRICAO_CARGO3 &
                   UF == input$UF3) %>% 
            unique()
          }
        }
      })
  })
  
  
  
# 2.3. Alienacao ----------------------------------------------------------  
  
  ## Modal inicial
  
  m_alien <- observeEvent(input$CepespIndicadores, ignoreInit = TRUE, once = TRUE, {
    if(input$CepespIndicadores == "Alienação")  {
        showModal(modalDialog(
        title = "Definição",
        footer = modalButton("Fechar"), 
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
    }else{
      return()
    }
  })
  
  ## Modal para ajuda
  
  ### Resumo
  
  observeEvent(input$modal_alien,{
    showModal(modalDialog(title = "Definição",
                          footer = modalButton("Fechar"), 
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
    showModal(modalDialog(title = "Definição",
                          footer = modalButton("Fechar"), 
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
                   <p style='line-height:150%'> A 'alienação percentual' é o índice de 'alienação absoluta' dividido pelo total de eleitores 
                   aptos da unidade eleitoral.</p></h5>
                   <p>
                   <strong>Fórmula: </strong>
                   <p>
                   AP = (Índice de alienação absoluta)/(Total de eleitores aptos)
                   <p><br />
                  <strong>Fonte:</strong> 
                  <p>1. Votos e partidos: almanaque de dados eleitorais: Brasil e outros 
                   países/ Organização de Wanderley Guilherme dos Santos, com a colaboração de Fabrícia Guimarães. -
                   Rio de Janeiro: Editora FGV, 2002).</p></font>")
    HTML(note)
  })
   
  
 
# 2.3.1. Alienacao absoluta --------------------------------------------------------


## Tabela para visualizacao    
  
### Deputado Federal BR
  
  depfeda_br <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$DESCRICAO_CARGO4
    if(indicador == "Alienação absoluta" & 
       agregacao == "Brasil"){
      return(input$alien_feda_br)
  }
  })
  

  output$alien_feda_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_feda_br()
  })
  
  balien_feda_br <- eventReactive(input$BCALC4, { ## Botao de acao da alienacao absoluta
    datatable(options = list(
                autoWidth = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'alien_abs_fed_br',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',  
                             'Select'),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO4
      agregacao <- input$AGREGACAO_REGIONAL4
      if(indicador == "Alienação absoluta" & 
         agregacao == "Brasil"){
        alien_br %>% 
          dplyr::filter(Cargo ==input$DESCRICAO_CARGO4) %>% 
          dplyr::select(`Ano da eleição`,
                        Turno,
                        `Alienação absoluta`) %>% 
          spread(`Ano da eleição`,
                 `Alienação absoluta`)
        
      }
    })
  }) 
  
## Resumo
  
### Deputado Federal BR  
  
  ag_alifeda_br <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    if(indicador == "Alienação absoluta" & 
       agregacao == "Brasil"){
      return(input$agreg_alifeda_br)
    }
  })
  
  output$agreg_alifeda_br <- DT::renderDataTable(server = FALSE,{
    bagreg_alifed_br()
  })
  
  bagreg_alifed_br <- eventReactive(input$BCALC4, {
    datatable(options = list(
                autoWidth = TRUE,
                scrollX = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'alien_abs_fed_br_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',   
                             'Select'),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO4
      agregacao <- input$AGREGACAO_REGIONAL4
      uf <- input$UF4
      if(indicador == "Alienação absoluta" & 
         agregacao == "Brasil"){
        data = alien_br %>%
          dplyr::filter(Cargo==input$DESCRICAO_CARGO4) 
        
      }
    })
  })
  
## Tabela para visualizacao  
  
### Deputado Federal UF
  
  depfeda_uf <- reactive({ ## Atributos das tabelas de alienacao absoluta 
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$DESCRICAO_CARGO4
    uf <- input$UF4
    if(indicador == "Alienação absoluta" & 
       agregacao == "UF"){
      return(input$alien_feda_uf)
  }    
  })
  
  output$alien_feda_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_feda_uf()
  })
  
  balien_feda_uf <- eventReactive(input$BCALC4, { ## Botao de acao da alienacao absoluta
    datatable(options = list(
                autoWidth = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(list(
                  extend = 'csv',
                  title = 'alien_abs_fed_uf',
                  bom = TRUE))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons', 
                              'Select'),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO4
      agregacao <- input$AGREGACAO_REGIONAL4
      uf <- input$UF4
      if(indicador == "Alienação absoluta" & agregacao == "UF"){
        if(uf=="Todas UFs"){
          alien_uf %>% 
            dplyr::filter(Cargo == input$DESCRICAO_CARGO4) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          Turno, 
                          `Alienação absoluta`) %>% 
            spread(`Ano da eleição`,
                  `Alienação absoluta`) 
        }else{
          alien_uf %>% 
            dplyr::filter(UF == input$UF4 & 
                          Cargo==input$DESCRICAO_CARGO4) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          Turno,
                          `Alienação absoluta`) %>% 
            spread(`Ano da eleição`,
                   `Alienação absoluta`)}
        
      }
    })
  })  
  
## Resumo
  
### Deputado Federal UF  
  
  ag_alifeda_uf <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    uf <- input$UF4
    if(indicador == "Alienação absoluta" & 
       agregacao == "UF"){
      return(input$agreg_alifeda_uf)
    }
  })
  
  output$agreg_alifeda_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_alifeda_uf()
  })
  
  bagreg_alifeda_uf <- eventReactive(input$BCALC4, {
    datatable(options = list(
                autoWidth = TRUE,
                scrollX = TRUE,
                select = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                columnDefs = list(list(
                  className = 'dt-right', targets = '_all')),
                dom = 'Bfrtip',
                buttons = list(
                               list(
                  extend = 'csv',
                  exportOptions = list(
                    columns = ':visible'),
                  title = 'alien_abs_fed_uf_agreg',
                  bom = TRUE),
                  list(                     
                    extend = 'colvis',                     
                    text = 'Colunas'))), 
               class = "display",
              rownames = FALSE,
              extensions = c('Buttons',  
                              'Select'),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO4
      agregacao <- input$AGREGACAO_REGIONAL4
      uf <- input$UF4
      if(indicador == "Alienação absoluta" & 
         agregacao == "UF"){
        if(input$UF4 == "Todas UFs"){
          data = alien_uf %>% 
            dplyr::filter(Cargo==input$DESCRICAO_CARGO4)
          } else{ 
              data = alien_uf %>% 
                dplyr::filter(Cargo==input$DESCRICAO_CARGO4 & 
                              UF == input$UF4) 
            }}
    })
  })  

# 2.3.2. Alienacao percentual ---------------------------------------------

## Tabela para visualizacao
    
### Deputado Federal BR

depfedp_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO4
  agregacao <- input$DESCRICAO_CARGO4
  if(indicador == "Alienação percentual" & 
     agregacao == "Brasil"){
    return(input$alien_fedp_br)
  }
})


output$alien_fedp_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  balien_fedp_br()
})

balien_fedp_br <- eventReactive(input$BCALC4, { ## Botao de acao da alienacao percentual
  datatable(options = list(
              autoWidth = TRUE,
              select = TRUE,
              ordering = TRUE, 
              searching = TRUE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              columnDefs = list(list(
                className = 'dt-right', targets = '_all')),
              dom = 'Bfrtip',
              buttons = list(list(
                extend = 'csv',
                title = 'alien_per_fed_br',
                bom = TRUE))), 
             class = "display",
            rownames = FALSE,
            extensions = c('Buttons',   
                           'Select'),{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    if(indicador == "Alienação percentual" & 
       agregacao == "Brasil"){
      alien_br %>% 
        dplyr::filter(Cargo==input$DESCRICAO_CARGO4) %>% 
        dplyr::select(`Ano da eleição`,
                      Turno, 
                      `Alienação percentual`) %>% 
        spread(`Ano da eleição`,
               `Alienação percentual`)
      
    }
  })
}) 

## Resumo

### Deputado Federal BR

ag_alifedp_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO4
  agregacao <- input$AGREGACAO_REGIONAL4
  if(indicador == "Alienação percentual" & 
     agregacao == "Brasil"){
    return(input$agreg_alifedp_br)
  }
})

output$agreg_alifedp_br <- DT::renderDataTable(server = FALSE,{
  bagreg_alifedp_br()
})

bagreg_alifedp_br <- eventReactive(input$BCALC4, {
  datatable(options = list(
              autoWidth = TRUE,
              scrollX = TRUE,
              select = TRUE,
              ordering = TRUE, 
              searching = TRUE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              columnDefs = list(list(
                className = 'dt-right', targets = '_all')),
              dom = 'Bfrtip',
              buttons = list(
                             list(
                extend = 'csv',
                exportOptions = list(
                  columns = ':visible'),
                title = 'alien_per_fed_br_agreg',
                bom = TRUE),
                list(                     
                  extend = 'colvis',                     
                  text = 'Colunas'))), 
             class = "display",
            rownames = FALSE,
            extensions = c('Buttons',
                           'Select'),{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    uf <- input$UF4
    if(indicador == "Alienação percentual" & 
       agregacao == "Brasil"){
      alien_br %>%
        dplyr::filter(Cargo==input$DESCRICAO_CARGO4) %>% 
        unique()
      
      
    }
  })
})


## Tabela para visualizacao

### Deputado Federal UF

depfedp_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO4
  agregacao <- input$DESCRICAO_CARGO4
  uf <- input$UF4
  if(indicador == "Alienação percentual" & 
     agregacao == "UF"){
    return(input$alien_fedp_uf)
  }
})


output$alien_fedp_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  balien_fedp_uf()
})

balien_fedp_uf <- eventReactive(input$BCALC4, { ## Botao de acao da alienacao percentual
  datatable(options = list(
              autoWidth = TRUE,
              select = TRUE,
              ordering = TRUE, 
              searching = TRUE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              columnDefs = list(list(
                className = 'dt-right', targets = '_all')),
              dom = 'Bfrtip',
              buttons = list(list(
                extend = 'csv',
                title = 'alien_per_fed_uf',
                bom = TRUE))), 
             class = "display",
            rownames = FALSE,
            extensions = c('Buttons',                              
                           'Select'),{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    uf <- input$UF4
    if(indicador == "Alienação percentual" & 
       agregacao == "UF"){
      if(uf=="Todas UFs"){
        alien_uf %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO4) %>% 
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
          dplyr::filter(UF == input$UF4 & 
                        Cargo==input$DESCRICAO_CARGO4) %>% 
          dplyr::select(`Ano da eleição`,
                        UF,
                        Turno,
                        `Alienação percentual`) %>% 
          spread(`Ano da eleição`,
                 `Alienação percentual`)}
      
    }
  })
})

## Resumo

### Deputado Federal UF

ag_alifedp_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO4
  agregacao <- input$AGREGACAO_REGIONAL4
  uf <- input$UF4
  if(indicador == "Alienação percentual" & 
     agregacao == "UF"){
    return(input$agreg_alifedp_uf)
  }
})

output$agreg_alifedp_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_alifedp_uf()
})

bagreg_alifedp_uf <- eventReactive(input$BCALC4, {
  datatable(options = list(
              autoWidth = TRUE,
              scrollX = TRUE,
              select = TRUE,
              ordering = TRUE, 
              searching = TRUE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              columnDefs = list(list(
                className = 'dt-right', targets = '_all')),
              dom = 'Bfrtip',
              buttons = list(
                             list(
                extend = 'csv',
                exportOptions = list(
                  columns = ':visible'),
                title = 'alien_per_fed_uf_agreg',
                bom = TRUE),
                list(                     
                  extend = 'colvis',                     
                  text = 'Colunas'))), 
             class = "display",
            rownames = FALSE,
            extensions = c('Buttons',
                           'Select'),{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    uf <- input$UF4
    if(indicador == "Alienação percentual" & 
       agregacao == "UF"){
      if(input$UF4 == "Todas UFs"){
        data =alien_uf %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO4) %>% 
          select(`Ano da eleição`, 
                 UF, 
                 `Alienação percentual`)
      } else{ 
        data = alien_uf %>% 
          dplyr::filter(UF == input$UF4 &
                        Cargo==input$DESCRICAO_CARGO4) %>% 
          unique()
      }}
  })
})

}





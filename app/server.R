

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
                   
                   Sobre </font></h4>
                   
                   <font size = '1' color = 'black'>


                   <h4 align = 'justify'><br />
                   Os indicadores eleitorais são uma iniciativa de disseminar análise de dados eleitorais. 
                   Os indicadores aqui calculados foram inspirados pelo livro 'Votos e Partidos - Almanaque 
                   de Dados Eleitorais' de Wanderley Guilherme dos Santos. Todos os indicadores foram calculados 
                   a partir dos dados do <a href='http://www.cepesp.io/cepesp-data/'> CepespData </a>. Desenvolvido 
                   por Rebeca Carvalho, Gabriela Campos e apoio da <a href='http://cepespdata.io/sobre'> 
                   equipe CEPESP</a>. </h4></font>")
    HTML(note)
  })
  
# 1.2. Agregacao regional -------------------------------------------------  
  
## Funcao que retorna uma nova caixa de selecao quando o usuario seleciona "UF" na agregacao regional    
  
### Fragmentacao partidaria
  
  
  agreg <- reactive({
    agregacao <- input$AGREGACAO_REGIONAL2
    if(agregacao == "UF"){
      return(input$UF2)
    } 
  })
  
  
  output$UF2 <- renderUI({
    agregacao <- input$AGREGACAO_REGIONAL2
    if(agregacao == "UF"){
      selectizeInput("UF2",
                     label = NULL,
                     choices = 
                       c("", "AC", "AL", "AM", "AP", "BA",
                         "CE", "DF", "ES","GO", "MA", "MG",
                         "MS", "MT", "PA", "PB", "PE", "PI",
                         "PR", "RJ", "RN", "RO", "RR","RS", 
                         "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
    }
  })
  
  
### Alienacao
  
  
  agreg <- reactive({
    agregacao <- input$AGREGACAO_REGIONAL3
    if(agregacao == "UF"){
      return(input$UF3)
    } 
  })
  
  
  output$UF3 <- renderUI({
    agregacao <- input$AGREGACAO_REGIONAL3
    if(agregacao == "UF"){
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
  
  
  
# 2. Indicadores ------------------------------------------------------------
  
  ## Definicao das atribuicoes das tabela dos indicadores e seus respectivos botoes de acao
  ## Definicao de cada indicador
  
# 2.1. Distribuicao de cadeiras -------------------------------------------  
 
  ## Funcao para descricao dos indicadores de distribuicao de cadeiras
  
  output$def_distc <- renderUI({
    note <- paste0("<h3>Defini&ccedil;&atilde;o dos indicadores</h3>
                   <h4><br />Quociente Eleitoral</h4>
                   <p>&Eacute; o n&uacute;mero m&iacute;nimo de votos que um partido ou coliga&ccedil;
                   &atilde;o deve atingir em determinada UF e elei&ccedil;&atilde;o para garantir uma vaga.</p>
                   <p>Quociente Eleitoral (QE) = (Votos V&aacute;lidos)/(N&uacute;mero de vagas existentes)&nbsp;</p>
                   <p><br /><strong>Fonte:</strong>&nbsp;Tribunal Superior Eleitoral - TSE -&nbsp;
                   <a href='http://www.tse.jus.br/eleitor/glossario/termos/quociente-eleitoral'>Link</a></p>
                   <h4>Quociente Partid&aacute;rio</h4>
                   <p>O indicador representa o n&uacute;mero de vagas que o partido ou coliga&ccedil;&atilde;o obteve, 
                   excluindo as vagas distribu&iacute;das por m&eacute;dia.</p>
                   <p>Quociente partid&aacute;rio (QP) = n&uacute;mero de votos v&aacute;lidos do partido ou coliga&ccedil;&atilde;o
                   / quociente eleitoral</p>
                   <p><strong>Fonte:</strong>&nbsp;Tribunal Superior Eleitoral - TSE -&nbsp;
                   <a href='http://www.tse.jus.br/eleitor/glossario/termos/quociente-partidario'>Link</a></p>")
    HTML(note)
  }) 
  
 
# 2.1.1. Quociente eleitoral -----------------------------------------------
  
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
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'quoc_elei_dep_fed',
                  bom = TRUE))),
              class = "display",
              extensions = 'Buttons',{
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
  
## Dados agregados
  
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
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'quoc_elei_dep_fed_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente eleitoral" &
         cargo == "Deputado Federal"){
        if(input$UF == "Todas UFs"){
          data = distcad_fed %>% 
            select(`Ano da eleição`,
                   UF,
                   Cargo,
                   `Cadeiras oferecidas`,
                   `Votos válidos`, 
                   `Quociente eleitoral`) %>% 
            unique()
        }
        else{
          data = distcad_fed %>% 
            dplyr::filter(UF == input$UF) %>% 
            select(`Ano da eleição`, 
                   UF,
                   Cargo,
                   `Cadeiras oferecidas`,
                   `Votos válidos`, 
                   `Quociente eleitoral`) %>% 
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
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'quoc_elei_dep_est',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
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
  
## Dados agregados
  
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
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'quoc_elei_dep_est_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente eleitoral" & 
         cargo == "Deputado Estadual"){
        if(input$UF == "Todas UFs"){
          expr = distcad_est %>% 
            select(`Ano da eleição`,
                   UF, 
                   Cargo,
                   `Cadeiras oferecidas` ,
                   `Votos válidos`, 
                   `Quociente eleitoral`) %>% 
            unique()
        } else {
          expr = distcad_est %>% 
            dplyr::filter(UF == input$UF) %>% 
            select(`Ano da eleição`, 
                   UF, 
                   Cargo, 
                   `Cadeiras oferecidas` ,
                   `Votos válidos`, 
                   `Quociente eleitoral`) %>% 
            unique()
          
        }}
    })
  })  
  
# 2.1.2. Quociente partidario ---------------------------------------------

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
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'quoc_part_dep_fed',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
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
  
## Dados agregados
  
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
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'quoc_part_dep_fed_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
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
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'quoc_part_dep_est',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
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
 
## Dados agregados
  
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
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'quoc_part_dep_est_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
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

# 2.2. Fragmentacao partidaria --------------------------------------------    
 
  ## Funcao para descricao dos indicadores de fragmentacao partidaria
  
  output$def_frag <- renderUI({
    note <- paste0("<h3>Defini&ccedil;&atilde;o dos indicadores</h3>
                   <h4><br />Fracionaliza&ccedil;&atilde;o</h4>
                   <p>Este indicador tem por objetivo medir a dispers&atilde;o partid&aacute;ria de um parlamento. 
                   Ele indica qual a probabilidade de dois parlamentares desse parlamento, tomados ao acaso, pertecerem a partidos diferentes.</p>
                   <p><strong>Formula:</strong>&nbsp;</p>
                   <p>Fracionaliza&ccedil;&atilde;o = 1 - &sum;(pe<sup>2</sup>), onde pe = percentual de cadeiras ocupadas por partido</p>
                   <p>&nbsp;</p>                   
                   <h4>Fracionaliza&ccedil;&atilde;o M&aacute;xima</h4>
                   <p>A fracionaliza&ccedil;&atilde;o m&aacute;xima n&atilde;o depende da vota&ccedil;&atilde;o dos partidos, mas da quantidade 
                   de cadeiras e partidos com representa&ccedil;&atilde;o parlamentar.&nbsp;</p>
                   <p><strong>Formula:</strong>&nbsp;</p>
                   <p>Fracionaliza&ccedil;&atilde;o m&aacute;xima = N*(n-1)/n*(N-1), onde N = n&uacute;mero de cadeiras e n = n&uacute;mero 
                   de partidos com representa&ccedil;&atilde;o parlamentar</p>
                   <p>&nbsp;</p>                   
                   <h4>Fragmenta&ccedil;&atilde;o</h4>
                   <p>A fragmenta&ccedil;&atilde;o mede quanto o &iacute;ndice de fracionaliza&ccedil;&atilde;o se aproxima da fracionaliza&ccedil;
                   &atilde;o m&aacute;xima.</p>
                   <p><strong>Formula</strong></p>
                   <p>(&Iacute;ndice de fracionaliza&ccedil;&atilde;o)/(&Iacute;ndice de fracionaliza&ccedil;&atilde;o m&aacute;xima)</p>
                   <p>&nbsp;</p>                   
                   <h4>N&uacute;mero efetivo de partidos por cadeiras&nbsp;</h4>
                   <p>O conceito de n&uacute;mero efetivo de partidos define o grau de fragmenta&ccedil;&atilde;o do sistema partid&aacute;rio, 
                   atrav&eacute;s da pondera&ccedil;&atilde;o da for&ccedil;a relativa das legendas que comp&otilde;em o parlamento. O valor calculado 
                   aponta a quantidade de partidos com alguma relev&acirc;ncia em um dado sistema pol&iacute;tico.</p>
                   <p><strong>Formula</strong></p>
                   <p>NEPC = 1/ &sum;(pe<sup>2</sup>)</p>
                   <p>&nbsp;</p>
                   <p>&nbsp;</p>
                   <p>Fonte: Votos e partidos: almanaque de dados eleitorais (Organiza&ccedil;&atilde;o de Wanderley Guilherme dos Santos, 2002) e 
                   <a href='http://datapolitica.com.br/eleicao/metodologia.html'>Data Politica</a></p>")
    HTML(note)
  }) 
  
  
  
# 2.2.1. Fracionalizacao -------------------------------------------------- 

## Tabela para visualizacao    
  
### Deputado Federal
  
  
  depfedf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
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
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'fracio_fed',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização" & 
         cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_part_fed %>% 
          ungroup() %>% 
          dplyr::select(`Ano da eleição`,
                        Fracionalização) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 Fracionalização)
        
        
      }
    })
  })  
  
## Dados agregados
  
### Deputado Federal  
  
  ag_fracfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização" & 
       cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return(input$agreg_fracfed)
    }
  })
  
  output$agreg_fracfed <- DT::renderDataTable(server = FALSE,{
    bagreg_fracfed()
  })
  
  bagreg_fracfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'fracio_fed_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização" & 
         cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        data = frag_part_fed %>% 
          select(`Ano da eleição`, 
                 Cargo, 
                 Fracionalização) %>% 
          unique()
      }
    })
  })  
  
# 2.2.2. Fracionalizacao maxima -------------------------------------------
  
## Tabela para visualizacao  
  
### Deputado Federal
  
  depfedfm <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização máxima" 
       & cargo == "Deputado Federal" & 
       agregacao == "Brasil"){
      return(input$fraciomax_fed)
    }
  })
  
  output$fraciomax_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bfraciomax_fed()
  })
  
  bfraciomax_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'fracio_max_fed',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização máxima" & 
         cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_part_fed %>% 
          ungroup() %>% 
          dplyr::select(`Ano da eleição`,
                        `Fracionalização máxima`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Fracionalização máxima`)
        
      }
    })
  })
  
## Dados agregados
  
### Deputado Federal  
  
  ag_fracmaxfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização máxima" & 
       cargo == "Deputado Federal" 
       & agregacao == "Brasil"){
      return(input$agreg_fracmaxfed)
    }
  })
  
  output$agreg_fracmaxfed <- DT::renderDataTable(server = FALSE,{
    bagreg_fracmaxfed()
  })
  
  bagreg_fracmaxfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'fracio_max_fed_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização máxima" & 
         cargo == "Deputado Federal"
         & agregacao == "Brasil"){
        data = frag_part_fed %>% 
          select(`Ano da eleição`, 
                 Cargo,
                 Fracionalização, 
                 `Fracionalização máxima`) %>% 
          unique()
      }
    })
  })  
  
# 2.2.3. Fragmentacao -----------------------------------------------------

## Tabela para visualizacao  
    
### Deputado Federal
  
  depfed_frag <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fragmentação" & 
       cargo == "Deputado Federal" & 
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
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'frag_fed',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fragmentação" & 
         cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_part_fed %>% 
          ungroup() %>% 
          dplyr::select(`Ano da eleição`,
                        Fragmentação) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 Fragmentação)
        
      }
    })
  })
  
## Dados agregados
  
### Deputado Federal  
  
  ag_fragfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fragmentação" & 
       cargo == "Deputado Federal"
       & agregacao == "Brasil"){
      return(input$agreg_fragfed)
    }
  })
  
  output$agreg_fragfed <- DT::renderDataTable(server = FALSE,{
    bagreg_fragfed()
  })
  
  bagreg_fragfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'frag_fed_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fragmentação" & 
         cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        data = frag_part_fed %>% 
          select(`Ano da eleição`, 
                 Cargo, 
                 Fracionalização,
                 `Fracionalização máxima`, 
                 Fragmentação) %>% 
          unique()
      }
    })
  })  
  
  
# 2.2.4. Numero efetivo de partidos por cadeiras -----------------------------------------------------
  
## Tabela para visualizacao  
  
### Deputado Federal
  
  depfedn <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Número efetivo de partidos por cadeiras" & 
       cargo == "Deputado Federal" 
       & agregacao == "Brasil"){
      return(input$nepc_fed)
    }
  })
  
  output$nepc_fed <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepc_fed()
  })
  
  bnepc_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'nepc_fed',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Número efetivo de partidos por cadeiras" & 
         cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_part_fed %>% 
          dplyr::select(`Ano da eleição`,
                        `Numero efetivo de partidos por cadeiras`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Numero efetivo de partidos por cadeiras`)
        
      }
    })
  })
  
## Dados agregados
  
### Deputado Federal  
  
  ag_nepfed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos por cadeiras" & 
       cargo == "Deputado Federal" 
       & agregacao == "Brasil"){
      return(input$agreg_nepfed)
    }
  })
  
  output$agreg_nepfed <- DT::renderDataTable(server = FALSE,{
    bagreg_nepfed()
  })
  
  bagreg_nepfed <- eventReactive(input$BCALC2, {
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'nepc_fed_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Número efetivo de partidos por cadeiras" & 
         cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        data = frag_part_fed %>% 
          select(`Ano da eleição`, 
                 Cargo, 
                 Fracionalização,
                 `Fracionalização máxima`, 
                 Fragmentação, 
                 `Numero efetivo de partidos por cadeiras`) %>% 
          unique()
      }
    })
  })  
  
## Tabela para visualizacao  
  
### Deputado Estadual
  
  depest_nep <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    uf <- input$UF2
    if(indicador == "Número efetivo de partidos por cadeiras" & 
       cargo == "Deputado Estadual"){
      return(input$nepc_est)
    }
  })
  
  output$nepc_est <- DT::renderDataTable(server = FALSE,{ ## Tabela que devera ser chamada na ui
    bnepc_est()
  })   
  
  bnepc_est <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'nepc_est',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Número efetivo de partidos por cadeiras" & 
         cargo == "Deputado Estadual" 
         & agregacao == "Brasil"){
        frag_part_fed %>% 
          dplyr::select(`Ano da eleição`,
                        `Numero efetivo de partidos por cadeiras`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,
                 `Numero efetivo de partidos por cadeiras`)
        
      }
    })
  })  
 
## Dados agregados
  
### Deputado Estadual 
  
  ag_nepest <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos por cadeiras" & 
       cargo == "Deputado Estadual" 
       & agregacao == "Brasil"){
      return(input$agreg_nepest)
    }
  })
  
  output$agreg_nepest <- DT::renderDataTable(server = FALSE,{
    bagreg_nepest()
  })
  
  bagreg_nepest <- eventReactive(input$BCALC2, {
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'nepc_est_agreg',
                  bom = TRUE))), 
              class = "display",
      extensions = 'Buttons',{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Número efetivo de partidos por cadeiras" & 
         cargo == "Deputado Estadual" 
         & agregacao == "Brasil"){
        data = frag_part_fed %>% 
          select(`Ano da eleição`, 
                 Cargo, 
                 Fracionalização,
                 `Fracionalização máxima`, 
                 Fragmentação, 
                 `Numero efetivo de partidos por cadeiras`) %>% 
          unique()
      }
    })
  })
  
  
# 2.3. Alienacao ----------------------------------------------------------  
  
  ## Funcao para descricao dos indicadores de alienacao
  
  output$def_alien <- renderUI({
    note <- paste0("<h3>Defini&ccedil;&atilde;o dos indicadores</h3>
                   <p>Indicadores de aliena&ccedil;&atilde;o medem a participa&ccedil;&atilde;o nas elei&ccedil;&otilde;es, por unidade eleitoral.</p>
                   <h4><br />Aliena&ccedil;&atilde;o Absoluta</h4>
                   <p>A aliena&ccedil;&atilde;o &eacute; a soma de quantidade de absten&ccedil;&otilde;es, votos brancos e votos nulos
                   de determinada elei&ccedil;&atilde;o.</p>
                   <p><strong>Formula:</strong>&nbsp;</p>
                   <p>(Absten&ccedil;&otilde;es + Votos Brancos + Votos Nulos)</p>
                   <p>&nbsp;</p>
                   <h4>Aliena&ccedil;&atilde;o Percentual</h4>
                   <p>Aliena&ccedil;&atilde;o percentual &eacute; alieana&ccedil;&atilde;o (absten&ccedil;&otilde;es, brancos e nulos), 
                   dividido pelo total de eleitores aptos da unidade eleitoral.</p>
                   <p><strong>Formula:</strong>&nbsp;</p>
                   <p>(Absten&ccedil;&otilde;es + Votos Brancos + Votos Nulos)/(Total de eleitores aptos)</p>
                   <p>&nbsp;</p>
                   <p>Fonte: Votos e partidos: almanaque de dados eleitorais (Organiza&ccedil;&atilde;o de Wanderley Guilherme dos Santos, 2002)")
    HTML(note)
  })
   
  
 
# 2.3.1. Alienacao absoluta --------------------------------------------------------

## Tabela para visualizacao    
  
### Deputado Federal BR
  
  depfeda_br <- reactive({ ## Atributos das tabelas 
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Alienação Absoluta" & 
       agregacao == "Brasil"){
      return(input$alien_feda_br)
  }
  })
  

  output$alien_feda_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_feda_br()
  })
  
  balien_feda_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'alien_abs_fed_br',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      if(indicador == "Alienação Absoluta" & 
         agregacao == "Brasil"){
        alien_br %>% 
          dplyr::filter(Cargo ==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,
                        Turno,
                        `Alienação Absoluta`) %>% 
          spread(`Ano da eleição`,
                 `Alienação Absoluta`)
        
      }
    })
  }) 
  
## Dados agregados
  
### Deputado Federal BR  
  
  ag_alifeda_br <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Alienação Absoluta" & 
       agregacao == "Brasil"){
      return(input$agreg_alifeda_br)
    }
  })
  
  output$agreg_alifeda_br <- DT::renderDataTable(server = FALSE,{
    bagreg_alifed_br()
  })
  
  bagreg_alifed_br <- eventReactive(input$BCALC3, {
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'alien_abs_fed_br_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação Absoluta" & 
         agregacao == "Brasil"){
        data = alien_br %>%
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) 
        
      }
    })
  })
  
## Tabela para visualizacao  
  
### Deputado Federal UF
  
  depfeda_uf <- reactive({ ## Atributos das tabelas de alienacao absoluta 
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    uf <- input$UF3
    if(indicador == "Alienação Absoluta" & 
       agregacao == "UF"){
      return(input$alien_feda_uf)
  }    
  })
  
  output$alien_feda_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_feda_uf()
  })
  
  balien_feda_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'alien_abs_fed_uf',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação Absoluta" & agregacao == "UF"){
        if(uf=="Todas UFs"){
          alien_uf %>% 
            dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          Turno, 
                          `Alienação Absoluta`) %>% 
            spread(`Ano da eleição`,
                   `Alienação Absoluta`)
        }else{
          alien_uf %>% 
            dplyr::filter(UF == input$UF3 & 
                          Cargo==input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,
                          UF,
                          Turno,
                          `Alienação Absoluta`) %>% 
            spread(`Ano da eleição`,
                   `Alienação Absoluta`)}
        
      }
    })
  })  
  
## Dados agregados
  
### Deputado Federal UF  
  
  ag_alifeda_uf <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação Absoluta" & 
       agregacao == "UF"){
      return(input$agreg_alifeda_uf)
    }
  })
  
  output$agreg_alifeda_uf <- DT::renderDataTable(server = FALSE,{
    bagreg_alifeda_uf()
  })
  
  bagreg_alifeda_uf <- eventReactive(input$BCALC3, {
    datatable(options = list(
                autoWidth = TRUE,
                ordering = TRUE, 
                searching = TRUE,
                lengthChange = FALSE,
                lengthMenu = FALSE,
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'csv',
                  title = 'alien_abs_fed_uf_agreg',
                  bom = TRUE))), 
              class = "display",
              extensions = 'Buttons',{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação Absoluta" & 
         agregacao == "UF"){
        if(input$UF3 == "Todas UFs"){
          data = alien_uf %>% 
            dplyr::filter(Cargo==input$DESCRICAO_CARGO3)} else{ 
              data = alien_uf %>% 
                dplyr::filter(Cargo==input$DESCRICAO_CARGO3 & 
                              UF == input$UF3) 
            }}
    })
  })  

# 2.3.2. Alienacao percentual ---------------------------------------------

## Tabela para visualizacao
    
### Deputado Federal BR

depfedp_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  if(indicador == "Alienação Percentual" & 
     agregacao == "Brasil"){
    return(input$alien_fedp_br)
  }
})


output$alien_fedp_br <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  balien_fedp_br()
})

balien_fedp_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
              autoWidth = TRUE,
              ordering = TRUE, 
              searching = TRUE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              dom = 'Bfrtip',
              buttons = list('copy', 'print', list(
                extend = 'csv',
                title = 'alien_per_fed_br',
                bom = TRUE))), 
            class = "display",
            extensions = 'Buttons',{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Alienação Percentual" & 
       agregacao == "Brasil"){
      alien_br %>% 
        dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
        dplyr::select(`Ano da eleição`,
                      Turno, 
                      `Alienação Percentual`) %>% 
        spread(`Ano da eleição`,
               `Alienação Percentual`)
      
    }
  })
}) 

## Dados agregados

### Deputado Federal BR

ag_alifedp_br <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  if(indicador == "Alienação Percentual" & 
     agregacao == "Brasil"){
    return(input$agreg_alifedp_br)
  }
})

output$agreg_alifedp_br <- DT::renderDataTable(server = FALSE,{
  bagreg_alifedp_br()
})

bagreg_alifedp_br <- eventReactive(input$BCALC3, {
  datatable(options = list(
              autoWidth = TRUE,
              ordering = TRUE, 
              searching = TRUE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              dom = 'Bfrtip',
              buttons = list('copy', 'print', list(
                extend = 'csv',
                title = 'alien_per_fed_br_agreg',
                bom = TRUE))), 
            class = "display",
            extensions = 'Buttons',{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação Percentual" & 
       agregacao == "Brasil"){
      alien_br %>%
        dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
        select(`Ano da eleição`, 
               Cargo, 
               `Alienação Percentual`)
      
    }
  })
})


## Tabela para visualizacao

### Deputado Federal UF

depfedp_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$DESCRICAO_CARGO3
  uf <- input$UF3
  if(indicador == "Alienação Percentual" & 
     agregacao == "UF"){
    return(input$alien_fedp_uf)
  }
})


output$alien_fedp_uf <- DT::renderDataTable(server = FALSE,{ ## Tabela da alienacao percentual que devera ser chamada na ui
  balien_fedp_uf()
})

balien_fedp_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
  datatable(options = list(
              autoWidth = TRUE,
              ordering = TRUE, 
              searching = TRUE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              dom = 'Bfrtip',
              buttons = list('copy', 'print', list(
                extend = 'csv',
                title = 'alien_per_fed_uf',
                bom = TRUE))), 
            class = "display",
            extensions = 'Buttons',{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação Percentual" & 
       agregacao == "UF"){
      if(uf=="Todas UFs"){
        alien_uf %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,
                        UF,
                        Cargo,
                        Turno,
                        `Alienação Percentual`) %>% 
          spread(`Ano da eleição`,
                 `Alienação Percentual`)
      }
      else{
        alien_uf %>% 
          dplyr::filter(UF == input$UF3 & 
                          Cargo==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,
                        UF,
                        Turno,
                        `Alienação Percentual`) %>% 
          spread(`Ano da eleição`,
                 `Alienação Percentual`)}
      
    }
  })
})

## Dados agregados

### Deputado Federal UF

ag_alifedp_uf <- reactive({
  indicador <- input$INDICADORES_ALIE
  cargo <- input$DESCRICAO_CARGO3
  agregacao <- input$AGREGACAO_REGIONAL3
  uf <- input$UF3
  if(indicador == "Alienação Percentual" & 
     agregacao == "UF"){
    return(input$agreg_alifedp_uf)
  }
})

output$agreg_alifedp_uf <- DT::renderDataTable(server = FALSE,{
  bagreg_alifedp_uf()
})

bagreg_alifedp_uf <- eventReactive(input$BCALC3, {
  datatable(options = list(
              autoWidth = TRUE,
              ordering = TRUE, 
              searching = TRUE,
              lengthChange = FALSE,
              lengthMenu = FALSE,
              dom = 'Bfrtip',
              buttons = list('copy', 'print', list(
                extend = 'csv',
                title = 'alien_per_fed_uf_agreg',
                bom = TRUE))), 
            class = "display",
            extensions = 'Buttons',{
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação Percentual" & 
       agregacao == "UF"){
      if(input$UF3 == "Todas UFs"){
        data =alien_uf %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
          select(`Ano da eleição`, 
                 UF, 
                 `Alienação Percentual`)
      } else{ 
        data = alien_uf %>% 
          dplyr::filter(UF == input$UF3) %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
          select(`Ano da eleição`, 
                 UF, 
                 `Alienação Percentual`)
      }}
  })
})

}





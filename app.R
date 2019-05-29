
# Shiny (BETA) dos indicadores CepespData
# Autor: Rebeca Carvalho



rm(list = ls())

# Pacotes utilizados

library(cepespR)
library(knitr)
library(plyr)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyalert)
library(shinyBS)
library(ggplot2)
library(shiny)
library(readr)
library(shiny)
library(shinythemes)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(DT)

# 1. Data ----------------------------------------------------------------

#source("script_dadosbeta.R", encoding = "UTF-8", local = TRUE)

files<-list.files(file.path(getwd(),"data"))

for(i in files){
  df<-read.csv(file.path(getwd(),"data",i), encoding = "UTF-8", check.names = FALSE)
  df<-df[,2:length(df)]
  assign(paste(substr(i,1,nchar(i)-4)), df)
  
}

# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  tags$head(
  tags$style(HTML(".navbar .navbar-nav {float: left}
          .navbar .navbar-header {float: right}"))),
  
  title = "CEPESP Indicadores",
    
  navbarPage(id = "CepespIndicadores",theme = shinytheme("flatly"),
               
             tags$div(class = "header", checked = NA,
                      
                      tags$a(href = "http://www.cepesp.io/cepesp-data/", class = 
                               "ir-cepesp-data w-hidden-medium w-hidden-small w-hidden-tiny" ,"CEPESP Data",
                             style = 
                             "top: -2px;   
                             background-color:white;
                             border-bottom-color:#1897d5;
                             right: 0;
                             width: 170px;
                             hover: background:#1897d5;
                             padding: 10px 15px; 
                             padding-top: 10px;
                             padding-right: 15px;
                             padding-bottom: 10px;
                             padding-left: 15px;
                             border-style: none solid solid;
                             border-width: 0 1px 1px;
                             border-radius: 0 0 3px 3px;
                             border-color: #1897d5;
                             border-image-source: initial;
                             border-image-slice: initial;
                             border-image-width: initial;
                             border-image-outset: initial;
                             border-image-repeat: initial;
                             text-align: center;
                             vertical-align: middle;
                             text-decoration: none;
                             text-decoration-line: none;
                             text-decoration-style: initial;
                             text-decoration-color: initial;
                             user-select: none;
                             border: 1px solid transparent;
                             color: #1897d5;
                             font-size: 14px;
                             font-weight: bold;
                             line-height: 12px;
                             text-transform: uppercase;
                             display: inline-block!important;
                             display: flex;
                             flex-direction: column;
                             padding-left: 0;
                             margin-bottom: 0;
                             font-family: Gotham,Open Sans,sans-serif!important;
                             box-sizing: border-box;
                             transition-property: color, border-color, box-shadow;
                             transition-duration: 0.15s, 0.15s, 0.15s, 0.15s;
                             -timing-function: ease-in-out, ease-in-out, ease-in-out, ease-in-out;
                             transition-delay: 0s, 0s, 0s, 0s;
                             webkit-tap-highlight-color: rgba(0,0,0,0);
                             list-style: none;
                             list-style-type: none;
                             list-style-position: initial;
                             list-style-image: initial"
                    
                    )),
    
  
             
              tabPanel("Distribuição de cadeiras",
                      
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),width = 3,
                                     
                                     
                                     selectizeInput(inputId = "INDICADORES_DISTR",
                                                 label = NULL, 
                                                 choices = c("","Quociente eleitoral", "Quociente partidário"),
                                                 selected = NULL,
                                                 options = list(placeholder = 'Escolha um indicador')),
                                     
                                     selectizeInput(inputId = "DESCRICAO_CARGO1",
                                                 label = NULL,
                                                 choices = c("","Deputado Federal", "Deputado Estadual"),
                                                 selected = NULL,
                                                 options = list(placeholder = 'Escolha um cargo')),
                                 
                                     selectizeInput(inputId = "UF",
                                                 label = NULL,
                                                 choices = c("", "Todas UFs", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                                    "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", "PR", 
                                                    "RJ", "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                                                 selected =  NULL,
                                                 options = list(placeholder = 'Escolha uma UF')),
                                     
                                 
                                     actionButton(inputId = "BCALC1",
                                                  label = strong("Calcular"),
                                                  width = "95%")
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills", 
                                                    tabPanel("Tabelas", br(),
                                                             DT::dataTableOutput("table1"),
                                                             DT::dataTableOutput("table2"),
                                                             DT::dataTableOutput("table3"),
                                                             DT::dataTableOutput("table4")),
                                                    tabPanel("Dados agregados", br(),
                                                             DT::dataTableOutput("table5"),
                                                             DT::dataTableOutput("table6"),
                                                             DT::dataTableOutput("table7"),
                                                             DT::dataTableOutput("table8")),
                                                    tabPanel("Definição", htmlOutput("Def_dist_cadeiras"))))))),  
  
             tabPanel("Fragmentação legislativa",
                      
                     
             sidebarLayout(
               
               sidebarPanel(h4("Opções:"),width = 3,
                            
                            
                            selectizeInput(inputId = "INDICADORES_FRAG",
                                        label = NULL, 
                                        choices = c("","Fracionalização", "Fracionalização máxima",
                                                    "Fragmentação", "Número efetivo de partidos por cadeiras"),
                                        selected = NULL,
                                        options = list(placeholder = 'Escolha um indicador')),
                            
                            selectizeInput(inputId = "DESCRICAO_CARGO2",
                                        label = NULL,
                                        choices = c("","Deputado Federal"),
                                        selected = NULL,
                                        options = list(placeholder = 'Escolha um cargo')),
                            
                            selectizeInput(inputId = "AGREGACAO_REGIONAL2",
                                        label = NULL,
                                        choices = c("","Brasil"),
                                        selected = NULL,
                                        options = list(placeholder = 'Escolha uma agregação regional')),
                            
                          uiOutput("UF2"),
                            
                           
                            actionButton(inputId = "BCALC2",
                                         label = strong("Calcular"),
                                         width = "95%")
                            
                            
        ),
               
               mainPanel(
                 
                 absolutePanel(top = 0, right = 0, left = 100,
                               tabsetPanel(type = "pills",
                                           tabPanel("Tabelas",br(), 
                                                        DT::dataTableOutput("table9"),
                                                        DT::dataTableOutput("table10"),
                                                        DT::dataTableOutput("table11"),
                                                        DT::dataTableOutput("table12"),
                                                        DT::dataTableOutput("table13")),
                                          tabPanel("Dados agregados",br(),
                                                        DT::dataTableOutput("table14"),
                                                        DT::dataTableOutput("table15"),
                                                        DT::dataTableOutput("table16"),
                                                        DT::dataTableOutput("table17"),
                                                        DT::dataTableOutput("table18")),
                                           tabPanel("Definição", htmlOutput("Def_frag"))))))),
        
        tabPanel("Alienação",
                 
                 
                 sidebarLayout(
                   
                   sidebarPanel(h4("Opções"),width = 3,
                                
                                
                                selectizeInput(inputId = "INDICADORES_ALIE",
                                            label = NULL, 
                                            choices = c("","Alienação Absoluta", "Alienação Percentual"),
                                            selected = NULL,
                                            options = list(placeholder = 'Escolha um indicador')),
                                
                                selectizeInput(inputId = "DESCRICAO_CARGO3",
                                            label = NULL,
                                            choices = c("","Presidente", "Governador", "Senador", "Deputado Federal", 
                                                        "Deputado Estadual"),
                                            selected = NULL,
                                            options = list(placeholder = 'Escolha um cargo')),
                                
                                selectizeInput(inputId = "AGREGACAO_REGIONAL3",
                                            label = NULL,
                                            choices = c("","Brasil", "UF"),
                                            selected = NULL,
                                            options = list(placeholder = 'Escolha uma agregação regional')),
                                
                                
                                uiOutput("UF3"),
                                
                                
                                actionButton(inputId = "BCALC3",
                                             label = strong("Calcular"),
                                             width = "95%")
                                
                                
                   ),
                   
                   mainPanel(
                     
                     absolutePanel(top = 0, right = 0, left = 100,
                                   tabsetPanel(type = "pills",
                                               tabPanel("Tabelas", br(),
                                                            DT::dataTableOutput("table_alienacao_df"),
                                                            DT::dataTableOutput("table_alienacao_df_p"), 
                                                            DT::dataTableOutput("table_alienacao_df2"), 
                                                            DT::dataTableOutput("table_alienacao_df2_p")),
                                               tabPanel("Dados agregados", br(),
                                                        #downloadButton("downloadData", 
                                                                       #label = strong("Download")),
                                                            DT::dataTableOutput("table23"),
                                                            DT::dataTableOutput("table23_p"),
                                                            DT::dataTableOutput("table24"),
                                                            DT::dataTableOutput("table24_p")),
                                               tabPanel("Definição", htmlOutput("Def_aliena"))))))),
        
        
        tabPanel("Sobre", htmlOutput("Note"))
      
      
      ))
               
               


# 3. Server ---------------------------------------------------------------


server <- function(input, output,session){
  
  
# Funcao para descricao do sobre
  
    output$Note <- renderUI({
    note <- paste0("
                   <font size='3'> 
                   Os indicadores eleitorais são uma iniciativa de disseminar análise de dados eleitorais. 
                   Os indicadores aqui calculados foram inspirados pelo livro 'Votos e Partidos - Almanaque 
                   de Dados Eleitorais' de Wanderley Guilherme dos Santos. Todos os indicadores foram calculados 
                   a partir dos dados do <a href='http://www.cepesp.io/cepesp-data/'> CepespData </a>. Desenvolvido 
                   por Rebeca Carvalho, Gabriela Campos e apoio da <a href='http://cepespdata.io/sobre'> 
                   equipe CEPESP </a>. </font>")
    HTML(note)
  })
  
  
  
  
  
 # 3.1. Agregacao regional -------------------------------------------------
  
  # Fragmentacao partidaria
  
  
# Funcao que retorna uma nova caixa de selecao quando o usuario seleciona "UF" na agregacao regional
    
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
                     choices = c("", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                 "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", 
                                 "PR", "RJ", "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
    }
  })
  
  
  # Alienacao
  
  
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
                     choices = c("","Todas UFs","AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                 "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", 
                                 "PR", "RJ", "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
    }
  })
  
  
  
  
  # 3.1. Tabelas ------------------------------------------------------------
  
  
  
  # 3.1.1 Quociente eleitoral -------------------------------------------------------------------
  
  
  # Deputado federal
  
  
  depfed <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    uf <- input$UF
    if(indicador == "Quociente eleitoral" & cargo == "Deputado Federal"){
      return(input$table1)
    }
  })
  
  output$table1 <- DT::renderDataTable({
    quoce_fed()
  })
  
  # Deputado estadual
  
  depest <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente eleitoral" & cargo == "Deputado Estadual"){
      return(input$table2)
    }
  })
  
  output$table2 <- DT::renderDataTable({
    quoce_est()
  })
  
  
  
  # 3.1.2. Quociente partidario ---------------------------------------------
  
  # Deputado federal
  
  depfed1 <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & cargo == "Deputado Federal"){
      return(input$table3)
    }
  })
  
  output$table3 <- DT::renderDataTable({
    quocp_fed()
  })
  
  # Deputado estadual
  
  depest2 <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & cargo == "Deputado Estadual"){
      return(input$table4)
    }
  })
  
  output$table4 <- DT::renderDataTable({
    quocp_est()
  })
  
  
  # 3.1.3. Fracionalizacao -------------------------------------------------- 
  
  # Deputado Federal
  
  
  depfed_frac <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table9)
    }
  })
  
  output$table9 <- DT::renderDataTable({
    fracio_fed()
  })
  
  
  # 3.1.4. Fracionalizacao maxima -------------------------------------------
  
  # Deputado Federal
  
  depfed_fracmax <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table10)
    }
  })
  
  output$table10 <- DT::renderDataTable({
    fraciomax_fed()
  })
  
 
  # 3.1.5. Fragmentacao -----------------------------------------------------
  
  # Deputado Federal
  
  depfed_frag <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fragmentação" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table11)
    }
  })
  
  output$table11 <- DT::renderDataTable({
    frag_fed()
  })
  
  
  # 3.1.6. Numero efetivo de partidos por cadeiras -----------------------------------------------------
  
  # Deputado Federal
  
  depfed_nep <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Federal" 
       & agregacao == "Brasil"){
      return(input$table12)
    }
  })
  
  output$table12 <- DT::renderDataTable({
    nepc_fed()
  })
  
  # Deputado Estadual
  
  depest_nep <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    uf <- input$UF2
    if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Estadual"){
      return(input$table13)
    }
  })
  
  output$table13 <- DT::renderDataTable({
    nepc_est()
  })   
 
  # 3.1.7. Alienacao --------------------------------------------------------
  
  # Deputado Federal BR
  
  depfed_ali1 <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Alienação Absoluta" & agregacao == "Brasil"){
      return(input$table_alienacao_df)
    }
  })
  
  output$table_alienacao_df <- DT::renderDataTable({
    alien_fed1()
  })

  depfed_ali1_p <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Alienação Percentual" & agregacao == "Brasil"){
      return(input$table_alienacao_df_p)
    }
  })
  
  output$table_alienacao_df_p <- DT::renderDataTable({
    alien_fed1_p()
  })
  

  # Deputado Federal UF
  
  depfed_ali <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    uf <- input$UF3
    if(indicador == "Alienação Absoluta" & agregacao == "UF"){
      return(input$table_alienacao_df2)
    }
  })
  
  output$table_alienacao_df2 <- DT::renderDataTable({
    alien_fed()
  })
  
  depfed_ali_p <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    uf <- input$UF3
    if(indicador == "Alienação Percentual" & agregacao == "UF"){
      return(input$table_alienacao_df2_p)
    }
  })
  output$table_alienacao_df2_p <- DT::renderDataTable({
    alien_fed_p()
  })
  

  # 3.2. Graficos -----------------------------------------------------------
  
  
  # 3.2.1 Quociente eleitoral -------------------------------------------------------------------
  
  # Deputado Federal
  
  grqe_df <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente eleitoral" & cargo == "Deputado Federal"){
      return(input$plot1)
    }
  })
  
  output$plot1 <- renderPlotly({
    plot1 <- vags_fed %>% 
      dplyr::filter(UF == input$UF) %>%
      ggplot(aes(x= UF, y = `Quociente eleitoral`)) + 
      geom_bar(stat="identity", width=0.5, fill="#023858", colour = "#023858") +
      facet_grid(. ~ `Ano da eleição`) +
      labs(title="Quociente eleitoral: Deputado Federal", 
           subtitle="Período de 1998-2018", 
           caption="fonte: CepespIndicadores") + 
      theme(plot.background=element_blank(),
            plot.title = element_text(size = rel(1.2)),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  })
  
  
  
  
# 3.3. Dados agregados ----------------------------------------------------
  
  # 3.3.1. Quociente eleitoral ----------------------------------------------
  
  # Deputado Federal
  
  ag_fed <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    uf <- input$UF
    if(indicador == "Quociente eleitoral" & cargo == "Deputado Federal"){
      return(input$table5) 
    }
  })
  
  output$table5 <- DT::renderDataTable({
    agreg_fed()
  })
  
  agreg_fed <- eventReactive(input$BCALC1, {
    datatable({
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente eleitoral" & cargo == "Deputado Federal"){
        if(input$UF == "Todas UFs"){
          data = vags_fed %>% 
          select(`Ano da eleição`, UF, Cargo, `Cadeiras oferecidas`,`Votos válidos `, 
                 `Quociente eleitoral`) %>% 
          unique()
        }
        else{
          data = vags_fed %>% 
            dplyr::filter(UF == input$UF) %>% 
            select(`Ano da eleição`, UF, Cargo, `Cadeiras oferecidas`,`Votos válidos `, 
                   `Quociente eleitoral`) %>% 
            unique()
        }}
    })
  })  
  
  # Deputado estadual
  
  ag_est <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente eleitoral" & cargo == "Deputado Estadual"){
      return(input$table6) 
    }
  })
  
  output$table6 <- DT::renderDataTable({
    agreg_est()
  })
  
  agreg_est <- eventReactive(input$BCALC1, {
    datatable({
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente eleitoral" & cargo == "Deputado Estadual"){
        if(input$UF == "Todas UFs"){
          expr = vags_est %>% 
          select(`Ano da eleição`, UF, Cargo, `Cadeiras oferecidas` ,`Votos válidos `, 
                 `Quociente eleitoral`) %>% 
          unique()
        } else {
          expr = vags_est %>% 
            dplyr::filter(UF == input$UF) %>% 
            select(`Ano da eleição`, UF, Cargo, `Cadeiras oferecidas` ,`Votos válidos `, 
                   `Quociente eleitoral`) %>% 
            unique()
          
      }}
    })
  })  
  
  
  # 3.3.2. Quociente partidario ---------------------------------------------
  
  # Deputado federal
  
  ag_fed1 <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & cargo == "Deputado Federal"){
      return(input$table7)
    }
  })
  
  output$table7 <- DT::renderDataTable({
    agreg_fed1()
  })
  
  agreg_fed1 <- eventReactive(input$BCALC1, {
    datatable({
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente partidário" & cargo == "Deputado Federal"){
        if(input$UF == "Todas UFs"){
          expr = vags_fed %>% 
            unique()
          
        }else{
        expr = vags_fed %>% 
          dplyr::filter(UF == input$UF) %>% 
          unique()}
      }
    })
  })  
  # Deputado estadual
  
  ag_est2 <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & cargo == "Deputado Estadual"){
      return(input$table8)
    }
  })
  
  output$table8 <- DT::renderDataTable({
    agreg_est2()
  })
  
  agreg_est2 <- eventReactive(input$BCALC1,{
    datatable({
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente partidário" & cargo == "Deputado Estadual"){
        if(input$UF == "Todas UFs"){
          expr = vags_est %>% 
            unique()
        }else{          
        expr = vags_est %>% 
          dplyr::filter(UF == input$UF) %>% 
          unique()}
      }
    })
  })
  
  
  # 3.3.4. Fracionalizacao --------------------------------------------------
  
  # Deputado Federal
  
  agfrac_fed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table14)
    }
  })
  
  output$table14 <- DT::renderDataTable({
    agregfrac_fed()
  })
  
  agregfrac_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        data = frag_partdf %>% 
          select(`Ano da eleição`, Cargo, Fracionalização) %>% 
          unique()
      }
    })
  })
  
  # 3.3.5. Fracionalizacao maxima -------------------------------------------
  
  # Deputado Federal
  
  agfracmax_fed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal" 
       & agregacao == "Brasil"){
      return(input$table15)
    }
  })
  
  output$table15 <- DT::renderDataTable({
    agregfracmax_fed()
  })
  
  agregfracmax_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal"
         & agregacao == "Brasil"){
        data = frag_partdf %>% 
          select(`Ano da eleição`, Cargo,Fracionalização, `Fracionalização máxima`) %>% 
          unique()
      }
    })
  })
  
  # 3.3.6. Fragmentacao -----------------------------------------------------
  
  
  # Deputado Federal
  
  agfrag_fed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fragmentação" & cargo == "Deputado Federal"
       & agregacao == "Brasil"){
      return(input$table16)
    }
  })
  
  output$table16 <- DT::renderDataTable({
    agregfrag_fed()
  })
  
  agregfrag_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fragmentação" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        data = frag_partdf %>% 
          select(`Ano da eleição`, Cargo, Fracionalização,`Fracionalização máxima`, Fragmentação) %>% 
          unique()
      }
    })
  })
  
  #3.3.6. Numero efetivo de partidos por cadeiras --------------------------
    
  
  # Deputado Federal
  
  agnep_fed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Federal" 
       & agregacao == "Brasil"){
      return(input$table17)
    }
  })
  
  output$table17 <- DT::renderDataTable({
    agregnep_fed()
  })
  
  agregnep_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        data = frag_partdf %>% 
          select(`Ano da eleição`, Cargo, Fracionalização,`Fracionalização máxima`, Fragmentação, 
                 `Numero efetivo de partidos por cadeiras`) %>% 
          unique()
      }
    })
  })
  
  # Deputado Estadual
  
  agnep_est <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Estadual" 
       & agregacao == "Brasil"){
      return(input$table18)
    }
  })
  
  output$table18 <- DT::renderDataTable({
    agregnep_est()
  })
  
  agregnep_est <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Estadual" 
         & agregacao == "Brasil"){
        data = frag_partdf %>% 
          select(`Ano da eleição`, Cargo, Fracionalização,`Fracionalização máxima`, Fragmentação, 
                 `Numero efetivo de partidos por cadeiras`) %>% 
          unique()
      }
    })
  })
  
  
  ## 3.3.3. Alienacao --------------------------------------------------------
  
  # Deputado Federal BR
  
  agali_fed1 <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
   if(indicador == "Alienação Absoluta" & agregacao == "Brasil"){
      return(input$table23)
    }
  })
  
  output$table23 <- DT::renderDataTable({
    agregali_fed1()
  })
  
  agregali_fed1 <- eventReactive(input$BCALC3, {
    datatable({
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação Absoluta" & agregacao == "Brasil"){
        data = alienacao_br %>%
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) 
         
      }
    })
  })  

  agali_fed1_p <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    if(indicador == "Alienação Percentual" & agregacao == "Brasil"){
      return(input$table23_p)
    }
  })
  
  output$table23_p <- DT::renderDataTable({
    agregali_fed1_p()
  })
  
  agregali_fed1_p <- eventReactive(input$BCALC3, {
    datatable({
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação Percentual" & agregacao == "Brasil"){
        alienacao_br %>%
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
          select(`Ano da eleição`, Cargo, `Alienação Percentual`)
        
      }
    })
  })  
  
  
  
  # Deputado Federal UF
  
  agali_fed <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação Absoluta" & agregacao == "UF"){
      return(input$table24)
    }
  })
  
  output$table24 <- DT::renderDataTable({
    agregali_fed()
  })
  
  agregali_fed <- eventReactive(input$BCALC3, {
    datatable({
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação Absoluta" & agregacao == "UF"){
        if(input$UF3 == "Todas UFs"){
        data = alienacao_uf %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3)} else{ 
          data = alienacao_uf %>% 
            dplyr::filter(Cargo==input$DESCRICAO_CARGO3 & UF == input$UF3) 
      }}
    })
  })  

  agali_fed_p <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação Percentual" & agregacao == "UF"){
      return(input$table24_p)
    }
  })
  
  output$table24_p <- DT::renderDataTable({
    agregali_fed_p()
  })
  
  agregali_fed_p <- eventReactive(input$BCALC3, {
    datatable({
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação Percentual" & agregacao == "UF"){
        if(input$UF3 == "Todas UFs"){
          data =alienacao_uf %>% 
            dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
            select(`Ano da eleição`, UF, `Alienação Percentual`)
          } else{ 
            data = alienacao_uf %>% dplyr::filter(UF == input$UF3) %>% 
              dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
              select(`Ano da eleição`, UF, `Alienação Percentual`)
          }}
    })
  })  
  

# 3.4. Definicao ----------------------------------------------------------  
  
  #Distribuição de cadeiras
  output$Def_dist_cadeiras <- renderUI({
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
  
  # Fragmentacao partidaria
  
  output$Def_frag <- renderUI({
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

  # Alienacao
  
  output$Def_aliena <- renderUI({
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
  
# 3.5. Botao de acao ------------------------------------------------------
  
  
  # 3.5.1. Quociente eleitoral ----------------------------------------------  
  
  # Deputado Federal
  
  
  
  quoce_fed <- eventReactive(input$BCALC1, {
    datatable({
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente eleitoral" & cargo == "Deputado Federal"){
        if(input$UF == "Todas UFs"){
          vags_fed %>% 
            select(`Ano da eleição`, UF, `Quociente eleitoral`) %>% 
            unique() %>% 
            spread(`Ano da eleição`, `Quociente eleitoral`)
        }else{
        vags_fed %>% 
          dplyr::filter(UF == input$UF) %>% 
          select(`Ano da eleição`, UF, `Quociente eleitoral`) %>% 
          unique() %>% 
          spread(`Ano da eleição`, `Quociente eleitoral`)}
        
      }
    })
  })  
  
  
  # Deputado Estadual  
  
  quoce_est <- eventReactive(input$BCALC1, {
    datatable({
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente eleitoral" & cargo == "Deputado Estadual"){
        if(input$UF=="Todas UFs"){
          expr = vags_est %>% 
            select(`Ano da eleição`, UF, `Quociente eleitoral`) %>% 
            unique %>% 
            spread(`Ano da eleição`, `Quociente eleitoral`)
        
        }else{
        expr = vags_est %>% 
          dplyr::filter(UF == input$UF) %>% 
          select(`Ano da eleição`, UF, `Quociente eleitoral`) %>% 
          unique %>% 
          spread(`Ano da eleição`, `Quociente eleitoral`)}
      }
    })
  })  
  
  
  
  # 3.5.2. Quociente partidario ----------------------------------------------  
  
  
  # Deputado Federal
  
  quocp_fed <- eventReactive(input$BCALC1, {
    datatable({
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente partidário" & cargo == "Deputado Federal"){
        if(input$UF=="Todas UFs"){
          expr = vags_fed %>% 
            select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
          
        }else{
        expr = vags_fed %>% 
          dplyr::filter(UF == input$UF) %>% 
          select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
        }
        
      }
    })
  })  
  
  # Deputado Estadual  
  
  quocp_est <- eventReactive(input$BCALC1, {
    datatable({
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente partidário" & cargo == "Deputado Estadual"){
        if(input$UF=="Todas UFs"){
          expr = vags_est %>% 
            select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
          
        }else{
        expr = vags_est %>% 
          dplyr::filter(UF == input$UF) %>% 
          select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
      }}
    })
  })
  
  
  
  
 # 3.5.3. Fracionalizacao --------------------------------------------------
  
  
  # Deputado Federal
  
  fracio_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          ungroup() %>% 
          dplyr::select(`Ano da eleição`,Fracionalização) %>% 
          unique() %>% 
          spread(`Ano da eleição`,Fracionalização)
        
        
      }
    })
  })  
  
  
  
  # 3.5.4. Fracionalizacao maxima -------------------------------------------
  
  # Deputado Federal
  
  fraciomax_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          ungroup() %>% 
          dplyr::select(`Ano da eleição`,`Fracionalização máxima`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,`Fracionalização máxima`)
        
      }
    })
  })  
  
  
  
  # 3.5.5. Fragmentacao -----------------------------------------------------
  
  # Deputado Federal
  
  frag_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fragmentação" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          ungroup() %>% 
          dplyr::select(`Ano da eleição`,Fragmentação) %>% 
          unique() %>% 
          spread(`Ano da eleição`,Fragmentação)
        
      }
    })
  }) 

# 3.5.6. Numero efetivo de partidos por cadeiras --------------------------

  # Deputado Federal
  
  nepc_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          dplyr::select(`Ano da eleição`,`Numero efetivo de partidos por cadeiras`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,`Numero efetivo de partidos por cadeiras`)
        
      }
    })
  }) 
  
  
  # Deputado Estadual
  
  nepc_est <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Estadual" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          dplyr::select(`Ano da eleição`,`Numero efetivo de partidos por cadeiras`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,`Numero efetivo de partidos por cadeiras`)
        
      }
    })
  })  
  


# 3.5.6. Alienacao --------------------------------------------------------

  

  # Deputado Federal BR
  
  alien_fed1 <- eventReactive(input$BCALC3, {
    datatable({
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      if(indicador == "Alienação Absoluta" & agregacao == "Brasil"){
        alienacao_br %>% 
          dplyr::filter(Cargo ==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,Turno,`Alienação Absoluta`) %>% 
          spread(`Ano da eleição`,`Alienação Absoluta`)
        
      }
    })
  })  
  
  alien_fed1_p <- eventReactive(input$BCALC3, {
    datatable({
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      if(indicador == "Alienação Percentual" & agregacao == "Brasil"){
        alienacao_br %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,Turno, `Alienação Percentual`) %>% 
          spread(`Ano da eleição`,`Alienação Percentual`)
        
      }
    })
  })  
  
  # Deputado Federal UF

alien_fed <- eventReactive(input$BCALC3, {
  datatable({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação Absoluta" & agregacao == "UF"){
      if(uf=="Todas UFs"){
        alienacao_uf %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,UF,Turno, `Alienação Absoluta`) %>% 
          spread(`Ano da eleição`,`Alienação Absoluta`)
      }else{
      alienacao_uf %>% 
        dplyr::filter(UF == input$UF3 & Cargo==input$DESCRICAO_CARGO3) %>% 
        dplyr::select(`Ano da eleição`,UF,Turno, `Alienação Absoluta`) %>% 
        spread(`Ano da eleição`,`Alienação Absoluta`)}
      
    }
  })
})  

alien_fed_p <- eventReactive(input$BCALC3, {
  datatable({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$AGREGACAO_REGIONAL3
    uf <- input$UF3
    if(indicador == "Alienação Percentual" & agregacao == "UF"){
      if(uf=="Todas UFs"){
        alienacao_uf %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,UF, Cargo, Turno, `Alienação Percentual`) %>% 
          spread(`Ano da eleição`,`Alienação Percentual`)
      }
    else{
        alienacao_uf %>% 
          dplyr::filter(UF == input$UF3 & Cargo==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,UF,Turno, `Alienação Percentual`) %>% 
          spread(`Ano da eleição`,`Alienação Percentual`)}
      
    }
  })
})




# 4. Download -------------------------------------------------------------

#output$downloadData <- downloadHandler(
  #filename = function() {
    #paste(input$INDICADORES_ALIE, ".csv", sep = "")
  #},
  #content = function(file) {
   # write.csv(agregali_fed1(), file, row.names = FALSE)
  #}
  #)
}

# 5. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)


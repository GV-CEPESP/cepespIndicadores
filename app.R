
# Shiny dos indicadores CepespData
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

#source("script_dados.R", encoding = "UTF-8")

# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  
  
  navbarPage("CepespIndicadores", theme = shinytheme("flatly"),
             
             
             tabPanel("Sobre"),
             
             tabPanel("Distribuição de cadeiras",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),width = 3,
                                     
                                     
                                     selectInput(inputId = "INDICADORES_DISTR",
                                                 label = "Escolha um indicador", 
                                                 choices = c("Quociente eleitoral", "Quociente partidário"),
                                                 selected = "Quociente eleitoral"),
                                     
                                     selectInput(inputId = "DESCRICAO_CARGO1",
                                                 label = "Escolha um cargo",
                                                 choices = c("Deputado Federal", "Deputado Estadual"),
                                                 selected = "Deputado Federal"),
                                     
                                     selectInput(inputId = "UF",
                                                 label = "Escolha um estado",
                                                 choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                                             "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", "PR", 
                                                             "RJ", "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                                                 selected = "AC"),
                                     
                                     
                                     actionButton(inputId = "BCALC1",
                                                  label = strong("Calcular"),
                                                  width = "95%")
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills", 
                                                    tabPanel("Tabelas", br(),
                                                             div(style = 'overflow-x: scroll',
                                                                 DT::dataTableOutput("table1"),
                                                                 DT::dataTableOutput("table2"),
                                                                 DT::dataTableOutput("table3"),
                                                                 DT::dataTableOutput("table4"))),
                                                    tabPanel("Gráficos", br(), 
                                                             plotlyOutput("plot1")),
                                                    tabPanel("Definição"),
                                                    tabPanel("Dados agregados", br(),
                                                             div(style = 'overflow-x: scroll',
                                                                 DT::dataTableOutput("table5"),
                                                                 DT::dataTableOutput("table6"),
                                                                 DT::dataTableOutput("table7"),
                                                                 DT::dataTableOutput("table8")))))))),  
             
             tabPanel("Fragmentação legislativa",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),width = 3,
                                     
                                     
                                     selectInput(inputId = "INDICADORES_FRAG",
                                                 label = "Escolha um indicador", 
                                                 choices = c("Desproporcionalidade de Gallagher", "Fracionalização", "Fracionalização máxima",
                                                             "Fragmentação", "Número efetivo de partidos por cadeiras",
                                                             "Número efetivo de partidos por votos"),
                                                 selected = "Desproporcionalidade de Gallagher"),
                                     
                                     selectInput(inputId = "DESCRICAO_CARGO2",
                                                 label = "Escolha um cargo",
                                                 choices = c("Deputado Federal", "Deputado Estadual"),
                                                 selected = "Deputado Federal"),
                                     
                                     selectInput(inputId = "AGREGACAO_REGIONAL2",
                                                 label = "Escolha uma agregação regional",
                                                 choices = c("Brasil", "UF"),
                                                 selected = "Brasil"),
                                     
                                     uiOutput("UF2"),
                                     
                                     
                                     actionButton(inputId = "BCALC2",
                                                  label = strong("Calcular"),
                                                  width = "95%")
                                     
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills",
                                                    tabPanel("Tabelas", br(),
                                                             div(style = 'overflow-x: scroll',
                                                                 DT::dataTableOutput("table13"),
                                                                 DT::dataTableOutput("table16"),
                                                                 DT::dataTableOutput("table19"))),
                                                    tabPanel("Gráficos"),
                                                    tabPanel("Definição"),
                                                    tabPanel("Dados agregados", br(),
                                                             div(style = 'overflow-x: scroll',
                                                                 DT::dataTableOutput("table15"),
                                                                 DT::dataTableOutput("table18"),
                                                                 DT::dataTableOutput("table21")))))))),
             tabPanel("Renovação das bancadas",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),width = 3,
                                     
                                     
                                     selectInput(inputId = "INDICADORES_REN",
                                                 label = "Escolha um indicador", 
                                                 choices = c("Conservação", "Renovação bruta", "Renovação líquida", "Volatilidade eleitoral"),
                                                 selected = "Desproporcionalidade de Gallagher"),
                                     
                                     selectInput(inputId = "DESCRICAO_CARGO",
                                                 label = "Escolha um cargo",
                                                 choices = c("Deputado Federal", "Deputado Estadual", "Vereador"),
                                                 selected = "Deputado Federal"),
                                     
                                     selectInput(inputId = "AGREGACAO_REGIONAL",
                                                 label = "Escolha uma agregação regional",
                                                 choices = c("Brasil", "UF", "Município"),
                                                 selected = "Brasil"),
                                     
                                     
                                     selectInput(inputId = "UF",
                                                 label = "Escolha um estado",
                                                 choices = c("Todos os estados","AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                                             "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
                                                             "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                                                 selected = "Todos os estados"),
                                     
                                     actionButton(inputId = "BCALC3",
                                                  label = strong("Calcular"),
                                                  width = "95%")),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills",
                                                    tabPanel("Tabelas"),
                                                    tabPanel("Gráficos"),
                                                    tabPanel("Definição"),
                                                    tabPanel("Dados agregados")))))),
             
             tabPanel("Alienação",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções"),width = 3,
                                     
                                     
                                     selectInput(inputId = "INDICADORES_ALIE",
                                                 label = "Escolha um indicador", 
                                                 choices = "Alienação",
                                                 selected = "Alienação"),
                                     
                                     selectInput(inputId = "DESCRICAO_CARGO4",
                                                 label = "Escolha um cargo",
                                                 choices = c("Deputado Federal", "Deputado Estadual", "Vereador"),
                                                 selected = "Deputado Federal"),
                                     
                                     selectInput(inputId = "AGREGACAO_REGIONAL4",
                                                 label = "Escolha uma agregação regional",
                                                 choices = c("Brasil", "UF", "Municipio"),
                                                 selected = "Brasil"),
                                     
                                     
                                     uiOutput("UF4"),
                                     
                                     
                                     actionButton(inputId = "BCALC4",
                                                  label = strong("Calcular"),
                                                  width = "95%")
                                     
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills",
                                                    tabPanel("Tabelas",br(),
                                                             div(style = 'overflow-x: scroll',
                                                             DT::dataTableOutput("table9"),
                                                             DT::dataTableOutput("table10"))),
                                                    tabPanel("Gráficos"),
                                                    tabPanel("Definição"),
                                                    tabPanel("Dados agregados",br(),
                                                             div(style = 'overflow-x: scroll',
                                                             DT::dataTableOutput("table11"),
                                                             DT::dataTableOutput("table12"))))))
                        ))))
             
            



# 3. Server ---------------------------------------------------------------


server <- function(input, output,session){
  

# 3.1. Agregacao regional -------------------------------------------------
  
# Fragmentacao partidaria
  
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
                     label = "Escolha um estado",
                     choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                  "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", 
                                 "PR", "RJ", "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                     selected = "AC")
    }
  })
  
  
  # Alienacao
  
  
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
                     label = "Escolha um estado",
                     choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                 "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", 
                                 "PR", "RJ", "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                     selected = "AC")
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
  
# 3.1.3. Alienacao --------------------------------------------------------
  
  # Deputado Federal
  
  depfed_ali <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$DESCRICAO_CARGO4
    uf <- input$UF4
    if(indicador == "Alienação" & cargo == "Deputado Federal" & agregacao == "UF"){
      return(input$table9)
    }
  })
  
  output$table9 <- DT::renderDataTable({
    alien_fed()
  })
  
  
  # Deputado Estadual
  
  depest_ali <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    uf <- input$UF4
    if(indicador == "Alienação" & cargo == "Deputado Estadual" & agregacao == "UF"){
      return(input$table10)
    }
  })
  
  output$table10 <- DT::renderDataTable({
    alien_est()
  })
  
# 3.1.4. Fracionalizacao -------------------------------------------------- 
  
  # Deputado Federal
  
  
  depfed_frac <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table13)
    }
  })
  
  output$table13 <- DT::renderDataTable({
    fracio_fed()
  })
  
  # Deputado Estadual
  
  depfed_frac <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    uf <- input$UF2
    if(indicador == "Fracionalização" & cargo == "Deputado Estadual"){
      return(input$table14)
    }
  })
  
  output$table14 <- DT::renderDataTable({
    fracio_est()
  })
  

# 3.1.5. Fracionalizacao maxima -------------------------------------------

  # Deputado Federal
  
  depfed_fracmax <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table16)
    }
  })
  
  output$table16 <- DT::renderDataTable({
    fraciomax_fed()
  })
  
  # Deputado Estadual
  
  depest_fracmax <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    uf <- input$UF2
    if(indicador == "Fracionalização" & cargo == "Deputado Estadual"){
      return(input$table17)
    }
  })
  
  output$table17 <- DT::renderDataTable({
    fraciomax_est()
  })

# 3.1.6. Fragmentacao -----------------------------------------------------
  
  # Deputado Federal
  
  depfed_frag <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fragmentação" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table19)
    }
  })
  
  output$table19 <- DT::renderDataTable({
    frag_fed()
  })
  
  # Deputado Estadual
  
  depest_frag <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    uf <- input$UF2
    if(indicador == "Fragmentação" & cargo == "Deputado Estadual"){
      return(input$table20)
    }
  })
  
  output$table20 <- DT::renderDataTable({
    frag_est()
  })
  
# 3.2. Graficos -----------------------------------------------------------
  
  
# 3.2.1 Quociente eleitoral -------------------------------------------------------------------
  
  # Deputado Federal
  
  grqe_df <- reactive({
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & cargo == "Deputado Federal"){
      return(input$plot1)
    }
  })
  
  output$plot1 <- renderPlotly({
    plot1 <- qef %>% 
      dplyr::filter(UF == input$UF) %>%
      ggplot(aes(x = reorder(`Ano da eleição`, order(`Ano da eleição`)), y=`Quociente eleitoral`)) + 
      geom_bar(stat="identity", width=0.5, fill="#023858", colour = "#023858") +
      labs(title="Quociente eleitoral: Deputado Federal", 
           subtitle="Período de 1998-2018", 
           caption="fonte: CepespIndicadores") + 
      theme(plot.background=element_blank(),
            panel.border=element_blank(),
            plot.title = element_text(size = rel(1.2)),
            axis.title.x = ,
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            legend.position = "right",
            axis.line = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 
    
  })
  
  
  
# 3.3. Definicao ----------------------------------------------------------
  
# 3.4. Dados agregados ----------------------------------------------------
  
# 3.4.1. Quociente eleitoral ----------------------------------------------
  
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
        data = vags_fed %>% 
          dplyr::filter(UF == input$UF) %>% 
          select(`Ano da eleição`, UF, Cargo, Vagas,`Votos válidos `, `Quociente eleitoral`) %>% 
          unique()
      }
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
        expr = vags_est %>% 
          dplyr::filter(UF == input$UF) %>% 
          select(`Ano da eleição`, UF, Cargo, Vagas,`Votos válidos `, `Quociente eleitoral`) %>% 
          unique()
      }
    })
  })  
  
  
# 3.4.2. Quociente partidario ---------------------------------------------
  
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
        expr = vags_fed %>% 
          dplyr::filter(UF == input$UF) %>% 
          unique()
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
        expr = vags_est %>% 
          dplyr::filter(UF == input$UF) %>% 
          unique()
      }
    })
  })
  
# 3.4.3. Alienacao --------------------------------------------------------
  
  # Deputado Federal
  
  agali_fed <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    uf <- input$UF4
    if(indicador == "Alienação" & cargo == "Deputado Federal" & agregacao == "UF"){
      return(input$table11)
    }
  })
  
  output$table11 <- DT::renderDataTable({
    agregali_fed()
  })
  
  agregali_fed <- eventReactive(input$BCALC4, {
    datatable({
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO4
      agregacao <- input$AGREGACAO_REGIONAL4
      uf <- input$UF4
      if(indicador == "Alienação" & cargo == "Deputado Federal" & agregacao == "UF"){
        data = dfc %>% 
          dplyr::filter(UF == input$UF4) 
      }
    })
  })  
  
  
  # Deputado Estadual
  
  
  agali_est <- reactive({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    uf <- input$UF4
    if(indicador == "Alienação" & cargo == "Deputado Estadual" & agregacao == "UF"){
      return(input$table12)
    }
  })
  
  output$table12 <- DT::renderDataTable({
    agregali_est()
  })
  
  agregali_est <- eventReactive(input$BCALC4, {
    datatable({
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO4
      agregacao <- input$AGREGACAO_REGIONAL4
      uf <- input$UF4
      if(indicador == "Alienação" & cargo == "Deputado Estadual" & agregacao == "UF"){
        data = dec %>% 
          dplyr::filter(UF == input$UF4) 
      }
    })
  }) 



# 3.4.4. Fracionalizacao --------------------------------------------------

  # Deputado Federal
  
  agfrac_fed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table15)
    }
  })
  
  output$table15 <- DT::renderDataTable({
    agregfrac_fed()
  })
  
  agregfrac_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização" & cargo == "Deputado Federal" & agregacao == "Brasil"){
        data = frag_partdf %>% 
          select(`Ano da eleição`, Cargo, Fracionalização)
      }
    })
  })
  
# 3.4.5. Fracionalizacao maxima -------------------------------------------
  
  # Deputado Federal
  
  agfracmax_fed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table18)
    }
  })
  
  output$table18 <- DT::renderDataTable({
    agregfracmax_fed()
  })
  
  agregfracmax_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal" & agregacao == "Brasil"){
        data = frag_partdf %>% 
          select(`Ano da eleição`, Cargo,Fracionalização, `Fracionalização máxima`)
      }
    })
  })
  
# 3.4.6. Fragmentacao -----------------------------------------------------
  
  
  # Deputado Federal
  
  agfrag_fed <- reactive({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fragmentação" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$table21)
    }
  })
  
  output$table21 <- DT::renderDataTable({
    agregfrag_fed()
  })
  
  agregfrag_fed <- eventReactive(input$BCALC2, {
    datatable({
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fragmentação" & cargo == "Deputado Federal" & agregacao == "Brasil"){
        data = frag_partdf %>% 
          select(`Ano da eleição`, Cargo, Fracionalização,`Fracionalização máxima`, Fragmentação)
      }
    })
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
        qef %>% 
          dplyr::filter(UF == input$UF) %>% 
          spread(`Ano da eleição`, `Quociente eleitoral`)
        
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
        expr = qee %>% 
          dplyr::filter(UF == input$UF) %>% 
          spread(`Ano da eleição`, `Quociente eleitoral`)
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
        expr = qpf %>% 
          dplyr::filter(UF == input$UF) 
          
        
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
        expr = qpe %>% 
          dplyr::filter(UF == input$UF) %>% 
          select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
      }
    })
  })
  
  


# 3.5.3. Alienacao --------------------------------------------------------

  # Deputado Federal

alien_fed <- eventReactive(input$BCALC4, {
  datatable({
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO4
    agregacao <- input$AGREGACAO_REGIONAL4
    uf <- input$UF4
    if(indicador == "Alienação" & cargo == "Deputado Federal" & agregacao == "UF"){
      dfc %>% 
        dplyr::filter(UF == input$UF4) %>% 
    dplyr::select(`Ano da eleição`, UF, Alienação) %>% 
        spread(`Ano da eleição`,Alienação)
      
    }
  })
})  

  # Deputado Estadual  
  
  alien_est <- eventReactive(input$BCALC4, {
    datatable({
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO4
      agregacao <- input$AGREGACAO_REGIONAL4
      uf <- input$UF4
      if(indicador == "Alienação" & cargo == "Deputado Estadual" & agregacao == "UF"){
      dec %>% 
          dplyr::filter(UF == input$UF4) %>% 
          dplyr::select(`Ano da eleição`, UF, Alienação) %>% 
          spread(`Ano da eleição`,Alienação)
          
        
      }
    })
  })  



# 3.5.4. Fracionalizacao --------------------------------------------------


  # Deputado Federal

fracio_fed <- eventReactive(input$BCALC2, {
  datatable({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização" & cargo == "Deputado Federal" & agregacao == "Brasil"){
     frag_partdf %>% 
        ungroup() %>% 
        dplyr::select(`Ano da eleição`,Fracionalização) %>% 
        unique() %>% 
       spread(`Ano da eleição`,Fracionalização)
        
      
    }
  })
})  



# 3.5.5. Fracionalizacao maxima -------------------------------------------

# Deputado Federal

fraciomax_fed <- eventReactive(input$BCALC2, {
  datatable({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      frag_partdf %>% 
        ungroup() %>% 
        dplyr::select(`Ano da eleição`,`Fracionalização máxima`) %>% 
        unique() %>% 
        spread(`Ano da eleição`,`Fracionalização máxima`)
      
    }
  })
})  



# 3.5.6. Fragmentacao -----------------------------------------------------

# Deputado Federal

frag_fed <- eventReactive(input$BCALC2, {
  datatable({
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fragmentação" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      frag_partdf %>% 
        ungroup() %>% 
        dplyr::select(`Ano da eleição`,Fragmentação) %>% 
        unique() %>% 
        spread(`Ano da eleição`,Fragmentação)
      
    }
  })
}) 
}


# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)

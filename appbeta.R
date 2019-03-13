
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

#source("script_dados.R")

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
                                                             DT::dataTableOutput("table1"),
                                                             DT::dataTableOutput("table2"),
                                                             DT::dataTableOutput("table3"),
                                                             DT::dataTableOutput("table4")),
                                                    tabPanel("Gráficos", br(), plotlyOutput("plotqe")),
                                                    tabPanel("Definição"),
                                                    tabPanel("Dados agregados")))))),  
  
             tabPanel("Fragmentação legislativa",
            
             sidebarLayout(
               
               sidebarPanel(h4("Opções:"),width = 3,
                            
                            
                            selectInput(inputId = "INDICADORES_FRAG",
                                        label = "Escolha um indicador", 
                                        choices = c("Desproporcionalidade de Gallagher", "Fracionalização", "Fracionalização máxima",
                                                    "Fragmentação", "Número efetivo de partidos por cadeiras",
                                                    "Número efetivo de partidos por votos"),
                                        selected = "Desproporcionalidade de Gallagher"),
                            
                            selectInput(inputId = "DESCRICAO_CARGO",
                                        label = "Escolha um cargo",
                                        choices = c("Deputado Federal", "Deputado Estadual"),
                                        selected = "Deputado Federal"),
                            
                            selectInput(inputId = "AGREGACAO_REGIONAL",
                                        label = "Escolha uma agregação regional",
                                        choices = c("Brasil", "UF"),
                                        selected = "Brasil"),
                            
                          
                            selectInput(inputId = "UF",
                                        label = "Escolha um estado",
                                        choices = c("Todos os estados","AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                                    "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
                                                    "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                                        selected = "Todos os estados"),
                            
                           
                            actionButton(inputId = "BCALC2",
                                         label = strong("Calcular"),
                                         width = "95%")
                            
                            
        ),
               
               mainPanel(
                 
                 absolutePanel(top = 0, right = 0, left = 100,
                               tabsetPanel(type = "pills",
                                           tabPanel("Tabelas", br()),
                                           tabPanel("Gráficos"),
                                           tabPanel("Definição"))
                               
                 )))
               
               
             )))





# 3. Server ---------------------------------------------------------------


server <- function(input, output,session){
  
 # Agregacao regional
  
  agreg <- reactive({
    cargo <- input$DESCRICAO_CARGO
    if(cargo == "Vereador"){
      return(input$AGREG_MUN)
    } 
  })
  
  output$AGREG_MUN <- renderUI({
    cargo <- input$DESCRICAO_CARGO
    if(cargo == "Vereador"){
      selectizeInput("AGREG_MUN",
                     label = "Escolha um município",
                     choices = cidades[[3]],
                     selected = NULL)  
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
  
# 3.2. Graficos -----------------------------------------------------------

  # Quociente eleitoral

 output$plotqe <- renderPlotly({
   
  p <- plot_ly(qef, 
           type = "histogram2d", 
           x = "Ano da eleição", 
           y = "Quociente eleitoral",
           color = "UF"
                ) 
   
     
 })
   




# 3.3. Definicao ----------------------------------------------------------

# 3.4. Dados agregados ----------------------------------------------------



# 3.5. Botao de acao ------------------------------------------------------


# 3.4.1. Quociente eleitoral ----------------------------------------------  
  
 # Deputado Federal
  
  
  
  quoce_fed <- eventReactive(input$BCALC1, {
    datatable({
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente eleitoral" & cargo == "Deputado Federal"){
        expr = qef %>% 
          dplyr::filter(UF == input$UF)
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
        dplyr::filter(UF == input$UF)
      }
    })
  })  
  
  
# 3.4.2. Quociente partidario ----------------------------------------------  
  
  
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
        dplyr::filter(UF == input$UF)
  }
  })
  })
  
  
}



# 4. ShinyApp -------------------------------------------------------------

  shinyApp(ui = ui, server = server)

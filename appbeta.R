
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
                                     
                                     selectInput(inputId = "AGREGACAO_REGIONAL",
                                                 label = "Escolha uma agregação regional",
                                                 choices = c("UF"),
                                                 selected = "UF"),
                                     
                                     
                                     selectInput(inputId = "UF",
                                                 label = "Escolha um estado",
                                                 choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                                             "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
                                                             "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                                                 selected = "AC"),
                                     
                                 
                                     actionButton(inputId = "Calcular",
                                                  label = strong("Calcular"),
                                                  width = "95%")
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills", 
                                                    tabPanel("Tabelas", br(),DT::dataTableOutput("table1")),
                                                    tabPanel("Gráficos", br(), plotlyOutput("plotqe")),
                                                    tabPanel("Definição")))))),  
  
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
                            
                           
                            actionButton(inputId = "Calcular",
                                         label = strong("Calcular"),
                                         width = "95%")
                            
                            
        ),
               
               mainPanel(
                 
                 absolutePanel(top = 0, right = 0, left = 100,
                               tabsetPanel(type = "pills",
                                           tabPanel("Tabelas", br(), tableOutput("table")),
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
  
 # Indicadores
  
  indistr <- reactive({
    switch(input$INDICADORES_DISTR,
           "Quociente eleitoral" = "Quociente eleitoral",
           "Quociente partidário" = "Quociente partidário")
  })
  
 # Cargo
    cargo <- reactive({
    switch(input$DESCRICAO_CARGO1,
           "Deputado Federal" = "DEPUTADO FEDERAL",
           "Deputado Estadual" = "DEPUTADO ESTADUAL")
  })
 
 # Uf
    
  uf <- reactive({
    switch(input$UF, 
           "AC" = AC,
           "AL" = AL,
           "AM" = AM,
           "AP" = AP,
           "BA" = BA,
           "CE" = CE,
           "DF" = DF,
           "ES" = ES,
           "GO" = GO,
           "MA" = MA,
           "MG" = MG,
           "MS" = MS,
           "MT" = MT,
           "PA" = PA,
           "PB" = PB,
           "PE" = PE,
           "PI" = PI,
           "PR" = PR,
           "RJ" = RJ,
           "RN" = RN,
           "RO" = RO,
           "RR" = RR,
           "RS" = RS,
           "SC" = SC,
           "SE" = SE,
           "SP" = SP,
           "TO" = TO)
  })
  
  

# 3.1. Tabelas ------------------------------------------------------------

  # Quociente eleitoral
  
  output$table1 <- DT::renderDataTable(qef)
  


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
 }   
  
?plot_ly
# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)

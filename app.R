

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
library(readr)
library(shinythemes)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)


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
                                                 choices = c("Deputado Federal", "Deputado Estadual", "Vereador"),
                                                 selected = "Deputado Federal"),
                                     
                                     selectInput(inputId = "AGREGACAO_REGIONAL",
                                                 label = "Escolha uma agregação regional",
                                                 choices = c("UF", "Município"),
                                                 selected = "UF"),
                                     
                                     
                                     selectInput(inputId = "UF",
                                                 label = "Escolha um estado",
                                                 choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                                             "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
                                                             "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                                                 selected = "AC"),
                                     
                                     uiOutput("AGREG_MUN1"),
                                     
                                     
                                     actionButton(inputId = "Calcular",
                                                  label = strong("Calcular"),
                                                  width = "95%")
                                     
                                     ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills",
                                                    tabPanel("Tabelas", tableOutput("table1")),
                                                    tabPanel("Gráficos"),
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
                                     
                                     selectInput(inputId = "DESCRICAO_CARGO2",
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
                                     
                                     uiOutput("AGREG_MUN2"),
                                     
                                     
                                     actionButton(inputId = "Calcular",
                                                  label = strong("Calcular"),
                                                  width = "95%")),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills",
                                                    tabPanel("Tabelas", tableOutput("table")),
                                                    tabPanel("Gráficos"),
                                                    tabPanel("Definição")))))),        
             
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
                                     
                                     actionButton(inputId = "Calcular",
                                                  label = strong("Calcular"),
                                                  width = "95%")),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills",
                                                    tabPanel("Tabelas", tableOutput("table")),
                                                    tabPanel("Gráficos"),
                                                    tabPanel("Definição")))))),
                                     
             
             tabPanel("Alienação",
             
             sidebarLayout(
               
               sidebarPanel(h4("Opções"),width = 3,
                            
                            
                            selectInput(inputId = "INDICADORES_ALIE",
                                        label = "Escolha um indicador", 
                                        choices = "Alienação",
                                        selected = "Alienação"),
                            
                            selectInput(inputId = "DESCRICAO_CARGO",
                                        label = "Escolha um cargo",
                                        choices = c("Deputado Federal", "Deputado Estadual", "Vereador"),
                                        selected = "Deputado Federal"),
                            
                            selectInput(inputId = "AGREGACAO_REGIONAL",
                                        label = "Escolha uma agregação regional",
                                        choices = c("Brasil", "UF", "Municipio"),
                                        selected = "Brasil"),
                            
                            
                            selectInput(inputId = "UF",
                                        label = "Escolha um estado",
                                        choices = c("Todos os estados","AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG",
                                                    "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", 
                                                    "RS", "SC", "SE", "SP", "TO"),
                                        selected = "Todos os estados"),
                            
                           
                            actionButton(inputId = "Calcular",
                                         label = strong("Calcular"),
                                         width = "95%")
                            
                            
               ),
               
               mainPanel(
                 
                 absolutePanel(top = 0, right = 0, left = 100,
                               tabsetPanel(type = "pills",
                                           tabPanel("Tabelas", tableOutput("table")),
                                           tabPanel("Gráficos"),
                                           tabPanel("Definição"))
                               
                 )))
               
               
             )))





# 3. Server ---------------------------------------------------------------



server <- function(input, output,session){
  
  # Agregacao regional
  
  agreg <- reactive({
    cargo <- input$DESCRICAO_CARGO1
    if(cargo == "Vereador"){
      return(input$AGREG_MUN1)
    } 
  })
  
  output$AGREG_MUN1 <- renderUI({
    cargo <- input$DESCRICAO_CARGO1
    if(cargo == "Vereador"){
      selectizeInput("AGREG_MUN1",
                     label = "Escolha um município",
                     choices = cidades[[3]],
                     selected = NULL)  
    }
  })
  
  
  observe({
    req(cidades)
    trigger <- input$x
    updateSelectizeInput(session, 
                         'AGREG_MUN1',choices = cidades[[3]],
                         selected = NULL, server = TRUE)
  })
  
  
  agreg <- reactive({
    cargo <- input$DESCRICAO_CARGO2
    if(cargo == "Vereador"){
      return(input$AGREG_MUN2)
    } 
  })
  
  output$AGREG_MUN2 <- renderUI({
    cargo <- input$DESCRICAO_CARGO2
    if(cargo == "Vereador"){
      selectizeInput("AGREG_MUN2",
                     label = "Escolha um município",
                     choices = cidades[[3]],
                     selected = NULL)  
    }
  })
  
  
  obse
  
  observe({
    req(cidades)
    trigger <- input$x
    updateSelectizeInput(session, 
                         'AGREG_MUN2',choices = cidades[[3]],
                         selected = NULL, server = TRUE)
  })
 
  # Calculo
  
  q <- reactive({
    
    INDICADORES_FRAG <- switch(input$INDICADORES_FRAG,
                               "Quociente eleitoral" = "QUOCIENTE_ELEITORAL")
    
  })  
  
  output$table1 <- renderTable(expr = vags_fed, striped = TRUE, bordered = TRUE, align = "c", spacing = "m", width = 100)
  
}  





# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)
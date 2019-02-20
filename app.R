

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

# 1. Data ----------------------------------------------------------------

#source("script_dados.R")

# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  

 navbarPage("CepespIndicadores", theme = shinytheme("flatly"),
 
 tabPanel("Sobre"),
 
 tabPanel("Distribuição de cadeiras"),
 
 tabPanel("Fragmentação legislativa"),
 
 tabPanel("Renovação das bancadas"),
          
 tabPanel("Alienação"),
 
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
             choices = c("Deputado Federal", "Deputado Estadual", "Vereador"),
             selected = "Deputado Federal"),
 
 selectInput(inputId = "AGREGACAO_REGIONAL",
             label = "Escolha uma agregação regional",
             choices = c("Brasil", "UF", "Municipio"),
             selected = "Brasil"),
 

 selectInput(inputId = "UF",
             label = "Escolha um estado",
             choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG",
                         "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", 
                         "RS", "SC", "SE", "SP", "TO"),
             selected = "AC"),
 
 uiOutput("AGREG_MUN"),
 
 actionButton(inputId = "Calcular",
              label = strong("Calcular"),
              width = "95%")
 
 
 ),
 
 mainPanel(
   
   absolutePanel(top = 0, right = 0, left = 100,
     tabsetPanel(type = "pills",
                tabPanel("Tabelas",h4("Quociente eleitoral"), tableOutput("table")),
                tabPanel("Gráficos"),
                tabPanel("Definição"))
   
   ))
 
 
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
  
  
  output$INDICADORES_FRAG <- renderUI({
    selectizeInput("INDICADORES_FRAG",
                   label = "Escolha um indicador", 
                   choices = c("Desproporcionalidade de Gallagher", "Fracionalização", "Fracionalização máxima",
                         "Fragmentação", "Número efetivo de partidos", "Quociente eleitoral"),
                   selected = "Desproporcionalidade de Gallagher")  
  })
  
 
  output$INDICADORES_REN <- renderUI({
    selectizeInput("INDICADORES_REN",
                   label = "Escolha um indicador",
                   choices = c("Conservação", "Renovação bruta", "Renovação líquida",
                               "Volatilidade eleitoral"),
                   selected = "Conservação")  
  })
  
  # Calculo
  
  q <- reactive({
    
    INDICADORES_FRAG <- switch(input$INDICADORES_FRAG,
                               "Quociente eleitoral" = "QUOCIENTE_ELEITORAL")
    
  })  
  
  output$table <- renderTable(expr = QE, striped = TRUE, bordered = TRUE, align = "c", spacing = "m", width = 100)
  
  }  





# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)


# Shiny dos indicadores CepespData
# Autor: Rebeca Carvalho

rm(list = ls())

# Pacotes utilizados

library(cepespR)
library(knitr)
library(tidyverse)
library(lubridate)
library(shiny)
library(plyr)
library(shinyalert)
library(shinyBS)
library(ggplot2)
library(shiny)
library(readr)
library(shiny)
library(shinythemes)


# 1. Data ----------------------------------------------------------------

df <- read.csv("df.csv")

de <- read.csv("de.csv")

# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  
 navbarPage("CepespIndicadores", theme = shinytheme("flatly"),
 
 tabPanel("Fragmentação legislativa", theme = shinytheme("flatly")),
 
 tabPanel("Renovação das bancadas"),
 
 tabPanel("Alienação"),
 
 tabPanel("Sobre")),
 
 absolutePanel(top = 60, left = 10, right = "auto", bottom = "auto",
               width = 260, height = "auto", draggable = FALSE, fixed = TRUE,
               h4("Opções:"),
      selectInput(inputId = "DESCRICAO_CARGO",
                  label = "Escolha um cargo",
                  choices = c("Deputado Federal", "Deputado Estadual", "Vereador"),
                  selected = "Deputado Federal"),
      
      uiOutput("ANO_EST"),
      
      uiOutput("ANO_MUN"),
      
      uiOutput("AGREG_EST"),
      
      uiOutput("AGREG_MUN"),
      
      selectInput("INDICADORES",
                  label = "Escolha um indicador",
                  choices = c("Desproporcionalidade de Gallagher", "Fracionalização", "Fracionalização máxima",
                              "Fragmentação", "Número efetivo de partidos", "Quociente eleitoral"),
                  selected = "Desproporcionalidade de Gallagher")
    
    ))




# 3. Server ---------------------------------------------------------------



  server <- function(input, output,session){
  
# Ano da eleicao
  
  ano <- reactive({
    cargo <- input$DESCRICAO_CARGO
    if(cargo == "Vereador"){
    return(ouput$ANO_MUN)
  } else {
    return(output$ANO_EST)
  }
    
  })
  
  output$ANO_MUN <- renderUI({
    cargo <- input$DESCRICAO_CARGO
    if(cargo == "Vereador"){
    selectizeInput("ANO_MUN", 
                    label = "Escolha um ano",
                    choices = c(2000,2004,2008,2012,2016),
                    selected = 2000)
  }
    
  })
     
  output$ANO_EST <- renderUI({
    cargo <- input$DESCRICAO_CARGO
    if(cargo %in% c("Deputado Federal", "Deputado Estadual")){
    selectizeInput("ANO_EST",
                   label = "Escolha um ano",
                   choices = c(1998,2002,2006,2010,2014),
                   selected = 1998)
  }
  })
  

# Agregacao regional

  agreg <- reactive({
   cargo <- input$DESCRICAO_CARGO
   if(cargo == "Vereador"){
   return(input$AGREG_MUN)
  } else {
   return(output$AGREG_EST)
  }
  })
  
  output$AGREG_MUN <- renderUI({
    cargo <- input$DESCRICAO_CARGO
    if(cargo == "Vereador"){
    selectizeInput("AGREG_MUN",
                   label = "Escolha um município",
                   choices = )  
  }
  })
  
  output$AGREG_EST <- renderUI({
    cargo <- input$DESCRICAO_CARGO
    if(cargo %in% c("Deputado Federal", "Deputado Estadual")){
    selectizeInput("AGREG_EST",
                   label = "Escolha um estado",
                   choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG",
                   "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC",
                   "SE", "SP", "TO"),
                   selected = "AC")  
  }
  })
  }
  


# 4. ShinyApp -------------------------------------------------------------


shinyApp(ui = ui, server = server)
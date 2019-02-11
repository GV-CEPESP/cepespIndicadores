

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

# 1. Data ----------------------------------------------------------------

df <- read.csv("df.csv")

de <- read.csv("de.csv")

cidades <- read.csv("cidades.csv")

# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  
 navbarPage("CepespIndicadores", theme = shinytheme("flatly"),
 
 tabPanel("Fragmentação legislativa"),
 
 tabPanel("Renovação das bancadas"),
          
 tabPanel("Alienação"),
 
 tabPanel("Sobre"),
 
 absolutePanel(top = 60, left = 10, right = "auto", bottom = "auto",
               width = 260, height = "auto", draggable = FALSE, fixed = TRUE,
              h4("Opções:"),
 selectInput(inputId = "DESCRICAO_CARGO",
             label = "Escolha um cargo",
             choices = c("Deputado Federal", "Deputado Estadual", "Vereador"),
             selected = "Deputado Federal"),
 
 uiOutput("ANO_EST"),
 
 uiOutput("ANO_MUN"),
 
 
 selectInput(inputId = "SIGLA_UE",
             label = "Escolha um estado",
             choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG",
                         "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", 
                         "RS", "SC", "SE", "SP", "TO"),
             selected = "AC"),
 
 uiOutput("AGREG_MUN"),
 
 selectInput(inputId = "INDICADORES_FRAG",
             label = "Escolha um indicador", 
             choices = c("Desproporcionalidade de Gallagher", "Fracionalização", "Fracionalização máxima",
                         "Fragmentação", "Número efetivo de partidos", "Quociente eleitoral"),
             selected = "Desproporcionalidade de Gallagher")
 
 
 ),
 
 mainPanel()
 
 
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
                    selected = 2016)
  }
  })
     
  output$ANO_EST <- renderUI({
    cargo <- input$DESCRICAO_CARGO
    if(cargo %in% c("Deputado Federal", "Deputado Estadual")){
    selectizeInput("ANO_EST",
                   label = "Escolha um ano",
                   choices = c(1998,2002,2006,2010,2014),
                   selected = 2014)
  }
  })
  

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
  }


# Calculo

 output$cal_qe <- renderTable({
   
 })



# 4. ShinyApp -------------------------------------------------------------


shinyApp(ui = ui, server = server)
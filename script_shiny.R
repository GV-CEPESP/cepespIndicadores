

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

# 1. User interface -------------------------------------------------------


ui <- fluidPage(
  
  # Titulo do app ----
  
 navbarPage("CepespIndicadores"),
 
 tabPanel("Indicadores de Fragmentação Legislativa"),
 
 absolutePanel(top = 60, left = 10, right = "auto", bottom = "auto",
               width = 260, height = "auto", draggable = FALSE, fixed = TRUE,
               h4("Opções:"),
      selectInput(inputId = "DESCRICAO_CARGO",
                  label = "Escolha um cargo:",
                  choices = c("Deputado Federal", "Deputado Estadual", "Vereador"),
                  selected = "Deputado Federal"),
      
      selectInput(inputId = "ANO_ELEICAO",
                  label = "Escolha um ano:",
                  choices = c(1998,2002,2006,2010,2014,2018),
                  selected = 1998),
      
      selectInput(inputId = "SIGLA_UE",
                  label = "Escolha um Estado:",
                  choices = c("AC","AM","AL","AP","BA","CE","DF","ES","GO","MA",
                              "MS","MG","MT","PA","PB","PE","PI","PR","RJ","RN",
                              "RO","RR","RS","SC","SE","SP","TO"),
                  selected = "AC"),
      
      selectInput("INDICADORES",
                  label = "Escolha um indicador:",
                  choices = c("Desproporcionalidade de Gallagher", "Fracionalização", "Fracionalização máxima",
                              "Fragmentação", "Número efetivo de partidos", "Quociente eleitoral"),
                  selected = "Desproporcionalidade de Gallagher")
    
    ),
    
        mainPanel(
      plotOutput("graph")
    )
  )




# 2. Server ---------------------------------------------------------------



server <- function(input, output,session) {
  
  
  output$text <- renderPlot({
    ggplot(df, mapping = aes(x = input$ANO_ELEICAO, y = df$VOTOS_VALIDOS/513)) +
      geom_density()
  }
  
  
  
  )
  
}


# 3. ShinyApp -------------------------------------------------------------


shinyApp(ui = ui, server = server)
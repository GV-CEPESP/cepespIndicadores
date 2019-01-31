

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


# 1. Data ----------------------------------------------------------------

df <- read.csv("df.csv")

de <- read.csv("de.csv")

# 1. User interface -------------------------------------------------------


ui <- fluidPage(
  
  # Titulo do app ----
  
  titlePanel("Cepesp Indicadores"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "DESCRICAO_CARGO",
                  label = "Escolha um cargo:",
                  choices = c("Deputado Federal", "Deputado Estadual", "Vereador"),
                  selected = "Deputado Federal"),
      
      selectInput(inputId = "AGREGACAO_REGIONAL",
                  label = "Escolha uma agregação regional:",
                  choices = c("Brasil", "UF", "Municipio"),
                  selected = "Brasil"),
      
      selectInput(inputId = "ANO_ELEICAO",
                  label = "Escolha um ano:",
                  choices = c(1998,2002,2006,2010,2014,2018),
                  selected = 1998),
      
      selectInput(inputId = "SIGLA_UE",
                  label = "Escolha um Estado:",
                  choices = c("AC","AM","AL","AP","BA","CE","DF","ES","GO","MA",
                              "MS","MG","MT","PA","PB","PE","PI","PR","RJ","RN",
                              "RO","RR","RS","SC","SE","SP","TO"),
                  selected = "AC")
    ),
    
    
    mainPanel(
      plotOutput("graph")
    )
  )
)



# 2. Server ---------------------------------------------------------------



write.csv(df, "df.csv")

server <- function(input, output) {
  
  output$graph <- renderPlot({
    ggplot(df, mapping = aes(x = input$ANO_ELEICAO, y = input$DESCRICAO_CARGO)) +
      geom_bar(stat = "identity")
  }
  
  
  
  )
  
}


# 3. ShinyApp -------------------------------------------------------------


shinyApp(ui = ui, server = server)
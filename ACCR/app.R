#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Test pour création d'une Shiny App (basé sur tuto https://mastering-shiny.org/basic-app.html)
library(shiny)
library(ggplot2)
library(tidyverse)
library(readr)

ACCR <- read_delim("~/Desktop/ProjectR/DataTestPerso/AccidentsRoute/ACCR2018.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

ui <- fluidPage(
    selectInput("accidents", "Type de véhicule", ACCR$catv),
    tableOutput("table")
)

server <- function(input, output, session) {
    accidents <- reactive({
        get(input$accidents)
    })
    output$table <- renderTable({
        accidents()
    })
}

# Reprendre à la session 2.8
shinyApp(ui, server)

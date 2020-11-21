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

#Define UI ----
ui <- fluidPage(
    titlePanel("Corporate Finance"),
    sidebarLayout(
        sidebarPanel(
            h2("Calcul du coût global du crédit"),
            br(),
            img(src = "finance.png", height = 70, width = 250, align = "center"),
            br(),
            numericInput(inputId = "C",
                          label = "Capital Emprunté :",
                          value = 0),
            numericInput(inputId = "D",
                         label = "Durée (en années) :",
                         value = 0),
            numericInput(inputId = "Tx",
                         label = "Taux :",
                         value = 0)
        ),
        mainPanel(mainPanel(h3(textOutput("CoutCredit"))
            
        )
    )
))

# Define server logic ----
server <- function(input, output, session) {
    output$CoutCredit <- renderPrint({
firstF <- input$Tx / 1-(1+input$Tx)^-input$D
results <- firstF * input$C
results  
})
}

# Run the App ----
shinyApp(ui, server)

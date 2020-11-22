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

#Define UI ----
ui <- fluidPage(
    titlePanel("Corporate Finance"),
    sidebarLayout(
        sidebarPanel(
            h2("Calcul du coût de l'annuité constante"),
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
                         label = "Taux (ex : si 1% entrer 1 et non 0,01) :",
                         value = 0)
        ),
        mainPanel(
            h3(textOutput("CoutAnnuite"),
               br(),
                  h3(textOutput("CoutCredit"),
                     br(),
                     h3(textOutput("CoutInterets"))
            
        )
    )
)))

# Define server logic ----
server <- function(input, output, session) {
    output$CoutAnnuite <- renderText({
firstF <- (input$Tx/100) / (1-(1+input$Tx/100)^-input$D)
secondF <- input$C
results <- firstF * secondF
paste("Le coût de l'annuité constante s'élève à ", results)
    })
    
    output$CoutCredit <- renderText({
firstF <- (input$Tx/100) / (1-(1+input$Tx/100)^-input$D)
secondF <- input$C
results <- firstF * secondF
thirdF <- results * input$D
paste("Le coût total du crédit s'élève à ", thirdF)
    })
    
    output$CoutInterets <- renderText({
firstF <- (input$Tx/100) / (1-(1+input$Tx/100)^-input$D)
secondF <- input$C
results <- firstF * secondF
thirdF <- results * input$D
fourthF <- thirdF - input$C
paste("Le coût total des intérêts s'élèvent à ", fourthF)
    })
}

# Run the App ----
shinyApp(ui, server)
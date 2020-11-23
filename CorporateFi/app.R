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
library(shinythemes)
library(ggplot2)
library(tidyverse)

#Define UI ----
ui = fluidPage(theme = shinytheme("superhero"),
    navbarPage("Corporate Finance",
    tabPanel("Crédit",
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
        )
    )
    ),
    tabPanel("Perpetuity",
             sidebarLayout(
                 sidebarPanel(
                     h2("Perpetuity"),
                     br(),
                     img(src = "finance.png", height = 70, width = 250, align = "center"),
                     br(),
                     numericInput(inputId = "AmountPerpetuity",
                                  label = "Montant de la perpétuité :",
                                  value = 0),
                     numericInput(inputId = "TxPerpetuity",
                                  label = "Taux (ex : si 1% entrer 1 et non 0,01) :",
                                  value = 0),
                 ),
                 mainPanel(
                     h3(textOutput("MontantPerpetuite"))
                        )
                     )
                 )
             )
)

            


# Define server logic ----
server <- function(input, output, session) {
    output$CoutAnnuite <- renderText({
firstF <- (input$Tx/100) / (1-(1+input$Tx/100)^-input$D)
secondF <- input$C
results <- firstF * secondF
results <- format(round(results, 2), nsmall = 2)
paste("Le coût de l'annuité constante s'élève à ", results)
    })
    
    output$CoutCredit <- renderText({
firstF <- (input$Tx/100) / (1-(1+input$Tx/100)^-input$D)
secondF <- input$C
results <- firstF * secondF
thirdF <- results * input$D
thirdF <- format(round(thirdF, 2), nsmall = 2)
paste("Le coût total du crédit s'élève à ", thirdF)
    })
    
    output$CoutInterets <- renderText({
firstF <- (input$Tx/100) / (1-(1+input$Tx/100)^-input$D)
secondF <- input$C
results <- firstF * secondF
thirdF <- results * input$D
fourthF <- thirdF - input$C
fourthF <- format(round(fourthF, 2), nsmall = 2)
paste("Le coût total des intérêts s'élèvent à ", fourthF)
    })
    
    output$MontantPerpetuite <- renderText({
results2 <- input$AmountPerpetuity / (input$TxPerpetuity/100)
results2 <- format(results2, scientific = FALSE)
paste("Le montant de la perpétuité s'élève à ", results2)
    })
}

# Run the App ----
shinyApp(ui, server)

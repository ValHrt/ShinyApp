#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(FinancialMath)

#Define UI ----
ui = fluidPage(theme = shinytheme("superhero"),
    navbarPage("Corporate Finance",
    tabPanel("Crédit",{
    sidebarLayout(
        sidebarPanel(
            h2("Calcul du coût de l'annuité constante"),
            br(),
            img(src = "annuite_constante.png", height = 70, width = 250, align = "center"),
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
    )}
    ),
    
    tabPanel("Perpetuity",{
             sidebarLayout(
                 sidebarPanel(
                     h2("Calcul de la valeur de la perpétuité"),
                     br(),
                     img(src = "perpetuity.png", height = 70, width = 250, align = "center"),
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
                     )}
    ),
    
    tabPanel("Effective Annual Rate",{
             sidebarLayout(
                 sidebarPanel(
                     h2("Calcul du Taux Annuel Effectif"),
                     br(),
                     img(src = "ear.png", height = 70, width = 350, align = "center"),
                     br(),
                     numericInput(inputId = "CompoundingPeriods",
                                  label = "Durée en années (ou en mois) :",
                                  value = 0),
                     numericInput(inputId = "TxEAR",
                                  label = "Taux (ex : si 1% entrer 1 et non 0,01) :",
                                  value = 0),
                 ),
                 mainPanel(
                     h3(textOutput("EAR"))
                 )
             )}
    ),
    
    tabPanel("Discounting",{
             sidebarLayout(
                 sidebarPanel(
                     h2("Discounting calcul (pour passer de Future Value à Present Value)"),
                     br(),
                     img(src = "discounting.png", height = 70, width = 250, align = "center"),
                     br(),
                     numericInput(inputId = "FutureValue",
                                  label = "Valeur de la Future Value :",
                                  value = 0),
                     numericInput(inputId = "TxDiscounting",
                                  label = "Taux (ex : si 1% entrer 1 et non 0,01) :",
                                  value = 0),
                     numericInput(inputId = "discountingPeriods",
                                  label = "Durée en années (ou en mois) :",
                                  value = 0)
                 ),
                 mainPanel(
                     h3(textOutput("Discounting"))
                 )
             )}
    ),
    
    tabPanel("Capitalisation",{
             sidebarLayout(
                 sidebarPanel(
                     h2("Capitalisation calcul (pour passer de Present Value à Future Value)"),
                     br(),
                     img(src = "capitalisation.png", height = 70, width = 250, align = "center"),
                     br(),
                     numericInput(inputId = "PresentValue1",
                                  label = "Valeur de la Present Value :",
                                  value = 0),
                     numericInput(inputId = "TxCapitalisation",
                                  label = "Taux (ex : si 1% entrer 1 et non 0,01) :",
                                  value = 0),
                     numericInput(inputId = "capitalisationPeriods",
                                  label = "Durée en années (ou en mois) :",
                                  value = 0)
                 ),
                 mainPanel(
                     h3(textOutput("Capitalisation"))
                 )
             )}
    ),
    
    tabPanel("IRR",{
             sidebarLayout(
                 sidebarPanel(
                     h2("Calcul de l'IRR"),
                     br(),
                     img(src = "IRR.png", height = 70, width = 250, align = "center"),
                     br(),
    textInput('vec1', 'Entrer les cashflows (en séparant par une virgule)', "0,1,2,3"),
    numericInput(inputId = "InvestIRR",
                 label = "Valeur de l'investissement de départ (ne pas mettre de -) :",
                 value = 0),
    textInput('vec2', 'Entrer les périodes (en séparant par une virgule)', "1,2,3,4")
                 ),
    mainPanel(
        h3(verbatimTextOutput("IRR1")),
        h5("Dans la case 'Entrer les périodes', il faut indiquer 
           1,2,3,4 si vous avez 4 périodes. Dans le cas où il y aurait
           2 cashflows sur une même période, par exemple en année 1,
           il faut alors indiquer 1,1,2,3,4 pour les périodes.", align = "center"),
    )
    
             )}
    ),
    
    tabPanel("NPV",{
             sidebarLayout(
                 sidebarPanel(
                     h2("Calcul de la NPV"),
                     br(),
                     img(src = "NPV.png", height = 70, width = 250, align = "center"),
                     br(),
                     textInput('vec3', 'Entrer les cashflows (en séparant par une virgule)', "0,1,2,3"),
                     numericInput(inputId = "InvestNPV",
                                  label = "Valeur de l'investissement de départ (ne pas mettre de -) :",
                                  value = 0),
                     textInput('vec4', 'Entrer les périodes (en séparant par une virgule)', "1,2,3,4"),
                     numericInput(inputId = "TxNPV",
                                  label = "Taux (ex : si 1% entrer 1 et non 0,01) :",
                                  value = 0)
                 ),
                 mainPanel(
                     h3(verbatimTextOutput("NPV1")),
                     h5("Dans la case 'Entrer les périodes', il faut indiquer 
           1,2,3,4 si vous avez 4 périodes. Dans le cas où il y aurait
           2 cashflows sur une même période, par exemple en année 1,
           il faut alors indiquer 1,1,2,3,4 pour les périodes.", align = "center"),
                 )
             )}
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
    
    output$EAR <- renderText({
TxEAR2 <- input$TxEAR / 100        
results3 <- (1+TxEAR2/input$CompoundingPeriods)^input$CompoundingPeriods - 1
results3 <- results3 * 100
results3 <- format(results3, scientific = FALSE)
paste("Le taux annuel effectif s'élève à ", results3, "%")
    })
    
    output$Discounting <- renderText({
TxDiscounting2 <- input$TxDiscounting / 100
results4 <- input$FutureValue / (1+TxDiscounting2)^input$discountingPeriods
results4 <- format(results4, scientific = FALSE)
paste("La Present Value vaut ", results4)
    })
    
    output$Capitalisation <- renderText({
TxCapitalisation2 <- input$TxCapitalisation / 100
results5 <- input$PresentValue1 * (1+TxCapitalisation2)^input$capitalisationPeriods
results5 <- format(results5, scientific = FALSE)
paste("La Future Value vaut ", results5)
    })
    
    output$IRR1 <- renderText({
vec1x <- as.numeric(unlist(strsplit(input$vec1,",")))
vec2x <- as.numeric(unlist(strsplit(input$vec2,",")))
results6 <- IRR(input$InvestIRR, vec1x, vec2x)
results6 <- results6 * 100
paste("L'IRR vaut ", results6, "%")
    })
    
    output$NPV1 <- renderText({
TxNPV1 <- input$TxNPV / 100
vec3x <- as.numeric(unlist(strsplit(input$vec3,",")))
vec4x <- as.numeric(unlist(strsplit(input$vec4,",")))
results7 <- NPV(input$InvestNPV, vec3x, vec4x, TxNPV1)
paste("La NPV vaut ", results7)
    })
}

# Run the App ----
shinyApp(ui, server)

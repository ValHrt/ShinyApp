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

datasets <- data(package = "ggplot2")$results[c(2, 4, 10), "Item"]

ui <- fluidPage(
    selectInput("dataset", "Dataset", choices = datasets),
    verbatimTextOutput("summary"),
    plotOutput("plot")
)

server <- function(input, output, session) {
    dataset <- reactive({
        get(input$dataset, "package:ggplot2")
    })
    output$summary <- renderPrint({
        summary(dataset())
    })
    output$plot <- renderPlot({
        plot(dataset())
    }, res = 96)
    
    
}

# Reprendre à la session 2.8
shinyApp(ui, server)

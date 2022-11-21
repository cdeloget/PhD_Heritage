#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source(file = "PhD_Heritage_MAIN.r")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$text <- renderText({print(input$motcle)})

    })


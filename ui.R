#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
 
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("PhD_Heritage"),
    
      textInput("discipline", label="Saisir une discipline"),
      textInput("motcle", label="Saisir un mot clé"),
    
      actionButton("request", label = "Requête sur theses.fr"),
    
      textOutput("text")
    
    )
)

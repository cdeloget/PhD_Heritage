#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel("PhD Heritage"),
  
  fluidRow(
    
    column(3,
      textInput("discipline", label="Discipline : "),
      textInput("motcle", label = "Mots clés : "),
      sliderInput("nbpages", label = "Nombre de pages à requêter : ", min = 1, max= 10, value = 1, ticks = F),
      h1(" "),
      selectInput("degres", label = "Degrés de parenté", choices = c("1", "2")),
      actionButton("displaygraphe", label="Lancer la recherche")
    ),
    
    column(9,
      tabsetPanel(
        tabPanel("Chronologie", 
                 plotOutput("chrono")),
        tabPanel("Graphe",
                 visNetworkOutput("graphe")
      )
    )
  )
  )  
))

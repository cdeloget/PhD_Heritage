#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    source(file = "../PhD_Heritage_MAIN.r", local = FALSE)
  
 
    
    observeEvent(input$displaygraphe, {
      theses_liens_from_json <- phd_request_json(input$discipline, input$motcle)
      output$chrono <- renderPlot({
        #pour affichage histogramme
        hist(year(theses_liens_from_json$dateSoutenance), breaks = length(year(theses_liens_from_json$dateSoutenance)), xlab = "année de soutenance", ylab="nombre de thèses soutenances", main='Chronogramme', col="darkblue", border="white")
        
      })
      
      
      
      output$graphe <- renderVisNetwork({
        multipage_theses_liens <- phd_request_n_pages(discipline_recherchee = input$discipline, 
                                                      motcles_recherche = input$motcle, nb_pages = input$nbpages)
        
        network_plus <- get_connections_from_results(multipage_theses_liens, distance = as.integer(input$degres))
        
        LIENS <- network_plus[,c(5,3)]
        nom_temp <- str_split(LIENS$DIR, " ", 2, simplify=T)[,2]
        prenom_tmp <- substring(str_split(LIENS$DIR, " ", 2,simplify=T)[,1], first=1, last=1)
        LIENS$DIR <- paste(prenom_tmp, nom_temp, sep=".")
        
        nom_temp <- str_split(LIENS$AUTEUR, " ", 2,simplify=T)[,2]
        prenom_tmp <- substring(str_split(LIENS$AUTEUR, " ", 2,simplify=T)[,1], first=1, last=1)
        LIENS$AUTEUR <- paste(prenom_tmp, nom_temp, sep=".")
        
        colnames(LIENS) <- c("from", "to")
        LIENS <- LIENS %>% mutate(arrows = "to")
        pers <- as.data.frame(cbind(LIENS$from, LIENS$to))
        pers <- unique(pers)
        noeuds <- unique(as.data.frame(c(pers$V1, pers$V2)))
        colnames(noeuds) <- "id"
        noeuds <- noeuds %>% mutate(label=id)
        
        visNetwork(nodes = noeuds, edges = LIENS)
      })
      })
      
      
    
    
  

})

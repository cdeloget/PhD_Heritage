#-----------------------------------------------------------------------
#-----------------------------Chargement des packages------------------
#----------------------------------------------------------------------

# install.packages(c("tidyverse", "igraph", "rvest", "pingr"))

library(tidyverse)
library(lubridate)
library(igraph)
library(rvest)
library(pingr)


#--------------------------------------------------------------------
#-------------------------- SCRIPT PRINCIPAL -------------------------
#--------------------------------------------------------------------

discipline <- readline("Discipline : ") #ex : Taper Geographie

motcle <- readline("mot clé ? : ") #ex : Taper "Saint-Julien" ou "mobilités" (taper "%20" à la place des espaces...)

url_base <- paste("https://theses.fr/fr/?q=",motcle,"&checkedfacets=discipline=",discipline, sep="") #création de la requête http get

print(url_base) #vérif de l'url

page_accueil <- read_html(url_base) #requete et récup du code html

resultats <- page_accueil %>% html_nodes("div#resultat") #on récup sur la page la div contenant les résultats de la recherche

infos_theses <- resultats %>% html_nodes("div.informations") #on récup dans les résultats les div contenant les infos de chaque thèse

infos_tmp <- infos_theses %>% html_text() %>% data.frame(RESULTS = .) #on convertir le html en texte pour affichage test
result_tmp #affichage pour vérif


#dans la div resultats on récup les dates (petit encart à droite du nom de la thèse)
dates_theses <- resultats %>% html_nodes("h5.soutenue") %>% html_text()
dates_theses <- str_split(string=dates_theses, pattern=" ", simplify=TRUE)[,3]
dates_theses <- dates_theses %>% str_sub(., -4, -1) %>% as.integer()


#on récupère la discipline
discipline_theses <- resultats %>% html_nodes("div.domaine") %>% html_node("h5") %>% html_text()




#-----------------------------------------------------------------------
#-----------------------------Chargement des packages------------------
#----------------------------------------------------------------------

# install.packages(c("tidyverse", "igraph", "rvest", "pingr", "httr", "jsonlite", "sf", "geojsonsf"))

library(tidyverse)#dplyr, pipe etc. HAdley Wickham
library(igraph)#pour faire des graphes
library(rvest)#recup du contenu html distant et parser les noeuds html
# library(pingr)
library(httr)#requetes http sur api
library(jsonlite)#manipuler des json
library(sf)
library(geojsonsf)#convertir un geojson en sf sur R


#-------------------------------------------------------
#----------------Variables globales----------------------
#-----------------------------------------------------------
langue <- "fr"

#------------------------------------------------------------------
#------------------ FONCTIONS -------------------------------------
#-----------------------------------------------------------------


get_resultats <- function(discipline, motcles){

  motcles <- motcles %>% str_replace_all(" ", "%20")
    
  url_base <- paste("https://theses.fr/fr/?q=",motcles,"&status=status:soutenue&checkedfacets=discipline=",discipline, sep="") #création de la requête http get
  
  print(url_base) #vérif de l'url
  
  page_accueil <- read_html(url_base) #requete get et récup du code html
  
  result_recherche <- page_accueil %>% html_nodes("div#resultat") #on récup sur la page la div contenant les résultats de la recherche
  return(result_recherche)
}

build_phd_table <- function(results, export=T){
  infos_theses <- resultats %>% html_nodes("div.informations") #on récup dans les résultats les div contenant les infos de chaque thèse
  
  infos_tmp <- infos_theses %>% html_text() %>% data.frame(RESULTS = .) #on convertir le html en texte pour affichage test
  infos_tmp$RESULTS #affichage des intitulés pour vérif
  
  
  
  #dans la div resultats on récup les dates (petit encart à droite du nom de la thèse)
  dates_theses <- resultats %>% html_nodes("h5.soutenue") %>% html_text() #récupération du contenue du petit encart soutenue à droite, dans un titre h5 de classe "soutenue" (texte vert sur le site)
  dates_theses <- str_split(string=dates_theses, pattern=" ", simplify=TRUE)[,3] #on split le texte pour ne garder que la date
  dates_theses <- dates_theses %>% str_sub(., -4, -1) %>% as.integer()#on ne garde que l'année, convertie en nombre entier
  
  #on récupère la discipline
  discipline_theses <- resultats %>% html_nodes("div.domaine") %>% html_node("h5") %>% html_text() #nom de la discipline dans une div de classe "domaine" (puis titre h5) dans l'encart à droite, convertie ensuite en texte
  
  #on récupère les noms, qui sont dans un lien dans un titre h2
  noms_theses <- infos_theses %>% html_nodes("h2") %>% html_text() %>% str_replace_all("\r\n", "")
  
  #on récupère l'auteur
  auteurs_theses <- infos_theses %>% html_nodes("p") %>% html_text()
  auteurs_theses <- str_split(auteurs_theses, pattern="\r\n", simplify = TRUE)[,1]
  auteurs_theses <- auteurs_theses %>% str_replace("par ", "") %>% str_to_title(locale=langue)
  #id de l'auteur #premier lien a du paragraphe p
  id_auteur <- infos_theses %>% html_node("p a:nth-child(1)") %>% html_attr("href") %>% substr(2,nchar(.))
  
  #on récupère l'encadrant numéro 1 et l'université de soutenance
  #nom du directeur
  dir_theses <- infos_theses %>% html_nodes("p") %>% html_text()
  dir_theses <- str_split(dir_theses, pattern="sous la direction de", simplify=TRUE)
  dir_theses <- dir_theses[,2] %>% str_replace_all("\r\n", "") %>% str_replace_all(" \r\n", "") %>% substr(x=.,start=2, stop=nchar(.))
  directeur_theses <- str_split(dir_theses, pattern=" - ", simplify=TRUE)[,1]
  directeur_theses <- str_split(directeur_theses, pattern=" et de ", simplify=TRUE)[,1]
  #id du dirthese #deuxième lien a du paragraphe p
  id_dirtheses <- infos_theses %>% html_nodes("p a:nth-child(2)") %>% html_attr("href") %>% substr(2,nchar(.)) %>% str_replace_all("fr/", "")
  #univ du directeur
  univ_theses <- str_split(dir_theses, pattern=" - ", simplify=TRUE)[,2] %>% substr(x=.,start=1, stop=nchar(.)-2)
  
  LIENS <- data.frame(ID_AUTEUR= id_auteur, AUTEUR=auteurs_theses, ANNEE= dates_theses, ID_DIR=id_dirtheses, DIR=directeur_theses, UNIV_DIR= univ_theses, INTITULE = noms_theses, DISCIPLINE = discipline_theses)
  
  
  if (export==T){#si utilisateur a choisi export en csv :
    write.csv(x = LIENS, file='LIENS.csv')
  }
  
  return(LIENS)#on retourne un df de toute les soutenances

}


#---------------------------------------------------------------------
#----------------------- ESSAIS ---------------------------------------
#------------------------------------------------------------------


# geocode_from_a_column <- function(univ_tab, geoname_field){ #fonction à laquelle on passe le tableau, le champ du nom des université, et qui va effectuer un géocodage avec l'API BAN de la DINUM pour chacune d'entre elle
nom_tab <- deparse(substitute(theses_liens))
champ_a_traiter <- paste(nom_tab, geoname_field, sep="$")#on reconstitue en chaine de caractères la bonne colonne du tableau
champ_a_traiter <- parse(text=champ_a_traiter)#on la transforme en expression
noms_univ <- eval(champ_a_traiter) #executable avec eval(), dont on stocke le résultat dans une variable noms_univ
noms_univ_complet <- paste("université", noms_univ)

url_apiban <- paste("https://api-adresse.data.gouv.fr/search/?limit=1&q=",noms_univ[2], sep="")
adresse <- fromJSON(httr::content(#on lit le contenu
  httr::GET(url_apiban),
  type="json",
  as="text"
))
bibi <- as.data.frame(adresse$features$properties)


#--------------------------------------------------------------------
#-------------------------- SCRIPT PRINCIPAL -------------------------
#--------------------------------------------------------------------


discipline_saisie <- readline("Discipline : ") #ex : Taper "Geographie"

motcles_saisis <- readline("mots clés ? : ")#ex : Taper "Thérèse Saint-Julien" ou "mobilités ferroviairew" 

resultats <- get_resultats(discipline_saisie, motcles_saisis)#on va requeter theses.fr et renvoyer le code html contenant les resultats de la recherche sur theses.fr
theses_liens <- build_phd_table(resultats)#recup des informations importantes et mis en forme dans un tableau df
# geocode_from_a_column(theses_liens, "UNIV_DIR")#ajout d'un geocodage d'une des colonne de notre choix (ici l'université de soutenance)



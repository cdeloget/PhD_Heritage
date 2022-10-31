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
library(mapview)
library(geojsonsf)#convertir un geojson en sf sur R


#-------------------------------------------------------
#----------------Variables globales----------------------
#-----------------------------------------------------------
langue <- "fr"


#------------------------------------------------------------------
#------------------ FONCTIONS -------------------------------------
#-----------------------------------------------------------------


get_resultats <- function(discipline, motcles){
  ###Fonction qui construit l'url de theses.fr avec la discipline et les mots clés passés en paramètres. Le code HTML qui contient les résultats est retourné

  motcles <- motcles %>% str_replace_all(" ", "%20")
    
  url_base <- paste("https://theses.fr/fr/?q=",motcles,"&status=status:soutenue&checkedfacets=discipline=",discipline, sep="") #création de la requête http get
  print("URL de la requête : ")
  print(url_base) #vérif de l'url
  print("Requête en cours")
  page_accueil <- read_html(url_base) #requete get et récup du code html
  print("requête OK")
  result_recherche <- page_accueil %>% html_nodes("div#resultat") #on récup sur la page la div contenant les résultats de la recherche
  return(result_recherche)
}



build_phd_table <- function(results, export=T){
  infos_theses <- resultats %>% html_nodes("div.informations") #on récup dans les résultats les div contenant les infos de chaque thèse
  
  infos_tmp <- infos_theses %>% html_text() %>% data.frame(RESULTS = .) #on convertir le html en texte pour affichage test
  infos_tmp$RESULTS #affichage des intitulés pour vérif
  
  
  print("récupération de l'année de soutenance...")
  #dans la div resultats on récup les dates (petit encart à droite du nom de la thèse)
  dates_theses <- resultats %>% html_nodes("h5.soutenue") %>% html_text() #récupération du contenue du petit encart soutenue à droite, dans un titre h5 de classe "soutenue" (texte vert sur le site)
  dates_theses <- str_split(string=dates_theses, pattern=" ", simplify=TRUE)[,3] #on split le texte pour ne garder que la date
  dates_theses <- dates_theses %>% str_sub(., -4, -1) %>% as.integer()#on ne garde que l'année, convertie en nombre entier
  print("récupération de la discipline et des titres des thèses...")
  #on récupère la discipline
  discipline_theses <- resultats %>% html_nodes("div.domaine") %>% html_node("h5") %>% html_text() #nom de la discipline dans une div de classe "domaine" (puis titre h5) dans l'encart à droite, convertie ensuite en texte
  
  #on récupère les noms, qui sont dans un lien dans un titre h2
  noms_theses <- infos_theses %>% html_nodes("h2") %>% html_text() %>% str_replace_all("\r\n", "")
  
  print("Infos sur l'auteur...")
  #on récupère l'auteur
  auteurs_theses <- infos_theses %>% html_nodes("p") %>% html_text()
  auteurs_theses <- str_split(auteurs_theses, pattern="\r\n", simplify = TRUE)[,1]
  auteurs_theses <- auteurs_theses %>% str_replace("par ", "") %>% str_to_title(locale=langue)
  #id de l'auteur #premier lien a du paragraphe p
  id_auteur <- infos_theses %>% html_node("p a:nth-child(1)") %>% html_attr("href") %>% substr(2,nchar(.))
  
  print("infos sur le directeur de thèse...")
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
  
  print("Mise en forme dans un data frame")
  LIENS <- data.frame(ID_AUTEUR= id_auteur, AUTEUR=auteurs_theses, ANNEE= dates_theses, ID_DIR=id_dirtheses, DIR=directeur_theses, UNIV_DIR= univ_theses, INTITULE = noms_theses, DISCIPLINE = discipline_theses)
  
  
  if (export==T){#si utilisateur a choisi export en csv :
    print("export en csv")
    write.csv(x = LIENS, file='LIENS.csv')
  }
  print("OK")
  return(LIENS)#on retourne un df de toute les soutenances

}


#---------------------------------------------------------------------
#----------------------- FONCTIONS EXPERIMENTALES  --------------------
#------------------------------------------------------------------

geocode_phds_from_column <- function(table, champ_a_traiter){
  write.csv(table, "table_a_geocoder.csv")#la table passée en paramètre est exportée en csv
  
  ###utilisation de l'API BAN de l'Etat FR pour géocoder un CSV qui est envoyé en méthode POST dans le paramètre data, avec un paramètre columns qui spécifie la colonne sur laquelle doit se baser le geocodage. result_columns permet de filtrer les colonnes souhaitées en résultat. Voir https://adresse.data.gouv.fr/api-doc/adresse
  print("geocodage en cours...")
  url_apiban <- "https://api-adresse.data.gouv.fr/search/csv/"
  raw_response_content <- content(#content permet de ne récupérer que le contenu de la reposne, débarassée des en-têtes et autres infos
    POST(
      url = url_apiban,#url où faire la requete
      body = list(data=upload_file("table_a_geocoder.csv"), columns=champ_a_traiter, result_columns="latitude", result_columns="longitude", result_columns="result_city"),
      verbose()#affichage des infos requete en console
    ),
    as = "raw",
    content_type="text/csv"#on precise que la réponse est un document csv
  )
#par défaut, le csv est encodé en hexadecimal : on le convertit en chaines de caractères classique
response_content <- rawToChar(raw_response_content)

write_lines(response_content, file="table_geocoded.txt", sep="\n")#le texte brut du csv est exporté ligne par ligne (les lignes sont séparées par "\n")

geocoded_data <- read.csv(file="table_geocoded.txt", encoding = "UTF-8")#on lit le fichier texte comme s'il s'agissait d'un csv. On le récupère donc en dataframe
print(geocoded_data)
geocoded_data <- geocoded_data %>% st_as_sf(coords=c("longitude", "latitude"), crs="EPSG:4326")
print("OK")
return(geocoded_data)
}

#--------------------------------------------------------------------
#-------------------------- SCRIPT PRINCIPAL -------------------------
#--------------------------------------------------------------------


discipline_saisie <- readline("Discipline : ") #ex : Taper "Geographie"

motcles_saisis <- readline("mots clés ? : ")#ex : Taper "Thérèse Saint-Julien" ou "mobilités ferroviairew" 

resultats <- get_resultats(discipline_saisie, motcles_saisis)#on va requeter theses.fr et renvoyer le code html contenant les resultats de la recherche sur theses.fr
theses_liens <- build_phd_table(resultats)#recup des informations importantes dans le code html et les met en forme dans un tableau df
View(theses_liens)
theses_lien_geocoded <- geocode_phds_from_column(theses_liens, "UNIV_DIR")

mapview(theses_lien_geocoded)



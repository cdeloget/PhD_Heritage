#-----------------------------------------------------------------------
#-----------------------------Chargement des packages------------------
#----------------------------------------------------------------------

# install.packages(c("tidyverse", "igraph", "rvest", "pingr", "httr", "jsonlite", "sf", "geojsonsf"))

library(tidyverse)#dplyr, pipe etc. Hadley Wickham le boss
# library(dplyr)
# library(magrittr)
# library(stringr)
library(igraph)#pour faire des graphes
library(data.table)
library(rvest)#recup du contenu html distant et parser les noeuds html
library(xml2)
library(xmlconvert)
# library(pingr)
library(httr)#pour requetes http
library(jsonlite)#manipuler des json
library(sf)
library(mapview)
library(lubridate)
# library(geojsonsf)#convertir un geojson en sf sur R


#-------------------------------------------------------
#----------------Variables globales----------------------
#-----------------------------------------------------------
langue <- "fr"

#------------------------------------------------------------------
#------------------ FONCTIONS -------------------------------------
#-----------------------------------------------------------------

#------Récupération des résultats de recherche sur theses.fr-----------

build_phd_url <- function(discipline, motcles){
  ###Fonction qui construit l'url de theses.fr avec la discipline et les mots clés passés en paramètres. Le code HTML qui contient les résultats est retourné
  
  motcles <- motcles %>% str_replace_all(" ", "%20")
  
  url_base <- paste("https://theses.fr/fr/?q=",motcles,"&status=status:soutenue&checkedfacets=discipline=",discipline, sep="") #création de la requête http get
}

get_resultats <- function(url_base){
  #fonction qui effectue la requete et retourne le résultat html brut
  print("URL de la requête : ")
  print(url_base) #vérif de l'url
  print("Requête en cours")
  page_accueil <- read_html(url_base) #requete get et récup du code html
  print("requête OK")
  result_recherche <- page_accueil %>% html_nodes("div#resultat") #on récup sur la page la div contenant les résultats de la recherche
  return(result_recherche)
}


#-----------Construction d'un tableau de données à partir du résultat html---------------


build_phd_table <- function(results, export=F){
  infos_theses <- results %>% rvest::html_nodes("div.informations") #on récup dans les résultats les div contenant les infos de chaque thèse

  print(paste("INFOS THESES", infos_theses)) #affichage des intitulés pour vérif
  
  
  #print("récupération de l'année de soutenance...")
  #dans la div resultats on récup les dates (petit encart à droite du nom de la thèse)
  # dates_theses <- results %>% html_nodes("h5.soutenue") %>% html_text() #récupération du contenue du petit encart soutenue à droite, dans un titre h5 de classe "soutenue" (texte vert sur le site)
  # dates_theses <- str_split(string=dates_theses, pattern=" ", simplify=TRUE)[,3] #on split le texte pour ne garder que la date
  # dates_theses <- dates_theses %>% str_sub(., -4, -1) %>% as.integer()#on ne garde que l'année, convertie en nombre entier
  print("récupération de la discipline et des titres des thèses...")
  #on récupère la discipline
  discipline_theses <- results %>% html_nodes("div.domaine") %>% html_node("h5") %>% html_text() #nom de la discipline dans une div de classe "domaine" (puis titre h5) dans l'encart à droite, convertie ensuite en texte
  id_theses <- infos_theses %>% html_node("h2 a") %>% html_attr("href") %>% as.character()
  #on récupère les noms, qui sont dans un lien dans un titre h2
  noms_theses <- infos_theses %>% html_nodes("h2") %>% html_text() %>% str_replace_all("\r\n", "")
  
  print("récupération de l'auteur, du directeur de thèse et de l'université de soutenance")
  ids_dirtheses <- NULL
  ids_auteurs <- NULL
  auteurs_theses <- NULL
  dirtheses <- NULL
  univs_theses <- NULL
  ids_univs <- NULL
  
  for (elem in infos_theses){
    print(paste("ELEM", elem))
    info_these <- elem %>% html_nodes("p a")
    print(info_these)
    print(length(info_these))
    if (length(info_these) == 2){
      auteur_these <- elem %>% html_node("p") %>% html_text()
      auteur_these <- str_split(auteur_these, pattern="\r\n", simplify = TRUE)[,1]
      auteur_these <- auteur_these %>% str_replace("par ", "") %>% str_replace("par  ", "") %>% str_to_title(locale=langue)
      id_auteur <- NA
      dir_these <- elem %>% html_node("p a:nth-child(1)") %>% html_text()
      id_dirthese <- elem %>% html_node("p a:nth-child(1)") %>% html_attr("href") %>% str_replace_all("/", "") %>% str_replace_all("fr", "")
      univ_these <- elem %>% html_node("p a:last-child") %>% html_attr("href") %>% str_replace_all(" ", "")
      id_univ <- elem %>% html_node("p a:last-child")%>% html_attr("href") %>% str_replace_all("/", "")
      
    } else if(length(info_these) >= 3){
      
      auteur_these <- elem %>% html_node("p a:nth-child(1)") %>% html_text()
      id_auteur <- elem %>% html_node("p a:nth-child(1)") %>% html_attr("href") %>% str_replace_all("/", "") %>% str_replace_all("fr", "")
      dir_these <- elem %>% html_node("p a:nth-child(2)") %>% html_text()
      id_dirthese <- elem %>% html_node("p a:nth-child(2)") %>% html_attr("href") %>% str_replace_all("/", "") %>% str_replace_all("fr", "")
      univ_these <- elem %>% html_node("p a:last-child") %>% html_text() %>% str_replace_all(" ", "")
      id_univ <- elem %>% html_node("p a:last-child") %>% html_attr("href") %>% str_replace_all("/", "")
    }
    
    ids_dirtheses <- c(ids_dirtheses, id_dirthese)
    dirtheses <- c(dirtheses, dir_these)
    auteurs_theses <- c(auteurs_theses, auteur_these)
    ids_auteurs <- c(ids_auteurs, id_auteur)
    univs_theses <- c(univs_theses, univ_these)
    ids_univs <- c(ids_univs, id_univ)
    
  }
  print("Mise en forme dans un data frame")
  LIENS <- data.frame(ID_THESE= id_theses, ID_AUTEUR= ids_auteurs, AUTEUR=auteurs_theses, ID_DIR=ids_dirtheses, DIR=dirtheses, UNIV_ID= ids_univs, UNIV_DIR= univs_theses, INTITULE = noms_theses, DISCIPLINE = discipline_theses)
  
  
  if (export==T){#si utilisateur a choisi export en csv :
    print("export en csv")
    write.csv(x = LIENS, file='LIENS.csv')
  }
  print("OK")
  #return(LIENS)#on retourne un df de toute les soutenances
  return(LIENS)

}



#meme chose que get_results() mais en utlisant API. la reponse est au format json

phd_request_json <- function(disc, keyword){
  keyword <- keyword %>% str_replace_all(" ", "%20")
  disc_prep <- paste("discipline", disc, sep = "=")
  print("Requête sur API en cours...")
  result <- content(
    GET(url = "https://www.theses.fr/fr/", path=langue, query=list(q = keyword, format = "json", checkedfacets = disc_prep)),
    as="raw",
    content_type("application/json")
  )
  print("Requête OK")
  #print(result)
  print("Conversion du résultat (json brut vers un dataframe)...")
  result_txt <- rawToChar(result)
  result_txt <- str_conv(result_txt, "UTF-8")
  #print(result_txt)
  resultat_df <- jsonlite::fromJSON(result_txt)
  #print(resultat_df)
  resultats_finaux <- resultat_df$response$docs
  print("OK")
  return(resultats_finaux)
}




#---------------------------------------------------------------------
#----------------------- FONCTIONS EXPERIMENTALES  --------------------
#------------------------------------------------------------------


#------géocodage à partir du nom de l'université de soutenance, en utilisant l'API BAN----

geocode_phds_from_column <- function(table, champ_a_traiter){
  table_a_geocoder <- table %>% select(id, champ_a_traiter)
  write.csv(table_a_geocoder, "table_a_geocoder.csv")#la table passée en paramètre est exportée en csv
  ###utilisation de l'API BAN de l'Etat FR pour géocoder un CSV qui est envoyé en méthode POST dans le paramètre data, avec un paramètre columns qui spécifie la colonne sur laquelle doit se baser le geocodage. result_columns permet de filtrer les colonnes souhaitées en résultat. Voir https://adresse.data.gouv.fr/api-doc/adresse
  print("geocodage en cours...")
  url_apiban <- "https://api-adresse.data.gouv.fr/search/csv/"
  raw_response_content <- content(#content permet de ne récupérer que le contenu de la reponse, débarassée des en-têtes et autres infos
    POST(
      url = url_apiban,#url où faire la requete
      body = list(data=upload_file("table_a_geocoder.csv"), 
                  columns=champ_a_traiter, 
                  result_columns="latitude", 
                  result_columns="longitude", 
                  result_columns="result_city"
                  ),
      verbose()#affichage des infos requete en console
    ),
    as = "raw",
    content_type="text/csv"#on precise que la réponse est un document csv
  )
  #par défaut, le csv est encodé en hexadecimal : on le convertit en chaines de caractères classique
  response_content <- rawToChar(raw_response_content)
  file_con <- file("table_geocoded.txt")
  writeLines(response_content, con=file_con, sep="\n")#le texte brut du csv est exporté ligne   par ligne (les lignes sont séparées par "\n")
  close(con = file_con)
  geocoded_data <- read.csv(file="table_geocoded.txt", encoding = "UTF-8")#on lit le fichier texte comme s'il s'agissait d'un csv. On le récupère donc en dataframe
  print("transformation en spatial feature...")
  geocoded_data <- geocoded_data %>% filter(!is.na(latitude) | !is.na(longitude)) %>% st_as_sf(coords = c("longitude", "latitude"), crs="EPSG:4326")
  geocoded_data$id <- as.character(geocoded_data$id)
  print("jointure avec table de base")
  geocoded_data_sf <- left_join(x=table, y=geocoded_data, by=c("id" = "id")) %>% st_as_sf()

  print(geocoded_data_sf)
  print("OK")
  return(geocoded_data_sf)
}



get_dirthese_from_results <- function(url, phd_table){
  
  #debut de tentative de boucles pour constituer une matrice de liens (branche boucles sur GIT)
  url_session <- session(url) #on crée une session de navigation à partir de l'url de la recherche initiale
  #liens <- resultats %>% html_nodes("div.informations p a") %>% html_attr("href") #on peut récupérer les liens des directeurs de thèses depuis la recheche en scrapping html
  id_dirtheses <- phd_table$ID_DIR #ou bien les récup depuis le résultat en json
  
  for (i in seq(1,length(id_dirtheses))){ #on vérifie visuellement que les liens sont bien ok
    print(id_dirtheses[i])
  }
  
  dirtheses_info_tot <- data.frame() #on crée un df vide qui contiendra les résultats
  for (i in seq(1, length(id_dirtheses))){ #pour chaque dir these
    # on va, grace à la session de nav, sur sa page grace à son id
    session_dirtheses <- url_session %>% session_jump_to(paste("https://theses.fr", id_dirtheses[i], sep="/"))
    dirtheses <- read_html(session_dirtheses$url)#on récup le contenu html de sa page
    motcles_dirtheses <- dirtheses %>% html_node("div#nuages") %>% html_text() #on isole les keywords du dirthese
    nom_dirtheses <-  dirtheses %>% html_node("h1") %>% html_text() #on isole son nom
    print(paste("Lien numéro ", i, "Id :", id_dirtheses[i], "Nom :", nom_dirtheses, sep=" "))
    dirtheses_info <- data.frame(ID=id_dirtheses[i], NOM = nom_dirtheses, MOTCLE = motcles_dirtheses) #on met toutes ses données dans un df temporaire
    dirtheses_info_tot <- rbind(dirtheses_info_tot, dirtheses_info) #qu'on ajoute au df global crée avant la boucle
  }
  return(dirtheses_info_tot)
}


get_childs_from_results <- function(url, phd_table){
  
  #debut de tentative de boucles pour constituer une matrice de liens (branche boucles sur GIT)
  url_session <- session(url) #on crée une session de navigation à partir de l'url de la recherche initiale
  #liens <- resultats %>% html_nodes("div.informations p a") %>% html_attr("href") #on peut récupérer les liens des directeurs de thèses depuis la recheche en scrapping html
  id_fils <- phd_table$ID_AUTEUR #ou bien les récup depuis le résultat en json
  
  for (i in seq(1,length(id_fils))){ #on vérifie visuellement que les liens sont bien ok
    print(id_fils[i])
  }
  
  fils_info_tot <- data.frame() #on crée un df vide qui contiendra les résultats
  for (i in seq(1, length(id_fils))){ #pour chaque dir these
    # on va, grace à la session de nav, sur sa page grace à son id
    session_fils <- url_session %>% session_jump_to(paste("https://theses.fr", id_fils[i], sep="/"))

    fils <- read_html(session_fils$url)#on récup le contenu html de sa page
    motcles_fils <- fils %>% html_node("div#nuages") %>% html_text() #on isole les keywords du dirthese
    nom_fils <-  fils %>% html_node("h1") %>% html_text() #on isole son nom
    print(paste("Lien numéro ", i, "Id :", id_fils[i], "Nom :", nom_fils, sep=" "))
    fils_info <- data.frame(ID=id_fils[i], NOM = nom_fils, MOTCLE = motcles_fils) #on met toutes ses données dans un df temporaire
    fils_info_tot <- rbind(fils_info_tot, fils_info) #qu'on ajoute au df global crée avant la boucle
  }
  return(fils_info_tot)
  
}


get_phds_from_persons_df <- function(PERSONS_DF){
  
  df_final <- data.frame()
  for (i in seq(1,length(PERSONS_DF$ID))){
    id_auteur_a_tester <- PERSONS_DF$ID[i]
    if (id_auteur_a_tester == ""){
      next
    }
    
    #AJouter un test de la div
    url_tmp <- paste("https://theses.fr/", id_auteur_a_tester, "#directeurSoutenue", sep="")
    result_test_url <- read_html(paste("https://theses.fr/", id_auteur_a_tester, sep="")) %>% html_node("#directeurSoutenue")
    if(is.na(result_test_url)){
      next
    } else {
    test_results <- get_resultats(url_base = url_tmp)
    test_table <- build_phd_table(results = test_results, export = F)
    
    print(test_table_dir)
    #test_table_fils$AUTEUR <- str_split(test_table_fils$AUTEUR, pattern=" ", simplify = T)[,2]
    test_table_dir <- test_table %>% filter(ID_DIR == id_auteur_a_tester)
    print(test_table_dir)
    df_final <- rbind(encadre, test_table)
    }
    }
  return(df_final)
  
}


#--------------------------------------------------------------------
#-------------------------- SCRIPT PRINCIPAL -------------------------
#--------------------------------------------------------------------
# 
# 
# discipline_saisie <- readline("Discipline : ") #ex : Taper "Geographie"
# 
# motcles_saisis <- readline("mots clés ? : ")#ex : Taper "Thérèse Saint-Julien" ou "mobilités ferroviaires" 
# 
# url <- build_phd_url(discipline_saisie, motcles_saisis)
# 
# resultats <- get_resultats(url)#on va requeter theses.fr et renvoyer le code html contenant les resultats de la recherche sur theses.fr
# 
# theses_liens <- build_phd_table(resultats)#recup des informations importantes dans le code html et les met en forme dans un tableau df
# 
# #View(theses_liens)
# 
# 
# 
# #resultat depuis API en json et non depuis un scrapping degueu (inachevé)
# theses_liens_from_json <- phd_request(discipline_saisie, motcles_saisis)
# 
# hist(year(theses_liens_from_json$dateSoutenance), breaks = length(year(theses_liens_from_json$dateSoutenance)), xlab = "année de soutenance", ylab="nombre de soutenances")
# 
# # theses_liensJSON_geocoded <- geocode_phds_from_column(theses_liens_from_json, "etabSoutenance")
# # class(theses_liensJSON_geocoded)
# # mapview(theses_liensJSON_geocoded)
# 
# ######----------------------bac à merde------------------------------
# 
# ENFANTS <- get_childs_from_results(url=url, theses_liens)
# PARENTS <- get_dirthese_from_results(url=url, theses_liens)
# 
# noeuds_tmp <- rbind(ENFANTS, PARENTS)
# 
# 
# theses_encadrees_par_doctorant <- get_phds_from_persons_df(ENFANTS)
# theses_encadrees_par_directeurs <- get_phds_from_persons_df(PARENTS)
# 
# 
# encadre <- encadre[,c(4,2,1,3 )]
# 
# plot.igraph(graph_from_data_frame(encadre), arrow.size=0.25, edge.arrow.size=0.05, vertex.size=0.5, vertex.label=encadre$AUTEUR, vertex.label.cex=0.7, curved=T,
#             layout=layout_with_kk(graph_from_data_frame(encadre)))
# 
# 
# 
# 
# read_html("https://theses.fr/027090248") %>% html_node("#directeurSoutenue")

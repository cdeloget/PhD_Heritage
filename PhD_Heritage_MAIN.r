
###############################################################################
###############################################################################
########################### PHD HERITAGE ######################################
############################ C Deloget ########################################
###############################################################################
##############################################################################




#-----------------------------------------------------------------------
#-----------------------------Chargement des packages------------------
#----------------------------------------------------------------------

# install.packages(c("tidyverse", "igraph", "rvest", "pingr", "httr", "jsonlite", "sf", "geojsonsf"))

library(tidyverse)#dplyr, pipe etc. Hadley Wickham le boss
# library(dplyr)
# library(magrittr)

# library(stringr)

library(stringr)

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
library(data.table)
library(RWDataPlyr)

library(ape)
 
library(alakazam)

# library(geojsonsf)#convertir un geojson en sf sur R


#-------------------------------------------------------
#----------------Variables globales----------------------
#-----------------------------------------------------------
langue <- "fr"

#-------------------------------------------------------------
#------------------ Lecture des données --------------------
#------------------------------------------------------------

data_theses <- fread("theses-soutenues.csv", encoding = "UTF-8") %>% filter(status == "soutenue") %>% 
  select(nnt,
         auteurs.0.idref, 
         auteurs.0.nom, 
         auteurs.0.prenom, 
         directeurs_these.0.idref,
         directeurs_these.0.nom,
         directeurs_these.0.prenom,
         etablissements_soutenance.0.idref,
         etablissements_soutenance.0.nom, titres.fr, discipline.fr, date_soutenance) %>% mutate(AUTEUR = paste(auteurs.0.prenom, auteurs.0.nom),
                                                     DIR = paste(directeurs_these.0.prenom, directeurs_these.0.nom), ANNEE = year(date_soutenance)) %>% select(-auteurs.0.prenom, -auteurs.0.nom, -directeurs_these.0.nom, -directeurs_these.0.prenom, date_soutenance)

data_theses <- data_theses[,c(1,2,9,3, 10, 4, 5, 6, 7, 11)]

colnames(data_theses) <- c("ID_THESE", "ID_AUTEUR", "AUTEUR", "ID_DIR","DIR", "UNIV_ID", "UNIV_DIR", "INTITULE", "DISCIPLINE", "ANNEE")

class(data_theses)

#------------------------------------------------------------------
#------------------ FONCTIONS -------------------------------------
#-----------------------------------------------------------------


########################################################################"
################# Fonctions utilisées ##################################

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

  return(url_base)
}


get_resultats <- function(url_ba, nb_pages = 1){
  #fonction qui effectue la requete et retourne le résultat html brut, et le fait pour un nombre n de pages de recherches

    print("URL de la requête : ")
    print(url_ba) #vérif de l'url
    print("Requête en cours")
    page_accueil <- read_html(url_ba) #requete get et récup du code html
    print("requête OK")
    result_recherche <- page_accueil %>% html_nodes("div#resultat") #on récup sur la page la div contenant les résultats de la recherche

  return(result_recherche)
}


#---Construction d'un tableau de données à partir du résultat html---


build_phd_table <- function(results, export=F){
  infos_theses <- results %>% rvest::html_nodes("div.informations") #on récup dans les résultats les div contenant les infos de chaque thèse

  #print(paste("INFOS THESES", infos_theses)) #affichage des intitulés pour vérif
  
  
  #print("récupération de l'année de soutenance...")
  #dans la div resultats on récup les dates (petit encart à droite du nom de la thèse)
  # dates_theses <- results %>% html_nodes("h5.soutenue") %>% html_text() #récupération du contenue du petit encart soutenue à droite, dans un titre h5 de classe "soutenue" (texte vert sur le site)
  # dates_theses <- str_split(string=dates_theses, pattern=" ", simplify=TRUE)[,3] #on split le texte pour ne garder que la date
  # dates_theses <- dates_theses %>% str_sub(., -4, -1) %>% as.integer()#on ne garde que l'année, convertie en nombre entier
  print("récupération de la discipline et des titres des thèses...")
  #on récupère la discipline
  discipline_theses <- results %>% html_nodes("div.domaine") %>% html_node("h5") %>% html_text() #nom de la discipline dans une div de classe "domaine" (puis titre h5) dans l'encart à droite, convertie ensuite en texte
  id_theses <- infos_theses %>% html_node("h2 a") %>% html_attr("href") %>% as.character()

  id_theses <- str_replace_all(id_theses, "/", "")

  id_theses <- str_replace_all(id_theses, "/", "")

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
    info_these <- elem %>% html_nodes("p a")

    print(paste("elem", elem))

    #print(paste("elem", elem))

    #print(paste("elem", elem))

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
  LIENS <- data.frame(ID_THESE= id_theses, ID_AUTEUR= ids_auteurs, AUTEUR=auteurs_theses, ID_DIR=ids_dirtheses, DIR=dirtheses, UNIV_ID= ids_univs, UNIV_DIR= univs_theses, INTITULE = noms_theses, DISCIPLINE = discipline_theses, ANNEE = "")
  
  
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
    GET(url = "https://www.theses.fr", path=langue, query=list(q = keyword, format = "json", checkedfacets = disc_prep)),
    as="raw",
    content_type("application/json")
  )
  print("Requête OK")
  print(result)
  print("Conversion du résultat (json brut vers un dataframe)...")
  result_txt <- rawToChar(result)
  result_txt <- str_conv(result_txt, "UTF-8")
  print(result_txt)
  resultat_df <- jsonlite::fromJSON(result_txt)
  #print(resultat_df)
  resultats_finaux <- resultat_df$response$docs
  print("OK")
  return(resultats_finaux)
}

phd_request_n_pages <- function(discipline_recherchee, motcles_recherche, nb_pages=1){
  url_m <- build_phd_url(discipline_recherchee, motcles_recherche)
  #url_base <- build_phd_url("Géographie", "mobilités")
  pages_requetees <- 0
  start_n <- 0
  result_final <- data.frame()
  
  while(pages_requetees < nb_pages){
    url_search <- paste(url_m, "&start=", as.character(start_n), sep="")
    #print(url_search)
    result_intermed <- build_phd_table(get_resultats(url_ba = url_search))
    result_final <- rbind(result_final, result_intermed)
    pages_requetees <- pages_requetees + 1
    start_n <-  start_n + 10
  }
  return(result_final)
}


#-----------Récupération des infos sur les personnes et les thèses apparues en résultat--------
get_persons_keyword_from_id <- function(df, id_field){
  pers_info_tot <- data.frame()
  df_sel <- df[,id_field]
  df_sel <- df_sel[!is.na(df_sel)]
  print(length(df_sel))
  for (pers in df_sel){
    if(pers == ""){
      next
    }
    #print(pers)
    url_pers <- paste("https://theses.fr/fr/", pers, sep="")
    result_pers <- rvest::read_html(url_pers)
    motcles <- result_pers %>% html_node("div#nuages") %>% html_text()
    nom_pers <-  result_pers %>% html_node("h1") %>% html_text()
    pers_info <- data.frame(ID=pers, NOM=nom_pers, MOTCLE = motcles)
    pers_info_tot <- rbind(pers_info_tot, pers_info)
  }
  return(pers_info_tot)
}


get_phds_from_persons_df <- function(data_gen, persons_df, persons_id = "ID", person_role){
  if(person_role == "dir" | person_role == "author"){
    tabgen <- data.frame()
    person_id_sel <- persons_df[,persons_id]
    person_id_sel <- person_id_sel[!is.na(person_id_sel)]
   
    if(person_role == "dir"){
      tabsel <- data_gen %>% filter(ID_DIR %in% person_id_sel)
    } else if(person_role == "author"){
      tabsel <- data_gen %>% filter(ID_AUTEUR %in% person_id_sel)
    }
  
      
    
    return(tabsel)
  } else {
    stop("role must be 'dir' or 'author'")
  }
  
  
}

get_first_neighborhood_from_results <- function(results){
  #thèses dirigées par les auteurs des thèses résultats
    liens_tmp <- get_phds_from_persons_df(data_theses, results, "ID_AUTEUR", "dir")
  #thèses écrites par les directeurs des thèses résultats
    liens_tmp <- rbind(liens_tmp, get_phds_from_persons_df(data_theses, results, "ID_DIR", "author"))
  #autres thèses dirigées par les directeurs de theses résultats
    liens_tmp <- rbind(liens_tmp, get_phds_from_persons_df(data_theses, results, "ID_DIR", "dir"))
    
    res.LIENS <- liens_tmp %>% group_by(ID_THESE) %>% summarise_all(first)
    

    
    return(res.LIENS)

    print(paste("Nombre de personnes testées " , as.character(length(res.LIENS)*2)))
    
    noeuds_tmp <- get_persons_keyword_from_id(df = res.LIENS, id_field = "ID_DIR")
    noeuds_tmp <- rbind(noeuds_tmp, get_persons_keyword_from_id(df = res.LIENS, id_field = "ID_AUTEUR"))
    res.NOEUDS <- noeuds_tmp %>% group_by(ID) %>% summarise_all(first)
    
    return(list(res.NOEUDS, res.LIENS))

}


get_connections_from_results <- function(resulta, distance = 2){

  
  first_results <- get_first_neighborhood_from_results(resulta)
  #first_results <- resulta
  
  pers_temp <- first_results
  
  liens_fnl <- data.frame()
  if(distance > 1){
    distance <- distance - 1
    for(i in seq(1,distance)){
      print(paste("passage num�ro : ", i+1))

  first_results <- get_first_neighborhood_from_results(resulta)
  pers_temp <- as.data.frame(first_results[2])
  liens_fnl <- data.frame()
  for(i in seq(1,distance)){
    print(paste("passage numéro : ", i))

      liens_temp <- get_phds_from_persons_df(data_gen = data_theses, persons_df = pers_temp, persons_id = "ID_DIR", person_role = "dir")
      liens_temp <- rbind(liens_temp, get_phds_from_persons_df(data_gen = data_theses, persons_df = pers_temp, persons_id = "ID_AUTEUR", person_role = "dir"))
      liens_temp <- rbind(liens_temp, get_phds_from_persons_df(data_gen = data_theses, persons_df = pers_temp, persons_id = "ID_DIR", person_role = "author"))
      liens_temp <- liens_temp %>% group_by(ID_THESE) %>% summarise_all(first)
      pers_temp <- liens_temp
      liens_fnl <- rbind(liens_fnl, liens_temp)
      

    }
  } else {
    return(first_results)
  }
  
  

  }
  

  liens_fnl <- liens_fnl %>% group_by(ID_THESE) %>% summarise_all(first)
  
  # pers_fnl <- get_persons_keyword_from_id(df = liens_fnl, id_field = "ID_AUTEUR")
  # pers_fnl <- rbind(pers_temp, get_persons_keyword_from_id(df = liens_fnl, id_field = "ID_DIR"))
  # pers_fnl <- pers_fnl %>% group_by(ID, .drop = F) %>% summarise_all(first)
  # 
  resu.LIENS <- liens_fnl %>% filter(!is.null(ID_DIR))
  #resu.NOEUDS <- pers_fnl
  
  #return(list(resu.NOEUDS, resu.LIENS))
  return(resu.LIENS)
  
}



####################################################################
################## Fonctions inutilisées #########################

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
  dirtheses_info_tot <- dirtheses_info_tot %>% group_by(ID) %>% summarise_all(first)
  return(dirtheses_info_tot)
}


get_authors_from_results <- function(url, phd_table){
  
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
  fils_info_tot <- fils_info_tot %>% group_by(ID) %>% summarise_all(first)
  return(fils_info_tot)
  
}



######################################################################
########################FONCTIONS EXPERIMENTALES  ####################



#------géocodage à partir du nom de l'université de soutenance, en utilisant l'API BAN----

geocode_phds_from_column <- function(table, champ_a_traiter){
  table_a_geocoder <- table %>% select(id, champ_a_traiter)
  write.csv(table_a_geocoder, "table_a_geocoder.csv")#la table passée en paramètre est exportée en csv
  ###utilisation de l'API BAN de l'Etat FR pour géocoder un CSV qui est envoyé en méthode POST dans le paramètre data, avec un paramètre columns qui spécifie la colonne sur laquelle doit se baser le geocodage. result_columns permet de filtrer les colonnes souhaitées en résultat. Voir https://adresse.data.gouv.fr/api-doc/adresse
}

geocode_phds_from_column <- function(table, champ_toponyme){
  
  toponymes <- table[,champ_toponyme]
  url_api <- "https://nominatim.openstreetmap.org/search.php?format=jsonv2&q="

  print("geocodage en cours...")
  coords <- data.frame()
  for (tp in toponymes){
   tp <- str_replace_all(tp, " ", "+")
   url_requete <- paste(url_api, "université+", tp, sep="")
  rep <-  httr::content(
            httr::GET(url_requete, verbose()),
            type="application/json", 
            as="text") %>% fromJSON()
  coord <- data.frame(LAT=rep$lat[1], LON=rep$lon[1])
  if(is_empty(coord)){
    coord <- data.frame(LAT="", LON="")
  }
  coords <- rbind(coords, coord)
  print(coord)
  }
  table_result <- cbind(table, coords)
  
  print("OK")
  return(table_result)
}




#--------------------------------------------------------------------
#-------------------------- SCRIPT PRINCIPAL -------------------------
#--------------------------------------------------------------------

#-----EFFECTUER UNE RECHERCHE SUR LE SITE---------------------


discipline_saisie <- readline("Discipline : ") #ex : Taper "Geographie"

motcles_saisis <- readline("mots clés ? : ")#ex : Taper "Thérèse Saint-Julien" ou "mobilités ferroviaires"

url <- build_phd_url(discipline_saisie, motcles_saisis)

resultats <- get_resultats(url)#on va requeter theses.fr et renvoyer le code html contenant les resultats de la recherche sur theses.fr

theses_liens <- build_phd_table(resultats)#recup des informations importantes dans le code html et les met en forme dans un tableau df


multipage_theses_liens$AUTEUR
#theses_liens_geocoded <- geocode_phds_from_column(theses_liens, "UNIV_DIR")
#View(theses_liens)



#resultat depuis API en json et non depuis un scrapping degueu (inachevé)

theses_liens_from_json <- phd_request_json(discipline_saisie, motcles_saisis)
#pour affichage histogramme
hist(year(theses_liens_from_json$dateSoutenance), breaks = length(year(theses_liens_from_json$dateSoutenance)), xlab = "année de soutenance", ylab="nombre de soutenances")

theses_liensJSON_geocoded <- geocode_phds_from_column(theses_liens_from_json, "etabSoutenance")
class(theses_liensJSON_geocoded)
mapview(theses_liensJSON_geocoded)

# theses_liensJSON_geocoded <- geocode_phds_from_column(theses_liens_from_json, "etabSoutenance")
# class(theses_liensJSON_geocoded)
# mapview(theses_liensJSON_geocoded)


######------INFORMATIONS DEPUIS LES RESULTATS---------------------------

##Essai fonctions intermédiaires

#mot cles des directeurs de thèse
infos_directeurs <- get_persons_keyword_from_id(theses_liens, "ID_DIR")

# mot clés des auteurs des thèses
infos_auteurs <- get_persons_keyword_from_id(theses_liens, "ID_AUTEUR")

#thèses où les auteurs ont été directeurs
bibi <- get_phds_from_persons_df(data_theses, theses_liens, "ID_AUTEUR", "dir")

#thèses dont les directeurs ont été les auteurs
bibi <- get_phds_from_persons_df(data_theses, theses_liens, "ID_DIR", "author")


phds <- get_phds_from_persons_df(data_theses, multipage_theses_liens, persons_id = "ID_DIR", person_role = "dir")


geocode_phds_from_column <- function(table, champ_toponyme){
  
  toponymes <- table[,champ_toponyme]
  url_api <- "https://nominatim.openstreetmap.org/search.php?format=jsonv2&q="
  print("geocodage en cours...")
  coords <- data.frame()
  for (tp in toponymes){
   tp <- str_replace_all(tp, " ", "+")
   print(tp)
   url_api <- "https://nominatim.openstreetmap.org/search.php?format=jsonv2&q="
   url_requete <- paste(url_api, "universit�", tp, sep="")
   print(url_requete)
  rep <-  httr::content(
            httr::GET(url=url_requete, verbose()),
            type="application/json", 
            as="text") %>% 
              jsonlite::fromJSON()
  coord <- data.frame(LAT=rep$lat[1], LON=rep$lon[1])
  if(is_empty(coord)){
    coord <- data.frame(LAT="", LON="")
  }
  coords <- rbind(coords, coord)
  print(coord)
  }
  #table_result <- cbind(table, coords)
  
  print("OK")
  return(coords)
}


#-------------CONSTITUTION D'UN RESEAU------------------------------

#récupération de la première page de résultat sur thès.fr
multipage_theses_liens <- phd_request_n_pages(discipline_recherchee = "Géographie", 
                                              motcles_recherche = "mobilités quotidiennes", nb_pages = 1)


#network <- get_first_neighborhood_from_results(multipage_theses_liens)

#récupération du voisinnage à 2 degrés

network_plus <- get_connections_from_results(multipage_theses_liens, distance = 1)


# mis en forme du tableau pour créaion du graphe
LIENS <- as.data.frame(network_plus)[,c(5,3)]
nom_temp <- str_split(LIENS$DIR, " ", 2, simplify=T)[,2]
prenom_tmp <- substring(str_split(LIENS$DIR, " ", 2,simplify=T)[,1], first=1, last=1)
LIENS$DIR <- paste(prenom_tmp, nom_temp, sep=".")

nom_temp <- str_split(LIENS$AUTEUR, " ", 2,simplify=T)[,2]
prenom_tmp <- substring(str_split(LIENS$AUTEUR, " ", 2,simplify=T)[,1], first=1, last=1)
LIENS$AUTEUR <- paste(prenom_tmp, nom_temp, sep=".")

plot(graph_from_data_frame(LIENS, directed = T), arrow.size=0.2, edge.arrow.size=0.2, edge.arrow.fill="red", vertex.size=0.5,vertex.label.cex=0.5, layout = layout_nicely(graph_from_data_frame(LIENS, directed = T)))#,
            #layout=layout_with_kk(graph_from_data_frame(graph_prep)))
# colnames(LIENS) <- c("from", "to")
# library(visNetwork)
# visNetwork(nodes = pers, edges = LIENS)
# ?visNetwork
# data_theses %>% filter(ID_DIR == "035711884")

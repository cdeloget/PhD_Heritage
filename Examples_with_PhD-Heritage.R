
#Appel du script principal et de ses fonctions :

source(file = "PhD_Heritage_MAIN.r", local = TRUE)

#-----EFFECTUER UNE RECHERCHE SUR LE SITE---------------------


discipline_saisie <- readline("Discipline : ") #ex : Taper "G�ographie"

motcles_saisis <- readline("mots cl�s ? : ")#ex : Taper "Th�r�se Saint-Julien" ou "mobilit�s ferroviaires"

url <- build_phd_url(discipline_saisie, motcles_saisis)

resultats <- get_resultats(url)#on va requeter theses.fr et renvoyer le code html contenant les resultats de la recherche sur theses.fr

theses_liens <- build_phd_table(resultats)#recup des informations importantes dans le code html et les met en forme dans un tableau df



#resultat depuis API en json et non depuis un scrapping degueu (inachevé)

theses_liens_from_json <- phd_request_json(discipline_saisie, motcles_saisis)
#pour affichage histogramme
hist(year(theses_liens_from_json$dateSoutenance), breaks = length(year(theses_liens_from_json$dateSoutenance)), xlab = "année de soutenance", ylab="nombre de soutenances")


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



#-------------CONSTITUTION D'UN RESEAU------------------------------

#r�cup�ration de la premi�re page de résultat sur th�ses.fr
multipage_theses_liens <- phd_request_n_pages(discipline_recherchee = "G�ographie", 
                                              motcles_recherche = "analyse spatiale", nb_pages = 1)

disciplines <- unique(data_theses$DISCIPLINE)
#network <- get_first_neighborhood_from_results(multipage_theses_liens)

#r�cup�ration du voisinnage � 2 degr�s

network_plus <- get_connections_from_results(multipage_theses_liens, distance = 2)

# G�ocodage

network_plus_geocoded <- geocode_phds_from_column(table = network_plus, champ_toponyme = "UNIV_DIR")
network_plus_geocoded_sf <- network_plus_geocoded %>% filter(LON != "") %>% st_as_sf(coords=c("LON", "LAT"), crs="EPSG:4326")
mapview(network_plus_geocoded_sf)


# mise en forme du tableau pour création du graphe
LIENS <- network_plus[,c(5,3)]
nom_temp <- str_split(LIENS$DIR, " ", 2, simplify=T)[,2]
prenom_tmp <- substring(str_split(LIENS$DIR, " ", 2,simplify=T)[,1], first=1, last=1)
LIENS$DIR <- paste(prenom_tmp, nom_temp, sep=".")

nom_temp <- str_split(LIENS$AUTEUR, " ", 2,simplify=T)[,2]
prenom_tmp <- substring(str_split(LIENS$AUTEUR, " ", 2,simplify=T)[,1], first=1, last=1)
LIENS$AUTEUR <- paste(prenom_tmp, nom_temp, sep=".")

plot(graph_from_data_frame(LIENS, directed = T), arrow.size=0.2, edge.arrow.size=0.3, edge.arrow.fill="red", vertex.size=0.2,vertex.label.cex=0.5, layout = layout_nicely(graph_from_data_frame(LIENS, directed = T)))#,
#layout=layout_with_kk(graph_from_data_frame(graph_prep)))


colnames(LIENS) <- c("from", "to")
LIENS <- LIENS %>% mutate(arrows = "to")
pers <- as.data.frame(cbind(LIENS$from, LIENS$to))
pers <- unique(pers)
noeuds <- unique(as.data.frame(c(pers$V1, pers$V2)))
colnames(noeuds) <- "id"
noeuds <- noeuds %>% mutate(label=id)
library(visNetwork)
visNetwork(nodes = noeuds, edges = LIENS)
?visHierarchicalLayout()
?visNetwork

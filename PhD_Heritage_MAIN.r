library(tidyverse)
library(igraph)
library(rvest)

discipline <- readline("Discipline : ") #taper "Géographie"
motcle <- readline("mot clé ? : ")

basepage <- paste("https://theses.fr/fr/?q=",motcle,"&checkedfacets=discipline=",discipline, sep="")

print(basepage)

page_accueil <- read_html(basepage)

result_zone <- page_accueil %>% html_nodes("div#resultat") %>% html_nodes("div.informations") %>% html_text() %>% as.data.frame(x=., row.names="TEST")

result_zone$.

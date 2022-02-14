# LIBRAIRIES 

library(rvest)
library(dplyr)
library(stringr)
library(plyr)
library(banR)
library(stringi)


# FONCTIONS

# Fonction qui permet de supprimer les espaces inutiles

trim_string <- function(string)   gsub("\\s+", " ", gsub("^\\s+|\\s+$", "", string))

# Fonction qui permet de formater une partie de l'adresse URL pour récupérer les adresses des banques du Crédit Mutuel

IDville <- function(villes) {
  villes <- trim_string(villes)
  Ville <- str_replace_all(villes," ","%20")
}

#

formatage_code_postal <- function(cp){
  if(nchar(cp) == 4){
    cp <- paste0("0", cp)
  }
  return(cp)
}

#

formatage <- function(str){
  str <- tolower(str)
  str <- gsub(" ", "-", str)
  str <- gsub("'", "-", str)
  str <- stri_trans_general(str, "Latin-ASCII")
  return(str)
}

#

get_villes <- function(region){
  l_region <- paste0("https://www.credit-agricole.fr/particulier/agence/",region,".html")
  p_region <- read_html(l_region)
  villes_region <- p_region %>% html_nodes(".js-alphabetList-item") %>% html_text()
  return(unique(villes_region))
}

#

get_codes_postaux <- function(ville){
  ville <- toupper(gsub("-", " ", ville))
  cp <- codes_postaux[codes_postaux$Nom_commune == ville,]$Code_postal
  return(cp)
}

#

get_adresses <- function(ville, region){
  cp <- get_codes_postaux(ville)
  adresses <- c()
  for(code in cp){
    l_a <- paste0("https://www.credit-agricole.fr/particulier/agence/", region, "/ville/", ville,"-", code, ".html")
    p_a <- read_html(l_a)
    adresse <- p_a %>% html_nodes(".StoreLocatorMap-AgencyAddress") %>% html_text()
    adresses <- c(adresses, adresse)
  }
  return(unique(adresses))
}


# BANQUES


# CREDIT MUTUEL


# Récupération des départements où il a des agences

link <- "https://www.creditmutuel.fr/fr/banques/contact/trouver-une-agence/BrowseSubdivision.aspx"
page <- read_html(link)
departement <- page %>% html_nodes(".c") %>% html_text()

# Suppression des DROM de la liste

departement<-departement[-c(88,89,91,92)]

Rue <-c()
Ville <-c()
Code_postal <-c()

# Récupération des adresses de chaque banque du Crédit Mutuel

for (i in 1:length(departement)){
  link <- paste0("https://www.creditmutuel.fr/fr/banques/contact/trouver-une-agence/BrowseLocality.aspx?SubdivisionId=FR-",departement[i])
  page <- read_html(link)
  
  # Récupération des villes du département
  
  villes <- page %>% html_nodes(".nowrap") %>% html_text()
  villesID <- IDville(villes)
  
  for (j in 1:length(villesID)){
    
    # Récupération des adresses (rue, code postal et ville) autour de chaque ville séléctionnée
    
    link <- paste0("https://www.creditmutuel.fr/fr/banques/contact/trouver-une-agence/SearchList.aspx?sub=true&type=branch&loca=",villesID[j])
    page <- read_html(link)
    adresses <- page %>% html_nodes("em span") %>% html_text()
    
    if(length(adresses>0)){
      indice_rue<- seq(3,length(adresses),5)
      indice_code<- seq(4,length(adresses),5)
      indice_ville<- seq(5,length(adresses),5)
      
      Rue <- c(Rue,adresses[indice_rue])
      Code_postal <- c(Code_postal,adresses[indice_code])
      Ville <- c(Ville,adresses[indice_ville])
    }
  }
}

# Suppression des adresses doublons

Crédit_mutuel <- data.frame(Rue,Code_postal,Ville)
sans_doublons<-unique(Crédit_mutuel[,c(1,3)])
indice_sans_doublon<-as.integer(row.names(sans_doublons))


# Ecriture des adresses dans un data frame

Credit_mutuel_sans_doublon<-data.frame(Banque=rep("Crédit Mutuel",length(indice_sans_doublon)-1),
                                       Type=rep("Coopérative",length(indice_sans_doublon)-1),
                                       Adresse=paste(Crédit_mutuel$Rue[indice_sans_doublon[-76]],
                                                     ", ",
                                                     Crédit_mutuel$Code_postal[indice_sans_doublon[-76]],
                                                     ", ",
                                                     Crédit_mutuel$Ville[indice_sans_doublon[-76]]))


# Correction d'une adresse ne donnant pas de résultat pour obtenir les longitudes et latitudes

Credit_mutuel_sans_doublon[2087,3] <- "9 Pl. Jean de Lattre de Tassigny, 68000 Colmar"

# Récupération des longitudes et latitudes pour chaque adresse

longitude <-c()
latitude<-c()

for(i in 1:length(Credit_mutuel_sans_doublon$Adresse)){
  adr<-Credit_mutuel_sans_doublon$Adresse[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])
}

# Ecriture des longitudes, latitudes et adresses de chaque banque du Crédit Mutuel dans un data frame

credit_mutuel_lng_lat<-data.frame(Banque=Credit_mutuel_sans_doublon$Banque,
                                  Type=Credit_mutuel_sans_doublon$Type,
                                  Adresse=Credit_mutuel_sans_doublon$Adresse,
                                  Longitude=longitude,
                                  Latitude=latitude)

write.csv(Credit_mutuel_sans_doublon,"Crédit_Mutuel.csv",row.names = FALSE)
write.csv(credit_mutuel_lng_lat,"Crédit_Mutuel_lgt_lat.csv",row.names = FALSE)


# BANQUE POPULAIRE


# Récupération des départements en France métropolitaine ainsi que de leurs numéros

link <- "https://www.departements-gouv.fr/"
page <- read_html(link)
liste_departements <- page %>% html_nodes("td") %>% html_text()
liste_departements <- paste0(tolower(liste_departements[seq(2,192,2)]),"-",liste_departements[seq(1,192,2)])


# Récupération des adresses des agences de la Banque Populaire

liste_adresses <- c()
part_link <- "https://agences.banquepopulaire.fr/banque-assurance/agences"

for(i in liste_departements){
  link <- paste0(part_link,"-",i)
  page <- read_html(link)
  adresses <- page %>% html_nodes(".em-poi-card__address") %>% html_text()
  liste_adresses <- c(liste_adresses,adresses)
}

# Correction des adresses ne donnant pas de résultats pour obtenir les longitudes et latitudes

liste_adresses[55]<-"8 Chemin de l'Oratoire Lieu-dit Les Jardins, 05240 La Salle-les-Alpes"
liste_adresses[228]<-"Centre Commercial Ancre Marine, Chem. du Puits de Brunet, 13600 La Ciotat"
liste_adresses[477]<- "3 A, rue de Besançon 25300 DOUBS"
liste_adresses[538]<-"Centre Ccial Leclerc Epicentre, 1 Rue des Orvilles, 28630 Barjouville"
liste_adresses[870]<-"93 Av. du Maréchal Juin, 34110 Frontignan"
liste_adresses[874]<-'Rue de la Pierre Plantée Lieudit La Plaine, Ctre Cial Espace de Bocaud, 34830 Jacou'
liste_adresses[1159]<-"Rue Gaëtan Rondeau Ctre Cial Case 14, 44200 Nantes"
liste_adresses[1566]<-"D1016 Entrée en face du Conforama, Ctre Cial CORA, Route Nationale 16, 60740 Saint-Maximin"
liste_adresses[1585] <- "33 Rue de l'Impératrice, 62600 Berck"
liste_adresses[1591]<- "Ctre Cial Auchan Ave Roger Salengro 62100 CALAIS"
liste_adresses[1696]<-"Ctre Cial, 2 Rue Louis Joseph Gay Lussac, 66330 Cabestany"
liste_adresses[2178]<-"Rue de l'Ouest Ctre Cial Porte de Normandie, 78200 Buchelay"
liste_adresses[2324]<-"1060, Route de l'Aerodrome-Zone Agroparc Res. l'Esplanade, Bat B, 84140"
liste_adresses[2517]<-"47 Rue du Point du Jour, 92100 Boulogne-Billancourt"

# Création d'un data frame contenant les adresses modifiées

banque_populaire <- data.frame(Banque=rep("Banque Populaire",length(liste_adresses)),
                                          Type=rep("Coopérative",length(liste_adresses)),
                                          Adresse=liste_adresses)

# Récupération des longitudes et latitudes de chaque adresse

longitude <-c()
latitude<-c()

for(i in 1:length(banque_populaire$Adresse)){
  adr<-banque_populaire$Adresse[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])

}

# Ecriture des longitudes, latitudes et adresses de chaque agence de la Banque Populaire dans un data frame

Banque_populaire_lgt_lat <- data.frame(Banque=banque_populaire$Banque,
                                       Type=banque_populaire$Type,
                                       Adresse=banque_populaire$Adresse,
                                       Longitude=longitude,
                                       Latitude=latitude)

write.csv(banque_populaire,"Banque_Populaire.csv",row.names = FALSE)
write.csv(Banque_populaire_lgt_lat,"Banque_Populaire_lgt_lat.csv",row.names = FALSE)


# BNP PARIBAS


# Récupération des adresses des agences Bnp Paribas

link_part <- "https://www.moneyvox.fr/pratique/agences/bnp-paribas/"

Adresses <-c()

for(i in 1:95){
  if(nchar(i)==1){
    # Création de l'URL pour le département
    link <- paste0(link_part,"0",i)
  } else {
    link <- paste0(link_part,i)
  }
  page <- read_html(link)
  liste_adresse <- page %>% html_nodes(".col-xs-7 p") %>% html_text()
  # Formatage de l'adresse pour l'avoir sous le format : rue, code postal, ville
  liste_adresses <- paste0(liste_adresse[seq(1,length(liste_adresse),2)]," , ",liste_adresse[seq(2,length(liste_adresse),2)])
  Adresses <- c(Adresses,liste_adresses)
  
  # Certains départements ont beaucoup d'agences et il y a plusieurs pages pour toutes ces agences
  nbr_pages <- page %>% html_nodes(".pagination-bloc a , .current") %>% html_text()
  
  if(length(nbr_pages)>0){
    nbr_pages <- nbr_pages[2:(length(nbr_pages)/2)]
    for(j in nbr_pages){
      # Création du lien quand il y a plusieurs pages pour un département
      link_pages <- paste0(link,"/",j)
      page <- read_html(link_pages)
      liste_adresse <- page %>% html_nodes(".col-xs-7 p") %>% html_text()
      liste_adresses <- paste0(liste_adresse[seq(1,length(liste_adresse),2)]," , ",liste_adresse[seq(2,length(liste_adresse),2)])
      Adresses <- c(Adresses,liste_adresses)
    }
  }
}


# Correction des adresses ne donnant pas de résultats pour obtenir les longitudes et latitudes

Adresses[41] <- "1160 Rte de Grasse, 06600 Antibes"
Adresses[53] <- "14 Av. Maréchal Joffre, 06160 Antibes"
Adresses[76] <- "77 Av. de Grasse, 06580 Pégomas"
Adresses[124] <- "645 Route De Berre Les, Bd des Deux Ormes, 13090 Aix-en-Provence"
Adresses[145] <- "680 Rue Guillaume Du Vair Pole, 13290 Aix-en-Provence"
Adresses[431] <- "11 rue Francois Mitterrand, 33160 Saint-Médard-en-Jalles"
Adresses[858] <- "Av. du Plateau, 64210 Bidart"
Adresses[1632] <- "117 Avenue du Bac 94210 Saint-Maur-des-Fossés"
Adresses[1633] <- "32bis Avenue du Bac 94210 Saint-Maur-des-Fossés"

# Création d'un data frame contenant les adresses modifiées

bnp_paribas <- data.frame(Banque=rep("Bnp Paribas",length(Adresses)),
                          Type=rep("Lucrative",length(Adresses)),
                          Adresse=Adresses)

# Récupération des longitudes et latitudes de chaque adresse

longitude <-c()
latitude<-c()

for(i in 1:length(bnp_paribas$Adresse)){
  adr<-bnp_paribas$Adresse[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])
}

# Ecriture des longitudes, latitudes et adresses de chaque agence Bnp Paribas dans un data frame

Bnp_paribas_lgt_lat <- data.frame(Banque=bnp_paribas$Banque,
                                  Type=bnp_paribas$Type,
                                  Adresse=bnp_paribas$Adresse,
                                  Longitude=longitude,
                                  Latitude=latitude)

write.csv(bnp_paribas,"Bnp_Paribas.csv",row.names = FALSE)
write.csv(Bnp_paribas_lgt_lat,"Bnp_Paribas_lgt_lat.csv",row.names = FALSE)


# SOCIETE GENERALE


# Récupération des départements en France métropolitaine ainsi que de leurs numéros

link <- "https://www.departements-gouv.fr/"
page <- read_html(link)
liste_departements <- page %>% html_nodes("td") %>% html_text()
liste_departements <- paste0(tolower(liste_departements[seq(2,192,2)]),"-",liste_departements[seq(1,192,2)])

# Récupération des adresses des agences de la Société Générale

link <- "https://agences.societegenerale.fr/banque-assurance/agences-"
Adresse <- c()

for(i in liste_departements){
  linK <- paste0(link,i)
  page <- read_html(linK)
  adresse <- page %>% html_nodes(".agencyaddress") %>% html_text() 
  Adresse <- c(Adresse,adresse)
}

# Création d'un data frame contenant les adresses modifiées

societe_generale <- data.frame(Banque=rep("Société Générale",length(Adresse)),
                          Type=rep("Lucrative",length(Adresse)),
                          Adresse=Adresse)

# Correction des adresses ne donnant pas de résultats pour obtenir les longitudes et latitudes

societe_generale$Adresse[1669] <- "C.Cial Croix Verte, 91250 Saint-Germain-lès-Corbeil"
societe_generale$Adresse[455] <- "7 Crs Mal, Cr de Lattre de Tassigny, 33390 Blaye"
societe_generale$Adresse[1108] <- "13 Crs F, Cr Franklin Roosevelt, 69006 Lyon"
societe_generale$Adresse[1376] <- "3 All. du Préambule, 77127 Lieusaint"
societe_generale$Adresse[1775] <- "20 Av P et M Curie, 93150 Le Blanc-Mesnil"
societe_generale$Adresse[1830] <- "95 Bd Paul Vaillant Couturier, 94240 L'Haÿ-les-Roses"
societe_generale$Adresse[48] <- "171 Avenue du 11 Novembre 06700 Saint-Laurent-du-Var"
societe_generale$Adresse[78] <- "171 Avenue du 11 Novembre 06700 Saint-Laurent-du-Var"

# Récupération des longitudes et latitudes de chaque adresse

longitude <-c()
latitude<-c()

for(i in 1:length(societe_generale$Adresse)){
  adr<-societe_generale$Adresse[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])
  
}

# Ecriture des longitudes, latitudes et adresses de chaque agence de la Société Générale dans un data frame

societe_generale_lgt_lat <- data.frame(Banque=societe_generale$Banque,
                                       Type=societe_generale$Type,
                                       Adresse=societe_generale$Adresse,
                                       Longitude=longitude,
                                       Latitude=latitude)

write.csv(societe_generale,"Société_Générale.csv",row.names = FALSE)
write.csv(societe_generale_lgt_lat,"Société_Générale_lgt_lat.csv",row.names = FALSE)


# CREDIT AGRICOLE


codes_postaux <- read.csv("laposte_hexasmal.csv", sep=";")
codes_postaux <- codes_postaux %>% select(c(Nom_commune, Code_postal))

codes_postaux$Code_postal <- as.character(codes_postaux$Code_postal)
codes_postaux$Code_postal <- unlist(lapply(codes_postaux$Code_postal, FUN = formatage_code_postal))

l <- "https://www.credit-agricole.fr/particulier/agence.html"
p <- read_html(l)

# liste des régions
regions <- p %>% html_nodes(".indexCR-itemLink") %>% html_text()
regions <- lapply(regions, FUN = formatage)


regions <- regions[-c(19,24,34)]
agences <- c()

for(r in regions){
  # liste des villes de la région "r"
  villes_r <- get_villes(r)
  villes_r <- lapply(villes_r, FUN = formatage)
  for(v in villes_r){
    agences <- c(agences, get_adresses(v, r))
  }
  agences <- unique(agences)
}

agences[516] <- "PLACE LOU CHAQUE DIT (PORT)  40130 CAPBRETON"
agences[1361] <- "83 AVENUE DE CLERMONT 63200 RIOM"
agences[1372] <- "FROMENTEAU - ROUTE NATIONALE 7  03000 MOULINS"
agences[2103] <- "30 AVENUE DU GRAND LARGE, 17137 NIEUL-SUR-MER"
agences[3123] <- "Parc des Ballius, Rue des Écoles, 34670 Baillargues"
agences[4774] <- "quartier Umede Chd, N3, 83560 Rians"
agences[3913] <- "Route de Suippes, Le Mont Bernard, 51000 Châlons-en-Champagne"
agences[5249] <- "70 Av. de la Muzelle, 38860 Les Deux Alpes"
agences[5775] <- "288B AV. ANDRE MAGINOT, 37100 TOURS"

longitude <-c()
latitude<-c()

for(i in 1:length(agences)){
  adr<-agences[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])
  
}

Credit_agricole <- data.frame(Banque = rep("Crédit Agricole",length(agences)),
                 Type = rep("Coopérative",length(agences)),
                 Adresse = agences)

write.csv(Credit_agricole,"Crédit_Agricole.csv",row.names=FALSE)

Credit_agricole_lgt_lat <- data.frame(Banque = rep("Credit Agricole",length(agences)),
                 Type = rep("Coopérative",length(agences)),
                 Adresse=agences,
                 Longitude=longitude,
                 Latitude=latitude)

write.csv(Credit_agricole_lgt_lat,"credit_agricole_lgt_lat.csv",row.names=FALSE)



# Création d'un data frame regroupant les adresses, longitudes et latitudes de toutes les banques ci-dessus

data_banque <- rbind(credit_mutuel_lng_lat,
                     Banque_populaire_lgt_lat,
                     Bnp_paribas_lgt_lat,
                     societe_generale_lgt_lat,
                     Credit_agricole_lgt_lat)


write.csv(data_banque,"Coordonnées_Banques.csv",row.names = FALSE)

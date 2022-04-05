# LIBRAIRIES -------------------------------------------------------------------

library(rvest)
library(dplyr)
library(stringr)
library(plyr)
library(banR)
library(stringi)


# FONCTIONS --------------------------------------------------------------------

# Fonction qui permet de supprimer les espaces inutiles
trim_string <- function(string)   gsub("\\s+", " ", gsub("^\\s+|\\s+$", "", string))

# Fonction qui permet de formater une partie de l'adresse URL pour récupérer les adresses des banques du Crédit Mutuel
IDville <- function(villes) {
  villes <- trim_string(villes)
  Ville <- str_replace_all(villes," ","%20")
}

# Rajoute un 0 au début du code postal si celui-ci ne contient que 4 chiffres
formatage_code_postal <- function(code_postal) {
  if (nchar(code_postal) == 4) {
    code_postal <- paste0("0", code_postal)
  }
  return(code_postal)
}

# Formater le nom des villes
formatage <- function(str){
  str <- tolower(str)
  str <- gsub(" ", "-", str)
  str <- gsub("'", "-", str)
  str <- stri_trans_general(str, "Latin-ASCII")
  return(str)
}

# Retourne les villes (dans laquelle se situent des  <- ) d'une région donnée
get_villes <- function(region) {
  lien_region <- paste0("https://www.credit-agricole.fr/particulier/agence/",region,".html") 
  page_region <- read_html(lien_region)
  villes_region <- page_region %>% html_nodes(".js-alphabetList-item") %>% html_text()
  return(unique(villes_region))
}

# Retourne le code postal d'une ville
get_code_postal <- function(ville) {
  ville <- toupper(gsub("-", " ", ville))
  code_postal <- codes_postaux[codes_postaux$Nom_commune == ville,]$Code_postal
  return(code_postal)
}

# Retourne les adresses des agences se situant au sein d'une ville donnée et d'une région donnée
get_adresses <- function(ville, region) {
  code_postal <- get_code_postal(ville)
  adresses <- c()
  for (code in code_postal) {
    lien_ville <- paste0("https://www.credit-agricole.fr/particulier/agence/", region, "/ville/", ville,"-", code, ".html")
    page_ville <- read_html(lien_ville)
    adresse <- page_ville %>% html_nodes(".StoreLocatorMap-AgencyAddress") %>% html_text()
    adresses <- c(adresses, adresse)
  }
  return(unique(adresses))
}

# Retourne l'ensemble des adresses des agences
get_all_adresses <- function() {
  lien <- "https://www.credit-agricole.fr/particulier/agence.html"
  page <- read_html(lien)
  
  # liste des régions
  regions <- page %>% html_nodes(".indexCR-itemLink") %>% html_text()
  regions <- lapply(regions, FUN = formatage)
  regions <- regions[regions!="reunion" & regions!="guadeloupe" & regions != "martinique-guyane"]
  regions[regions=="paris-et-ile-de-france"] <- "paris"
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
  return(agences)
}

# Retourne la longitude d'une adresse
get_long <- function(adr) {
  geocode(adr)[1,'longitude']
}

# Retourne la lattitude d'une adresse
get_lat <- function(adr) {
  geocode(adr)[1,'latitude']
}

# BANQUES - BANQUE POPULAIRE----------------------------------------------------

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
liste_adresses[which(liste_adresses=="Résidence Printemps- 8, ch de l'OratoireLieu-dit Les Jardins - SERRE CHEVALIER05240 LA SALLE LES ALPES")]<-"8 Chemin de l'Oratoire Lieu-dit Les Jardins, 05240 La Salle-les-Alpes"
liste_adresses[which(liste_adresses=="Centre Commercial Ancre MarineChemin du Puits de Brunet13600 LA CIOTAT")]<-"Centre Commercial Ancre Marine, Chem. du Puits de Brunet, 13600 La Ciotat"
liste_adresses[which(liste_adresses=="1, rue des OrvillesCentre Ccial Leclerc Epicentre28630 BARJOUVILLE")]<-"Centre Ccial Leclerc Epicentre, 1 Rue des Orvilles, 28630 Barjouville"
liste_adresses[which(liste_adresses=="Ctre Cial Les Portes du MuscatAve du Maréchal Juin34110 FRONTIGNAN")]<-"93 Av. du Maréchal Juin, 34110 Frontignan"
liste_adresses[which(liste_adresses=="Ctre Cial Espace de BocaudLieudit La Plaine34830 JACOU")]<-'Rue de la Pierre Plantée Lieudit La Plaine, Ctre Cial Espace de Bocaud, 34830 Jacou'
liste_adresses[which(liste_adresses=="Ctre Cial Case 1444200 NANTES")]<-"Rue Gaëtan Rondeau Ctre Cial Case 14, 44200 Nantes"
liste_adresses[which(liste_adresses=="33, rue de l'Impératrice62600 BERCK SUR MER")] <- "33 Rue de l'Impératrice, 62600 Berck"
liste_adresses[which(liste_adresses=="Ctre Cial AuchanAve Roger Salengro62100 CALAIS")]<- "Ctre Cial Auchan Ave Roger Salengro 62100 CALAIS"
liste_adresses[which(liste_adresses=="2, rue Gay LussacCtre Cial - Mas Guérido66330 CABESTANY")]<-"Ctre Cial, 2 Rue Louis Joseph Gay Lussac, 66330 Cabestany"
liste_adresses[which(liste_adresses=="Ctre Cial Porte de Normandie78200 BUCHELAY")]<-"Rue de l'Ouest Ctre Cial Porte de Normandie, 78200 Buchelay"
liste_adresses[which(liste_adresses=="1060, route de l'Aerodrome-Zone AGROPARCRes. l'Esplanade Bat B84140 MONTFAVET")]<-"1060, Route de l'Aerodrome-Zone Agroparc Res. l'Esplanade, Bat B, 84140"
liste_adresses[which(liste_adresses=="146-146 bis, rue du Point du Jour3 bis, pl Jules Guesde92100 BOULOGNE BILLANCOURT")]<-"47 Rue du Point du Jour, 92100 Boulogne-Billancourt"
liste_adresses[which(liste_adresses=="entrée en face du ConforamaCtre Cial CORA - RN 1660740 ST MAXIMIN")]<-"282 Av. Gabriel Péri, 83470 Saint-Maximin-la-Sainte-Baume"
liste_adresses[which(liste_adresses=="3 A, rue de Besançon25300 DOUBS")]<-"3 A Rue de Besançon, 25300 Doubs"
liste_adresses[which(liste_adresses=="211 bis, ave de Versailles75016 PARIS 16")]<-"211 Bis Av. de Versailles, 75016 Paris"

# Création d'un data frame contenant les adresses modifiées
banque_populaire <- data.frame(Banque=rep("Banque Populaire",length(liste_adresses)),
                               Type=rep("Coopérative",length(liste_adresses)),
                               Adresse=toupper(liste_adresses))

# Récupération des longitudes et latitudes de chaque adresse
longitude <- c()
latitude <- c()

for(i in 1:length(banque_populaire$Adresse)){
  adr<-banque_populaire$Adresse[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])
}

# On retire les doublons d'adresse
coord <- data.frame(longitude,latitude)

indice_sans_doublon <- as.integer(row.names(unique(coord)))

banque_populaire <- banque_populaire[indice_sans_doublon,]

# Ecriture des longitudes, latitudes et adresses de chaque agence de la Banque Populaire dans un data frame
Banque_populaire_lgt_lat <- data.frame(Banque=banque_populaire$Banque,
                                       Type=banque_populaire$Type,
                                       Adresse=banque_populaire$Adresse,
                                       Longitude=longitude[indice_sans_doublon],
                                       Latitude=latitude[indice_sans_doublon])

write.csv(Banque_populaire_lgt_lat,"Données/Banques/banque_populaire.csv",row.names = FALSE)


# BANQUES - BNP PARIBAS --------------------------------------------------------

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
Adresses[which(Adresses=="1160, Route de Grasse / Riviera Park , 06600 Antibes")] <- "1160 Rte de Grasse, 06600 Antibes"
Adresses[which(Adresses=="14, Avenue du Marechal Joffre , 06160 Juan-les-Pins" )] <- "14 Av. Maréchal Joffre, 06160 Antibes"
Adresses[which(Adresses=="77, Avenue de Grasse / Quartier du Logis , 06580 Pégomas")] <- "77 Av. de Grasse, 06580 Pégomas"
Adresses[which(Adresses=="645, Route de Berre / Centre Commercial Les Deux Ormes , 13100 Aix-en-Provence")] <- "645 Route De Berre Les, Bd des Deux Ormes, 13090 Aix-en-Provence"
Adresses[which(Adresses=="Za la Pioline Lot 37 , 13546 Les Milles" )] <- "Za La Pioline, Lot37, 13100 Aix-en-Provence"
Adresses[which(Adresses=="Rue Francois Mitterrand , 33160 Saint-Aubin-de-Médoc")] <- "11 rue Francois Mitterrand, 33160 Saint-Médard-en-Jalles"
Adresses[which(Adresses=="Avenue du Plateau / Bide Artean Quartier du Plateau , 64210 Bidart")] <- "Av. du Plateau, 64210 Bidart"
Adresses[which(Adresses=="115-117 Avenue du Bac , 94210 La Varenne-Saint-Hilaire")] <- "117 Avenue du Bac 94210 Saint-Maur-des-Fossés"
Adresses[which(Adresses=="32, B Avenue Du Bac , 94210 La Varenne-Saint-Hilaire")] <- "32bis Avenue du Bac 94210 Saint-Maur-des-Fossés"
Adresses[which(Adresses=="60, Rue De La Tour - ZAC Des Verries , 34980 Saint-Clément-de-Rivière")] <- "ZAC LES Verries, 60 Rue de la Tour, 34980 Saint-Gély-du-Fesc"

# Création d'un data frame contenant les adresses modifiées
bnp_paribas <- data.frame(Banque=rep("BNP Paribas",length(Adresses)),
                          Type=rep("Lucrative",length(Adresses)),
                          Adresse=toupper(Adresses))

# Récupération des longitudes et latitudes de chaque adresse
longitude <-c()
latitude <- c()

for(i in 1:length(bnp_paribas$Adresse)){
  adr<-bnp_paribas$Adresse[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])
}

# On retire les doublons d'adresse
coord <- data.frame(longitude,latitude)

indice_sans_doublon <- as.integer(row.names(unique(coord)))

bnp_paribas <- bnp_paribas[indice_sans_doublon,]

# Ecriture des longitudes, latitudes et adresses de chaque agence Bnp Paribas dans un data frame
Bnp_paribas_lgt_lat <- data.frame(Banque=bnp_paribas$Banque,
                                  Type=bnp_paribas$Type,
                                  Adresse=bnp_paribas$Adresse,
                                  Longitude=longitude[indice_sans_doublon],
                                  Latitude=latitude[indice_sans_doublon])

write.csv(Bnp_paribas_lgt_lat,"Données/Banques/bnp_paribas.csv",row.names = FALSE)

# BANQUES - CREDIT AGRICOLE ----------------------------------------------------

# Avoir le nom d'une ville et la région dans laquelle elle se situe ne suffit pas pour accéder 
# aux agences au sein de cette ville : le code postal de la ville est également compris dans l'URL
# ex : https://www.credit-agricole.fr/particulier/agence/alpes-provence/ville/aix-en-provence-13090.html

# Utilisation de la base officielle des codes postaux, réalisée par la Poste
codes_postaux <- read.csv("Web_scraping/Banques/laposte_hexasmal.csv", sep=";")
codes_postaux <- codes_postaux %>% select(c(Nom_commune, Code_postal))
codes_postaux$Code_postal <- as.character(codes_postaux$Code_postal)
codes_postaux$Code_postal <- unlist(lapply(codes_postaux$Code_postal, FUN = formatage_code_postal))

agences <- get_all_adresses()
agences <- c(agences,"64 Rue de Passy 75116 Paris","16 Avenue George V 75008 Paris")
agences[which(agences=="PLACE LOU CAHQUE DIT (PORT)  40130 CAPBRETON")] <- "PLACE LOU CHAQUE DIT (PORT)  40130 CAPBRETON"
agences[which(agences=="Centre Commercial de Riom Sud Avenue de Clermont  63200 MENETROL")] <- "83 AVENUE DE CLERMONT 63200 RIOM"
agences[which(agences=="FROMENTEAU - ROUTE NATIONALE 7  03003 MOULINS")] <- "FROMENTEAU - ROUTE NATIONALE 7  03000 MOULINS"
agences[which(agences=="Avenue du Grand Large Le Moulin des Chênes Verts  17137 Nieul-sur-Mer")] <- "30 AVENUE DU GRAND LARGE, 17137 NIEUL-SUR-MER"
agences[which(agences=="PARC DES BALLIUS RUE DES ECOLES  34670 BAILLARGUES")] <- "410 RUE DES ECOLES 34670 BAILLARGUES"
agences[which(agences=="Complexe du Mont Bernard - Route de Suippes  51000 CHALONS-EN-CHAMPAGNE")] <- "CHBRE AGRICULTURE MARNE BP 525, ROUTE DE SUIPPES, 51009 CHALONS-EN-CHAMPAGNE"
agences[which(agences=="Immeuble l'Eperon B 1  38860 LES DEUX ALPES")] <- "70 AV. DE LA MUZELLE, 38860 LES DEUX ALPES"
agences[which(agences=="Centre Commercial Station des Orres  05200 Les Orres")] <- "11 Pl. des Étoiles, 05200 Les Orres"
agences[which(agences=="IMMEUBLE LE ROND POINT  06340 LA TRINITE")] <- "Bd François Suarez, 06340 La Trinité"
agences[which(agences=="2 PLACE DU MARÉCHAL LECLERC  88510 ELOYES")] <- "2 Rue du Perreuil 88510 Éloyes"

longitudes <- lapply(agences, FUN=get_long)
latitudes <- lapply(agences, FUN=get_lat)
longitudes <- unlist(longitudes)
latitudes <- unlist(latitudes)

credit_agricole <- data.frame(agences, longitudes, latitudes)
credit_agricole <- cbind(data.frame(banque="Crédit Agricole", type="Coopérative"), credit_agricole)
colnames(credit_agricole) <- c('Banque','Type','Adresse','Longitude','Latitude')
credit_agricole$Adresse <- toupper(credit_agricole$Adresse)

#cred_agri <- data.frame(Banque="Crédit Agricole",Type="Coopérative",Adresse=toupper(agences))

write.csv(credit_agricole, "Données/Banques/credit_agricole.csv", row.names=FALSE)

# BANQUES - CREDIT MUTUEL-------------------------------------------------------

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
Credit_mutuel_sans_doublon<-data.frame(Banque=rep("Crédit Mutuel",length(indice_sans_doublon)),
                                       Type=rep("Coopérative",length(indice_sans_doublon)),
                                       Adresse=toupper(paste(Crédit_mutuel$Rue[indice_sans_doublon],
                                                             ", ",
                                                             Crédit_mutuel$Code_postal[indice_sans_doublon],
                                                             ", ",
                                                             Crédit_mutuel$Ville[indice_sans_doublon])))


# Correction d'une adresse ne donnant pas de résultat pour obtenir les longitudes et latitudes
Credit_mutuel_sans_doublon$Adresse[which(Credit_mutuel_sans_doublon$Adresse=="5 PLACE J DE LATTRE DE TASSIGNY ,  68025 ,  COLMAR")]<-"9 Pl. Jean de Lattre de Tassigny, 68000 Colmar"


# Récupération des longitudes et latitudes pour chaque adresse
longitude <-c()
latitude<-c()

for(i in 1:length(Credit_mutuel_sans_doublon$Adresse)){
  adr<-Credit_mutuel_sans_doublon$Adresse[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])
}

# On retire les doublons d'adresse
coord <- data.frame(longitude,latitude)

indice_sans_doublon <- as.integer(row.names(unique(coord)))

Credit_mutuel_sans_doublon <- Credit_mutuel_sans_doublon[indice_sans_doublon,]

# Ecriture des longitudes, latitudes et adresses de chaque banque du Crédit Mutuel dans un data frame
credit_mutuel_lng_lat<-data.frame(Banque=Credit_mutuel_sans_doublon$Banque,
                                  Type=Credit_mutuel_sans_doublon$Type,
                                  Adresse=Credit_mutuel_sans_doublon$Adresse,
                                  Longitude=longitude[indice_sans_doublon],
                                  Latitude=latitude[indice_sans_doublon])

# CREDIT MUTUEL BRETAGNE

link <- "https://www.cmb.fr/reseau-bancaire-cooperatif/web/recherche-agence-credit-mutuel-de-bretagne"
page <- read_html(link)
departement <- page %>% html_nodes(".cta_list_item") %>% html_text()

departement <- trim_string(departement)
departement <- substr(departement,1,nchar(departement)-5)
departement <- stri_trans_general(departement, "Latin-ASCII")
departement <- tolower(departement)
departement <- str_replace_all(departement,"'"," ")
departement <- str_replace_all(departement," ","-")


adresses <- c()

for(i in departement){
  link_part <- paste0(link,"/",i)  
  page <- read_html(link_part)
  Villes_code <- page %>% html_nodes("a div") %>% html_text()
  Villes_code <- Villes_code[-c(1,2,3,4)]
  Villes_code <- trim_string(Villes_code)
  adresses <- c(adresses,Villes_code)
}

adresses <- str_replace_all(adresses," -","")

longitude <-c()
latitude<-c()

for(i in 1:length(adresses)){
  adr<-adresses[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])
  
}

Credit_mutuel_bretagne <- data.frame(Banque="Crédit Mutuel",Type="Coopérative", Adresse=adresses,Longitude=longitude,Latitude=latitude)
credit_mutuel_lng_lat <- rbind(Credit_mutuel_bretagne,credit_mutuel_lng_lat)

write.csv(credit_mutuel_lng_lat,"Données/Banques/credit_mutuel.csv",row.names = FALSE)

# BANQUES - SOCIETE GENERALE ---------------------------------------------------

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
societe_generale$Adresse[which(Adresse=="C.CIAL CROIX VERTE91250 ST GERMAIN/CORB.")] <- "C.Cial Croix Verte, 91250 Saint-Germain-lès-Corbeil"
societe_generale$Adresse[which(Adresse=="7 CRS MAL DE LATTRE DE TASSIGNY33390 BLAYE")] <- "7 Crs Mal, Cr de Lattre de Tassigny, 33390 Blaye"
societe_generale$Adresse[which(Adresse=="13 CRS F.ROOSEVELT69006 LYON")] <- "13 Crs F, Cr Franklin Roosevelt, 69006 Lyon"
societe_generale$Adresse[which(Adresse=="CTRE CIAL CARRE SENART77127 LIEUSAINT")] <- "3 All. du Préambule, 77127 Lieusaint"
societe_generale$Adresse[which(Adresse=="20 AV P ET M CURIE93150 LE BLANC MESNIL")] <- "20 Av P et M Curie, 93150 Le Blanc-Mesnil"
societe_generale$Adresse[which(Adresse=="95 AV P V COUTURIER94240 L'HAY-LES-ROSES")] <- "95 Bd Paul Vaillant Couturier, 94240 L'Haÿ-les-Roses"
societe_generale$Adresse[which(Adresse=="CENTRE COMMERCIAL 300006700 SAINT-LAURENT-DU-VAR")] <- "171 Avenue du 11 Novembre 06700 Saint-Laurent-du-Var"
societe_generale$Adresse[which(Adresse=="CENTRE COMMERCIAL 300006700 SAINT LAURENT DU VAR")] <- "171 Avenue du 11 Novembre 06700 Saint-Laurent-du-Var"

# Récupération des longitudes et latitudes de chaque adresse
longitude <-c()
latitude <-c()

for(i in 1:length(societe_generale$Adresse)){
  adr<-societe_generale$Adresse[i]
  coordonnees <- geocode(adr)
  longitude<-c(longitude,coordonnees$longitude[1])
  latitude<-c(latitude,coordonnees$latitude[1])
}

# On retire les doublons d'adresse
coord <- data.frame(longitude,latitude)

indice_sans_doublon <- as.integer(row.names(unique(coord)))

societe_generale <- societe_generale[indice_sans_doublon,]


# Ecriture des longitudes, latitudes et adresses de chaque agence de la Société Générale dans un data frame
societe_generale_lgt_lat <- data.frame(Banque=societe_generale$Banque,
                                       Type=societe_generale$Type,
                                       Adresse=societe_generale$Adresse,
                                       Longitude=longitude[indice_sans_doublon],
                                       Latitude=latitude[indice_sans_doublon])

write.csv(societe_generale_lgt_lat,"Données/Banques/societe_generale.csv",row.names = FALSE)

# CONCATENATION BANQUES  ------------------------------------------------------

# Création d'un data frame regroupant les adresses, longitudes et latitudes de 
# toutes les banques ci-dessus
data_banque <- rbind(Banque_populaire_lgt_lat,
                     Bnp_paribas_lgt_lat,
                     credit_agricole,
                     credit_mutuel_lng_lat,
                     societe_generale_lgt_lat)

# Ecriture de la data frame contenant toutes les banques dans un csv
write.csv(data_banque,"Données/bdd_coordonnees_banques_2022.csv",row.names = FALSE)

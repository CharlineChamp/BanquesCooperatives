#LIBRAIRIES
library(BP2CAMSG)
library(spatstat)

#Chargement des données
banque <- bdd_coordonnees_banques2022

#Fenêtre polygonale des banques à l'échelle de la France
long <- as.numeric(banque$Longitude)
lat <- as.numeric(banque$Latitude)
mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))

# Initialisation d'une colonne vide
banque$ze <- c(rep(NA,length(banque[,1]))) 
banque <- banque[,-6]
# Parcours des zones d'emplois
for (i in bdd_zese$`Zone d'emploi 2020`){
  # Récupération de l'indice associé à la zone d'emploi
  l <- which(bdd_zese$`Zone d'emploi 2020`==i)
  # Récupération des longitudes et latitudes associées à la zone d'emploi
  longlat <- recup.longlat(l)
  # Polygone de la zone d'emploi
  ow <- owin(poly = list(x = longlat$Longitude,y = longlat$Latitude))
  # Data frame répertoriant toutes les longitudes et latitudes des banques à l'intérieur de la ze
  liste <- data.frame(long=mypattern[ow]$x, lat=mypattern[ow]$y)
  # Parcours des banques à l'échelle national 
  for(j in 1:length(bdd_coordonnees_banques2022$Banque)){
    # Parcours à l'échelle de la zone d'emploi
    for(k in 1:length(liste[,1])){
      # Test des long/lat dans chaque jeu de donnée
      if(banque$Longitude[j] == liste$long[k] & banque$Latitude[j] == liste$lat[k]){
        banque$ze[j] <- i
      }
    }
  }
  cat("Zone d'emploi:",i,'\n')
}

#Sauvegarde du fichier
write.csv(banque,'bdd_coordonnees_ze_banques2022.csv',row.names = FALSE)



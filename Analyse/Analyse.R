# LIBRAIRIES
library(BP2CAMSG)
library(spatstat)


# Chargement des données
banque <- bdd_coordonnees_banques2022

# Longitudes/Latitudes
long <- banque$Longitude
lat <- banque$Latitude

# Fenêtre polygonale des banques à l'échelle de la France métropolitaine
mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))

# Choix d'une ze au hasard
s <- sample(bdd_zese$`Zone d'emploi 2020`,1)
# Indice de la zone d'emploi sélectionnée
ind <- which(bdd_zese$`Zone d'emploi 2020`=='1109')

# Coordonnées de la zone d'emploi sélectionnée
coord.ze <- recup.longlat(ind)

# Polygone de la zone d'emploi
ow <- owin(poly = list(x = coord.ze$Longitude,y = coord.ze$Latitude))
#plot(ow)

# Zone d'emplois avec toutes les banques à l'intérieur
banque.ze <- mypattern[ow]
#plot(banque.ze)

plot(density(banque.ze))
points(banque.ze,cex=.4)

N <- mypattern[ow]$n

banq.dens <- function(ze,type){
  ind.ze <- which(bdd_zese$`Zone d'emploi 2020`==ze)
  coord.ze <- recup.longlat(ind.ze)
  ow <- owin(poly = list(x = coord.ze$Longitude,y = coord.ze$Latitude))
  if(type == 'Lucrative'){
    ind.lucr <- which(bdd_coordonnees_banques2022$Type=='Lucrative')
    banq <- bdd_coordonnees_banques2022[ind.lucr,]
    long <- banq$Longitude
    lat <- banq$Latitude
    mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
    }
  if(type=='Coopérative'){
    ind.coop <- which(bdd_coordonnees_banques2022$Type=='Coopérative')
    banq <- bdd_coordonnees_banques2022[ind.coop,]
    long <- banq$Longitude
    lat <- banq$Latitude
    mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
      }
  if(type='Tout'){
    banq <- bdd_coordonnees_banques2022
    long <- banq$Longitude
    lat <- banq$Latitude
    mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
    }
  banque.ze <- mypattern[ow]
  plot(density(banque.ze),main='Densité')
  points(banque.ze,cex=.4)
}


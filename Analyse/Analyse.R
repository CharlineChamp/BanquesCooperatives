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



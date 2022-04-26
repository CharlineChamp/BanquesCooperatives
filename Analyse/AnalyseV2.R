# LIBRAIRIES
library(BP2CAMSG)
library(spatstat)

recup.longlat <- function(indice){
  list.long <- vector(length = 0)
  list.lat <- vector(length = 0)
  for (k in 1:length(bdd_zese[[35]][[indice]])){
    list.long <- c(list.long, bdd_zese[[35]][[indice]][[k]][[1]][,1])
    list.lat <- c(list.lat, bdd_zese[[35]][[indice]][[k]][[1]][,2])
  }
  coord.test <- data.frame(Longitude = list.long, Latitude = list.lat)
}

banq.dens <- function(ze,type){
  ind.ze <- which(bdd_zese$`Zone d'emploi 2020`==ze)
  coord.ze <- recup.longlat(ind.ze)
  ow <- owin(poly = list(x = coord.ze$Longitude,y = coord.ze$Latitude))
  if(type == "Lucrative"){
    ind.lucr <- which(bdd_coordonnees_banques2022$Type=='Lucrative')
    banq <- bdd_coordonnees_banques2022[ind.lucr,]
    long <- banq$Longitude
    lat <- banq$Latitude
    mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
  }
  if(type=="Coopérative"){
    ind.coop <- which(bdd_coordonnees_banques2022$Type=='Coopérative')
    banq <- bdd_coordonnees_banques2022[ind.coop,]
    long <- banq$Longitude
    lat <- banq$Latitude
    mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
  }
  if(type=="Tout"){
    banq <- bdd_coordonnees_banques2022
    long <- banq$Longitude
    lat <- banq$Latitude
    mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
    banque.ze <- mypattern[ow]
    plot(density(banque.ze),main=paste0("Densité en fonction des deux types de banques pour la zone d'emploi ",ze))
    points(banque.ze,cex=.4)
  }else{
    banque.ze <- mypattern[ow]
    plot(density(banque.ze),main=paste0("Densité banques de type ",type," pour la zone d'emploi ",ze))
    points(banque.ze,cex=.4)
  }
}

#TEST
typ <- c(unique(bdd_coordonnees_banques2022$Type),"Tout")
pop.type <- sample(typ,1)
pop.ze <- sample(bdd_zese$`Zone d'emploi 2020`,1)

banq.dens(pop.ze,pop.type)
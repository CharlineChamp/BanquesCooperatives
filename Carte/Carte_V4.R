#LIBRAIRIES
library(BP2CAMSG)
library(spatstat)
library(ggplot2)
library(viridis)

recup.longlat <- function(indice){
  list.long <- vector(length = 0)
  list.lat <- vector(length = 0)
  for (k in 1:length(bdd_zese[[35]][[indice]])){
    list.long <- c(list.long, bdd_zese[[35]][[indice]][[k]][[1]][,1])
    list.lat <- c(list.lat, bdd_zese[[35]][[indice]][[k]][[1]][,2])
  }
  coord.test <- data.frame(Longitude = list.long, Latitude = list.lat)
}


gg_map_ze <- function(ze){
  #Fenêtre polygonale des banques 
  long <- as.numeric(bdd_coordonnees_banques2022$Longitude)
  lat <- as.numeric(bdd_coordonnees_banques2022$Latitude)
  mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
  # Récupération de l'indice associé à ze
  l <- which(bdd_zese$`Zone d'emploi 2020`==ze)
  longlat <- recup.longlat(l)
  ow <- owin(poly = list(x = longlat$Longitude,y = longlat$Latitude))
  banq<-c()
  test<-c()
  for(i in 1:length(mypattern[ow]$x)){
    lo <- which(bdd_coordonnees_banques2022$Longitude==mypattern[ow]$x[i])
    lat <- which(bdd_coordonnees_banques2022$Latitude==mypattern[ow]$y[i])
    if(lo==lat){
      banq <- c(banq,bdd_coordonnees_banques2022[lo,]$Banque)
    }
    test<-c(test,lo==lat)
  }
  dat <- data.frame(banque=banq,
                    long=mypattern[ow]$x,
                    lat=mypattern[ow]$y)
  map <- ggplot()+
         geom_sf(data=bdd_zese[l,], aes(geometry=geometry),color='black',size=.2)+
         scale_fill_viridis_c(option = 'E')+
         theme_minimal()+
         theme(panel.background = element_rect(fill = "light blue"))+
         geom_point(data=dat,aes(x=long,y=lat,group=banq,color=banq),size=.6)+
         xlab("Longitude")+
         ylab("Latitude")+
         labs(col="Banques")
  map
}





# # Test avec ppp
# data.eff <- data.frame(table(banque$ze))
# ind.vrai <- vector(length = 0)
# ind.faux <- vector(length = 0)
# for(i in 1:length(data.eff$Var1)){
#   longlat <- recup.longlat(i)
#   ow <- owin(poly = list(x = longlat$Longitude,y = longlat$Latitude))
#   if(data.eff$Freq[i] == mypattern[ow]$n){
#     ind.vrai <- c(ind.vrai,i)
#   }
#   else{
#     ind.faux <- c(ind.faux,i)
#   }
# }
# cat('Indices Vrais : ',ind.vrai,'\n','Indices Faux : ',ind.faux)
# 
# s <- sample(bdd_zese$`Zone d'emploi 2020`,1)
# ggplotly(gg_map_ze(s))
# 
# write.csv(banque,'bdd_coordonnees_ze_banques2022.csv',row.names = FALSE)
#7616 / 8410 /8405
#Résoudre problème carte (verifier test log==la ??) (1)



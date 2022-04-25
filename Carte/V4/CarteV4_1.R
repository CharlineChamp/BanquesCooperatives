#LIBRAIRIES
library(BP2CAMSG)


gg_map_ze <- function(bdd_zese,bdd_coordonnees_ze_banques2022,type=FALSE,ze='0051'){
 
  # ligne correspondant à la zone d'emploi sélectionnée
  l <- which(bdd_zese$`Zone d'emploi 2020`==ze)
  
  # lignes correspondants aux banques présentes dans la zone d'emploi sélectionnée
  l1 <- which(bdd_coordonnees_ze_banques2022$ze==ze)
  banque <- bdd_coordonnees_ze_banques2022[l1,]
  
  # Construction de la map en ggplot
  if(type==FALSE){
    map <- ggplot()+
    geom_sf(data=bdd_zese[l,], aes(geometry=geometry),color='black',size=.2)+
    scale_fill_viridis_c(option = 'E')+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "light blue"))+
    geom_point(data=banque,aes(x=Longitude,y=Latitude,group=Banque,color=Banque,label=Banque),size=.6)
  }
  else{
    map <- ggplot()+
      geom_sf(data=bdd_zese[l,], aes(geometry=geometry),color='black',size=.2)+
      scale_fill_viridis_c(option = 'E')+
      theme_minimal()+
      theme(panel.background = element_rect(fill = "light blue"))+
      geom_point(data=banque,aes(x=Longitude,y=Latitude,group=Type,color=Type,label=Banque),size=.6)
  }
  map
}










library(sf)

#Chargement des données
fd_c <- st_read('fond_ZE2020_geo20.shp')

l <- c()

#Indices des codes correspondants aux départements d'outre mer
indices <- c('971','972','973','974','976')
for(i in indices){
  l <- c(l,grep(i, fd_c$code, ignore.case = TRUE))
}
#On retire les départements d'outre mer
fd_cnew <- fd_c[-l,]
fd <- fd_cnew[3]

#Affichage graphique des zones d'emplois
par(mar = c(0,0,1,0))
plot(fd,reset = FALSE)

#Fonction permettant de combiner les polygons
st_union_by = function(geo, group) {
  
  y2 = list()
  #parcours les groupes et fusionne les zones d'emplois
  for (i in unique(group)) {
    #les ze concernées
    z = geo[group == i]
    #fusion
    y = Reduce(st_union, z)
    y2[[i]] = y
  }
  #Permet de créer la colonne de geometry
  st_sfc(y2)
}

fd_geo <- st_union_by(fd$geometry, fd$ze2020)

plot(fd_geo,col=c(1:287))

#ggplot
library(ggplot2)
library(ggspatial)
library(viridis)

#Data frame comprenant les codes de ze et les geometry
map_data <- data.frame(ze=as.numeric(unique(fd$ze2020)),fd_geo)

#plot avec ggplot2
map <- ggplot() +
       geom_sf(data = map_data,aes(fill=,geometry=geometry),color='black',size=.2)+
       scale_fill_viridis_c(option = 'G')+
       theme_minimal()+
       theme(legend.position = "none")+
       theme(panel.background = element_rect(fill = "light blue"))+
       #annotation_scale(location = "br", line_width = .3) +
       annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
map
######################################################################################################
#TEST


nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
fd_geo <- st_as_sf(fd_geo)

plot_geo(nc)


library(dplyr)
test <- fd_cnew %>% 
        group_by(ze2020) %>% 
        summarize()


plot(test)

library(mapview)

mapview(test)

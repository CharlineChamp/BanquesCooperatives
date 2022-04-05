library(sf)
library(plotly)
library(ggplot2)
library(viridis)

# Chargement des données
fd_c <- st_read('Carte/shapefile/fond_ZE2020_geo20.shp')
banque <- read.csv('Données/bdd_coordonnees_banques2022.csv')
ze <- st_read('Données/ze2020/bdd_polygon_ze2020.shp')


# Construction de la map en ggplot
map <- ggplot()+
       geom_sf(data=ze, aes(geometry=geometry),color='black',size=.2)+
       scale_fill_viridis_c(option = 'E')+
       theme_minimal()+
       theme(panel.background = element_rect(fill = "light blue"))+
       geom_point(data=banque,aes(x=Longitude,y=Latitude,group=Type,color=Type,label=Banque),size=.6)
map
# Conversion en plotly
ggplotly(map)


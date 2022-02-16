library(sf)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggspatial)
library(viridis)

#Pour utiliser l'API mapbox
mapboxToken <- paste("pk.eyJ1IjoiZ3JhbnQyNDk5IiwiYSI6ImNremZ6enYweDJjbjAybm8xejVqN3IwemQifQ.UTVkE6hkSjPESfp-0CPD7Q", collapse="")
Sys.setenv("MAPBOX_TOKEN" = mapboxToken)

#Chargement des données
fd_c <- st_read('shapefile/fond_ZE2020_geo20.shp')
bdd_zese <- readxl::read_xlsx('../Données/bdd_social_ze2020.xlsx')
sg <- read.csv("../Données/Banques/coordonnees_banque.csv")

l <- vector(length = 0)
#Indices des codes correspondants aux départements d'outre mer
indices <- c('971','972','973','974','976')
for(i in indices){
    l <- c(l,grep(i, fd_c$code, ignore.case = TRUE))
}

#On retire les départements d'outre mer
fd_cnew <- fd_c[-l,]
fd <- fd_cnew[3]

#Fusion pour obtenir des zones d'emplois
fd_cnew_plot <- fd_cnew%>% 
    group_by(ze2020)%>% 
    summarize()

plot(fd_cnew_plot)

fd_cnew_plot$geometry <- st_cast(fd_cnew_plot$geometry,'MULTIPOLYGON')

bdd_zese <- cbind(bdd_zese,fd_cnew_plot$geometry)
#ggplot
#Construction de la map en ggplot
map <- ggplot()+
    geom_sf(data=bdd_zese, aes(fill=`Taux de pauvreté (en %) - Ensemble...6`,geometry=geometry),color='white',size=.2)+
    scale_fill_viridis_c(option = 'E')+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "light blue"))+
    geom_point(data=sg,aes(x=Longitude,y=Latitude,group=Type,color=Type,label=Banque),size=.6)+
    labs(fill = "Taux de pauvreté")
map
#Conversion en plotly
ggplotly(map)

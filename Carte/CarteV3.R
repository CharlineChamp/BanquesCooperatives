library(sf)library(dplyr)
library(plotly)
library(ggplot2)
library(ggspatial)
library(viridis)

#Pour utiliser l'API mapbox
mapboxToken <- paste("pk.eyJ1IjoiZ3JhbnQyNDk5IiwiYSI6ImNremZ6enYweDJjbjAybm8xejVqN3IwemQifQ.UTVkE6hkSjPESfp-0CPD7Q", collapse="")
Sys.setenv("MAPBOX_TOKEN" = mapboxToken)

#Chargement des données
fd_c <- st_read('fond_ZE2020_geo20.shp')
bdd_zese <- readxl::read_xlsx('bdd_social_ze2020.xlsx')
sg <- read.csv("societe_generale_lgt_lat.csv")

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
       scale_fill_viridis_d(option = 'G')+
       theme_minimal()+
       theme(legend.position = "none")+
       theme(panel.background = element_rect(fill = "light blue"))+
       #geom_point(data=sg,aes(x=Longitude,y=Latitude),color='red',size=.6)+
       #annotation_scale(location = "br", line_width = .3) +
       annotation_north_arrow(location = "bl", height = unit(0.7,"cm"), width = unit(0.7,"cm"))
map
#Conversion en plotly
ggplotly(map)


#PLOTLY
#Construction de la carte avec plotly

#Tracé de la France découpée en zone d'emplois
fig1 <- fd_cnew_plot %>% plot_mapbox() %>% layout(mapbox = list(style = 'dark'))
#Positionnement des banques sur la carte
fig2 <- sg %>% plot_mapbox(lat = ~Latitude, lon = ~Longitude, 
                           size=1,
                           mode = 'scattermapbox', hoverinfo='name') 
fig2 <- fig2 %>% layout(title = 'Banques Coopératives',
                        font = list(color='white'),
                        plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                        mapbox = list(style = 'dark'),
                        legend = list(orientation = 'h',
                                      font = list(size = 8)),
                        margin = list(l = 25, r = 25,
                                      b = 25, t = 25,
                                      pad = 2)) 

#Permet d'afficher les deux cartes cote a cote
subplot(fig1,fig2)

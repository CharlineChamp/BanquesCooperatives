library(sf)
library(dplyr)
library(plotly)
library(ggplot2)
library(viridis)

#PLOTLY
#Construction de la carte avec plotly

<<<<<<< HEAD
#CHARGEMENT DES DONNEES
fd_c <- st_read('fond_ZE2020_geo20.shp')
bdd_zese <- readxl::read_xlsx('bdd_social_ze2020.xlsx')
sg <- read.csv("societe_generale_lgt_lat.csv")
=======
#Chargement des données
fd_c <- st_read('shapefile/fond_ZE2020_geo20.shp')
bdd_zese <- readxl::read_xlsx('../Données/bdd_social_ze2020.xlsx')
sg <- read.csv("../Données/Banques/coordonnees_banque.csv")
>>>>>>> de5bf6d1d6fb813fdbd15471a3664538b2b8a7fb

#NETTOYAGE/UNIFORMISATION DES DONNEES
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
                dplyr::summarize()

fd_cnew_plot$geometry <- st_cast(fd_cnew_plot$geometry,'MULTIPOLYGON')

<<<<<<< HEAD
#plot(fd_cnew_plot)
#bdd_zese <- cbind(bdd_zese,fd_cnew_plot$geometry)
=======
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
>>>>>>> de5bf6d1d6fb813fdbd15471a3664538b2b8a7fb


#CONSTRUCTION DE LA CARTE
#API mapbox
mapboxToken <- paste("pk.eyJ1IjoiZ3JhbnQyNDk5IiwiYSI6ImNremZ6enYweDJjbjAybm8xejVqN3IwemQifQ.UTVkE6hkSjPESfp-0CPD7Q", collapse="")
Sys.setenv("MAPBOX_TOKEN" = mapboxToken)

#Tracé de la France découpée en zone d'emplois
fig <- fd_cnew_plot %>% plot_mapbox() %>% layout(mapbox = list(style = 'dark'))

#Positionnement des banques sur la carte
fig <- plot_mapbox(data = sg,lat = ~Latitude, lon = ~Longitude, 
                          split= ~Type,
                          size=1,
                          mode = 'markers', hoverinfo='text',
                          marker = list(size = 8))

fig <- fig %>% layout(title = 'Banques Coopératives',
                      font = list(color='white'),
                      plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                      mapbox = list(style = 'dark'),
                      legend = list(orientation = 'h',
                                    font = list(size = 8)),
                      margin = list(l = 25, r = 25,
                                    b = 25, t = 25,
                                    pad = 2)) 
fig <- fig %>% config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


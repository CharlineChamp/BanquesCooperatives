# LIBRAIRIES
library(plotly)

# CHARGEMENT DES DONNEES
banque <- read.csv('Données/bdd_coordonnees_banques2022.csv')
ze <- st_read('Données/ze2020/bdd_polygon_ze2020.shp')

# CONSTRUCTION DE LA CARTE
# API mapbox
mapboxToken <- paste("pk.eyJ1IjoiZ3JhbnQyNDk5IiwiYSI6ImNremZ6enYweDJjbjAybm8xejVqN3IwemQifQ.UTVkE6hkSjPESfp-0CPD7Q", collapse="")
Sys.setenv("MAPBOX_TOKEN" = mapboxToken)

# Tracé de la France découpée en zone d'emplois
fig1 <- plot_mapbox(data = ze[3,], split=~Z.202) %>% add_markers(data = banque, x=~Longitude, y=~Latitude, split=~Banque) %>% layout(mapbox = list(style = 'dark'))
fig1

# Positionnement des banques sur la carte
fig <- plot_mapbox(data = banque,lon=~Longitude,lat=~Latitude,
                   split=~Banque,
                   size=1,
                   mode = 'markers', hoverinfo='text',
                   marker = list(size = 5, opacity = .5))

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

subplot(fig,fig1)



# UTILITE DE CE FICHIER
# Nous avons réalisé ce fichier pour nous permettre d'observer les banques 
# individuellement. Cela nous a permis de trouver plus facilement les outliers.
# Ainsi la résolution de certaines erreurs de convertion entre adresse et 
# longitude/latitute a été beaucoup plus simple à trouver.


# LIBRAIRIES
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(plotly)

# CHARGEMENT DES DONNÉES
banque <- read.csv('Données/bdd_coordonnees_banques2022.csv')
ze <- st_read('Données/ze2020/bdd_polygon_ze2020.shp')


# FONCTIONS TEST

TestBanqWebCV2 <- function(label_banque){
  banque <- filter(banque,banque$Banque==label_banque)
  
  map <- ggplot()+
         geom_sf(data=ze, aes(geometry=geometry),color='black',size=.2)+
         scale_fill_viridis_c(option = 'E')+
         theme_minimal()+
         theme(panel.background = element_rect(fill = "light blue"))+
         geom_point(data=banque,aes(x=Longitude,y=Latitude,group=Type,color=Type,label=Banque),size=.6)+
         ggtitle(paste('Test webscraping : ',label_banque))

  ggplotly(map)
}

TestBanqWebCV3 <- function(label_banque){
  banque <- filter(banque,banque$Banque==label_banque)
  
  # CONSTRUCTION DE LA CARTE
  # API mapbox
  mapboxToken <- paste("pk.eyJ1IjoiZ3JhbnQyNDk5IiwiYSI6ImNremZ6enYweDJjbjAybm8xejVqN3IwemQifQ.UTVkE6hkSjPESfp-0CPD7Q", collapse="")
  Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
  # Positionnement des banques sur la carte
  map <- plot_mapbox(data = banque,lon=~Longitude,lat=~Latitude,
                     size=1,
                     mode = 'markers', hoverinfo='text',
                     marker = list(size = 8, opacity = .5))
  map <- map %>% layout(title = paste('Test webscraping : ',label_banque),
                        font = list(color='white'),
                        plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                        mapbox = list(style = 'dark'),
                        legend = list(orientation = 'h',
                                      font = list(size = 8)),
                        margin = list(l = 25, r = 25,
                                      b = 25, t = 25,
                                      pad = 2)) 
  map <- map %>% config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
  map
}

# TEST BANQUE POPULAIRE
TestBanqWebCV2(label_banque = "Banque Populaire")
TestBanqWebCV3(label_banque = "Banque Populaire")

# TEST BNP PARIBAS
TestBanqWebCV2(label_banque = "BNP Paribas")
TestBanqWebCV3(label_banque = "BNP Paribas")

# TEST CREDIT AGRICOLE
#TestBanqWebCV2(label_banque = "Crédit Agricole")
#TestBanqWebCV3(label_banque = "Crédit Agricole")

# TEST CREDIT MUTUEL 
#TestBanqWebCV2(label_banque = "Crédit Mutuel")
#TestBanqWebCV3(label_banque = "Crédit Mutuel")

# TEST SOCIETE GENERALE
TestBanqWebCV2(label_banque = "Société Générale")
TestBanqWebCV3(label_banque = "Société Générale")

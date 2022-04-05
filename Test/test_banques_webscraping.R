#LIBRAIRIES
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(plotly)

#CHARGEMENT DES DONNÉES
#social <- readxl::read_xlsx('Données/bdd_social_ze2020.xlsx')
banque <- read.csv('Données/bdd_coordonnees_banques2022.csv')
ze <- st_read('Données/ze2020/bdd_polygon_ze2020.shp')


#FONCTION TEST

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

TestBanqWebCV2(label_banque = "Banque Populaire")
TestBanqWebCV2(label_banque = "BNP Paribas")
TestBanqWebCV2(label_banque = "Société Générale")

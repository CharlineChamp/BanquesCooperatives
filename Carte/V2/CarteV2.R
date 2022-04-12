library(sf)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggspatial)
library(viridis)



#Chargement des données
fd_c <- st_read('Carte/shapefile/fond_ZE2020_geo20.shp')
bdd_zese <- readxl::read_xlsx('Données/bdd_social_ze2020.xlsx')
sg <- read.csv("Données/bdd_coordonnees_banques2022.csv")

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


fd_cnew_plot$geometry <- st_cast(fd_cnew_plot$geometry,'MULTIPOLYGON')

bdd_zese <- cbind(bdd_zese,fd_cnew_plot$geometry)


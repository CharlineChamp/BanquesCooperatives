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
fd_cnew <- fd_c[-l,-c(1:2,4:5)]

#Affichage graphique
plot(st_geometry(fd_cnew))
plot(st_geometry(fd_cnew[24511, ]), add = TRUE, col = "red") #Paris


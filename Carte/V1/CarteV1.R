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

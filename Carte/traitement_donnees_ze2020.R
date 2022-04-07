# LIBRAIRIES
library(sf)
library(dplyr)

# CHARGEMENT DES DONNÉES
fd_c <- st_read('Carte/shapefile/fond_ZE2020_geo20.shp')
bdd_zese <- readxl::read_xlsx('Données/bdd_social_ze2020.xlsx')

# NETTOYAGE DES DONNÉES

l <- vector(length = 0)
# Indices des codes correspondants aux départements d'outre mer
indices <- c('971','972','973','974','976')
for(i in indices){
  l <- c(l,grep(i, fd_c$code, ignore.case = TRUE))
}

# On retire les départements d'outre mer
fd_c <- fd_c[-l,]

# Fusion pour obtenir des zones d'emplois
fd_c <- fd_c%>% 
        group_by(ze2020)%>% 
        dplyr::summarize()

# Uniformisation en MUTLTIPOLYGON
fd_c$geometry <- st_cast(fd_c$geometry,'MULTIPOLYGON')

# Concaténation des geometry sur la bdd principale
bdd_zese <- cbind(bdd_zese,fd_c$geometry)
bdd_zese <- st_as_sf(bdd_zese)

# outfile <- tempfile(fileext = ".shp")
# st_write(bdd_zese,outfile)


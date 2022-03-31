library(sp)
library(readr)
library(dplyr)

bdd_social_ze2020 <- read_excel("bdd_social_ze2020.xlsx")
bdd_social_ze2020 <- cbind(bdd_social_ze2020, fd_cnew_plot$geometry)
banque <- read_csv("Coordonnées_Banques.csv")
banque <- unique(banque)

eff_banq_ze <- function(label_banque){
  df <- data.frame()
  ze <- c()
  nb_eff <- c()
  banque <- filter(banque,banque$Banque==label_banque)
  for (i in 1:length(bdd_social_ze2020$`Zone d'emploi 2020`)){
    nb <- 0 
    for (j in 1:length(banque$Banque)){
      point.long <- banque$Longitude[j]
      point.lat <- banque$Latitude[j]
      list.long <- c()
      list.lat <- c()
      for (k in 1:length(bdd_social_ze2020[[35]][[i]])){
        list.long <- c(list.long, bdd_social_ze2020[[35]][[i]][[k]][[1]][,1])
        list.lat <- c(list.lat, bdd_social_ze2020[[35]][[i]][[k]][[1]][,2])
      }
      eff <- point.in.polygon(point.long, point.lat, list.long,list.lat)
      if (eff != 0){
        nb <- nb + 1
      }
    }
    ze <- c(ze, bdd_social_ze2020$`Zone d'emploi 2020`[i])
    nb_eff <- c(nb_eff, nb)
  }
  df <- data.frame(ze, nb_eff)
}


# Banque Populaire
df_bp <- eff_banq_ze(label_banque = "Banque Populaire")

# Crédit Agricole
df_ca <- eff_banq_ze(label_banque = "Credit Agricole")

# Crédit Mutuelle
df_cm <- eff_banq_ze(label_banque = "Crédit Mutuel")

# Société Générale 
df_sg <- eff_banq_ze(label_banque = "Société Générale")

# BNP Paribas 
df_bnp <- eff_banq_ze(label_banque = "Bnp Paribas")

# Concaténation des data frame

df_eff <- c(df_bp, df_ca, df_cm, df_sg, df_bnp)

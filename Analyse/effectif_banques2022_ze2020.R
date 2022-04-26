# Ecriture du jeu de données effectif banques 2022 ze 2020

library(BP2CAMSG)

# Chargement des données
banque_ze <- bdd_coordonnees_ze_banques2022

# Création table de contingence du nombre de banque dans chaque zone d'emploi par banques
a <- table(banque_ze$Banque, banque_ze$ze)

# Création d'une date frame contenant pour chaque banque pour chaque zone d'emploi le nombre de banque présentes
t <- data.frame(socio$`Zone d'emploi 2020`,a[1,1:287],a[2,1:287],a[3,1:287],a[4,1:287],a[5,1:287])
colnames(t) <- c("Zone d'emploi 2020", "Banque Populaire", "BNP Paribas", "Crédit Agricole", "Crédit Mutuel", "Société Générale")
rownames(t) <- NULL


# Ecriture de la data frame dans un fichier csv
write.csv(t, "Données/bdd_effectif_banques2022_ze2020.csv")


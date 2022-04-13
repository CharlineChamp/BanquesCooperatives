library(rvest) # Utile pour read_html
library(stringr) # Utile pour str_replace_all
library(xlsx) # Utile pour write.xlsx
library(readxl) # Utile pour read.xlsx

###############################################################################
#  Récupération des revenus et pauvreté des ménages en 2019 par webscrapping
###############################################################################

# Numéros zones d'emplois
ze <- c("0052","0053","0054","0055","0056","0057","0058","0059","0060","0061","0062","0063","0064",
        "1101","1102","1103","1104","1105","1106","1107","1108","1109","1110","1111","1112","1113","1114",
        "1115","2401","2402","2403","2404","2405","2406","2407","2408","2409","2410","2411","2412","2413",
        "2414","2415","2701","2702","2703","2704","2705","2706","2707","2708","2709","2710","2711","2712",
        "2713","2714","2715","2716","2717","2718","2801","2802","2803","2804","2805","2806","2807","2808",
        "2809","2810","2811","2812","2813","2814","2815","2816","2817","2818","2819","3201","3202","3203",
        "3204","3205","3206","3207","3208","3209","3210","3211","3212","3213","3214","3215","3216","3217",
        "3218","3219","3220","3221","3222","4401","4402","4403","4404","4405","4406","4407","4408","4409",
        "4410","4411","4412","4413","4414","4415","4416","4417","4418","4419","4420","4421","4422","4423",
        "4424","4425","4426","4427","5201","5202","5203","5204","5205","5206","5207","5208","5209","5210",
        "5211","5212","5213","5214","5215","5216","5217","5218","5219","5220","5221","5301","5302","5303",
        "5304","5305","5306","5307","5308","5309","5310","5311","5312","5313","5314","5315","5316","5317",
        "5318","5319","7501","7502","7503","7504","7505","7506","7507","7508","7509","7510","7511","7512",
        "7513","7514","7515","7516","7517","7518","7519","7520","7521","7522","7523","7524","7525","7526",
        "7527","7528","7529","7530","7531","7532","7601","7602","7603","7604","7605","7606","7607","7608",
        "7609","7610","7611","7612","7613","7614","7615","7616","7617","7618","7619","7620","7621","7622",
        "7623","7624","7625","8401","8402","8403","8404","8405","8406","8407","8408","8409","8410","8411",
        "8412","8413","8414","8415","8416","8417","8418","8419","8420","8421","8422","8423","8424","8425",
        "8426","8427","8428","8429","8430","8431","8432","8433","8434","8435","9301","9302","9303","9304",
        "9305","9306","9307","9308","9309","9310","9311","9312","9313","9314","9315","9316","9317","9318",
        "9401","9402","9403","9404","9405","9406","9407")

liste_valeurs <- c() # Liste qui va nous permettre de stocker toutes nos valeurs par zone d'emploi
valeurs <- c()

# Récupération des valeurs directement sur le site de l'Insee pour la première zone d'emploi
# Nous ne réalisons pas tout de suite une boucle pour pouvoir récupérer les caractères inconnues 
# qui nous empèche la convertion
link <- paste0("https://www.insee.fr/fr/statistiques/6037462?geo=ZE2020-0051")
page <- read_html(link)
valeurs <- page %>% html_nodes(".nombre") %>% html_text()

# Nous cherchons à enlever l'espace qui n'est pas un espace
# Pour cela nous récupérons ce fameux caractère inconnu
carac_inconnu <- substr(valeurs[1],3,3)
# Ensuite en le retire dans l'intégralité des valeurs
valeurs <- str_replace_all(valeurs,carac_inconnu ,"")

# Modification des virgules par des points
valeurs <- str_replace_all(valeurs,",",".")

# Nous cherchons à enlever un tiret qui n'est pas un tiret
# Pour cela nous récupérons ce fameux caractère inconnu 2
carac_inconnu2 <- substr(valeurs[26],1,1)
# Ensuite en le retire dans l'intégralité des valeurs
valeurs <- str_replace_all(valeurs,carac_inconnu2 ,"-")

# Transformation des valeurs en numérique
valeurs <- as.numeric(valeurs)

liste_valeurs <- rbind(liste_valeurs, t(valeurs))

# Création d'une boucle qui parcours toute les zones d'emplois sauf la première comme vu auparavant
for (i in ze){
  valeurs <- c()
  link <- paste0("https://www.insee.fr/fr/statistiques/6037462?geo=ZE2020-", i)
  page <- read_html(link)
  valeurs <- page %>% html_nodes(".nombre") %>% html_text()
  valeurs <- str_replace_all(valeurs,carac_inconnu,"")
  valeurs <- str_replace_all(valeurs,",",".")
  valeurs <- str_replace_all(valeurs,carac_inconnu2,"-")
  valeurs <- as.numeric(valeurs)
  liste_valeurs <- rbind(liste_valeurs, t(valeurs))
}

# Création de la data frame 
df_socio <- data.frame(c("0051",ze), liste_valeurs)

# Noms des colonnes de la data frame
nom_colonnes <- c("Zone d'emploi 2020", 
                  "Nombre de ménages fiscaux", 
                  "Nombre de personnes dans les ménages fiscaux", 
                  "Médiane du revenu disponible par unité de consommation (en euros)", 
                  "Part des ménages fiscaux imposés (en %)", 
                  "Taux de pauvreté (en %) - Ensemble", 
                  "Taux de pauvreté (en %) - Moins de 30 ans", 
                  "Taux de pauvreté (en %) - De 30 à 39 ans", 
                  "Taux de pauvreté (en %) - De 40 à 49 ans", 
                  "Taux de pauvreté (en %) - De 50 à 59 ans", 
                  "Taux de pauvreté (en %) - De 60 à 74 ans", 
                  "Taux de pauvreté (en %) - 75 ans ou plus", 
                  "Taux de pauvreté (en %) - Ensemble",
                  "Taux de pauvreté (en %) - Propriétaire", 
                  "Taux de pauvreté (en %) - Locataire", 
                  "Revenus (en %) - Ensemble", 
                  "Revenus (en %) - Part des revenus d'activité", 
                  "Revenus (en %) - Dont part salaires et traitements", 
                  "Revenus (en %) - Dont part indemnités et chômage", 
                  "Revenus (en %) - Dont part revenus des activités non salariées", 
                  "Revenus (en %) - Part des pensions, retraites et rentes", 
                  "Revenus (en %) - Part des revenus du patrimoine et autres revenus", 
                  "Revenus (en %) - Part de l'ensemble des prestations sociales", 
                  "Revenus (en %) - Dont part des prestations familiales", 
                  "Revenus (en %) - Dont part des minima sociaux", 
                  "Revenus (en %) - Dont part des prestations logement", 
                  "Revenus (en %) - Part des impôts", 
                  "Distribution des revenus (en euros) - Médiane du revenu disponible par unité de consommation", 
                  "Distribution des revenus (sans unité) - Rapport interdécile (9e décile/1er décile)", 
                  "Distribution des revenus (en euros) - 1er décile", 
                  "Distribution des revenus (en euros) - 9e décile")

# Renommage des colonnes de la data frame 
colnames(df_socio) <- nom_colonnes

# On retire les colonnes en double ou inutile 
df_socio <- df_socio[-c(7:13)]
df_socio <- df_socio[-9]


# Ecriture de la data frame dans un fichier excel
#write.xlsx(df_socio, "bdd_social_ze2020.xlsx", sheetName = "Revenus et pauvreté des ménages", col.names = TRUE, row.names = FALSE, append = FALSE)

###############################################################################
#           Récupération Emplois en 2018 par fichier Excel
###############################################################################

# Récupération de la feuille excel Emploi total
emploi_total_1998_2018 <- read_excel("emploi-zone-1998-2018.xlsx", sheet = "Emploi total - ZE", range = "A5:W310")

# Récupération de la feuille excel Emploi non salarié
emploi_non_salarie_1998_2018 <- read_excel("emploi-zone-1998-2018.xlsx", sheet = "Emploi non salarié - ZE", range = "A5:W310")

# Récupération de la feuille excel Emploi salarié
emploi_salarie_1998_2018 <- read_excel("emploi-zone-1998-2018.xlsx", sheet = "Emploi salarié - ZE", range = "A4:X1835")
emploi_salarie_1998_2018 <- emploi_salarie_1998_2018[-1831,]

# Modification de la première colonne pour avoir les zones d'emplois à part de leur libellé
emploi_non_salarie_1998_2018[c(1:2)] <- str_split_fixed(emploi_non_salarie_1998_2018$`Zone d'emploi`, " - ", 2)
emploi_total_1998_2018[c(1:2)] <- str_split_fixed(emploi_total_1998_2018$`Zone d'emploi`, " - ", 2)
emploi_salarie_1998_2018[c(1:2)] <- str_split_fixed(emploi_salarie_1998_2018$`Zone d'emploi`, " - ", 2)

# On retire les années de 1998 à 2017
emploi_non_salarie_2018 <- emploi_non_salarie_1998_2018[-c(3:22)]
emploi_total_2018 <- emploi_total_1998_2018[-c(3:22)]
emploi_salarie_2018 <- emploi_salarie_1998_2018[-c(4:23)]

# Zone d'emploi des outre-mers
ze_outre_mer <- c("0104","0102","0105","0101","0103","0203","0206","0204","0202","0205","0201","0301","0303","0302","0404","0401","0402","0403","0601")

# On retire les ze d'outre-mers
l <- c()
l1 <- c()
for (i in ze_outre_mer){
  l <- c(l,which(emploi_non_salarie_2018$`Zone d'emploi` == i))
  l1 <- c(l1,which(emploi_salarie_2018$`Zone d'emploi` == i))
}
l1 <- sort(l1)
emploi_non_salarie_2018 <- emploi_non_salarie_2018[-l,]
emploi_total_2018 <- emploi_total_2018[-l,]
emploi_salarie_2018 <- emploi_salarie_2018[-l1,]

# On retire les deux premiers colonnes d'emploi non salarie 
emploi_non_salarie_2018 <- emploi_non_salarie_2018[-c(1:2)]

# On retire les deux premiers colonnes d'emploi salarie 
emploi_salarie_2018 <- emploi_salarie_2018[-c(1:3)]

# Transposition de l'emploi salarie
emploi_salarie_2018 <- t(emploi_salarie_2018)

# On coupe le vecteur ligne en 6 pour les 6 secteurs
emploi_salarie_2018 <- matrix(emploi_salarie_2018, ncol=6)

# Renommage des colonnes
colnames(emploi_total_2018) <- c("Zone d'emploi 2020", "Libellé", "Emploi total")
colnames(emploi_non_salarie_2018) <- c("Emploi non salarié")
colnames(emploi_salarie_2018) <- c("Emploi salarié - Agriculture", 
                                   "Emploi salarié - Industrie", 
                                   "Emploi salarié - Construction", 
                                   "Emploi salarié - Tertiaire marchand", 
                                   "Emploi salarié - Tertiaire non marchand",
                                   "Emploi salarié - Total")

# Création de la data frame emploi 
df_emploi <- cbind(emploi_total_2018, emploi_non_salarie_2018, emploi_salarie_2018)

# Ecriture de la data frame dans un fichier excel
#write.xlsx(df_emploi, "bdd_social_ze2020.xlsx", sheetName = "Emploi en 2018", col.names = TRUE, row.names = FALSE, append = TRUE)

###############################################################################
#         Récupération Taux de chomage en 2020 par fichier Excel
###############################################################################

# Question: Est-ce que nous prendrions pas le taux de chômage en 2019 pour être cohérent 
# Récupération de la feuille excel 
taux_chomage_2003_2020 <- read_excel("chomage-zone-2003-2020.xlsx", sheet = "txcho_ze", range = "A5:V307")

# Zone d'emploi des outre-mers
ze_outre_mer <- c("0104","0102","0105","0101","0103","0203","0206","0204","0202","0205","0201","0301","0303","0302","0404","0401","0402","0403","0601")

# On retire les ze d'outre-mers
l <- c()
for (i in ze_outre_mer){
  l <- c(l,which(taux_chomage_2003_2020$`Code de la zone d'emploi 2020` == i))
}
taux_chomage_2003_2020 <- taux_chomage_2003_2020[-l,]

# On retire les colonnes inutiles
taux_chomage_2020 <- taux_chomage_2003_2020[-c(1:21)]

# Renommage des colonnes
colnames(taux_chomage_2020 ) <- c("Taux de chômage (en %)")

###############################################################################
#    Récupération de la population active en 2017 par webscrapping
###############################################################################

# Numéros zones d'emplois
ze <- c("0052","0053","0054","0055","0056","0057","0058","0059","0060","0061","0062","0063","0064",
        "1101","1102","1103","1104","1105","1106","1107","1108","1109","1110","1111","1112","1113","1114",
        "1115","2401","2402","2403","2404","2405","2406","2407","2408","2409","2410","2411","2412","2413",
        "2414","2415","2701","2702","2703","2704","2705","2706","2707","2708","2709","2710","2711","2712",
        "2713","2714","2715","2716","2717","2718","2801","2802","2803","2804","2805","2806","2807","2808",
        "2809","2810","2811","2812","2813","2814","2815","2816","2817","2818","2819","3201","3202","3203",
        "3204","3205","3206","3207","3208","3209","3210","3211","3212","3213","3214","3215","3216","3217",
        "3218","3219","3220","3221","3222","4401","4402","4403","4404","4405","4406","4407","4408","4409",
        "4410","4411","4412","4413","4414","4415","4416","4417","4418","4419","4420","4421","4422","4423",
        "4424","4425","4426","4427","5201","5202","5203","5204","5205","5206","5207","5208","5209","5210",
        "5211","5212","5213","5214","5215","5216","5217","5218","5219","5220","5221","5301","5302","5303",
        "5304","5305","5306","5307","5308","5309","5310","5311","5312","5313","5314","5315","5316","5317",
        "5318","5319","7501","7502","7503","7504","7505","7506","7507","7508","7509","7510","7511","7512",
        "7513","7514","7515","7516","7517","7518","7519","7520","7521","7522","7523","7524","7525","7526",
        "7527","7528","7529","7530","7531","7532","7601","7602","7603","7604","7605","7606","7607","7608",
        "7609","7610","7611","7612","7613","7614","7615","7616","7617","7618","7619","7620","7621","7622",
        "7623","7624","7625","8401","8402","8403","8404","8405","8406","8407","8408","8409","8410","8411",
        "8412","8413","8414","8415","8416","8417","8418","8419","8420","8421","8422","8423","8424","8425",
        "8426","8427","8428","8429","8430","8431","8432","8433","8434","8435","9301","9302","9303","9304",
        "9305","9306","9307","9308","9309","9310","9311","9312","9313","9314","9315","9316","9317","9318",
        "9401","9402","9403","9404","9405","9406","9407")

liste_valeurs <- c() # Liste qui va nous permettre de stocker toutes nos valeurs par zone d'emploi

# Récupération des valeurs directement sur le site de l'Insee pour la première zone d'emploi
# Nous ne réalisons pas tout de suite une boucle pour pouvoir récupérer les caractères inconnues 
# qui nous empèche la convertion
link <- paste0("https://www.insee.fr/fr/statistiques/4515512?sommaire=4515574&geo=ZE2020-0051")
page <- read_html(link)
valeur <- page %>% html_nodes("#produit-tableau-EMP_T1 tr:nth-child(2) .nombre~ .total+ .total") %>% html_text()

# Nous cherchons à enlever l'espace qui n'est pas un espace
# Pour cela nous récupérons ce fameux caractère inconnu
carac_inconnu <- substr(valeur[1],3,3)
# Ensuite en le retire dans l'intégralité des valeurs
valeur <- str_replace_all(valeur,carac_inconnu ,"")

# Transformation des valeurs en numérique
valeur <- as.numeric(valeur)

liste_valeurs <- rbind(liste_valeurs, valeur)

# Création d'une boucle qui parcours toute les zones d'emplois sauf la première comme vu auparavant
for (i in ze){
  link <- paste0("https://www.insee.fr/fr/statistiques/4515512?sommaire=4515574&geo=ZE2020-", i)
  page <- read_html(link)
  valeur <- page %>% html_nodes("#produit-tableau-EMP_T1 tr:nth-child(2) .nombre~ .total+ .total") %>% html_text()
  valeur <- str_replace_all(valeur,carac_inconnu,"")
  valeur <- as.numeric(valeur)
  liste_valeurs <- rbind(liste_valeurs, valeur)
}

# Création de la data frame 
df_pop <- data.frame(liste_valeurs)

# Renommage des colonnes de la data frame 
colnames(df_pop) <- "Population active"


###############################################################################
#     Création du fichier excel avec les trois df récupérer auparavant
###############################################################################

# Concaténation des trois df
df_final <- cbind(df_socio,taux_chomage_2020, df_emploi, df_pop)

# Changement d'ordre dans la df final 
df_final <- cbind(df_final[,c(1,26,2:24,27:35)])

# Ecriture de la data frame dans un fichier excel
write.xlsx(df_final, "bdd_social_ze2020.xlsx", sheetName = "Zone d'emploi 2020", col.names = TRUE, row.names = FALSE, append = FALSE)





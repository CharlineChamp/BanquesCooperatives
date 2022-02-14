library(rvest) # Utile pour read_html
library(stringr) # Utile pour str_replace_all

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

# Ecriture de la data frame dans un fichier excel
write.xlsx(df_socio, "bdd_social_ze2020.xlsx", sheetName = "Données 2019 sur ZE 2020", col.names = TRUE, row.names = FALSE, append = FALSE)


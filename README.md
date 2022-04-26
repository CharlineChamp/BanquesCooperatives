# BanquesCooperatives

Ce projet porte sur la visualisation de données spatiales pour tenter d’expliquer la position géographique des banques en France métropolitaine. Le but étant de proposer une application de visualisation de données qui nous permettra de mettre en relation la position géographique des banques en fonction de données socio-économiques à l’échelle des zones d’emploi. 

## Informations Générales 

Le dossier le plus important sur ce Git, est le dossier [```Shiny```](https://github.com/CharlineChamp/BanquesCooperatives/tree/main/Shiny). En effet, ce dernier permet de créer l'application de visualisation de données. Il fait référence aux dossiers : [```Carte```](https://github.com/CharlineChamp/BanquesCooperatives/tree/main/Carte) et [```Données```](https://github.com/CharlineChamp/BanquesCooperatives/tree/main/Données).  
Tous les fichiers contenus dans ```Données``` sont issus de le récupération de données réalisée par les codes présents dans le dossier[```Web_scraping```](https://github.com/CharlineChamp/BanquesCooperatives/tree/main/Web_scraping).Vous trouverez dans le dossier [```Test```](https://github.com/CharlineChamp/BanquesCooperatives/tree/main/Test) tous les fichiers permettant de tester nos codes et fonctions. Le dossier [```Analyse```](https://github.com/CharlineChamp/BanquesCooperatives/tree/main/Analyse) contient  les fichiers de codes relatifs à une brève analyse statistique. Le dossier [```Rapport```](https://github.com/CharlineChamp/BanquesCooperatives/tree/main/Rapport) retrace toutes les étapes de la réalisation de l’écriture de notre rapport finale. . Chaque dossier comporte plusieurs versions. Nous utilisons à chaque fois la plus récente. Les autres versions sont disponibles car elles permettent la compréhension et la visualisation de l'évolution de notre travail.  
  
## Packages

Plusieurs packages ont été utilisés pour mener a bien ce projet. Nous recensons ceux qui sont relatifs à l'application *shiny* : 
```shiny```,```shinythemes```,```shinydashboard```,```dashboardthemes```,```shinydashboardPlus```.

Et d'autres pour la visualisation et manipulation de données : 
```plotly```,```ggplot2```,```dplyr```,```sf```,```spatstat``` et ```BP2CAMSG```.

Vérifiez bien que vous les ayez tous pour pouvoir lancer le fichier [```shiny_V6.R```](https://github.com/CharlineChamp/BanquesCooperatives/blob/main/Shiny/shiny_V6.R).

Le package ```BP2CAMSG``` contient toutes les données recueillies ainsi que toutes les fonctions permettant de produire des cartes. Pour plus d'informations sur ce dernier, il est disponible [ici](https://github.com/CharlineChamp/BP2CAMSG). Pour profiter plainement de notre application shiny, il est nécessaire de l'installer.
Pour ce faire, vous pouvez procéder comme suit: 
```r
remotes::install_github('CharlineChamp/BP2CAMSG') 
```
## Auteurs

[Batiste Amistadi](https://github.com/devilbaba), [Charline Champ](https://github.com/CharlineChamp), [Antoine Grancher](https://github.com/Antoine7526) 

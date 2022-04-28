library(shiny)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)
library(shinydashboardPlus)
library(plotly)
library(BP2CAMSG)


ui <- dashboardPage(
  # Creation du tableau du haut
  header = dashboardHeader(title = "Visualisation de banques",
                           titleWidth = 270,
                           dropdownMenu(type = "messages",
                                        icon = icon("info-circle"),
                                        badgeStatus = NULL,
                                        headerText = "Mise à jour le 26/04/2022")),
  # Creation du menu de l'interface à gauche
  sidebar = dashboardSidebar(
    width="250px",
    sidebarMenu(
      menuItem("Accueil", tabName = "Accueil", icon = icon("bank")),
      menuItem("Carte France", tabName = "Comparaison Banque-Type", icon = icon("map"),
               menuSubItem("Map",tabName = "Map",icon=icon("map")),
               menuSubItem("Comparaison Type",tabName = "Comparaison-Type",icon=icon("not-equal")),
               menuSubItem("Comparaison Banque",tabName = "Comparaison-Banque",icon=icon("bank")),
               menuSubItem("Comparaison Critère Type",tabName = "Comparaison-Critere-Type", icon=icon("not-equal")),
               menuSubItem("Comparaison Critère Banque",tabName = "Comparaison-Critere-Banque",icon=icon("bank"))),
      menuItem("Autres choix de carte",tabName = "autres_carte",icon=icon("map"),
               menuSubItem("Carte d'une zone d'emploi",tabName = "Carte-zone",icon=icon("square")),
               menuSubItem("Carte dynamique",tabName = "Carte_dyna",icon=icon("map"))),
      menuItem("Données", tabName = "Donnees", icon = icon("database"),
               menuSubItem("Banques",tabName = "Banques",icon=icon("bank")),
               menuSubItem("Critères",tabName = "Criteres",icon=icon("hashtag")),
               menuSubItem("Effectif",tabName = "Effectif",icon=icon("database"))))),
  
  body = dashboardBody(
    # Theme de l'interface
    shinyDashboardThemes("purple_gradient"),
    # Mise en forme de chaque page de l'interface
    tabItems(
      tabItem(tabName = "Accueil", 
              fluidRow(column(width = 8,offset = 2,  align = "center", tags$div(tags$h1("Visualisation du positionnement des banques coopératives suivant des facteurs socio-économiques avec en population témoin les banques lucratives."),tags$br(),tags$br(),tags$br()))),
              fluidRow(column(width=6,tags$img(src='populaire.jpg', height = '150', width ='150'),
                       tags$img(src='mutuel.jpg', height = '150', width ='150'),
                       tags$img(src='agricole.png', height = '150', width ='150'),br(),br(),br()),
                      column(width = 4, align = "center",tags$div(style= "font-weight: 1000; font-family: DejaVu Sans Mono; monospace","Cette application a été realisée dans le cadre d'un projet tutoré du master SSD sous l'encadrement de M.Coeurjolly et Amélie Artis.",tags$br(),"Réalisé par : ",tags$br(), "Batiste Amistadi, Charline Champ, Antoine Grancher, Cheikh Darou beye et Alicia Lorandi"))),
              fluidRow(column(width=5,tags$img(src='generale.jpg', height = '150', width ='150'),
                       tags$img(src='bnp.jpg', height = '150', width ='150')),
                       column(width = 4,offset = 1,tags$img(src='logoblanc.png',width='296',height='181')))
              
        ),
      tabItem("Comparaison Banque-Type"),
      tabItem("Map",
              fluidRow(column(width=4,selectInput("choice",label="Choix de la représentation : ",choices=c("Type","Banque"))),
                       column(width=4,selectInput("criteria",label="Choix du critère :", choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width=4,uiOutput("Choice"))),
              actionButton("aff","Afficher"),
              fluidRow(column(width = 12,div(style= "font-weight: 1000; font-family: Arial; monospace","Carte au choix",tags$br()),plotlyOutput("map_generale", width = "auto",height = "auto"),align="center"))),
      tabItem("Comparaison-Type",
              fluidRow(column(width=12,selectInput("Critere", label = "Choix du critère",choices =c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des banques coopératives",tags$br(),tags$br()),plotlyOutput("carte_cooperative", width = "auto",height = "auto"),align="center"),
                       column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace",tags$br(),tags$br(),"Carte des banques lucratives",tags$br(),tags$br()),plotlyOutput("carte_lucrative", width = "auto",height = "auto"),align="center")),
      ),
      tabItem("Comparaison-Banque",
              fluidRow(column(width=12,selectInput("critere", label = "Choix du critère",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width=12,selectInput("Banque", label = "Choix des premières banques : " ,choices = c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple=TRUE)),
                       column(width=12,selectInput("Banqu", label = "Choix des deuxièmes banques : ",choices = c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple=TRUE))),
              actionButton("affiche","Afficher"),
              fluidRow(column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des premières banques",tags$br()),plotlyOutput("carte_banque_1", width = "auto",height = "auto"),align="center"),
                       column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace",tags$br(),tags$br(),"Carte des deuxièmes banques",tags$br()),plotlyOutput("carte_banque_2", width = "auto",height = "auto"),align="center"))),
      tabItem("Comparaison-Critere-Type",
              fluidRow(column(width=12,colum.height=12,selectInput("Type_banque",label="Choix du type de banque : ",choices=unique(bdd_coordonnees_banques2022$Type)))),
              fluidRow(column(width=12,selectInput("Critere_1", label = "Choix du premier critère : ",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)])))),
                       column(width=12,selectInput("Critere_2", label = "Choix du deuxième critère : ",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte du premier critère",tags$br(),tags$br()), plotlyOutput("carte_critere_type_1", width = "auto",height = "auto"),align="center"),
                       column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace",tags$br(),tags$br(),"Carte du deuxième critère",tags$br(),tags$br()),plotlyOutput("carte_critere_type_2", width = "auto",height = "auto"),align="center"))),
      tabItem("Comparaison-Critere-Banque",
              fluidRow(column(width=12,selectInput("critere_1", label = "Choix du premier critère : ",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)])))),
                       column(width=12,selectInput("critere_2", label = "Choix du deuxième critère : ",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width=12,selectInput("Banque_1", label = "Choix des premières banques : ",choices = c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple=TRUE)),
                       column(width=12,selectInput("Banque_2", label = "Choix des deuxièmes banques : ",choices = c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple=TRUE))),
              actionButton("afficher","Afficher"),
              fluidRow(column(width = 12,div(style= "font-weight: 1000; font-family: Arial; monospace","Carte des premières banques",tags$br()),plotlyOutput("carte_critere_banque_1", width = "auto",height = "auto"),align="center"),
                       column(width = 12,div(style= "font-weight: 1000; font-family: Arial; monospace",tags$br(),tags$br(),"Carte des deuxièmes banques",tags$br()),plotlyOutput("carte_critere_banque_2", width = "auto",height = "auto"),align="center"))),
      tabItem("Carte-zone",
              fluidRow(column(width = 6,selectInput("zone_carte",label="Choix de la zone d'emploi : ",choices = paste0(bdd_zese$`Zone d'emploi 2020`," - ",bdd_zese$Libellé))),
                       column(width = 6,checkboxInput("colo_type", "Coloration par Type", FALSE))),
              fluidRow(column(width = 12,align="center",tags$h3("Carte d'une zone d'emploi"))),
              plotlyOutput("carte_zone",width = "auto",height = "auto")),
      tabItem("Carte_dyna",
              fluidRow(column(width=12,align="center",tags$h3("Carte dynamiques des données coloriées par banque"))),
              plotlyOutput("carte_dyn",width = "auto",height = "auto")),
      tabItem("Donnees"),
      tabItem("Banques",
              fluidRow(
                column(4,selectInput("Banques","Banque :",c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple = TRUE)),
                column(4,selectInput("Types","Type :",c("All",unique(bdd_coordonnees_banques2022$Type))))),
              DT::dataTableOutput("table1")),
      tabItem("Criteres",
              fluidRow(
                column(4,selectInput("Crit","Critère :",c("All",colnames(bdd_zese[-c(1,2,35)])),multiple = TRUE)),
                column(4,selectInput("Zone","Zone d'emploi :",c("All",bdd_zese$`Zone d'emploi 2020`),multiple = TRUE))),
              DT::dataTableOutput("table2")),
      tabItem("Effectif",
              fluidRow(
                column(4,selectInput("Banquess","Banque :",c("All",unique(bdd_coordonnees_ze_banques2022$Banque)),multiple = TRUE)),
                column(4,selectInput("Zones","Zone d'emploi :",c("All",bdd_zese$`Zone d'emploi 2020`),multiple = TRUE))),
              DT::dataTableOutput("table3"))
    )
  ),
  
  title = "Banques"
)

server <- function(input,output){
  # Conception du choix de données à afficher de l'onglet Map
  output$Choice <- renderUI({
    if(input$choice=="Type"){
      selectInput("types",label = "Choix du type : ",choices=c("All",unique(bdd_coordonnees_banques2022$Type)))
    }else{
      selectInput("bank",label = "Choix des banques : ",choices=unique(bdd_coordonnees_banques2022$Banque),multiple = TRUE)
    }
    
  })
  
  # Bouton afficher de l'onglet Map
  data_carte <- eventReactive(input$aff,{
    data <- bdd_coordonnees_banques2022
    if(input$choice=="Banque"){
      if (!(length(input$bank)==0)){
        data <- filter(data,Banque %in% input$bank)
      }    
    }
    if(input$choice=="Type"){
      if(!(input$types=="All")){
        data <- filter(data,Type %in% input$types)
      }
    }
    data
    
  })
  
  # Conception de la carte de l'onglet Map
  output$map_generale <- renderPlotly({
    data <- data_carte()
    point <- if(input$choice=="Banque"){0}else{if(input$types=="All"){1}else{2}}
    gg_map(bdd_zese,input$criteria,point,data)
  })
  
  # Conception de la carte des banques coopératives de l'onglet Comparaison Type
  output$carte_cooperative <- renderPlotly({
    data <- bdd_coordonnees_banques2022[bdd_coordonnees_banques2022$Type=="Coopérative",]
    gg_map(bdd_zese,input$Critere,2,data)
  })
  
  # Conception de la carte des banques lucratives de l'onglet Comparaison Type
  output$carte_lucrative <- renderPlotly({
    data <- bdd_coordonnees_banques2022[bdd_coordonnees_banques2022$Type=="Lucrative",]
    gg_map(bdd_zese,input$Critere,2,data)
  })
  
  # Bouton afficher de l'onglet Comparaison Banque
  data_banque <- eventReactive(input$affiche,{
    
    data <- bdd_coordonnees_banques2022
    if (!(length(input$Banqu)==0 | "All" %in% input$Banque)){
      data <- filter(data,Banque %in% input$Banque)
    }
    data2 <- bdd_coordonnees_banques2022
    if (!(length(input$Banqu)==0 | "All" %in% input$Banqu)){
      data2 <- filter(data2,Banque %in% input$Banqu)
    }
    data
    
  })
  
  # Conception de la première carte de l'onglet Comparaison Banque
  
  output$carte_banque_1 <- renderPlotly({
    data <- data_banque()
    gg_map(bdd_zese,input$critere,0,data)
  })
  
  # Conception de la deuxième carte de l'onglet Comparaison Banque
  
  output$carte_banque_2 <- renderPlotly({
    data <- data_banque()
    gg_map(bdd_zese,input$critere,0,data)
  })
  
  # Conception de la première carte de l'onglet Comparaison Critère Type

  output$carte_critere_type_1 <- renderPlotly({
    data <- bdd_coordonnees_banques2022[bdd_coordonnees_banques2022$Type==input$Type_banque,]
    gg_map(bdd_zese,input$Critere_1,2,data)
  })
  
  # Conception de la deuxième carte de l'onglet Comparaison Critère Type
    
  output$carte_critere_type_2 <- renderPlotly({
    data <- bdd_coordonnees_banques2022[bdd_coordonnees_banques2022$Type==input$Type_banque,]
    gg_map(bdd_zese,input$Critere_2,2,data)
  })
  
  # Bouton afficher de l'onglet Comparaison Critère Banque
  data_type <- eventReactive(input$afficher,{
    
    data <- bdd_coordonnees_banques2022
    if (!(length(input$Banque_1)==0 | "All" %in% input$Banque_1)){
      data <- filter(data,Banque %in% input$Banque_1)
    }
    data2 <- bdd_coordonnees_banques2022
    if (!(length(input$Banque_2)==0 | "All" %in% input$Banque_2)){
      data2 <- filter(data2,Banque %in% input$Banque_2)
    }
    list(data,data2)
    
  })
  
  # Conception de la première carte de l'onglet Comparaison Critère Banque
  output$carte_critere_banque_1 <- renderPlotly({
    tt<-data_type()
    data <- tt[[1]]
    gg_map(bdd_zese,input$critere_1,0,data)
  })
  
  # Conception de la deuxième carte de l'onglet Comparaison Critère Banque
  output$carte_critere_banque_2 <- renderPlotly({
    tt <- data_type()
    data <- tt[[2]]
    gg_map(bdd_zese,input$critere_2,0,data)
  })
  
  # Conception de la carte de l'onglet Carte d'une zone d'emploi
  output$carte_zone <- renderPlotly({
    temp <- unlist(strsplit(input$zone_carte,"-"))[1]
    temp <- substr(temp,1,nchar(temp)-1)
    gg_map_ze(bdd_zese,bdd_coordonnees_ze_banques2022,input$colo_type,temp)
  })
  
  # Conception de la carte de l'onglet Carte dynamique
  output$carte_dyn <- renderPlotly({
    plotly_map(bdd_coordonnees_banques2022)
  })
  
  # Tri du jeu données de l'onglet Banques
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- bdd_coordonnees_banques2022
    if(length(input$Banques)==0){
      if (input$Types != "All") {
        data <- data[data$Type == input$Types,]
      }else{
        return(data)
      }
    }else{
      if (!("All" %in% input$Banques)) {
        
        data <- filter(data,Banque %in% input$Banques)
      }else{
        if (input$Types != "All") {
          data <- data[data$Type == input$Types,]
        }else{
          return(data)
        }
      }
      if (input$Types != "All") {
        data <- data[data$Type == input$Types,]
      }
      data
    }
  }))
  
  # Tri du jeu données de l'onglet Critères
  output$table2 <- DT::renderDataTable(DT::datatable({
    data <- bdd_zese[,-35]
    if (!(("All" %in% input$Zone) || (length(input$Zone)==0))) {
      data <- filter(data,data$`Zone d'emploi 2020`%in% input$Zone)
    }
    if (!(("All" %in% input$Crit) || (length(input$Crit)==0))) {
      data <- data[,c(colnames(data)[1:2],input$Crit)]
    }
    data
  }))
  
  # Tri du jeu données de l'onglet Effectif
  output$table3 <- DT::renderDataTable(DT::datatable({
    data <- bdd_effectif_banques2022_ze2020[,-1]
    
    if (!(("All" %in% input$Zones) || (length(input$Zones)==0))) {
      data <- filter(data,data$`Zone d'emploi 2020`%in% input$Zones)
    }
    if (!(("All" %in% input$Banquess) || (length(input$Banquess)==0))) {
      data <- data[,c("Zone d'emploi 2020",input$Banquess)]
    }
    if(class(data)=="integer"){
      data<-data.frame(data)
      colnames(data)<- input$Banquess
      if("All" %in% input$Zones || length(input$Zones)==0){
        row.names(data)<-bdd_zese$`Zone d'emploi 2020`
      }else{
        row.names(data)<- input$Zones
      }
    }
    data
  }))    
  
}

shinyApp(ui,server)
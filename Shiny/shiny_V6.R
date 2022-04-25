library(shiny)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)
library(shinydashboardPlus)
library(plotly)
library(BP2CAMSG)


ui <- dashboardPage(
  header = dashboardHeader(title = "Comparatif de banques avec des critères socio-économiques",
                           titleWidth = 600,
                           dropdownMenu(type = "messages",
                                        icon = icon("info-circle"),
                                        badgeStatus = NULL,
                                        headerText = "Mise à jour le 25/04/2022")),
  
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
      menuItem("Carte zone d'emploi",tabName = "Carte_zone",icon=icon("map")),
      menuItem("Données", tabName = "Donnees", icon = icon("database"),
               menuSubItem("Banques",tabName = "Banques",icon=icon("bank")),
               menuSubItem("Critères",tabName = "Criteres",icon=icon("hashtag")),
               menuSubItem("Effectif",tabName = "Effectif",icon=icon("database"))))),
  
  body = dashboardBody(
    shinyDashboardThemes("purple_gradient"),
    tabItems(
      tabItem(tabName = "Accueil", fluidRow(column(width = 10,offset = 1,  align = "center", tags$div(tags$br(),tags$br(), tags$h1("Analyse du positionnement des banques coopératives suivant des facteurs socio-économiques avec en population témoin les banques lucratives."),tags$br(),tags$br(),tags$br(),tags$br()))),
              fluidRow(align="center",tags$img(src='populaire.jpg', height = '203', width ='203'),
                       tags$img(src='mutuel.jpg', height = '203', width ='203'),
                       tags$img(src='agricole.png', height = '203', width ='203'),br(),br(),br()),
              fluidRow(align="center",tags$img(src='generale.jpg', height = '203', width ='203'),
                       tags$img(src='bnp.jpg', height = '203', width ='203')),
              fluidRow(column(width = 2,offset = 10,  align = "center",tags$div(style= "font-weight: 1000; font-family: DejaVu Sans Mono; monospace",tags$br(),tags$br(),tags$br(),"Realise par:", tags$br(), "Batiste Amistadi, Charline Champ, Antoine Grancher, Cheikh Darou beye et Alicia Lorandi")))),
      tabItem("Comparaison Banque-Type"),
      tabItem("Map",
              fluidRow(column(width=4,selectInput("choice",label="Choix de la représentation : ",choices=c("Type","Banque"))),
                       column(width=4,selectInput("criteria",label="Choix du critère :", choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width=4,uiOutput("Choice"))),
              actionButton("aff","Afficher"),
              fluidRow(column(width = 12,div(style= "font-weight: 1000; font-family: Arial; monospace","Carte au choix",tags$br()),plotlyOutput("map_generale", width = "auto",height = "800px"),align="center"))),
      tabItem("Comparaison-Type",
              fluidRow(column(width=12,selectInput("Critere", label = "Choix du critère",choices =c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des banques coopératives",tags$br()),plotlyOutput("carte_cooperative", width = "auto",height = "600px"),align="center"),
                       column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des banques lucratives",tags$br()),plotlyOutput("carte_lucrative", width = "auto",height = "600px"),align="center")),
      ),
      tabItem("Comparaison-Banque",
              fluidRow(column(width=12,selectInput("critere", label = "Choix du critère",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width=12,selectInput("Banque", label = "Choix des premières banques : " ,choices = c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple=TRUE)),
                       column(width=12,selectInput("Banqu", label = "Choix des deuxièmes banques : ",choices = c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple=TRUE))),
              actionButton("affiche","Afficher"),
              fluidRow(column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des premières banques",tags$br()),plotlyOutput("carte_banque_1", width = "auto",height = "600px"),align="center"),
                       column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des deuxièmes banques",tags$br()),plotlyOutput("carte_banque_2", width = "auto",height = "600px"),align="center"))),
      tabItem("Comparaison-Critere-Type",
              fluidRow(column(width=12,colum.height=12,selectInput("Type_banque",label="Choix du type de banque : ",choices=unique(bdd_coordonnees_banques2022$Type)))),
              fluidRow(column(width=12,selectInput("Critere_1", label = "Choix du premier critère : ",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)])))),
                       column(width=12,selectInput("Critere_2", label = "Choix du deuxième critère : ",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte du premier critère",tags$br(),tags$br()), plotlyOutput("carte_critere_type_1", width = "auto",height = "600px"),align="center"),
                       column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte du deuxième critère",tags$br(),tags$br()),plotlyOutput("carte_critere_type_2", width = "auto",height = "600px"),align="center"))),
      tabItem("Comparaison-Critere-Banque",
              fluidRow(column(width=12,selectInput("critere_1", label = "Choix du premier critère : ",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)])))),
                       column(width=12,selectInput("critere_2", label = "Choix du deuxième critère : ",choices = c("Pas de critère",colnames(bdd_zese[-c(1,2,35)]))))),
              fluidRow(column(width=12,selectInput("Banque_1", label = "Choix des premières banques : ",choices = c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple=TRUE)),
                       column(width=12,selectInput("Banque_2", label = "Choix des deuxièmes banques : ",choices = c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple=TRUE))),
              actionButton("afficher","Afficher"),
              fluidRow(column(width = 12,div(style= "font-weight: 1000; font-family: Arial; monospace","Carte des premières banques",tags$br()),plotlyOutput("carte_critere_banque_1", width = "auto",height = "600px"),align="center"),
                       column(width = 12,div(style= "font-weight: 1000; font-family: Arial; monospace","Carte des deuxièmes banques",tags$br()),plotlyOutput("carte_critere_banque_2", width = "auto",height = "600px"),align="center"))),
      tabItem("Carte_zone"),
      tabItem("Donnees"),
      tabItem("Banques",
              fluidRow(
                column(4,selectInput("Banques","Banque :",c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple = TRUE)),
                column(4,selectInput("Types","Type :",c("All",unique(bdd_coordonnees_banques2022$Type))))),
              DT::dataTableOutput("table1")),
      tabItem("Criteres",
              fluidRow(
                column(4,selectInput("Critères","Critère :",c("All",colnames(bdd_zese[-c(1,2,35)])),multiple = TRUE)),
                column(4,selectInput("Zone","Zone d'emploi :",c("All",bdd_zese$`Zone d'emploi 2020`),multiple = TRUE))),
              DT::dataTableOutput("table2")),
      tabItem("Effectif",
              fluidRow(
                column(4,selectInput("Banquess","Banque :",c("All",unique(bdd_coordonnees_banques2022$Banque)),multiple = TRUE)),
                column(4,selectInput("Zones","Zone d'emploi :",c("All",bdd_zese$`Zone d'emploi 2020`),multiple = TRUE))),
              DT::dataTableOutput("table3"))
    )
  ),
  
  title = "Banques"
)

server <- function(input,output){
  
  output$Choice <- renderUI({
    if(input$choice=="Type"){
      selectInput("types",label = "Choix du type : ",choices=c("All",unique(bdd_coordonnees_banques2022$Type)))
    }else{
      selectInput("bank",label = "Choix des banques : ",choices=unique(bdd_coordonnees_banques2022$Banque),multiple = TRUE)
    }
    
  })
  
  
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
  
  output$map_generale <- renderPlotly({
    data <- data_carte()
    point <- if(input$choice=="Banque"){0}else{if(input$types=="All"){1}else{2}}
    gg_map(bdd_zese,input$criteria,point,data)
  })
  
  
  output$carte_cooperative <- renderPlotly({
    data <- bdd_coordonnees_banques2022[bdd_coordonnees_banques2022$Type=="Coopérative",]
    gg_map(bdd_zese,input$Critere,2,data)
  })
  
  output$carte_lucrative <- renderPlotly({
    data <- bdd_coordonnees_banques2022[bdd_coordonnees_banques2022$Type=="Lucrative",]
    gg_map(bdd_zese,input$Critere,2,data)
  })
  
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
  
  
  output$carte_banque_1 <- renderPlotly({
    data <- data_banque()
    gg_map(bdd_zese,input$critere,0,data)
  })
  
  output$carte_banque_2 <- renderPlotly({
    data <- data_banque()
    gg_map(bdd_zese,input$critere,0,data)
  })
  
  output$carte_critere_type_1 <- renderPlotly({
    data <- bdd_coordonnees_banques2022[bdd_coordonnees_banques2022$Type==input$Type_banque,]
    gg_map(bdd_zese,input$Critere_1,2,data)
  })
  
  output$carte_critere_type_2 <- renderPlotly({
    data <- bdd_coordonnees_banques2022[bdd_coordonnees_banques2022$Type==input$Type_banque,]
    gg_map(bdd_zese,input$Critere_2,2,data)
  })
  
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
  
  output$carte_critere_banque_1 <- renderPlotly({
    tt<-data_type()
    data <- tt[[1]]
    gg_map(bdd_zese,input$critere_1,0,data)
  })
  
  
  output$carte_critere_banque_2 <- renderPlotly({
    tt <- data_type()
    data <- tt[[2]]
    gg_map(bdd_zese,input$critere_2,0,data)
  })
  
  
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
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    data <- bdd_zese[,-35]
    if(length(input$Criteres)==0){
      if(length(input$Zone)==0){
        return(data)
      } else {
        if("All" %in% input$Zone){
          return(data)
        }else{
          return(filter(data,data$`Zone d'emploi 2020`%in% input$Zone))
        }
      }
    } else {
      if (!("All" %in% input$Criteres)) {
        data <- data[,colnames(data) %in% c(colnames(data)[1:2],input$Criteres)]
      }
      if(length(input$Zone)==0){
        return(data)
      } else {
        if("All" %in% input$Zone){
          return(data)
        }else{
          return(filter(data,data$`Zone d'emploi 2020` %in% input$Zone))
        }
      }
    }
    data
  }))
  
  output$table3 <- DT::renderDataTable(DT::datatable({
    data <- bdd_coordonnees_banques2022
    if (input$Banquess != "All") {
      data <- data[data$Banque == input$Banquess,]
    }
    if (input$Zones != "All") {
      data <- data[data$Type == input$Zones,]
    }
    data
  }))    
  
}

shinyApp(ui,server)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)
library(shinydashboardPlus)
library(dplyr)
library(sf)
library(ggspatial)
library(viridis)
library(plotly)

ui <- dashboardPage(
    header = dashboardHeader(title = "Comparatif de banques avec des criteres socio-economiques",
                             titleWidth = 600,
                             dropdownMenu(type = "messages",
                                          icon = icon("info-circle"),
                                          badgeStatus = NULL,
                                          headerText = "Mise à jour le 29/03/2022")),
    
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Accueil", tabName = "Accueil", icon = icon("bank")),
            menuItem("Carte", tabName = "Comparaison Banque-Type", icon = icon("map"),
                     menuSubItem("map",tabName = "map",icon=icon("map")),
                     menuSubItem("Comparaison Type",tabName = "Comparaison-Type",icon=icon("not-equal")),
                     menuSubItem("Comparaison Banque",tabName = "Comparaison-Banque",icon=icon("bank")),
                     menuSubItem("Comparaison Critere Type",tabName = "Comparaison-Critere-Type", icon=icon("not-equal")),
                     menuSubItem("Comparaison Critere Banque",tabName = "Comparaison-Critere-Banque",icon=icon("bank"))),
            menuItem("Donnees", tabName = "Donnees", icon = icon("database"),
                     menuSubItem("Banques",tabName = "Banques",icon=icon("bank")),
                     menuSubItem("Criteres",tabName = "Criteres",icon=icon("hashtag")),
                     menuSubItem("Effectif",tabName = "Effectif",icon=icon("database"))))),
    
    body = dashboardBody(
        shinyDashboardThemes("poor_mans_flatly"),
        tabItems(
            tabItem(tabName = "Accueil", fluidRow(column(width = 10,offset = 1,  align = "center", tags$div(tags$br(),tags$br(), tags$h1("Analyse du positionnement des banques cooperatives suivant des facteurs socio-economiques avec en population temoin les banques lucratives."),tags$br(),tags$br(),tags$br(),tags$br()))),
                    fluidRow(align="center",tags$img(src='populaire.jpg', height = '203', width ='203'),
                             tags$img(src='mutuel.jpg', height = '203', width ='203'),
                             tags$img(src='agricole.png', height = '203', width ='203'),br(),br(),br()),
                    fluidRow(align="center",tags$img(src='generale.jpg', height = '203', width ='203'),
                             tags$img(src='bnp.jpg', height = '203', width ='203')),
                    fluidRow(column(width = 2,offset = 10,  align = "center",tags$div(style= "font-weight: 1000; font-family: DejaVu Sans Mono; monospace",tags$br(),tags$br(),tags$br(),"Realise par:", tags$br(), "Batiste Amistadi, Charline Champ, Antoine Grancher, Cheikh Darou beye et Alicia Lorandi")))),
            tabItem("Carte"),
            tabItem("map",
                    fluidRow(column(width=4,selectInput("choice",label="Choix de la representation : ",choices=c("Type","Banque"))),
                             column(width=4,selectInput("criteria",label="Choix du critere :", choices = paste0(3:34,"_",colnames(bdd_zese[-c(1,2,35)]))))),
                    fluidRow(column(width=4,uiOutput("Choice"))),
                    actionButton("aff","Afficher"),
                    fluidRow(column(width = 12,div(style= "font-weight: 1000; font-family: Arial; monospace","Carte au choix",tags$br()),plotlyOutput("map_generale", width = "auto",height = "auto"),align="center"))),
            tabItem("Comparaison-Type",
                    fluidRow(column(width=12,selectInput("Critere", label = "Choix du critere",choices = paste0(3:34,"_",colnames(bdd_zese[-c(1,2,35)]))))),
                    fluidRow(column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des banques cooperatives",tags$br()),plotlyOutput("carte_cooperative", width = "auto",height = "auto"),align="center"),
                             column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des banques lucratives",tags$br()),plotlyOutput("carte_lucrative", width = "auto",height = "auto"),align="center")),
            ),
            tabItem("Comparaison-Banque",
                    fluidRow(column(width=12,selectInput("critere", label = "Choix du critere",choices = paste0(3:34,"_",colnames(bdd_zese[-c(1,2,35)]))))),
                    fluidRow(column(width=12,selectInput("Banque", label = "Choix des premieres banques : " ,choices = c("All",unique(sg$Banque)),multiple=TRUE)),
                             column(width=12,selectInput("Banqu", label = "Choix des deuxiemes banques : ",choices = c("All",unique(sg$Banque)),multiple=TRUE))),
                    actionButton("affiche","Afficher"),
                    fluidRow(column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des premieres banques",tags$br()),plotlyOutput("carte_banque_1", width = "auto",height = "auto"),align="center"),
                             column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte des deuxiemes banques",tags$br()),plotlyOutput("carte_banque_2", width = "auto",height = "auto"),align="center"))),
            tabItem("Comparaison-Critere-Type",
                    fluidRow(column(width=12,colum.height=12,selectInput("Type_banque",label="Choix du type de banque : ",choices=unique(sg$Type)))),
                    fluidRow(column(width=12,selectInput("Critere_1", label = "Choix du premier critere : ",choices = paste0(3:34,"_",colnames(bdd_zese[-c(1,2,35)])))),
                             column(width=12,selectInput("Critere_2", label = "Choix du deuxieme critere : ",choices = paste0(3:34,"_",colnames(bdd_zese[-c(1,2,35)]))))),
                    fluidRow(column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte du premier critere",tags$br(),tags$br()), plotlyOutput("carte_critere_type_1", width = "auto",height = "auto"),align="center"),
                             column(width = 12,tags$div(style= "font-weight: 1000; font-family: Arial, monospace","Carte du deuxieme critere",tags$br(),tags$br()),plotlyOutput("carte_critere_type_2", width = "auto",height = "auto"),align="center"))),
            tabItem("Comparaison-Critere-Banque",
                    fluidRow(column(width=12,selectInput("critere_1", label = "Choix du premier critere : ",choices = paste0(3:34,"_",colnames(bdd_zese[-c(1,2,35)])))),
                             column(width=12,selectInput("critere_2", label = "Choix du deuxieme critere : ",choices = paste0(3:34,"_",colnames(bdd_zese[-c(1,2,35)]))))),
                    fluidRow(column(width=12,selectInput("Banque_1", label = "Choix des premieres banques : ",choices = c("All",unique(sg$Banque)),multiple=TRUE)),
                             column(width=12,selectInput("Banque_2", label = "Choix des deuxiemes banques : ",choices = c("All",unique(sg$Banque)),multiple=TRUE))),
                    actionButton("afficher","Afficher"),
                    fluidRow(column(width = 12,div(style= "font-weight: 1000; font-family: Arial; monospace","Carte des premieres banques",tags$br()),plotlyOutput("carte_critere_banque_1", width = "auto",height = "auto"),align="center"),
                             column(width = 12,div(style= "font-weight: 1000; font-family: Arial; monospace","Carte des deuxiemes banques",tags$br()),plotlyOutput("carte_critere_banque_2", width = "auto",height = "auto"),align="center"))),
            tabItem("Donnees"),
            tabItem("Banques",
                    fluidRow(
                        column(4,selectInput("Banques","Banque :",c("All",unique(sg$Banque)),multiple = TRUE)),
                        column(4,selectInput("Types","Type :",c("All",unique(sg$Type))))),
                    DT::dataTableOutput("table1")),
            tabItem("Criteres",
                    fluidRow(
                        column(4,selectInput("Criteres","Critere :",c("All",paste0(3:34,"_",colnames(bdd_zese[-c(1,2,35)]))),multiple = TRUE)),
                        column(4,selectInput("Zone","Zone d'emploi :",c("All",bdd_zese$`Zone d'emploi 2020`),multiple = TRUE))),
                    DT::dataTableOutput("table2")),
            tabItem("Effectif",
                    fluidRow(
                        column(4,selectInput("Banquess","Banque :",c("All",unique(sg$Banque)),multiple = TRUE)),
                        column(4,selectInput("Zones","Zone d'emploi :",c("All",bdd_zese$`Zone d'emploi 2020`),multiple = TRUE))),
                    DT::dataTableOutput("table3"))
        )
    ),
    
    title = "Banques"
)

server <- function(input,output){
    
    output$Choice <- renderUI({
        if(input$choice=="Banque"){
            selectInput("bank",label = "Choix des banques : ",choices=unique(sg$Banque),multiple = TRUE)
        } else {
            selectInput("types",label = "Choix du type : ",choices=c("All",unique(sg$Type)))
        }
    })
    
    
    data_carte <- eventReactive(input$aff,{
        data <- sg
        if(input$choice=="Banque"){
            if (!(length(input$bank)==0)){
                data <- filter(data,Banque %in% input$bank)
            }    
        }else if(input$choice=="Type"){
            if(!(input$types=="All")){
                data <- filter(data,Type %in% input$types)
            }
        }
        temp <- strsplit(input$criteria,split="_")
        list(data,temp)
        
    })
    
    output$map_generale <- renderPlotly({
        dat <- data_carte()
        data <- dat[[1]]
        temp <- unlist(dat[[2]])
        crit <- as.numeric(temp[1])
        
        if(input$choice=="Banque"){
            
            map <- ggplot()+
                geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
                scale_fill_viridis_c(option = 'E')+
                theme_minimal()+
                theme(panel.background = element_rect(fill = "light blue"))+
                geom_point(data=data,aes(x=Longitude,y=Latitude,group=Banque,color=Banque),size=.5)+
                geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
                labs(fill = temp[2])
            
        } else if(input$choice=="Type"){
            if(input$types=="All"){
             
                map <- ggplot()+
                    geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
                    scale_fill_viridis_c(option = 'D')+
                    theme_minimal()+
                    theme(panel.background = element_rect(fill = "light blue"))+
                    geom_point(data=data,aes(x=Longitude,y=Latitude,group=Type,color=Type),size=.5)+
                    geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
                    labs(fill = temp[2])   
                
            }else{
                map <- ggplot()+
                    geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
                    scale_fill_viridis_c(option = 'D')+
                    theme_minimal()+
                    theme(panel.background = element_rect(fill = "light blue"))+
                    geom_point(data=data,aes(x=Longitude,y=Latitude),size=.5,colour="white")+
                    geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
                    labs(fill = temp[2])
            }
        }
        
        map
    })
    
    
    output$carte_cooperative <- renderPlotly({
        data <- sg[sg$Type=="CoopÃ©rative",]
        temp <- unlist(strsplit(input$Critere,split="_"))
        crit <- as.numeric(temp[1])
        map <- ggplot()+
            geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
            scale_fill_viridis_c(option = 'D')+
            theme_minimal()+
            theme(panel.background = element_rect(fill = "light blue"))+
            geom_point(data=data,aes(x=Longitude,y=Latitude),size=.5,colour="white")+
            geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
            labs(fill = temp[2])
        
        map
    })
    
    output$carte_lucrative <- renderPlotly({
        data <- sg[sg$Type=="Lucrative",]
        temp <- unlist(strsplit(input$Critere,split="_"))
        crit <- as.numeric(temp[1])
        map <- ggplot()+
            geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
            scale_fill_viridis_c(option = 'D')+
            theme_minimal()+
            theme(panel.background = element_rect(fill = "light blue"))+
            geom_point(data=data,aes(x=Longitude,y=Latitude),size=.5,colour="white")+
            geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
            labs(fill = temp[2])
        
        map
    })
    
    data_banque <- eventReactive(input$affiche,{
        data2 <- sg
        if (!(length(input$Banqu)==0 | "All" %in% input$Banqu)){
            data2 <- filter(data2,Banque %in% input$Banqu)
        }
        data <- sg
        if (!(length(input$Banqu)==0 | "All" %in% input$Banque)){
            data <- filter(data,Banque %in% input$Banque)
        }
        temp <- strsplit(input$critere,split="_")
        list(data,data2,temp)
        
    })
    
    
    output$carte_banque_1 <- renderPlotly({
        tt <- data_banque()
        data <- tt[[1]]
        temp <- unlist(tt[[3]])
        crit <- as.numeric(temp[1])
        map <- ggplot()+
            geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
            scale_fill_viridis_c(option = 'E')+
            theme_minimal()+
            theme(panel.background = element_rect(fill = "light blue"))+
            geom_point(data=data,aes(x=Longitude,y=Latitude,group=Banque,color=Banque),size=.5)+
            geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
            labs(fill = temp[2])
        
        map
    })
    
    output$carte_banque_2 <- renderPlotly({
        tt <- data_banque()
        data <- tt[[2]]
        temp <- unlist(tt[[3]])
        crit <- as.numeric(temp[1])
        map <- ggplot()+
            geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
            scale_fill_viridis_c(option = 'E')+
            theme_minimal()+
            theme(panel.background = element_rect(fill = "light blue"))+
            geom_point(data=data,aes(x=Longitude,y=Latitude,group=Banque,color=Banque),size=.5)+
            geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
            labs(fill = temp[2])
        
        map
    })
    
    output$carte_critere_type_1 <- renderPlotly({
        data <- sg[sg$Type==input$Type_banque,]
        temp <- unlist(strsplit(input$Critere_1,split="_"))
        crit <- as.numeric(temp[1])
        map <- ggplot()+
            geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
            scale_fill_viridis_c(option = 'D')+
            theme_minimal()+
            theme(panel.background = element_rect(fill = "light blue"))+
            geom_point(data=data,aes(x=Longitude,y=Latitude),size=.5,colour="white")+
            geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
            labs(fill = temp[2])
        
        map
    })
    
    output$carte_critere_type_2 <- renderPlotly({
        data <- sg[sg$Type==input$Type_banque,]
        temp <- unlist(strsplit(input$Critere_2,split="_"))
        crit <- as.numeric(temp[1])
        map <- ggplot()+
            geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
            scale_fill_viridis_c(option = 'D')+
            theme_minimal()+
            theme(panel.background = element_rect(fill = "light blue"))+
            geom_point(data=data,aes(x=Longitude,y=Latitude),size=.5,colour="white")+
            geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
            labs(fill = temp[2])
        
        map
    })
    
    data_type <- eventReactive(input$afficher,{
        data2 <- sg
        if (!(length(input$Banque_2)==0 | "All" %in% input$Banque_2)){
            data2 <- filter(data2,Banque %in% input$Banque_2)
        }
        temp2 <- strsplit(input$critere_2,split="_")
        data <- sg
        if (!(length(input$Banque_1)==0 | "All" %in% input$Banque_1)){
            data <- filter(data,Banque %in% input$Banque_1)
        }
        temp <- strsplit(input$critere_1,split="_")
        list(data,data2,temp,temp2)
        
    })
    
    output$carte_critere_banque_1 <- renderPlotly({
        tt<-data_type()
        data <- tt[[1]]
        temp <- unlist(tt[[3]])
        crit <- as.numeric(temp[1])
        map <- ggplot()+
            geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
            scale_fill_viridis_c(option = 'E')+
            theme_minimal()+
            theme(panel.background = element_rect(fill = "light blue"))+
            geom_point(data=data,aes(x=Longitude,y=Latitude,group=Banque,color=Banque),size=.5)+
            geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
            labs(fill = temp[2])
        
        map
    })
    
    
    output$carte_critere_banque_2 <- renderPlotly({
        tt <- data_type()
        data <- tt[[2]]
        temp <- unlist(tt[[4]])
        crit <- as.numeric(temp[1])
        map <- ggplot()+
            geom_sf(data=bdd_zese, aes(fill=bdd_zese[,crit],geometry=geometry),color=NA,size=.2)+
            scale_fill_viridis_c(option = 'E')+
            theme_minimal()+
            theme(panel.background = element_rect(fill = "light blue"))+
            geom_point(data=data,aes(x=Longitude,y=Latitude,group=Banque,color=Banque),size=.5)+
            geom_sf_text(data=bdd_zese,aes(label=`Zone d'emploi 2020`,geometry=geometry),size=2)+
            labs(fill = temp[2])
        
        map
    })
    
    
    output$table1 <- DT::renderDataTable(DT::datatable({
        data <- sg
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
                temp <- unlist(strsplit(input$Criteres,split="_"))
                crit <- as.numeric(temp[seq(1,length(temp),2)])
                data <- data[,c(1,2,crit)]
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
        data <- sg
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


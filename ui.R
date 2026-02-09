
dashboardPage(
    
    skin = "red",
    
    # En-tête du tableau de bord
    dashboardHeader(title = "UKRA"),
    
    # Barre latérale avec menu
    dashboardSidebar(
      sidebarMenu(
        menuItem("Accueil", tabName = "home", icon = icon("home")),
        menuItem("Vue globale", tabName = "overview", icon = icon("eye")),
        menuItem("Gravité et facteurs influents", tabName = "influential_factors", icon = icon("car-crash")),
        menuItem("Analyse temporelle", tabName = "temporal_analysis", icon = icon("clock"))
      )
    ), 
    
    # Corps du tableau de bord
    dashboardBody(
      tabItems(
        # Onglet Accueil
        tabItem(tabName = "home",
                div(
                  style = "display: flex; align-items: center;",
                  img(
                    src = "accident_illustration.png",
                    height = "80px",
                    style = "margin-right: 15px;"
                  ),
                  h2(
                    "Bienvenue dans le tableau de bord UKRA  !",
                    style = "margin: 0;"
                  )
                ),
                br(),
                p("Ce tableau de bord présente une étude des accidents de la route au Royaume-Uni en 2021 et 2022. Il permet de visualiser l’évolution du nombre d’accidents, leur gravité ainsi que les principaux facteurs qui les influencent. L’objectif est de mieux comprendre les causes et les circonstances des accidents de la route afin d’aider à l’identification des situations à risque."),
                p("Ce tableau de bord propose trois pages principales :"),
                tags$ul(
                  tags$li(strong("Vue globale : "), "présente une vue d’ensemble des accidents de la route sur la période 2021-2022"),
                  tags$li(strong("Gravité et facteurs influents : "), "analyse la gravité des accidents en fonction de différents facteurs tels que le type de route, les conditions météorologiques, les conditions d’éclairage et l’état de la chaussée"),
                  tags$li(strong("Analyse temporelle : "), "montre la répartition des accidents dans le temps (selon le mois, le jour de la semaine et l’heure) afin de mettre en évidence les périodes à risque")
                ),
                
                
                br(),
                p(em("Application réalisée par : Amadou Bocoum & Anya Levêque"))
        ),
        
        # Onglet Vue globale
        tabItem(
          tabName = "overview",
          
          # Titre de la page
          uiOutput("titre_overview"),
          br(),
          
          # InfoBox
          fluidRow(
            infoBoxOutput("total_accidents", width = 4),
            infoBoxOutput("vehicules_par_accident", width = 4),
            infoBoxOutput("victimes_par_accident", width = 4)
          ),
          br(),
          
          # Filtre et graphique côte à côte
          fluidRow(
            
            # Filtre
            column(
              width = 3,
              box(
                title = "Filtres",
                status = "danger",
                solidHeader = TRUE,
                width = 12,
                
                # Sélection multiple pour les types de gravités
                checkboxGroupInput(
                  "severity",
                  "Gravité :",
                  choices = sort(unique(df$Accident_Severity)),
                  selected = sort(unique(df$Accident_Severity))
                )
              )
            ),
            
            # Graphique
            column(
              width = 9,
              box(
                width = 12,
                plotlyOutput("plot_overview")
              )
            )
          )
        ),
        
        
        # Onglet Gravité et facteurs influents
        tabItem(tabName = "influential_factors"),
        
        # Onglet Analyse temporelle
        tabItem(tabName = "temporal_analysis")
        
        
      )
    )
)
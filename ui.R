
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
      # Page Accueil
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
              p("La carte de chaleur ci-dessous présente la répartition géographique des accidents de la route au Royaume-Uni, permettant de visualiser les zones les plus à risque."),
              leafletOutput("map_accueil"),
              hr(style = "border-top: 1px solid grey;"),
              p(em("Application réalisée par Amadou Bocoum & Anya Levêque"))
      ),
      
      
      # Page Vue globale
      tabItem(
        tabName = "overview",
        
        # Titre de la page
        div(
          style = "display: flex; align-items: center;",
          img(
            src = "accident_illustration.png",
            height = "80px",
            style = "margin-right: 15px;"
          ),
          h2(
            "Évolution du nombre d'accidents au Royaume-Uni entre 2021 et 2022",
            style = "margin: 0;"
          )
        ),
        br(),
        
        # InfoBox
        fluidRow(
          # Infobox sur le nombre total d'accidents
          infoBox(
            title = span("Au total", style = "text-transform: none;"),,
            value = nrow(df),
            subtitle = "Accidents",
            icon = icon("car-crash"),
            color = "red",
            fill = TRUE,
            width = 3
          ),
          # Infobox sur le nombre moyen d'accidents par jour
          infoBox(
            title = span("En moyenne", style = "text-transform: none;"),
            value = round(nrow(df) / n_distinct(as.Date(df$Date)), 1),
            subtitle = "Accidents / jour",
            icon = icon("calendar-day"),
            color = "red",
            fill = TRUE,
            width=3
          ),
          # Infobox sur le nombre médian de véhicules impliqués par accidents
          infoBox(
            title = span("Médiane", style = "text-transform: none;"),
            value = round(median(df$Number_of_Vehicles, na.rm = TRUE), 1),
            subtitle = "Véhicules impliqués / accident",
            icon = icon("car"),
            color = "orange",
            fill = TRUE,
            width = 3
          ),
          # Infobox sur le nombre médian de victimes par accident
          infoBox(
            title = span("Médiane", style = "text-transform: none;"),
            value = round(median(df$Number_of_Casualties, na.rm = TRUE), 1),
            subtitle = "Victime / accident",
            icon = icon("user-injured"),
            color = "orange",
            fill = TRUE,
            width = 3
          )
        ),
        br(),
        
        # Onglets
        tabsetPanel(
          tabPanel(
            "Synthèse",
            box(
              width = 12,
              plotlyOutput("plot_overview")
            )
          ),
          
          tabPanel(
            "Analyse détaillée",
            fluidRow(
              box(
                width = 2,
                title = "Filtre",
                status = "danger",
                solidHeader = TRUE,
                checkboxGroupInput(
                  "severity",
                  "Gravité :",
                  choices = c("Légère", "Grave", "Mortelle"),
                  selected = c("Légère", "Grave", "Mortelle")
                )
              ),
              box(
                width = 10,
                plotlyOutput("plot_overview_detail")
              )
            )
          ),
          tabPanel(
            "Données",
            box(
              width = 12,
              DTOutput("table_overview")
            )
          )
          
        )
      ),
      
      
      # Page Gravité et facteurs influents
      tabItem(
        tabName = "influential_factors",
        
        fluidRow(
          
          box(
            width = 3,
            title = "Filtres",
            status = "danger",
            solidHeader = TRUE,
            
            checkboxGroupInput(
              "severity_factor",
              "Gravité :",
              choices = c("Légère","Grave","Mortelle"),
              selected = c("Légère","Grave","Mortelle")
            ),
            
            selectInput(
              "factor_choice",
              "Facteur étudié :",
              choices = c(
                "Type de route" = "Road_Type",
                "Météo" = "Weather_Conditions",
                "Éclairage" = "Light_Conditions",
                "Etat chaussée" = "Road_Surface_Conditions",
                "Zone" = "Urban_or_Rural_Area",
                "Limitation vitesse" = "Speed_limit"
              )
            )
          ),
          
          box(
            width = 9,
            plotlyOutput("plot_factor_gravity")
          )
        )
      ),

      
      # Page Analyse temporelle
      tabItem(
        tabName = "temporal_analysis",
        
        fluidRow(
          
          box(
            width = 3,
            title = "Indicateur",
            status = "danger",
            solidHeader = TRUE,
            
            selectInput(
              "temporal_metric",
              "Mesure :",
              choices = c(
                "Nombre d'accidents" = "accidents",
                "Nombre de victimes" = "victims"
              )
            )
          ),
          
          box(width = 9, plotlyOutput("plot_month"))
        ),
        
        fluidRow(
          box(width = 6, plotlyOutput("plot_weekday")),
          box(width = 6, plotlyOutput("plot_hour"))
        )
      )
      
    )
  )
  
)

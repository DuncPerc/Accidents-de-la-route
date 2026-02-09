
function(input, output, session) {
  
  
  ##########  PAGE VUE GLOBALE  ##########
  
  ## Fonction réactive pour la sélection du périmètre choisi par l'utilisateur
  perimetre_overview <- reactive({
    
    # Filtrage sur le(s) type(s) de gravité choisis par l'utilisateur
    df_overview <- df %>% 
      filter(Accident_Severity %in% input$severity) 
    
    return(df_overview)
    
  })
  
  ## Titre de la page
  output$titre_overview <- renderUI({
    sousTitre <- if (length(input$severity) > 0 && length(input$severity) <= 2){
      paste0(
        "Accidents de gravité : ",
        paste(input$severity, collapse = ", ")
      )
    } else {
      NULL
    }
    
    div(
      style = "display: flex; align-items: center;",
      img(
        src = "accident_illustration.png",
        height = "80px",
        style = "margin-right: 15px;"
      ),
      div(
        h3("Évolution du nombre d'accidents au Royaume-Uni entre 2021 et 2022",
           class = "mb-0"),
        if (!is.null(sousTitre)) {
          h5(sousTitre, class = "mb-0")
        }
      )
    )
  })

  ## Infobox sur le nombre total d'accidents
  output$total_accidents <- renderInfoBox({
    
    df_overview <- perimetre_overview()
    
    infoBox(
      title = "Accidentologie",
      value = nrow(df_overview),
      subtitle = "Accidents",
      icon = icon("car-crash"),
      color = "red",
      fill = TRUE
    )
  })
  
  ## Infobox sur le nombre médian de véhicules impliqués par accidents
  output$vehicules_par_accident <- renderInfoBox({
    
    df_overview <- perimetre_overview()
    
    validate(
      need(nrow(df_overview) > 0, "Aucune donnée")
    )
    
    median_vehicules <- median(df_overview$Number_of_Vehicles, na.rm = TRUE)
    
    infoBox(
      title = "Accidentologie (médiane)",
      value = round(median_vehicules, 1),
      subtitle = "Véhicule(s) impliqué(s) / accident",
      icon = icon("car"),
      color = "blue",
      fill = TRUE
    )
  })
  
  ## Infobox sur le nombre médian de victimes par accident
  output$victimes_par_accident <- renderInfoBox({
    
    df_overview <- perimetre_overview()
    
    validate(
      need(nrow(df_overview) > 0, "Aucune donnée")
    )
    
    median_victimes <- median(df_overview$Number_of_Casualties, na.rm = TRUE)
    
    infoBox(
      title = "Victimologie (médiane)",
      value = round(median_victimes, 1),
      subtitle = "Victime(s) / accident",
      icon = icon("user-injured"),
      color = "orange",
      fill = TRUE
    )
  })
  
  ## Graphique sur l'évolution du nombre d'accidents
  output$plot_overview <- renderPlotly({
    
    # Regrouper par mois et année
    df_overview <- perimetre_overview() %>%
      # Conserve l'année et le mois + met le jour au premier du mois
      mutate(Date = lubridate::floor_date(Date, "month")) %>%
      group_by(Date) %>%
      summarise(valeur = n(), .groups = "drop")
    
    validate(
      need(nrow(df_overview) > 0, "Veuillez sélectionner un type de gravité !")
    )
    
    graphique_overview <- df_overview %>%
      arrange(Date) %>%
      ggplot(aes(x = Date, y = valeur)) +
      geom_line(size = 1.2, color = "#dd4b39") +
      geom_point(color = "#dd4b39") +
      labs(
        x = "",
        y = "Nombre d'accidents"
      ) +
      theme_minimal()
    
    ggplotly(graphique_overview)
  })
  
}  
  
  

function(input, output, session) {
  
  
  ##########  PAGE ACCUEIL ##########
  
  # Carte heatmap représentant la répartition géographique des accidents
  output$map_accueil <- renderLeaflet({
    
    df_map <- df %>%
      filter(!is.na(Latitude), !is.na(Longitude))
    
    leaflet(df_map) %>%
      addTiles() %>%
      addHeatmap(
        lng = ~Longitude,
        lat = ~Latitude,
        blur = 20,
        radius = 15
      )
  })
  
  ##########  PAGE VUE GLOBALE  ##########

  ## Graphique d'évolution du nombre d'accidents (avec une courbe pour chaque type de gravité et une courbe Total)
  output$plot_overview <- renderPlotly({
    
    df_plot <- df %>%
      mutate(
        Date = lubridate::floor_date(Date, "month")
      ) %>%
      group_by(Date, Severity_fr) %>%
      summarise(valeur = n(), .groups = "drop") %>%
      
      # Ajout de la courbe Total
      bind_rows(
        df %>%
          mutate(Date = lubridate::floor_date(Date, "month")) %>%
          group_by(Date) %>%
          summarise(
            Severity_fr = "Total",
            valeur = n(),
            .groups = "drop"
          )
      )
    
    gg <- ggplot(
      df_plot,
      aes(
        x = Date,
        y = valeur,
        color = Severity_fr,
        group = Severity_fr,
        text = paste0(
          "Date : ", format(Date, "%b %Y"),
          "<br>Gravité : ", Severity_fr,
          "<br>Nombre d'accidents : ", valeur
        )
      )
    ) +
      geom_line(linewidth=1.1, alpha = 0.9) +
      geom_point(show.legend = FALSE) +
      scale_color_manual(
        name = "Gravité", 
        values = c(
          "Légère" = "dodgerblue",
          "Grave" = "#f39c12",
          "Mortelle" = "#dd4b39",
          "Total" = "black"
        )
      ) +
      labs(x = "", y = "Nombre d'accidents") +
      theme_minimal() +
      theme(
        legend.position = "bottom"
      )
    
    ggplotly(gg, tooltip = "text")
  })
  
  
  ## Graphique sur l'évolution du nombre d'accidents selon le filtrage
  output$plot_overview_detail <- renderPlotly({
    
    # Regrouper par mois et année
    df_overview <- df %>%
      # Filtrage sur le(s) type(s) de gravité choisis par l'utilisateur
      filter(Severity_fr %in% input$severity) %>%
      # Conserve l'année et le mois + met le jour au premier du mois
      mutate(Date = floor_date(Date, "month")) %>%
      group_by(Date) %>%
      summarise(valeur = n(), .groups = "drop") %>%
      arrange(Date) %>%
      mutate(
        tooltip_text = paste0(
          "Date : ", format(Date, "%b %Y"),
          "<br>Nombre d'accidents : ", valeur
        )
      )
    
    validate(
      need(nrow(df_overview) > 0, "Veuillez sélectionner un type de gravité !")
    )
    
    graphique_overview <- ggplot(df_overview, aes(x = Date, y = valeur)) +
      geom_line(size = 1.2, color = "#dd4b39") +
      geom_point(aes(text=tooltip_text), color = "#dd4b39") +
      labs(
        x = "",
        y = "Nombre d'accidents"
      ) +
      theme_minimal()
    
    ggplotly(graphique_overview, tooltip = "text")
    
  })
  
  
  output$table_overview <- renderDT({
    
    # Table pivotée avec une colonne par gravité, une pour le total et une pour la date
    df_table_overview <- df %>%
      mutate(
        Date = floor_date(Date, "month")
      ) %>%
      group_by(Date, Severity_fr) %>%
      summarise(Accidents = n(), .groups = "drop") %>%
      # Transformation au format large : une colonne par gravité
      pivot_wider(
        names_from = Severity_fr,
        values_from = Accidents,
        values_fill = 0  # remplit les NA par 0
      ) %>%
      # Calcul du total des accidents
      mutate(Total_Accidents = rowSums(select(., -Date))) %>%
      # Tri par Date
      arrange(Date) %>%
      # Colonne d'affichage de la date
      mutate(Mois = format(Date, "%b %Y"))
    
    # Renommer les colonnes
    df_table_overview <- df_table_overview %>%
      rename(
        `Accidents légers` = `Légère`,
        `Accidents graves` = Grave,
        `Accidents mortels` = Mortelle,
        `Total Accidents` = Total_Accidents
      )
    
    datatable(
      df_table_overview %>%
        select(Mois, `Accidents légers`, `Accidents graves`, `Accidents mortels`, `Total Accidents`),
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        dom = 'p'
      ),
      class = "cell-border stripe hover order-column"
    )
  })

  ##########  PAGE GRAVITÉ ET FACTEURS INFLUENTS ##########
  
  ## Graphique analysant la gravité des accidents selon différents facteurs
  ## (type de route, météo, éclairage, état de la chaussée, zone).
  ## Le graphique affiche la proportion d'accidents légers, graves et mortels
  ## pour chaque catégorie du facteur sélectionné par l'utilisateur.
  
  output$plot_factor_gravity <- renderPlotly({
    
    # Filtrage des données selon les niveaux de gravité sélectionnés
    df_factor <- df %>%
      filter(Severity_fr %in% input$severity_factor)
    
    # Variable dynamique correspondant au facteur choisi par l'utilisateur
    var <- sym(input$factor_choice)
    
    # Agrégation du nombre d'accidents par facteur et niveau de gravité
    df_plot <- df_factor %>%
      group_by(!!var, Severity_fr) %>%
      summarise(n = n(), .groups = "drop")
    
    # Construction du graphique montrant la proportion de chaque niveau de gravité
    gg <- ggplot(
      df_plot,
      aes(x = !!var, y = n, fill = Severity_fr)
    ) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = scales::percent)+
      labs(
        x = "",
        y = "Proportion d'accidents",
        fill = "Gravité"
      ) +
      theme_minimal() +
      coord_flip()
    
    ggplotly(gg)
  })
  
  ##########  PAGE ANALYSE TEMPORELLE ##########
  
  ## Fonction réactive permettant de choisir la mesure analysée :
  ## soit le nombre d'accidents, soit le nombre total de victimes.
  metric_fun <- reactive({
    if(input$temporal_metric == "victims"){
      function(d) sum(d$Number_of_Casualties, na.rm=TRUE)
    } else {
      function(d) nrow(d)
    }
  })
  
  
  ## Graphique montrant la répartition des accidents (ou des victimes)
  ## selon les mois de l'année afin d'identifier une éventuelle saisonnalité.
  output$plot_month <- renderPlotly({
    
    df_month <- df %>%
      mutate(month = month(Date, label=TRUE)) %>%
      group_by(month) %>%
      summarise(value = metric_fun()(cur_data()), .groups="drop")
    
    gg<-ggplot(df_month, aes(month, value)) +
      geom_col(fill="#dd4b39") +
      theme_minimal() 
    
    ggplotly(gg)
  })
  
  
  ## Graphique représentant la répartition des accidents (ou des victimes)
  ## selon les jours de la semaine afin d'identifier les jours les plus à risque.
  output$plot_weekday <- renderPlotly({
    
    df_day <- df %>%
      mutate(day = wday(Date, label=TRUE, week_start = 1)) %>%
      group_by(day) %>%
      summarise(value = metric_fun()(cur_data()), .groups="drop")
    
    gg<-ggplot(df_day, aes(day, value)) +
      geom_col(fill="#f39c12") +
      theme_minimal() 
    
    ggplotly(gg)
  })
  
  
  ## Graphique montrant la distribution des accidents (ou des victimes)
  ## selon l'heure de la journée afin d'identifier les périodes horaires
  ## les plus accidentogènes.
  output$plot_hour <- renderPlotly({
    
    df_hour <- df %>%
      mutate(hour = hour(Time)) %>%
      group_by(hour) %>%
      summarise(value = metric_fun()(cur_data()), .groups="drop")
    
    gg<-ggplot(df_hour, aes(hour, value)) +
      geom_col(fill="dodgerblue") +
      theme_minimal()
    
    ggplotly(gg)
  })
}  

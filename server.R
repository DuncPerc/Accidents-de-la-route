
function(input, output, session) {
  
  
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
  
}  
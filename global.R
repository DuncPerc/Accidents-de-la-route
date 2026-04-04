library(tidyverse)
library(readxl)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(leaflet)
library(leaflet.extras)

# Importation du fichier des données
df <- read_excel("Data/Road_Accident_Data.xlsx") %>%
  # Renommage de colonnes
  rename(Local_Authority_District = `Local_Authority_(District)`) %>%
  rename(Date = `Accident Date`) %>%
  # Création d'une colonne traduisant les différents type de gravité en français
  mutate(
    Severity_fr = dplyr::recode(
      Accident_Severity,
      "Slight" = "Légère",
      "Serious" = "Grave",
      "Fatal" = "Mortelle",
      "Total" = "Total"
    )
  )%>%
  mutate(
    Weather_Conditions_reduc = dplyr::recode(
      Weather_Conditions,
      'Fine no high winds' = "Fine",
      'Fine + high winds' = "Fine",
      'Raining no high winds' = "Raining",
      'Raining + high winds' = "Raining",
      'Snowing no high winds' = "Snowing", 
      'Snowing + high winds' = "Snowing"
    )
  )


# Traduction des facteurs étudiés
factor_choices <- c(
  "Type de route" = "Road_Type",
  "Météo" = "Weather_Conditions_reduc",
  "Éclairage" = "Light_Conditions",
  "État chaussée" = "Road_Surface_Conditions",
  "Zone" = "Urban_or_Rural_Area",
  "Limitation vitesse" = "Speed_limit"
)


# Fonction qui crée l'en-tête avec une image et le titre passé en paramètre
header_block <- function(page_title) {
  div(
    style = "display: flex; align-items: center;",
    img(
      src = "accident_illustration.png",
      height = "80px",
      style = "margin-right: 15px;"
    ),
    h2(
      page_title,
      style = "margin: 0;"
    )
  )
}
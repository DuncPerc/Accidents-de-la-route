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
  )

# Traduction des conditions d'éclairage en français
df$Light_Conditions <- dplyr::recode(
  df$Light_Conditions,
  "Daylight" = "Jour",
  "Darkness - lights lit" = "Nuit – éclairage allumé",
  "Darkness - lights unlit" = "Nuit – éclairage éteint",
  "Darkness - no lighting" = "Nuit – sans éclairage",
  "Darkness - lighting unknown" = "Nuit – éclairage inconnu"
)
# Conversion de certaines variables en facteurs (variables catégorielles)

df <- df %>%
  mutate(across(
    c(
      Accident_Severity,
      Road_Type,
      Weather_Conditions,
      Light_Conditions,
      Road_Surface_Conditions,
      Urban_or_Rural_Area
    ),
    as.factor
  ))

# Traitement des valeurs manquantes pour certaines variables catégorielles.
# Les valeurs NA sont remplacées par une catégorie explicite afin d'éviter
# la perte d'informations dans les analyses et graphiques.
df <- df %>%
  mutate(
    Carriageway_Hazards = fct_na_value_to_level(Carriageway_Hazards, "None"),
    Weather_Conditions = fct_na_value_to_level(Weather_Conditions, "Unknown"),
    Road_Type = fct_na_value_to_level(Road_Type, "Unknown"),
    Road_Surface_Conditions = fct_na_value_to_level(Road_Surface_Conditions, "Unknown")
  ) %>%
  # Suppression des observations pour lesquelles l'heure de l'accident est inconnue.
  # Ces cas représentent seulement 0,0055 % du dataset
  filter(!is.na(Time))

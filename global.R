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

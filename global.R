library(tidyverse)
library(readxl)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

# Importation du fichier des données
df <- read_excel("Data/Road_Accident_Data.xlsx") %>%
  rename(Local_Authority_District = `Local_Authority_(District)`) %>%
  rename(Date = `Accident Date`)

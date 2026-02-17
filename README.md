# Tableau de bord : Analyse des accidents de la route au Royaume-Uni (2021-2022)

Cette application Shiny propose une analyse exploratoire des accidents de la route au Royaume-Uni sur les années 2021 et 2022. Elle permet de visualiser :
- l’évolution temporelle des accidents
- leur répartition géographique
- la gravité des accidents
- les principaux facteurs influents

L’objectif est de mieux comprendre les causes et les circonstances des accidents afin d’identifier les situations à risque.

## Données
Les données proviennent de Kaggle (https://www.kaggle.com/datasets/xavierberge/road-accident-dataset). Elles regroupent 307 973 accidents enregistrés au Royaume-Uni sur la période 2021-2022.
Pour chaque accident, on dispose des informations suivantes : 
- Dimension temporelle : date, heure, jour de la semaine, mois, année
- Dimension spatiale : zone urbaine ou rurale, district, coordonnées géographiques (latitude et longitude)
- Caractéristiques de l’accident : gravité, nombre de victimes, nombre de véhicules impliqués
- Caractéristiques de l’infrastructure routière : type de route, type et détail du carrefour, dangers présents sur la chaussée
-	Conditions environnementales : conditions de luminosité, conditions météorologiques, état de la chaussée
-	Autres informations : type de véhicule impliqué, force de police en charge de l’accident

## Pages de l'application
- **Page d’accueil** : Présentation du projet
- **Vue globale** : Présentation d’une vue d’ensemble des accidents de la route sur la période 2021-2022
- **Gravité et facteurs influents** : Analyse de la gravité des accidents selon les différents facteurs (le type de route, les conditions météorologiques, les conditions d’éclairage, l’état de la chaussée)
- **Analyse temporelle** : Répartition des accidents dans le temps (selon le mois, le jour de la semaine et l’heure)

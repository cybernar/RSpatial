# Atelier R-Spatial

CEFE-CNRS



## Séance 1 : le package `sp`

### Résumé de la séance
Il s'agit d'un package historique pour la gestion des données spatiales dans R : de nombreux autres packages dépendent de `sp`.

Même si le package `sp` est en voie d'obsolescence et que ses fonctionnalités sont limitées, il est toujours intéressant (recommandé ?) de se familiariser avec les classes qu'il a introduit dans R.

Nous l'étudierons donc dans cette séance, mais sans nous attarder. Tout ce qui concerne l'analyse de données vectorielles sera abordé en détail plus tard, avec `sf`.

 Nous utiliserons aussi le package `rgdal` pour la lecture/écriture de fichiers, et nous aborderons la visualisation des données vectorielles avec les packages `leaflet` et `tmap`.

### Le programme

- Données ponctuelles : la classe `SpatialPoints`
- Créer des données ponctuelles 'from scratch'
- Créer des données vectorielles linéaires et surfaciques 'from scratch'
- Gestion des données attributaires avec les classes `Spatial*DataFrame`
- Lecture / écriture de shapefiles avec `rgdal`
- Gestion des systèmes de coordonnées, transformation des données vers un autre système
- Affichage de données spatiales : bref aperçu avec `plot` et avec le package `leaflet`
- Création de cartes avec la package `tmap`
- Calcul de matrices de distance
- Jointures spatiales et attributaires
- Exercice récapitulatif

### Les données utilisées dans cette séance
- Nous utiliserons peu de données externes pour nous concentrer la structuration des données et les fonctionnalités
- Nous commencerons par créer des données spatiales ex nihilo
- Nous utiliserons 3 shapefiles téléchargés sur le site de Montpellier Métropole Méditerrannée



## Séance 2 : le package `raster`

### Résumé de la séance

Il s'agit actuellement du principal package pour gérer et traiter les données raster (données de type _grilles géoréférencées_, si vous préférez). Il est donc indispensable pour traiter de nombreuses données environnementales spatialisées, en particulier les produits issus de satellites.

Les fonctionnalités proposées par le package sont assez similaires à celles de logiciel SIG tels que QGIS + GRASS ou ArcGIS + Spatial Analyst. Mais R offre l'avantage de la traçabilité et de la reproductibilité, et permet dans certains cas des gains de productivité.

### Le programme

- Création de raster ex nihilo, indexation des pixels
- Ouvrir et enregistrer des données raster
- Afficher un raster avec la fonction plot; dégradé de couleurs avec le package `RColorBrewer`
- Les classes de données raster : raster simple avec `RasterLayer`
- Les classes de données raster : raster multibandes avec `RasterBrick` et `RasterStack`
- Opérations arithmétiques sur les rasters, calcul sur les rasters multibandes.
- Obtenir le résumé statistique et la distribution des valeurs d'un raster
- Rastérisation de données vectorielles avec les packages `raster`, `gdalUtils`, `fasterize`
- Visualiser des données raster avec le package `tmap`
- Définir et appliquer un masque sur un raster
- Extraire des valeurs d'un raster sur un ensemble de points
- Calculer des statistiques de zones sur un ensemble de polygones
- Les statistiques focales et leurs applications : lissage de données, automates cellulaires

### Les données utilisées dans cette séance
- Un extrait d'une image satellite Sentinel2 de la Baie de Somme en 2018, issue du site theia.cnes.fr. Une zone d'étude de 10x10 km a été préalablement découpée pour limiter la taille du jeu de données
- Un shapefile avec l'occupation du sol de la Somme en 2018 (Corine Land Cover), que nous transformerons en raster



## Séance 3 : le package `sf`

### Résumé de la séance

Le package `sf` propose des fonctions pour gérer et traiter les données spatiales de manière simple et efficace. Grâce à l'utilisation d'index spatiaux pour optimiser les calculs, il est aussi performant que des logiciels SIG comme QGIS.

### Le programme

- géométries et colonnes géométriques : création de données ex nihilo
- relations spatiales et requêtes spatiales
- calcul de surface
- zones tampons
- intersecter des données vectorielles (lignes et polygones, polygones et polygones)
- jointures attributaires avec `dplyr`
- cartes choroplèthes

### Les données utilisées dans cette séance

A préciser

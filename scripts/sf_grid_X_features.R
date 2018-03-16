####

# GRID x FEATURES v 1 : DECOUPAGE ET AGREGATION DE DONNEES DANS UNE GRILLE D'ANALYSE VECTORIELLE
# Les shapefiles pour tester le script sont librement accessibles sur le site de Montpellier Open Data
# - MMM_MMM_Limites.shp (http://data.montpellier3m.fr/dataset/contours-des-communes-de-montpellier-mediterranee-metropole)
# - MMM_MMM_OccupSol.shp (http://data.montpellier3m.fr/dataset/evolution-de-loccupation-du-sol-entre-2004-et-2012-de-montpellier-méditerranée-métropole)

# EXEMPLE 1 : crée une grille vecteur à cellule carrée (500 m) correspondant à l'emprise de MMM_MMM_OccupSol.shp,
# calcule la surface de chaque poste d'occupation du sol (1 à 8) pour chaque cellule de la grille,
# enregistre le résultat en shapefile puis en GeoTIFF

# EXEMPLE 2 : crée une grille vecteur hexagonale (largeur 500 m) correspondant à l'emprise de MMM_MMM_Limites.shp,
# calcule la surface de chaque poste d'occupation du sol (1 à 8) pour chaque cellule de la grille,
# enregistre le résultat en shapefile

# EXEMPLE 3 : utilise les limites communales (MMM_MMM_Limites.shp) comme grille d'analyse,
# calcule la surface de chaque poste d'occupation du sol (1 à 8) en 2012 et en 2015 pour chaque commune,
# enregistre le résultat en CSV

# 2017-07-21
# Cyril Bernard, CEFE-CNRS

###

library(sf)
library(units) # installé avec sf
library(sp)
library(raster)

# ---------------------
# DEFINITION FONCTIONS
# ---------------------

# génère une classe Spatial (sp) avec bbox et proj4string à partir d'un objet sf
sf2spatial <- function(sfobj) {
  sp_bbox <- matrix(st_bbox(sfobj),nrow=2,dimnames=list(c("x","y"),c("min","max")))
  sp_proj4string <- st_crs(sfobj)[["proj4string"]]
  Spatial(bbox=sp_bbox, proj4string=CRS(sp_proj4string))
}

# génère une grille d'analyse sous forme de SpatialPixelsDataFrame (sp)
# remarque 1 : on obtient une pretty grid (coordonnées des bords arrondies)
# remarque 2 : on réindexe les pixels avec attribut idpix
#   (sinon l'indexation naturelle des pixels part du coin en bas à gauche
#   exemple soit une grille de 100x100, le pixel en bas à gauche est en 1
#   et le pixel en haut à droite est en 10000)
makePixelGrid <- function (shp_ext, pix_res, init_val=NA) {
  ptgrid <- makegrid(shp_ext, cellsize=pix_res)
  crsgrid <- shp_ext@proj4string
  idpix <- seq_len(nrow(ptgrid))
  valpix <- rep(init_val, nrow(ptgrid))
  datagrid <- data.frame (idpix, valpix, xpix=ptgrid$x1, ypix=ptgrid$x2)
  # genere SpatialPixelsDataFrame
  SpatialPixelsDataFrame(points=ptgrid, data=datagrid, proj4string=crsgrid)
}

# génère une grille d'analyse sous forme de SpatialPolygonsDataFrame (sp) avec des cellules carrées
# d'abord SpatialPixelsDataFrame est généré, 
# puis il est converti en SpatialPolygonsDataFrame
makeSquarePolyGrid <- function (shp_ext, pix_res) {
  # genere SpatialPixels
  spPix <- makePixelGrid(shp_ext, pix_res)
  as(spPix,"SpatialPolygonsDataFrame")
}

# génère une grille d'analyse sous forme de SpatialPolygonsDataFrame (sp) avec des cellules hexagonales
makeHexaGrid <- function (shp_ext, pix_res) {
  ptgrid <- spsample(shp_ext, type="hexagonal", cellsize=pix_res)
  hex_plyg <- HexPoints2SpatialPolygons(ptgrid)  
  datagrid <- data.frame (idpix=seq_len(length(ptgrid)), xpix=ptgrid@coords[,1], ypix=ptgrid@coords[,2])
  SpatialPolygonsDataFrame(hex_plyg, datagrid, match.ID=F)
}

# génère à partir de sf intersection un tableau croisé dynamique avec 
#   en ligne : id grille, 
#   en colonnes : catégorie (ex code ocsol), 
#   en valeur : somme surface ou longueur par id grille et par categorie
# données en entrée : 
# sf_inter: produit intersection data X grid (sf)
# fld_cat: nom colonne catégorie dans "sf_data"
# fld_idgrid: nom colonne id grille dans "sf_grid"
# fld_mesure: nom champ numérique à agréger
# val_cal: sous ensemble catégories à traiter, sinon ttes les catégories
tableX_IDGrid_Cat <- function (sf_inter, fld_cat, fld_idgrid, fld_mesure, val_cat) {
  idgrid <- unique(sf_inter[[fld_idgrid]])
  df_result <- data.frame(idgrid)
  # pour chaque catégorie X (ex foret, culture, zones urbaines) à agréger :
  # - sélectionner polygones (ou lignes) de la catégorie X
  # - agréger mesure par id grille
  # - ajouter nouvelle colonne "surf cat X" dans dataframe en sortie
  for(X in 1:length(val_cat)) {
    # filtrer par code catégorie
    val_catX <- val_cat[X]
    cat_filtre_X <- (sf_inter[[fld_cat]] == val_catX)
    if (sum(cat_filtre_X) == 0) next
    sf_interX <- sf_inter[cat_filtre_X,]
    # calculer somme(surface) par idpix
    IDGRID <- sf_interX[[fld_idgrid]]
    MESURE <- sf_interX[[fld_mesure]]
    # agregation surface par idgrid
    df_interX <- data.frame(IDGRID, MESURE)
    df_aggX <- aggregate(MESURE~IDGRID, df_interX, sum)
    # le résultat est accolé à df_result avec comme nom M_catégorie
    nom_COLX <- paste0("M_",val_catX)
    val_COLX <- rep(0, nrow(df_result))
    corresp <- match(df_aggX$IDGRID, df_result[["idgrid"]])
    val_COLX[corresp] <- df_aggX$MESURE
    # nouvelle colonne dans df_result
    df_result[[nom_COLX]] <- val_COLX
  }
  df_result
}

# SORTIE RASTER (nouveaux fichiers .tif) pour les valeurs calculées puis agrégées 
# à partir d'une grille vecteur (à cellules carrées uniquement !)
outputRastersAgg <- function (shp_ext, pix_res, df_tableX, outputDir, val_cat) {
  for(X in 1:length(val_cat)) {
    # generer une grille SpatialPixels initialisee a NA, avec etendue de shp_ext
    pix_grid <-  makePixelGrid(shp_ext, pix_res, NA)
    # code catégorie à traiter
    val_catX <- val_cat[X]
    # nom colonne à traiter
    nom_COLX <- paste0("M_",val_catX)
    PIXVAL_buffer <- pix_grid@data$valpix
    # enregistrer ds la grille SpatialPixels les longueurs / surfaces par pixel
    PIXVAL_buffer[df_tableX$idgrid] <- df_tableX[[nom_COLX]]
    #PIXVAL_buffer <- 100 * PIXVAL_buffer / (grid_res * grid_res) # pourcentage 
    pix_grid@data$valpix <- PIXVAL_buffer
    # convertir SpatialPixels en raster (valpix = variable 2)
    rasterX <- raster(pix_grid, layer=2)
    frasterX <- paste0(draster, "gridX", val_catX, ".tif")
    writeRaster(rasterX, frasterX, format="GTiff", datatype="FLT4S", overwrite=T)
  }
}







# ------------------------------------------
# EXEMPLE 1 : CALCULER SURFACE OCCUP SOL
# DANS UN GRILLE VECTEUR AVEC DES CELLULES DE 500 x 500 m 
# + ENREGISTRER LE RESULTAT EN RASTER
# (année 2015, niveau 1, catégories "1" à "8") 
# ------------------------------------------
library(magrittr)
library(dplyr)

# EXEMPLE 1 / DONNEES ENTREE
# nom de la couche donnees (ex: land cover, rivieres)
fdata <- "D:/GIS_DATA/Montpellier_OpenData/Metropole/MMM_MMM_OccupSol.shp"
# nom du champ catégorie. Remarque : script testé avec un champ de type caractere
fld_cat <- "c2015_niv1"
# definition des catégories a traiter successivement
# (ex: codes occupation du sol, codes importance route)
# un code = un raster en sortie
# Rem : vérifier présence des codes dans les données 
val_cat <- c(1, 2, 3, 4, 5, 6, 7, 8)

# EXEMPLE 1 / DONNEES SORTIE
# resolution grille en m (ex: 1000 = pixel a 1000 m)
grid_res <- 500
# nom de la couche grille a enregistrer
fgrid <- "D:/bacasable/mmm/grid_500_MMM.shp"
# nom de la couche resultat intersection grille / donnees
finter <- "D:/bacasable/mmm/inter_grid_data.shp"
# répertoire rasters sortie
draster <- "D:/bacasable/mmm/outraster/"
# champ id grille => laisser idpix ds la cas d une grille generee par le script
fld_idgrid <- "idpix"

# EXEMPLE 1 / SCRIPT 
print("EXEMPLE 1 > DEBUT")
t0 <- Sys.time()
# lire les données OcSOL
sf_data <- st_read(fdata)
# générer une grille (SpatialPolygonsDataFrame) sur l'étendue des données
sp_data_bbox <- sf2spatial(sf_data)
sp_grid <- makeSquarePolyGrid(sp_data_bbox, grid_res)
# générer grille sf
sf_grid <- st_as_sf(sp_grid)
sf_grid$valpix <- NULL
# intersection polygones data X polygones grille
sf_inter <- st_intersection(sf_data, sf_grid)
# nouvelle colonne surf_ha : surface en ha (autres unites possibles : m^2, km^2)
sf_inter$surf_ha <- st_area(sf_inter) %>% set_units(ha)
st_write(sf_inter, finter, delete_dsn=TRUE)
# calculer total surf par idgrid et catégorie (tableau croisé dynamique)
table_surf_par_idgrid <- tableX_IDGrid_Cat(sf_inter, fld_cat, fld_idgrid, "surf_ha", val_cat)
# jointure entre sf_grid et tableau croisé _surf_par_idgrid (dplyr)
sf_grid2 <- sf_grid %>% left_join(table_surf_par_idgrid, by=c(idpix="idgrid"))
st_write(sf_grid2, fgrid, delete_dsn=TRUE)
# SORTIE RASTER (nouveaux fichiers .tif) pour les valeurs calculées puis agrégées 
# à partir d'une grille vecteur (à cellules carrées uniquement !)
outputRastersAgg(sp_data_bbox, grid_res, table_surf_par_idgrid, draster, val_cat)
print("EXEMPLE 1 > TERMINE")
Sys.time() - t0



# ------------------------------------------
# EXEMPLE 2 : CALCULER SURFACE OCCUPATION SOL
# DANS UN GRILLE HEXAGONALE (largeur hexagones 500 m) 
# (année 2012, niveau 1, catégories "1", "2", "5", "6", "7", "8") 
# ------------------------------------------

# EXEMPLE 2 / DONNEES ENTREE
# nom de la couche donnees (ex: land cover, rivieres)
fdata <- "D:/GIS_DATA/Montpellier_OpenData/Metropole/MMM_MMM_OccupSol.shp"
# nom de la couche etendue (exemple : communes, departements qui delimitent la zone d'etude)
fext <- "D:/GIS_DATA/Montpellier_OpenData/Metropole/MMM_MMM_Limites.shp"
# nom du champ catégorie. Remarque : script testé avec un champ de type caractere
fld_cat <- "c2012_niv1"
# definition des catégories a traiter successivement
val_cat <- c(1, 2, 5, 6, 7, 8)

# EXEMPLE 2 / DONNEES SORTIE
# resolution grille en m (ex: 1000 = pixel a 1000 m)
grid_res <- 500
# nom de la couche grille a enregistrer
fgrid <- "D:/bacasable/mmm/gridhexa500m.shp"
# nom de la couche resultat intersection grille / donnees
finter <- "D:/bacasable/mmm/inter_gridhexa500m_data.shp"
# champ id grille => laisser idpix ds la cas d une grille generee par le script
fld_idgrid <- "idpix"

# EXEMPLE 2 / SCRIPT 
print("EXEMPLE 2 > DEBUT")
t0 <- Sys.time()
# lire les données OcSOL
sf_data <- st_read(fdata)
# lire les données Etendue (communes) pour régler l'étendue de la grille
sf_ext <- st_read(fext)
# forcer définition des CRS pour éviter le problème des définitions hétérogènes 
st_crs(sf_ext) <- st_crs(sf_data)
# générer grille hexagonal (SpatialPolygonsDataFrame)
sp_data_bbox <- sf2spatial(sf_ext)
sp_hexagrid <- makeHexaGrid(sp_data_bbox, grid_res)
# générer grille sf
sf_hexagrid <- st_as_sf(sp_hexagrid)
sf_hexagrid$valpix <- NULL
# sélectionner les cellules de la grille heax qui intersectent la zone d'etude (communes)
# 1) liste des entités communes intersectées pour chaque cellule
inter_grid_ext <- st_intersects(sf_hexagrid, sf_ext)
# 2) filtre : FAUX pour les cellules qui intersectent 0 communes, VRAI pour les autres
filtre = (sapply(inter_grid_ext,length) > 0)
# 3) appliquer le filtre
sf_hexagrid <- sf_hexagrid[filtre,]
# intersection polygones data X polygones grille
sf_inter <- st_intersection(sf_data, sf_hexagrid)
# nouvelle colonne surf_ha : surface en ha (autres unites possibles : m^2, km^2)
sf_inter$surf_ha <- st_area(sf_inter) %>% set_units(ha)
st_write(sf_inter, finter, delete_dsn=TRUE)
# calculer total surf par idgrid et catégorie (tableau croisé dynamique)
table_surf_par_idgrid <- tableX_IDGrid_Cat(sf_inter, fld_cat, fld_idgrid, "surf_ha", val_cat)
# jointure entre sf_grid et tableau croisé _surf_par_idgrid (dplyr)
sf_hexagrid <- sf_hexagrid %>% left_join(table_surf_par_idgrid, by=c(idpix="idgrid"))
st_write(sf_hexagrid, fgrid, delete_dsn=TRUE)
print("EXEMPLE 2 > TERMINE")
Sys.time() - t0




# ------------------------------------------
# EXEMPLE 3 : CALCULER SURFACE TOTALE ET
# SURFACE DE CHAQUE CATEGORIE OCCUPATION SOL
# POUR CHAQUE COMMUNE DE LA METROPOLE
# (année 2012 et année 2015, niveau 1, catégories "1" à "8") 
# ENREGISTRER LE RESULTAT DANS UN FICHIER .csv
# ------------------------------------------

# EXEMPLE 3 / DONNEES ENTREE
# nom de la couche donnees (ex: land cover)
fdata <- "D:/GIS_DATA/Montpellier_OpenData/Metropole/MMM_MMM_OccupSol.shp"
# nom de grille d'analyse : ici ce sont les communes
fgrid <-  "D:/GIS_DATA/Montpellier_OpenData/Metropole/MMM_MMM_Limites.shp"
# champ id grille => le champ identifiant des communes
fld_idgrid <- "codcomm"
# nom du champ catégorie. Remarque : script testé avec un champ de type caractere
fld_cat2012 <- "c2012_niv1"
fld_cat2015 <- "c2015_niv1"
# definition des catégories a traiter successivement
val_cat <- c(1, 2, 3, 4, 5, 6, 7, 8)

# EXEMPLE 3 / DONNEES SORTIE
# nom de la couche resultat intersection grille / donnees
finter <- "D:/bacasable/mmm/inter_communes_data.shp"
fcsv <- "D:/bacasable/mmm/commXocsol.csv"

# EXEMPLE 3 / SCRIPT 
print("EXEMPLE 3 > DEBUT")
t0 <- Sys.time()
# lire les données OcSOL
sf_data <- st_read(fdata)
# lire les données Etendue (communes) pour régler l'étendue de la grille
sf_comm <- st_read(fgrid)
# forcer définition des CRS pour éviter le problème des définitions hétérogènes 
st_crs(sf_comm) <- st_crs(sf_data)
# sélectionner les cellules de la grille qui intersectent les départements
# 1) liste des entités intersectés pour chaque cellule
inter_grid_ext <- st_intersects(sf_data, sf_comm)
# 2) filtre : FAUX pour les cellules qui intersectent 0 communes, VRAI pour les autres
filtre = (sapply(inter_grid_ext,length) > 0)
# 3) appliquer le filtre
sf_data <- sf_data[filtre,]
# intersection polygones data X polygones grille
sf_inter <- st_intersection(sf_data, sf_comm)
# REMARQUE : parfois, st_intersection entre 2 shapes polygones peut générer
# des géométries de type LINESTRING ou POINT qu'on ne tient pas à conserver
# Voici un moyen de filtrer les entités par type de géométrie
sfc_test_polygon <- (st_geometry_type(sf_inter) %in% c("POLYGON", "MULTIPOLYGON"))
sf_inter <- sf_inter[sfc_test_polygon,]
# nouvelle colonne surf_ha : surface en ha (autres unites possibles : m^2, km^2)
sf_inter$surf_ha <- st_area(sf_inter) %>% set_units(ha)
# REMARQUE : si on n'avait pas expurger les géométries de types POINT et MULTILINESTRING,
# le st_write suivant aurait généré une erreur
st_write(sf_inter, finter, delete_dsn=TRUE)

# calculer total surf par idgrid et catégorie (tableau croisé dynamique)
table_surf2012_par_idgrid <- tableX_IDGrid_Cat(sf_inter, fld_cat2012, fld_idgrid, "surf_ha", val_cat)
table_surf2015_par_idgrid <- tableX_IDGrid_Cat(sf_inter, fld_cat2015, fld_idgrid, "surf_ha", val_cat)
surf2012 <- rename(table_surf2012_par_idgrid,
                   "2012_Surfaces_urbaines"=M_1, "2012_Infrastructures"=M_2,
                   "2012_Extraction_materiaux"=M_3, "2012_Espaces_verts_loisirs"=M_4,
                   "2012_Milieux_agricoles"=M_5, "2012_Surfaces_boisees"=M_6,
                   "2012_Autres_surfaces_naturelles"=M_7, "2012_Eau"=M_8)
surf2015 <- rename(table_surf2015_par_idgrid,
                   "2015_Surfaces_urbaines"=M_1, "2015_Infrastructures"=M_2,
                   "2015_Extraction_materiaux"=M_3, "2015_Espaces_verts_loisirs"=M_4,
                   "2015_Milieux_agricoles"=M_5, "2015_Surfaces_boisees"=M_6,
                   "2015_Autres_surfaces_naturelles"=M_7, "2015_Eau"=M_8)
# nouvelle colonne surf_totale_ha : surface en ha des communes
sf_comm$surf_totale_ha <- st_area(sf_comm) %>% set_units(ha)
# jointure entre sf_comm et tableau croisé surf_par_commune (dplyr)
sf_comm <- sf_comm %>% left_join(surf2012, by=c(codcomm="idgrid"))
sf_comm <- sf_comm %>% left_join(surf2015, by=c(codcomm="idgrid"))
df_comm_no_geom <- as.data.frame(sf_comm)
df_comm_no_geom$geometry <- NULL
# sauvegarder sous forme de fichier csv les valeurs agrégées
write.csv2(df_comm_no_geom,file=fcsv, row.names=F)
print("EXEMPLE 3 > TERMINE")
Sys.time() - t0

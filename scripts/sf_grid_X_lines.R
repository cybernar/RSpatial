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
library(gdalUtils)
library(magrittr)
library(dplyr)


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
# EXEMPLE 3 : CALCULER SURFACE TOTALE ET
# SURFACE DE CHAQUE CATEGORIE OCCUPATION SOL
# POUR CHAQUE COMMUNE DE LA METROPOLE
# (année 2012 et année 2015, niveau 1, catégories "1" à "8") 
# ENREGISTRER LE RESULTAT DANS UN FICHIER .csv
# ------------------------------------------

# EXEMPLE 3 / DONNEES ENTREE
# nom de la couche donnees (ex: land cover)
fdata <- "D:/Support/PierrickD/Devoucoux/LIGNE_ELECTRIQUE.SHP"
# nom de grille d'analyse : ici ce sont les communes
fgrid <-  "D:/Support/PierrickD/Devoucoux/Grille_50m_Gard_contours.shp"
# champ id grille => le champ identifiant des communes
fld_idgrid <- "FID_1"

# EXEMPLE 3 / DONNEES SORTIE
# nom de la couche resultat intersection grille / donnees
finter <- "D:/bacasable/inter_grille_data.shp"
ftotallong <-  "D:/Support/PierrickD/Devoucoux/Grille_50m_Gard_LIGNES.shp"

# EXEMPLE 3 / SCRIPT 
print("EXEMPLE 3 > DEBUT")
t0 <- Sys.time()
# lire les données OcSOL
sf_data <- st_read(fdata)
# lire les données Etendue (communes) pour régler l'étendue de la grille
sf_grid <- st_read(fgrid)
# forcer définition des CRS pour éviter le problème des définitions hétérogènes 
st_crs(sf_grid) <- st_crs(sf_data)
# sélectionner les cellules de la grille qui intersectent les départements
# 1) liste des entités intersectés pour chaque cellule
#inter_grid_ext <- st_intersects(sf_data, sf_grid)
# 2) filtre : FAUX pour les cellules qui intersectent 0 communes, VRAI pour les autres
#filtre = (sapply(inter_grid_ext,length) > 0)
# 3) appliquer le filtre
#sf_data <- sf_data[filtre,]
# intersection polygones data X polygones grille
sf_inter <- st_intersection(sf_data, sf_grid)
# REMARQUE : parfois, st_intersection entre 2 shapes polygones peut générer
# des géométries de type LINESTRING ou POINT qu'on ne tient pas à conserver
# Voici un moyen de filtrer les entités par type de géométrie
sfc_test_polygon <- (st_geometry_type(sf_inter) %in% c("LINESTRING", "MULTILINESTRING"))
sf_inter <- sf_inter[sfc_test_polygon,]
# nouvelle colonne length_m : long en m 
sf_inter$length_m <- st_length(st_cast(sf_inter,"MULTILINESTRING")) %>% set_units(m)
# REMARQUE : si on n'avait pas expurger les géométries de types POINT et MULTILINESTRING,
# le st_write suivant aurait généré une erreur
st_write(sf_inter, finter, delete_dsn=TRUE)

df_aggX <- aggregate(length_m~FID_1, sf_inter, sum)

val_COLX <- rep(0, nrow(sf_grid))
corresp <- match(df_aggX$FID_1, sf_grid[["FID_1"]])
val_COLX[corresp] <- df_aggX$length_m

sf_grid$length_m <- val_COLX
# jointure entre sf_comm et tableau croisé surf_par_commune (dplyr)
#sf_grid <- sf_grid %>% left_join(df_aggX, by=c(FID_1="FID_1"))
st_write(sf_grid, ftotallong, delete_dsn=TRUE)

# RASTERISATION
# lire l'étendue du shp grid
sp_grid <- as_Spatial(st_geometry(sf_grid))
# resolution raster x,y / no data value / type (Int16 ou Float32) / option compression
out_res <- c(50, 50)
out_nodata <- -99
out_type <- "Float32" #float 32 bits avec signe -/+
out_ext <- as.numeric(sp_grid@bbox) # etendue shp

frast_ocsol <- "D:/Support/PierrickD/output/lignes_electriques_50m.tif"
gdal_rasterize(ftotallong, frast_ocsol, 
                     a="length_m", of="GTiff", a_srs="EPSG:2154", 
                     a_nodata=out_nodata, te=out_ext, tr=out_res, ot=out_type)


print("EXEMPLE 3 > TERMINE")
Sys.time() - t0

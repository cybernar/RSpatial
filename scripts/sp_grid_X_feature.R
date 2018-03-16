####

# GRID x FEATURES v 3
# Ce script synthetise les données vectorielles 
# d'une couche polygones ou polylignes en entrée
# dans un ou plusieurs rasters en sortie
#
# . Une grille vecteur polygone recouvrant les données en entrée ou l'emprise d'une autre couche est générée
# . Pour chaque catégorie d'entité (ex polygones ocsol='221', cours d'eau importance = '2'), intersection avec les cellules de la grille
#   Les cellules de grille sont traitées par grappes de (grille de nb_cells_process = 200) afin de ne pas saturer la mémoire
# . La surface ou longueur totale par ID cellules est calculée
# . Pour chaque catégorie, la surface ou longueur totale par ID cellules est enregistrée dans un raster

# 2017-07-18
# Cyril Bernard, CEFE-CNRS

###

library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(raster)


# ---------------------
# ICI VARIABLES A CHANGER
# ---------------------

# DONNEES ENTREE
# nom du repertoire avec les donnees (entree)
#datadir <- "D:/GIS_DATA/IGN/IGN_BDCARTHAGE/91_LANGUEDOC-ROUSSILLON"
datadir <- "D:/GIS_DATA/SIG-LR/OcSOL/V1_1_OCSOL_LR_POLY_2006"
# nom de la couche donnees (sans extension .shp)
#fdata <- "COURS_D_EAU"
fdata <- "V1_1_OCSOL_LR_POLY_2006"
# nom de la couche etendue pour la grille (sans extension .shp )
fetendue <- "DEP_LR"
#fetendue <- "DEP_OCCITANIE"
# sys coord (optionnel)
pcs_l93 <- "+init=EPSG:2154"

# DONNEES SORTIE
# nom du repertoire ecriture (sortie)
outdir <- "D:/GIS_DATA/__divers/R_output"
# resolution grille en m (ex: 1000 = pixel a 1000 m)
grid_res <- 1000
#grid_res <- 5000
# nombre de cellules a traiter dans chaque calcul d'intersection
# si ce nb est trop eleve, problemes de memoire
# ex : sur ma machine j'ai mis 200 car 1000 entrainait des erreurs de memoire
nb_cells_process <- 200
# nom de la grille a enregistrer (sans extension .shp)
#fgrid <- "grid_1k_lr"
fgrid <- "grid_5k_lr"
# nom de la couche resultat intersection
finter <- "inter_grid_data"
# champ id grille => laisser PIXID ds la cas d une grille generee par le script
fldgrid <- "PIXID"
# definition des catégories a traiter successivement
# (ex: codes occupation du sol, codes importance route)
# un code = un raster en sortie
# nom du champ catégorie. Remarque : script testé avec un champ de type caractere
#fldcat <- "CLASSE"
fldcat <- "niv3_06_b"
# liste des valeurs pour les catégories.
# En cas de code absent dans le fichier, une erreur peut survenir !
#val_cat <- c("1", "2", "3", "4")
val_cat <- c("112", "221", "311", "312", "313", "512")
# multiplicateur unités
# par défaut les unités sont en m pour les longueurs et m2 pour les surfaces
# mettre 0.001 pour obtenir des longueurs en km
# mettre 0.0001 pour des surfaces en ha ou 0.000001 pour des km2
#r_unites <- 1
r_unites <- 0.0001


# ---------------------
# DEFINITION FONCTIONS
# ---------------------

# genere une grille de pixels qui recouvre un shapefile
# avec le meme CRS que le shape en entree
# retour : SpatialPixelsDataFrame
makePixelGrid <- function (shp_ext, pix_res, p4s=as.character(NA)) {
  ptgrid <- makegrid(shp_ext, cellsize=pix_res)
  if (is.na(p4s)) {
    crsgrid <- shp_ext@proj4string
  } else {
    crsgrid <- CRS(p4s)
  }
  # data pixels
  #PIXID <- seq_len(nrow(ptgrid))
  PIXVAL <- rep(0, nrow(ptgrid))  # valeur defaut = 0 
  #PIXSURF <- pix_res ^ 2
  datagrid <- data.frame (PIXVAL) #(, PIXID, PIXSURF, XPIX=ptgrid$x1, YPIX=ptgrid$x2)
  # genere SpatialPixelsDataFrame
  SpatialPixelsDataFrame(points=ptgrid, data=datagrid, proj4string=crsgrid)
}

# genere une grille vectorielle de polygones qui recouvre un shapefile
# avec le meme CRS que le shape en entree
# retour : SpatialPolygonsDataFrame
makePolygonGrid <- function(shp_ext, pix_res, p4s=as.character(NA)) {
  ptgrid <- makegrid(shp_ext, cellsize=pix_res)
  if (is.na(p4s)) {
    crsgrid <- shp_ext@proj4string
  } else {
    crsgrid <- CRS(p4s)
  }
  # data polygons
  PIXID <- seq_len(nrow(ptgrid))
  PIXVAL <- 1 # valeur defaut
  PIXSURF <- pix_res ^ 2
  datagrid <- data.frame (PIXID, PIXVAL, PIXSURF, XPIX=ptgrid$x1, YPIX=ptgrid$x2)
  row.names(datagrid) <- as.character(PIXID)
  # polygons
  matgrid <- cbind(as.matrix(ptgrid), PIXID)
  lplyg <- apply(matgrid, 1, function(m, hr) {
    x <- m[1]
    y <- m[2]
    id <- as.character(m[3])
    pcoords <- matrix(c(x-hr, x-hr, x+hr, x+hr, x-hr, y-hr, y+hr, y+hr, y-hr, y-hr), 5, 2)
    Polygons(list(Polygon(pcoords)), id)
  }, pix_res/2)
  splyg <- SpatialPolygons(lplyg, proj4string=crsgrid)
  SpatialPolygonsDataFrame(splyg, datagrid)
}

# intersection entre une couche de données polylignes (ex: Coursdeau) et une grille d'analyse (ex: grille polygone, couche communes)
# shp_data: couche de donnees (classe SpatialLinesDataFrame)
# shp_grid: couche de découpage (classe SpatialPolygonsDataFrame)
# fld_grid: champ ID grid
# fld_data: champ ID donnees, ou Valeur donnees (ex: classe Cours d'eau)
interFeatGrid <- function(shp_data, shp_grid, fld_grid, fld_data, p_units) {
  df_grid <- slot(shp_grid, "data")
  df_data <- slot(shp_data, "data")
  # quel est le type des données en entrée ?
  datageom <- switch(class(shp_data), 
                     "SpatialLinesDataFrame" = "SpatialLines",
                     "SpatialPolygonsDataFrame" = "SpatialPolygons"
                     )
  spdf_result <- NULL
  if (datageom == "SpatialLines") {
    result2 <- gIntersection(shp_data, shp_grid, byid=TRUE, drop_lower_td=TRUE)
    # trouver les identifiants des géométries
    v_id <- sapply(slot(result2,"lines"), function(features) slot(features, "ID"))
    v_id_grid <- sapply(strsplit(v_id, " "), function(id2) id2[2])
    v_id_data <- sapply(strsplit(v_id, " "), function(id2) id2[1])
    PIXID <- df_grid[v_id_grid, fld_grid]
    DATAVAL <- df_data[v_id_data, fld_data]
    MESURE <- gLength(result2, byid=TRUE) * p_units
    df_result <- data.frame(PIXID, DATAVAL, MESURE, row.names=v_id)
    spdf_result <- SpatialLinesDataFrame(result2, df_result)
  }
  if (datageom == "SpatialPolygons") {
    result2 <- gIntersection(shp_data, shp_grid, byid=TRUE, drop_lower_td=TRUE)
    # trouver les identifiants des géométries
    v_id <- sapply(slot(result2,"polygons"), function(features) slot(features, "ID"))
    v_id_grid <- sapply(strsplit(v_id, " "), function(id2) id2[2])
    v_id_data <- sapply(strsplit(v_id, " "), function(id2) id2[1])
    PIXID <- df_grid[v_id_grid, fld_grid]
    DATAVAL <- df_data[v_id_data, fld_data]
    MESURE <- gArea(result2, byid=TRUE)* p_units
    df_result <- data.frame(PIXID, DATAVAL, MESURE, row.names=v_id)
    spdf_result <- SpatialPolygonsDataFrame(result2, df_result)
  }
  return(spdf_result)
}


# ---------------------
# LECTURE DONNEES
# ---------------------
# remarque : en cas de probleme avec les sys de coord des donnees en entree,
# declarer le sys de coord dans les fonctions readOGR, makePolygonGrid et makePixelGrid
# pour avoir des systemes identiques
t0 <- Sys.time()

# lire les donnees : bd carthage
shp_data <- readOGR(datadir, fdata, p4s=pcs_l93)

# lire la couche qui sert de reference pour la generation de la grille : departements
shp_ext <-  readOGR(datadir, fetendue, p4s=pcs_l93)

# generer la grille avec etendue de shp_ext
shp_grid <- makePolygonGrid(shp_ext, grid_res, pcs_l93)
shp_grid <- shp_grid[shp_ext,]
writeOGR(shp_grid, outdir, fgrid, "ESRI Shapefile", overwrite_layer=T)

# verification coherence donnees
if (!(fldcat %in% names(shp_data))) {
  print ("ERREUR : Colonne fldcat absente des données")
  return()
}
Sys.time() - t0
print("LECTURE DONNEES OK")

# ---------------------
# CALCUL INTERSECTION PAR PAQUETS DE [nb_cells_process]
# ---------------------

# definir paquet de cellule a traiter ulterieurement > creer 2 vecteurs
# ideb = index cellule 1
# ifin = index cellule nb_cells_process
nb_cells <- length(shp_grid)
ideb <- seq.int(from=1, to=nb_cells, nb_cells_process)
ifin <- ideb - 1 + nb_cells_process
ifin[ifin > nb_cells] <- nb_cells
total_paquets <- length(ideb)

# barre de progression
print("Intersection DATA X GRID")
pb <- txtProgressBar(min = 0, max = total_paquets, style = 3)
for(X in seq_len(total_paquets)) {
  shp_gridX <- shp_grid[ideb[X]:ifin[X],]
  shp_dataX <- shp_data[shp_gridX,]
  if (X == 1) {
    shp_inter1 <- interFeatGrid(shp_dataX, shp_gridX, fldgrid, fldcat, r_unites)
  } else {
    shp_interX <- interFeatGrid(shp_dataX, shp_gridX, fldgrid, fldcat, r_unites)
    shp_inter1 <- spRbind(shp_inter1, shp_interX)
  }
  setTxtProgressBar(pb, X)
}
close(pb)

# sauvegarde du fichier intersection
writeOGR(shp_inter1, outdir, finter, "ESRI Shapefile", overwrite_layer=T)

# liberer memoire
rm(shp_dataX)
rm(shp_interX)

# ---------------------
# CALCUL LONGUEUR OCC SOL PAR PIXEL
# ET CREATION RASTERS
# ---------------------

# pour chaque code ocsol,
# generer rasters et enregistrer dans un fichier format GeoTiff float 32 bits
for(X in 1:length(val_cat)) {
  # filtrer par code OcSOL
  val_catX <- val_cat[X]
  cat_filtre_X <- shp_inter1@data[,"DATAVAL"] == val_catX
  shp_interX <- shp_inter1[cat_filtre_X,]
  # calculer somme(surface) par PIXID 
  df_interX <- shp_interX@data
  df_aggX <- aggregate(MESURE~PIXID, df_interX, sum)
  # generer une grille SpatialPixels initialisee a 0, avec etendue de shp_ext
  pix_grid <- makePixelGrid(shp_ext, grid_res, pcs_l93)
  # enregistrer ds la grille SpatialPixels les longueurs / surfaces par pixel
  PIXVAL_buffer <- pix_grid@data$PIXVAL
  PIXVAL_buffer[df_aggX$PIXID] <- df_aggX$MESURE
  #PIXVAL_buffer <- 100 * PIXVAL_buffer / (grid_res * grid_res)
  pix_grid@data$PIXVAL <- PIXVAL_buffer
  
  rasterX <- raster(pix_grid)
  frasterX <- paste0(outdir, "/gridX", val_catX, ".tif")
  writeRaster(rasterX, frasterX, format="GTiff", datatype="FLT4S", overwrite=T)
}

Sys.time() - t0
print("FIN DU TRAITEMENT !!")

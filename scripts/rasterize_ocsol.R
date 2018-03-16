
library(sp)
library(rgdal)
library(raster)
library(gdalUtils)

# nom de la couche donnees (ex: land cover)
fdata <- "D:/Support/PierrickD/RasteriseS30/OCS_GE_SUD_GARD_2012_NIV4.shp"
ddata <- "D:/Support/PierrickD/RasteriseS30"
ldata <- "OCS_GE_SUD_GARD_2012_NIV4"
fldcat <- "NIV4_12"
# nom de grille d'analyse
fgrid <- "D:/Support/PierrickD/RasteriseS30/Grille_50m_Gard_contours.shp"
lgrid <- "Grille_50m_Gard_contours"

# lire la grille vecteur 50 m 
# juste pour récupérer son emprise puis 
# puis créer un raster qui occupe la même étendue
lyr_grid <- readOGR(dsn=ddata, layer=lgrid)

# resolution raster x,y / no data value / type (Int16 ou Float32) / option compression
out_res <- c(10, 10)
out_nodata <- -99
out_type <- "Int16" #entier 16 bits avec signe -/+
out_co_int <- "COMPRESS=LZW PREDICTOR=2" # à utiliser pour compresser le GeoTiff entier si trop lourd
out_co_float <- "COMPRESS=LZW PREDICTOR=3" # à utiliser pour compresser le GeoTiff float si trop lourd
out_ext <- as.numeric(lyr_grid@bbox) # etendue shp
  
# rasterisation grille => finalement inutile
#frast_grid <- "D:/Support/PierrickD/output/grid_10m.tif"
#rb <- gdal_rasterize(fgrid, frast_grid, 
#               a="Id", of="GTiff", a_srs="EPSG:2154", 
#               a_nodata=out_nodata, te=out_ext, tr=out_res, ot=out_type, output_Raster=T)
# remarque : équivalent en Shell :
# gdal_rasterize -a "Id" -of "GTiff" -a_srs "EPSG:2154" -a_nodata "-99" -tr 10 10 -te 780540.2 6263120.5  834040.2 6322320.5 -ot "Int16" "D:/Support/Grille_50m_Gard_contours.shp" "D:/Support/grid_50m.tif"
#rast_grid <- raster(rb,layer=0) # raster brick to raster


# rasterisation data ocsol, colonne NIV4_12 (entier)
frast_ocsol <- "D:/Support/PierrickD/output/ocsol_10m.tif"
rb <- gdal_rasterize(fdata, frast_ocsol, 
                     a=fldcat, of="GTiff", a_srs="EPSG:2154", 
                     a_nodata=out_nodata, te=out_ext, tr=out_res, ot=out_type, output_Raster=T)
# remarque : équivalent en Shell :
# gdal_rasterize -a "NIV4_12" -of "GTiff" -a_srs "EPSG:2154" -a_nodata "-99" -tr 10 10 -te 780540.2 6263120.5  834040.2 6322320.5 -ot "Int16" "D:/Support/PierrickD/RasteriseS30/OCS_GE_SUD_GARD_2012_NIV4.shp" "D:/Support/ocsol_10m.tif"
rast_ocsol <- raster(frast_ocsol) # raster brick to raster

# ###################################################
# statistiques focales = fenêtre mouvante 
# pour trouver la proportion de NIV4_12=3111 (foret de feuillus)
# dans un rayon de 200 m autour de chaque pixel

# definir matrice de poids de 200 m de rayon
w <- focalWeight(rast_ocsol, d=200, type="circle")
# filtrer raster ocsol value=3111
frast_3111 <- "D:/Support/PierrickD/output/cat3111_r200.tif"
rast_3111 <- (rast_ocsol == 3111)
# equivalent a
#rast_3111 <- (rast_ocsol * 0)
#rast_3111[rast_ocsol == 3111] <- 1
focal_3111 <- focal(rast_3111, w, fun=sum, filename=frast_3111, overwrite=T)
 
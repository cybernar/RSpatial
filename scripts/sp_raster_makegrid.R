library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(raster)


# genere une grille qui recouvre un shapefile
# sous forme de tableau avec coordonnees XY
# retour : data.frame
makeGrid <- function (shp_ext, pix_res) {
  ptgrid <- makegrid(shp_ext, cellsize=pix_res)
  crsgrid <- shp_ext@proj4string
  # data pixels
  PIXID <- seq_len(nrow(ptgrid))
  PIXVAL <- 0
  PIXSURF <- pix_res ^ 2
  data.frame (PIXID, PIXVAL, PIXSURF, XPIX=ptgrid$x1, YPIX=ptgrid$x2)
}

# genere une grille vectorielle de points qui recouvre un shapefile
# avec le meme CRS que le shape en entree
# retour : SpatialPointsDataFrame
makePointGrid <- function (shp_ext, pix_res) {
  ptgrid <- makegrid(shp_ext, cellsize=pix_res)
  crsgrid <- shp_ext@proj4string
  # data pixels
  PIXID <- seq_len(nrow(ptgrid))
  PIXVAL <- 0
  PIXSURF <- pix_res ^ 2
  datagrid <- data.frame (PIXID, PIXVAL, PIXSURF, XPIX=ptgrid$x1, YPIX=ptgrid$x2)
  # genere SpatialPixelsDataFrame
  SpatialPointsDataFrame(coords=ptgrid, data=datagrid, proj4string=crsgrid)
}

# genere une grille de pixels qui recouvre un shapefile
# avec le meme CRS que le shape en entree
# retour : SpatialPixelsDataFrame
makePixelGrid <- function (shp_ext, pix_res) {
  ptgrid <- makegrid(shp_ext, cellsize=pix_res)
  if (is.na(p4s)) {
    crsgrid <- shp_ext@proj4string
  } else {
    crsgrid <- CRS(p4s)
  }
  # data pixels
  PIXID <- seq_len(nrow(ptgrid))
  PIXVAL <- 0
  PIXSURF <- pix_res ^ 2
  datagrid <- data.frame(PIXVAL)  #(PIXID, PIXVAL, PIXSURF, XPIX=ptgrid$x1, YPIX=ptgrid$x2)
  # genere SpatialPixelsDataFrame
  SpatialPixelsDataFrame(points=ptgrid, data=datagrid, proj4string=crsgrid)
}

# genere une grille vectorielle de polygones qui recouvre un shapefile
# avec le meme CRS que le shape en entree
# retour : SpatialPolygonsDataFrame
makePolygonGrid <- function(shp_ext, pix_res) {
  ptgrid <- makegrid(shp_ext, cellsize=pix_res)
  crsgrid <- shp_ext@proj4string
  # data polygons
  PIXID <- seq_len(nrow(ptgrid))
  PIXVAL <- 1
  PIXSURF <- pix_res ^ 2
  datagrid <- data.frame (PIXID, PIXVAL, PIXSURF, XPIX=ptgrid$x1, YPIX=ptgrid$x2)
  row.names(datagrid) <- as.character(PIXID)
  # polygons
  matgrid <- cbind(as.matrix(ptgrid), PIXIDINT)
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

# transforme un SpatialPixelsDataFrame en SpatialPolygonsDataFrame
# [bug ne pas utiliser]
pixel2PolygonGrid <- function (pixel_grid) {
  gridtopo <- getGridTopology(pixel_grid)
  crsgrid <- pixel_grid@proj4string
  datagrid <- pixel_grid@data
  plyg <- as.SpatialPolygons.GridTopology(gridtopo, crsgrid)
  GID <- sapply(slot(plyg,"polygons"), function(x) slot(x,"ID"))
  datagrid <- cbind(GID, datagrid)
  row.names(datagrid) <- GID
  SpatialPolygonsDataFrame(plyg, datagrid)
}

datadir <-"D:/GIS_DATA/IGN/IGN_GEOFLA"
shp_ext <-  readOGR(datadir,"DEP_OCCITANIE")

# generer la grille SpatialPixels avec etendue de shp_ext
pix_grid <- makePixelGrid(shp_ext, 5000)
# generer la grille SpatialPolygons avec etendue de shp_ext
shp_grid <- makePolygonGrid(shp_ext, 5000)
# filtrer les polygons
shp_grid <- shp_grid[shp_ext,]
writeOGR(shp_grid, datadir, "grid_5k", "ESRI Shapefile", overwrite_layer=T)

data_shp_grid <- shp_grid@data[,c("PIXID", "PIXVAL")]
PIXVAL_buffer <- pix_grid@data$PIXVAL
PIXVAL_buffer[data_shp_grid$PIXID] <- data_shp_grid$PIXVAL
pix_grid@data$PIXVAL <- PIXVAL_buffer

rast <- raster(pix_grid, layer=2)
writeRaster(rast, "D:/GIS_DATA/IGN/IGN_GEOFLA/rast.tif", format="GTiff", datatype="FLT4S", overwrite=T)

plot(pix_grid["PIXVAL"])
# generer la grille SpatialPolygons avec etendue de shp_ext
#shp_ptgrid <- makePointGrid(shp_ext, 5000)
#shp_ptgrid <- shp_ptgrid[shp_ext,]
#writeOGR(shp_ptgrid, datadir, "ptgrid_5k", "ESRI Shapefile", overwrite_layer=T)



# ####
# test
pix_res=5000
ptgrid <- makegrid(shp_ext, cellsize=pix_res)
matgrid <- cbind(as.matrix(ptgrid),seq_len(nrow(ptgrid)))
p4s=as.character(NA)
plyg <- apply(matgrid, 1, function(m, hr) {
  x <- m[1]
  y <- m[2]
  id <- m[3]
  pcoords <- matrix(c(x-hr, x-hr, x+hr, x+hr, x-hr, y-hr, y+hr, y+hr, y-hr, y-hr), 5, 2)
  Polygons(Polygon(pcoords), id)
}, pix_res/2)

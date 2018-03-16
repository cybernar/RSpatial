# leaflet test

library(sp)
library(leaflet)
library(magrittr)

# génère une grille d'analyse sous forme de SpatialPolygonsDataFrame (sp) avec des cellules hexagonales
makeHexaGrid <- function (shp_ext, pix_res) {
  ptgrid <- spsample(shp_ext, type="hexagonal", cellsize=pix_res)
  hex_plyg <- HexPoints2SpatialPolygons(ptgrid)  
  datagrid <- data.frame (idpix=seq_len(length(ptgrid)), xpix=ptgrid@coords[,1], ypix=ptgrid@coords[,2])
  SpatialPolygonsDataFrame(hex_plyg, datagrid, match.ID=F)
}

mk_rad <- 4
mk_col <-"#00C"
mk_opa <- 0.6
crs_l93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
crs_wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs")

m <- cbind(
  lon=rnorm(20, 0, 0.01) + 3.636, 
  lat=rnorm(20, 0, 0.01) + 43.868
)

df <- data.frame(
  m,
  cat=sample(LETTERS[1:5],20,replace=T)
)

spts <- SpatialPoints(m, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
spts_l93 <- spTransform(spts, crs_l93)
grid_l93 <- makeHexaGrid(spts_l93, 500)
grid_hex <- spTransform(grid_l93, crs_wgs84)

#sp_bbox <- matrix(c(3.626, 43.858, 3.646, 43.878),nrow=2,dimnames=list(c("x","y"),c("min","max")))
#sp_proj4string <- crs_wgs84
#sp_l93 <- spTransform(Spatial(bbox=sp_bbox, proj4string=crs_wgs84), crs_l93)
  
leaflet() %>%
  addTiles(group = "OSM Mapnik") %>%
  addCircleMarkers(data=m, radius=mk_rad, stroke=F, fillOpacity=mk_opa, fillColor=mk_col, group="data (individual)") %>%
  addPolygons(data=grid_hex,)


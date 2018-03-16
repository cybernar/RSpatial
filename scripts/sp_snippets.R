library(sp)
library(maptools)
library(rgeos)

setwd("D:/bacasable/RTest/CLC06_D34_RGF")

shp_commune <- readShapePoly("COMMUNE", IDvar="ID", proj4string = CRS("+init=epsg:2154"))
shp_grid <- readShapePoly("GRID5K_D34_RGF", IDvar="ID", proj4string = CRS("+init=epsg:2154"))
shp_clc06 <- readShapePoly("CLC06_D34_RGF", proj4string = CRS("+init=epsg:2154"))

# ex1. filtrer par valeur CLC (CODE_06="311" ou "312" ou "313") et chercher surf totale
clc_filtre_foret <- shp_clc06@data$CODE_06 %in% c("311","312","313")
shp_clc06_foret <- shp_clc06[clc_filtre_foret,]
total_area_foret <- sum(shp_clc06_foret@data$AREA_HA)

# ex2. chercher les cellules de la grille qui intersectent les forets
#poly_communes <- shp_commune@polygons
#poly_grid <- shp_grid@polygons
#grid_over <- over(shp_grid, shp_commune)
#grid_over_communes <- shp_grid[!is.na(grid_over[,1]),]
shp_grid_over_foret <- shp_grid[shp_clc06_foret,]

# ex3. intersecter grille + foret avec en sortie l'id grille, le type de foret et la surface du nouveau poly
data_grid <- shp_grid_over_foret@data
data_clc06 <- shp_clc06_foret@data
result <- gIntersection(shp_grid_over_foret, shp_clc06_foret, byid=TRUE)
v_id <- sapply(seq_along(result), function(l,i){"[["(l,i)@ID}, l=result@polygons)
v_id_grid <- sapply(seq_along(v_id), function(v,i,j){strsplit(v[i], " ")[[1]][j]}, v=v_id, j=1)
v_id_clc <- sapply(seq_along(v_id), function(v,i,j){strsplit(v[i], " ")[[1]][j]}, v=v_id, j=2)
#f<-function(l,i){
#   v<-"[["(l,i)@ID
#   v2<-strsplit(v, " ")[[1]]
#   return(v2)
# }
# v_id <- sapply(seq_along(result), f, l=result@polygons)
ID_GRILLE <- data_grid[v_id_grid,]$ID
CODE_06 <- data_clc06[v_id_clc,]$CODE_06
SURFACE <- gArea(result,byid=TRUE) / 10000
df_result <- data.frame(ID_GRILLE, CODE_06, SURFACE, row.names=v_id)
shp_result <- SpatialPolygonsDataFrame(result, df_result)
writePolyShape(shp_result, "inter_foret_5k")

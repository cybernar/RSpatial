#distance_

library(sp)
library(rgdal)
library(rgeos)

#setwd("D:/bacasable/RTest2")
setwd("/Users/loulou/Travail/R-Spatial/RWorkshop")
URL <- "http://cartotheque.cefe.cnrs.fr/wp-content/uploads/2016/06/RCEFE_140616_data.zip"
download.file(URL,"RCEFE_140616_data.zip")
unzip("RCEFE_140616_data.zip")

shp_origin <- readOGR(".","Arbres_align")
shp_target <- readOGR(".","Parc_Jardin")

# 553 points from origin, 12 polygons from target
# gDistance will return a 553 x 12 matrix
dist_mat <- gDistance(shp_target, shp_origin, byid=TRUE)
# if the target is identical to the origin, we apply this line to remove self-distance (0 m)
# from the search of nearest neighbour ... 
if (identical(shp_origin, shp_target)) {
  is.na(dist_mat) <- (dist_mat==0)
}

FROMID <- shp_origin$IDARBRE 
TOID <- shp_target$OBJECTID

ind_min <- apply(dist_mat, MARGIN = 1, which.min)
NEARID <- TOID[ind_min]
# mi will be an indices matrix to find the distance to the nearest target
# with 1: row indices(sequence from 1 to N)  and 2:col indices(from which.min) 
mi <- cbind(i=seq.int(length(ind_min)), ind_min)
NEARDIST <- dist_mat[mi]

#plyg_result <- slot(shp_origin,"polygons")
df_result <- data.frame(FROMID, NEARID, NEARDIST, row.names = row.names(shp_origin))

shp_result <- spCbind(shp_origin, df_result)
writeOGR(shp_result,"/Users/loulou/Travail/R-Spatial/RWorkshop","result_dist2",driver="ESRI Shapefile")

# ------------------------------------------------------------
# intersection between grid polygons & target polygons
shp_data <- readOGR(".","Parc_Jardin")
shp_grid <- readOGR(".", "grid_500m")
plot(shp_grid)
plot(shp_data, add=T)
View(shp_data@data)

# optimization : select grid cells that intersects data 
shp_grid_over_data <- shp_grid[shp_data,]
# then intersection !
result <- gIntersection(shp_data, shp_grid_over_data, byid=TRUE)
# 
v_id <- sapply(slot(result,"polygons"), function(plyg) slot(plyg, "ID"))
v_id_data <- sapply(strsplit(v_id, " "), function(id2) id2[1])
v_id_grid <- sapply(strsplit(v_id, " "), function(id2) id2[2])

# get values of "idfld_grid" field from df_grid data.frame
df_data <- slot(shp_data, "data")
df_inter1 <- df_data[v_id_data,]

# get values of "GID" field from df_grid data.frame
df_grid <- slot(shp_grid, "data")
df_inter2 <- df_grid[v_id_grid,]

SURF_INTER <- gArea(result, byid=TRUE)
df_result <- data.frame(df_inter1, df_inter2, SURF_INTER, row.names=v_id)
shp_inter <- SpatialPolygonsDataFrame(result, df_result)
writeOGR(shp_inter,".","result_inter",driver="ESRI Shapefile")

# what is the total surface of wood by grid cell ?
# let us aggregate the result of intersection by grid cell

df_agg <- aggregate(SURF_INTER~GID, df_result, sum)

v_agg_id <- df_agg$GID
v_grid_id <- df_grid$GID
o <- match(v_agg_id, v_grid_id)

v_agg_surf <- df_agg$SURF_INTER
SURFGRID <- rep(0,length(v_grid_id))
SURFGRID[o] <- v_agg_surf
shp_grid2 <- spCbind(shp_grid, SURFGRID)

writeOGR(shp_grid2,".","result_grid_aggr",driver="ESRI Shapefile")

# sf snippets

library(sf)

# creer un objet sf à partir d'un data.frame
name <- c("Agropolis","Bois de Montmaur","CEFE","Station météo TE","FDS Bâtiment 4","Statue Peyrou")
lon <- c(3.86921,3.86898,3.86450,3.86306,3.86282,3.87093)
lat <- c(43.64541,43.64266,43.63881,43.63885,43.63464,43.61165)
color <- c("blue","green","blue","blue","blue","green")
df <- data.frame(name, lon, lat, color)

# creer un objet sfg (Simple Feature Geometry) : ici, un point
p1 <- st_point(c(lon[1], lat[1]))
p1

# creer un objet sfc (Simple Feature Geometry list-column) : ici, une liste de point
library(purrr)
list_geom <- map2(lon, lat, function(x,y){st_point(c(x,y))})
sfc_lonlat <- st_sfc(list_geom, crs=4326)
sfc_lonlat
class(sfc_lonlat)

# creer un objet sf
st_geometry(df) <- sfc_lonlat
class(df)

# lire un shape avec sf



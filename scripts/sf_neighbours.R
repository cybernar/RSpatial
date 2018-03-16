library(sf)

fgrid <- "D:/GIS_DATA/INPN/Referentiel/L93_10K.shp"

# lire les entites du shp (9546 polygones)
grd <- st_read(fgrid)
nrow(grd)

# chercher les voisins de chaque entite 
# uniquement les voisins qui partagent 1 cote, pas les coins
# chaque cellule comprend 1 à 4 voisins (dont elle-même)
st_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****")
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
# (1 liste de 9546 vecteurs)
l_adj <- st_queen(grd)
l_adj <- st_touches(grd)
length(l_adj)

# ex: cellule 271 
i <- 271
# attributs de la cellule
grd[i,]
# attributs des voisins 
i_voisins <- l_adj[[i]]
grd[i_voisins,]


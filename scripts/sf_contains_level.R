#
# analyse de voisinage avec R et sf
#

library(sf)
library(dplyr)
library(purrr)

# donnees en entree
#setwd("D:/bacasable")
#f_input <- "TEST_DE9IM.shp"
#f_output <- "OUT_DE9IM.shp"

setwd("D:/GIS_Analysis/Bolivie_2018/Stats Pucara/1_georef_plans")
f_input <- "JiriraB2.shp"
f_output <- "jiriraB_nettoyage.shp"
#JiriraB2

# 1. lire le fichier shapefile pucara 
shp_data <- st_read(f_input)
st_crs(shp_data) <- 32719
shp_data <- shp_data %>% transmute(FID=seq_len(nrow(shp_data)), Color)
#shp_data <- shp_data %>% transmute(FID=seq_len(nrow(shp_data)), surf_m2=st_area(shp_data), Color)
names(shp_data)
head(shp_data)


# 2. scission murs / surfaces utiles
shp_murs <- shp_data %>% filter(Color==7)
shp_utiles <- shp_data %>% filter(Color!=7)


# 3B. supprime doublons parmi les polygones murs 
# METHODE SURFACE IDENTIQUE
surf_m2 <- round(as.numeric(st_area(shp_murs)),6)
which(!surf_m2>0)
#shp_murs <- shp_murs[surf_m2>0,]
centr <- st_coordinates(st_centroid(shp_murs))
centr_x <- round(centr[,1])
centr_y <- round(centr[,2])
#f_bounds <- function(x,i) st_bbox(x)[i]
df_murs <- data.frame(surf_m2, centr_x, centr_y)
f_tokeep2 <- !duplicated(df_murs)
shp_murs <- shp_murs[f_tokeep2,]


# 4. analyse relation contenant / contenu dans les polygones utiles
#    supprimer les polygonses à la fois contenus ET contenants
list_within_p <- st_relate(shp_utiles, pattern="2FF1FF212")
v_within <- sapply(list_within_p, length)
list_contains_p <- st_relate(shp_utiles, pattern="212FF1FF2")
v_contains <- sapply(list_contains_p, length)
f_tokeep <- v_within == 0 | v_contains == 0
shp_utiles <- shp_utiles[f_tokeep,]


# 5B. supprime doublons dans shp_murs par rapport à shp_utiles
# METHODE SURFACE IDENTIQUE
f_surf_centr <- function(shp_in, dsname) {
  i <- seq_len(nrow(shp_in))
  surf_m2 <- round(as.numeric(st_area(shp_in)),4)
  mcentr <- st_coordinates(st_centroid(shp_in))
  centr_x <- round(mcentr[,1])
  centr_y <- round(mcentr[,2])
  data.frame(dsname, i, surf_m2, centr_x, centr_y)
}
df_murs <- f_surf_centr(shp_murs, "murs")
df_utiles <- f_surf_centr(shp_utiles, "utiles")
df_test <- rbind(df_utiles, df_murs)
f_tokeep <- !duplicated(df_test[,c("surf_m2","centr_x","centr_y")])
f_tokeep <- f_tokeep[df_test$dsname == "murs"]
shp_murs <- shp_murs[f_tokeep,] 


# 6. refusionne shp_murs et shp_utiles
shp_data2 <- rbind(shp_utiles, shp_murs)
# et simplifier les géométries
shp_data2 <- st_simplify(shp_data2,dTolerance=0.005)


 # 7. analyse relation contenant / contenu dans shp_data2

# quels sont les polygones contenus dans aucun autre ?
is_within <- function(x) length(x) >= 1
is_within_one <- function(x) length(x) == 1

shp_data2 <- shp_data2 %>% mutate(FID_parent=FID, lev_within=0)
names(shp_data2)

FID_data2 <- shp_data2$FID
shp_process <- shp_data2
level_within <- 0
#index_process <- seq_len(nrow(shp_data2))

repeat {
  # chercher si x (shp_data2) completement contenu dans y (shp_process)
  # -> pour chaque x, vecteur des y dans lequel il est contenu
  # autrement dit : x (1) est contenu dans y (0-N)
  list_within_p <- st_relate(shp_process, pattern="2FF1FF212")
  level_within <- level_within + 1

  # quels sont les index des polygones contenus dans 1 seul autre polygone ?
  index_is_within_one <- which(sapply(list_within_p, is_within_one))
  if (length(index_is_within_one) > 0) {
    # quels sont les FID des polygones contenus dans 1 seul autre polygone ?
    index_parent <- unlist(list_within_p[index_is_within_one])
    FID_is_within_one <- shp_process$FID[index_is_within_one]
    FID_parent <- shp_process$FID[index_parent]
    index_is_within_one <- match(FID_is_within_one, FID_data2)
    index_parent <- match(FID_parent, FID_data2)  
    shp_data2$FID_parent[index_is_within_one] <- shp_data2$FID[index_parent]
    shp_data2$lev_within[index_is_within_one] <- level_within
  }
  
  # quel est l'ensemble des polygones contenus dans d'autres polygones ?
  index_is_within <- which(sapply(list_within_p, is_within))
  if (length(index_is_within) > 0) {
    # si ensemble non vide alors continue
    shp_process <- shp_process [index_is_within,]
  } else {
    # sinon arreter la boucle
    break
  }
}


# 8. trouer les polygones pour supprimer les surfaces redondantes
#    si le polygone P1 a un parent P2, alors P2 = P2 - P1
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

# partir de l=0 vers l=level_within
level_inf <- 1
repeat {
  # trouer level_sup avec level_inf
  if (level_inf > level_within) {
    break
  } else {
    index_level_inf <- which(shp_data2$lev_within==level_inf)
    for (i in index_level_inf) {
      FID_i <- shp_data2$FID[i]
      FID_p <- shp_data2$FID_parent[i]
      if (FID_i != FID_p) {
        i_parent <- match(FID_p, shp_data2$FID)
        shp_data2[i_parent,] <- st_erase(shp_data2[i_parent,], shp_data2[i,])
      }
    }
    level_inf <- level_inf + 1
  }
}


# 9. verifier coherence geometries
sfc_test_polygon <- (st_geometry_type(shp_data2) %in% c("POLYGON", "MULTIPOLYGON"))
all(sfc_test_polygon)
if (!all(sfc_test_polygon)) {
  shp_data2B <- shp_data2[!sfc_test_polygon,]
  shp_data2A <- shp_data2[sfc_test_polygon,]
  shp_data2B <- st_collection_extract(shp_data2B,"POLYGON")
  shp_data2 <- rbind(shp_data2A, shp_data2B)
  shp_data2 <- st_cast(shp_data2, "MULTIPOLYGON")
}

st_write(shp_data2, f_output ,delete_layer = T)


# -------------- 
# DE9-IM relations
#list_contains_p <- st_contains_properly(shp_data, shp_data)
#list_contains_p <- st_relate(shp_data, shp_data, "212FF1FF2")
#list_equals_p <- st_relate(shp_data, shp_data, "2FFF1FFF2")
#list_equals <- st_relate(shp_data, shp_data, "2***1***2")

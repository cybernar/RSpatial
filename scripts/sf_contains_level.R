#
# analyse de voisinage avec R et sf
#

library(sf)
library(dplyr)


setwd("D:/GIS_Analysis/Bolivie_2018")
f_input_d <- "Stats Pucara/2_nettoyage_plans/jachapucara_decor2.shp"
f_input_s <- "Stats Pucara/2_nettoyage_plans/jachapucara_structures2.shp"

f_output_d <- "Stats Pucara/2_nettoyage_plans/jachapucara_decor3.shp"
f_output_s <- "Stats Pucara/2_nettoyage_plans/jachapucara_structures3.shp"


# 1. lire le fichier shapefile pucara 
shp_decor <- st_read(f_input_d)
st_crs(shp_decor) <- 32719
shp_structures <- st_read(f_input_s)
st_crs(shp_structures) <- 32719
names(shp_structures)
head(shp_structures)


# simplify
shp_data <- st_simplify(shp_decor,dTolerance=0.005) %>% 
  select(Color, CONTIENT, CONTENU) %>%
  mutate(l_contains=0, l_within=0)
shp_structures2 <- st_simplify(shp_structures,dTolerance=0.005)  %>% 
  select(Color, CONTIENT, CONTENU) %>%
  mutate(l_contains=0, l_within=0)


# tant que des feat decor contiennent d'autres feat decor: découper
repeat {
  
  interlist <- st_contains_properly(shp_decor2, shp_decor2)
  v_contains <- sapply(interlist, length)
  v_contains

  level_contains <- ifelse(v_contains==0, 1, 0)

  f_contained <- level_contains==1
  f_containing <- level_contains==0
  
  nb_polygon_contenants <- sum(f_containing)

  if (nb_polygon_contenants < 1) {
    break
  }
  
  # découper murs conteneurs avec mur contenant
  
  shp_cut1 <- shp_decor2[filtre_within & !filtre_contains,]
  shp_decor3 <- shp_decor2[!filtre_within | filtre_contains,]
  shp_decor3 <- st_erase(shp_decor3, shp_cut1)
  shp_decor2 <- rbind(shp_cut1, shp_decor3)
  
}



# fonction emporte-pièces
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

st_write(shp_decor2, f_output_d ,delete_layer = T)




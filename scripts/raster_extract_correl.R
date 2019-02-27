# coeff correlation entre 2 rasters par pays

library(raster)
library(rnaturalearth) # pour les contours des pays
library(rnaturalearthdata) # pour les contours des pays echelle medium 1/50M

# -----
# lecture donnees
# -----

# contours et nom pays en vectoriel (natural earth)
sp_countries <- ne_countries(type = 'countries', scale = 'medium')
nom_pays <- sp_countries$name
head(nom_pays)

# rasters 1 et 2
setwd("D:/bacasable")
rast_1 <- raster("res03_crav6190h_sihr_cer.tif")
rast_2 <- raster("res03_crav6190l_silr_pml.tif")


# -----
# exemple pour visualiser raster et contour pays : Maroc
# -----
sp_countries_maroc <- sp_countries[nom_pays=="Morocco",]
par(mfrow=c(1,2))
# apercu raster 1
plot(rast_1, ext=sp_countries_maroc)
plot(sp_countries_maroc, add=T)
# apercu raster 2
plot(rast_2, ext=sp_countries_maroc)
plot(sp_countries_maroc, add=T)


# -----
# CALCUL
# -----
# extraction des valeurs et calcul coeff cor pour tous les pays
# extraire valeurs du raster 1 et 2 (liste de vecteurs)
list_values_rast_1 <- extract(rast_1, sp_countries)
list_values_rast_2 <- extract(rast_2, sp_countries)

nlignes <- length(nom_pays)
df_sortie <- data.frame(nom_pays,
                        npixels_r1 = rep(NA, nlignes),
                        npixels_r2 = rep(NA, nlignes),
                        coeff_r1_r2 = rep(NA, nlignes))
for (i in 1:nlignes) {
  # valeurs pixel R1 et R2 
  v1_i <- list_values_rast_1[[i]]
  v2_i <- list_values_rast_2[[i]]
  # nb pixels R1 et R2
  df_sortie[i,"npixels_r1"] <- length(v1_i)
  df_sortie[i,"npixels_r2"] <- length(v2_i)
  # coeff cor
  # TODO : filter les valeurs NA ??
  df_sortie[i,"coeff_r1_r2"] <- cor(v1_i, v2_i)
}

head(df_sortie)

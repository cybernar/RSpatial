# safran 2 raster

library(data.table)
library(raster)
library(rgdal)

# ---- fonctions ----

# genere 1 raster à partir de DT(data.table) pour la variable et la date spécifiées
DT2rast <- function(extr_date, DT, varname) {
  varlist <- c("LAMBX", "LAMBY", varname)
  DTi <- DT[DATE==extr_date, ..varlist]
  dfi <- as.data.frame(DTi)
  dfi$LAMBX <- dfi$LAMBX * 100
  dfi$LAMBY <- dfi$LAMBY * 100
  rasterFromXYZ(as.data.frame(dfi), crs=CRS("+init=EPSG:27572"), digits=0)
}

# convertir safran (data.frame) en raster
safran2rast <- function(DT, varname, d_date, f_date, etendue_l2e=NULL) {
  if (!is.null(etendue_l2e)) {
    # subset etendue Lambert2 en hm
    ethm <- round(etendue_l2e/100, 0)
    DT <- DT[LAMBX>ethm[1] & LAMBX<ethm[2] & LAMBY>ethm[3] & LAMBY<ethm[4]]
  }
  seq_dates <- strftime(seq(d_date, f_date, by="days"), "%Y%m%d")
  list_raster <- lapply(seq_dates, DT2rast, DT, varname)
  stack(list_raster)
}

# genere grille points à partie de DT(data.table) pour la date spécifiée
DT2sp <- function(extr_date, DT) {
  DTi <- DT[DATE==extr_date]
  pts <- as.data.frame(DTi)
  pts$coordx <- pts$LAMBX * 100
  pts$coordy <- pts$LAMBY * 100
  coordinates(pts) <- c("coordx", "coordy")
  proj4string(pts) <- CRS("+init=EPSG:27572")
}

# ---- exemple : param ----

setwd("D:/MeteoFrance/Safran-Isba données quotidiennes")
f_safran <- "SIM2_2018_201903.csv"

DT_safran <- fread(f_safran, header = TRUE)

# etendue de l'Occitanie en Lambert 2 etendu ?
shp_reg <- readOGR(dsn="regions-20180101-shp", 
                   layer="regions-20180101", encoding="UTF-8")
shp_occ <- shp_reg[shp_reg$nom=="Occitanie",]
shp_occ_l2 <- spTransform(shp_occ, CRS("+init=EPSG:27572"))
# etendue format xmin, xmax, ymin, ymax
etendue <- bbox(shp_occ_l2)[c(1,3,2,4)]
# dates debut et fin
jour1 <- as.Date("2018-07-01")
jourN <- as.Date("2018-12-31")


# ---- exemple : process stack (multitemporel, 1 variable) ----

stack_tm <- safran2rast(DT_safran, varname="T_Q", d_date=jour1, f_date=jourN, 
                         etendue_l2e=etendue)
stack_tn <- safran2rast(DT_safran, varname="TINF_H_Q", d_date=jour1, f_date=jourN, 
                         etendue_l2e=etendue)
stack_tx <- safran2rast(DT_safran, varname="TSUP_H_Q", d_date=jour1, f_date=jourN, 
                         etendue_l2e=etendue)
stack_rr <- safran2rast(DT_safran, varname="PRELIQ_Q", d_date=jour1, f_date=jourN, 
                         etendue_l2e=etendue)

writeRaster(stack_tm, "output/tmoy_occitanie_2018.tif")
writeRaster(stack_tn, "output/tmin_occitanie_2018.tif")
writeRaster(stack_tx, "output/tmax_occitanie_2018.tif")
writeRaster(stack_rr, "output/rr_occitanie_2018.tif")

# ... extraire temp + prec montpellier
cefe_l2e <- matrix(c(723548, 1849625), ncol=2)
cefe_stats <- data.frame(
  dates_ = strftime(seq(jour1, jourN, by="days"), "%Y%m%d"),
  tm = extract(stack_tm, cefe_l2e),
  tn = extract(stack_tn, cefe_l2e),
  tx = extract(stack_tx, cefe_l2e),
  rr = extract(stack_rr, cefe_l2e)
)
write.csv2(cefe_stats, "output/cefe_stats.csv", row.names = F)


# ---- exemple : process raster (1 jour, 1 variable) ----

raster_tm_0703 <- DT2rast("20180703", DT_safran, "T_Q")
raster_tn_0703 <- DT2rast("20180703", DT_safran, "TINF_H_Q")
raster_tx_0703 <- DT2rast("20180703", DT_safran, "TSUP_H_Q")
raster_rr_0703 <- DT2rast("20180703", DT_safran, "PRELIQ_Q")

writeRaster(raster_tn_0703, "output/tmin_france_20180703.tif")
writeRaster(raster_tx_0703, "output/tmax_france_20180703.tif")
writeRaster(raster_rr_0703, "output/rr_france_20180703.tif")


# ---- exemple : process grille points (1 jour, N variables) ----

pts_20180201 <- DT2sp("20180201", DT_safran)
writeOGR(pts_20180201, "output", "data_20180201", "ESRI Shapefile",overwrite_layer=T)

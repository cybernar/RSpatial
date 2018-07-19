# snippets R raster
library(raster)

# creer raster vide type entier 60 x 80 km, res 500 m
rvide.crs <- CRS("+init=EPSG:2154")
rvide.ext <- extent(c(688500,748500,6292500,6372500))
rvide.val <- rep(0, 120*160)
rvide <- raster(crs=rvide.crs, ext=rvide.ext, resolution=500, vals=rvide.val)
writeRaster(rvide, filename="D:/GIS_DATA/HomeRange/RSpatial/reference_grid2.asc", format="ascii", datatype='INT2S', NAflag=-9999)

# lire raster
r34 <- raster("D:/GIS_DATA/IGN/IGN_BDALTI25/DEPT34.asc")
projection(r34) <- "+init=epsg:2154"

# plot raster
plot(r34)

# calcul et plot contour (isolignes)
# remarque : pas de courbe de niveau 0 car pas d'altitude <= 0 ds les données ...
isolignes <- c(0,200,400,600,800,1000,1200,1400)
contour(r34,levels=isolignes,add=TRUE)

# calculer et enregistrer les courbes ds un shape de lignes
isoSLDF <- rasterToContour(r34,levels=isolignes)
library(maptools)
writeLinesShape(isoSLDF,"D:/GIS_DATA/IGN/IGN_BDALTI25/iso34_200m")

# fusionner 2 rasters (mosaic)
r30 <- raster("DEPT30.asc")
r <- merge(r30,r34)

# enregistrer le fichier de sortie en ascii
writeRaster(r, "DEPT30_34.asc", "ascii", NAflag=-9999)

# associer le CRS EPSG:2154 (Lambert 93) au raster
projection(r) <- "+init=epsg:2154"
# ou bien la def proj4 (cf http://spatialreference.org/ref/epsg/2154/proj4/)
projection(r) <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#enregistrer le fichier de sortie en GeoTIFF entier 16 bits signe (necessite rgdal)
writeRaster(r, "DEPT30_34.tif", format="GTiff", datatype="INT2S", overwrite=TRUE, NAflag=-9999)

# charger le raster r en memoire
r <- readAll(r)

#extraire une zone de 20 x 20 km dans le secteur (xll = 680 000 m, yll = 6 280 000 m)
e = extent(680000,700000,6280000,6300000)
rcrop = crop(r,e)

#aligner une etendue sur le raster r
e = alignExtent(e, r)

# lire un shapefile avec maptools
library(maptools)
PCS_LAMB93 <- CRS("+init=EPSG:2154")
fShape <- "E:/DATA_SIG/IGN/BDTOPO/BDT_2-0_SHP_LAMB93_D034/H_ADMINISTRATIF/COMMUNE.SHP"
commune34.spdf <- readShapePoly(fShape, proj4string=PCS_LAMB93)

# lire un shapefile avec rgdal
library(rgdal)
repertoireCouchesSIG <- "E:/DATA_SIG/IGN/BDTOPO/BDT_2-0_SHP_LAMB93_D030/H_ADMINISTRATIF"
nomCouche <- "COMMUNE"
commune30.spdf <- readOGR(dsn=repertoireCouchesSIG, layer=nomCouche, p4s="+init=EPSG:2154")

# requete pour trouver les communes du canton de SAINT-MARTIN-DE-LONDRES
vFiltre <- (commune34.spdf$CANTON=="SAINT-MARTIN-DE-LONDRES")
communeSMDL.spdf <- commune34.spdf[vFiltre,]

#extraire l'etendue correspondant aux communes de SMDL 
communeSMDL.extent <- extent(bbox(communeSMDL.spdf))
communeSMDL.extent <- alignExtent(communeSMDL.extent, r34)

communeSMDL.raster <- crop(r34, communeSMDL.extent)
plot(communeSMDL.raster)
plot(communeSMDL.spdf,add=TRUE)

#extraire l'altitude du point x=745322, y=6296728 (roc de la Vigne) et du point pic st loup
pts <- matrix(c(745322, 765314, 6296728, 6298095),ncol=2,dimnames=list(c("roc de la Vigne", "pic st-loup"),c("x","y")))
altitudes <- extract(r34,pts)

# extraire l'altitude, methode 2
altitude1 <- r34[cellFromXY(r34, c(745322, 6296728))]

# calculer l'altitude moyenne par commune
commune34.raster <- rasterize(commune34.spdf, r34)
commune34.avgAlt <- zonal(r34, commune34.raster, 'mean')
spCbind(commune34.spdf, commune34.avgAlt[,"mean"])

# plotter commune
library(RColorBrewer)
library(classInt)   
plotvar <- commune34.avgAlt
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

plot(commune34.spdf, col=colcode)
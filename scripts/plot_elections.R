#
# plot election
#

# creer dossier data
dir.create("data")
setwd("data")

# téléchargement 
url1 <- "http://data.montpellier3m.fr/sites/default/files/ressources/VilleMTP_MTP_BureauxVote.geojson"
url2 <- "http://data.montpellier3m.fr/sites/default/files/ressources/VilleMTP_MTP_Elections.csv"
f_bv <- "VilleMTP_MTP_BureauxVote.geojson"
f_elections <- "VilleMTP_MTP_Elections.csv"

#url1 <- "https://data.montpellier3m.fr/sites/default/files/ressources/MMM_MMM_Limites.zip"
#url2 <- "https://data.montpellier3m.fr/sites/default/files/ressources/MMM_MMM_ArretsBusUrb.zip"
#url3 <- "https://data.montpellier3m.fr/sites/default/files/ressources/MMM_MMM_ArretsTram.zip"

download.file(url1, f_bv)
download.file(url2, f_elections)

#
library(geojsonio)
library(sp)
# ouverture donnees

resultats <- read.csv2(f_elections,fileEncoding = "LATIN1")
colnames(resultats)
pres2017_1 <- "Election présidentielle - Scrutin du 23 Avril 2017, premier tour"

#resultats_pres2017_1 <- subset(resultats, resultats$Election==pres2017_1)
resultats_pres2017_1 <- subset(resultats, resultats$Election==pres2017_1, select=c("Election","N..Bureau","Bureau","Inscrits","Votants"))

gj <- geojson_read(f_bv, method="local", what="sp")



# plot
library(sp)
library(spplot)

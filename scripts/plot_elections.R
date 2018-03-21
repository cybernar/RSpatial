# plot election

# setwd("D:/GitRepo/RSpatial/scripts")

# creer dossier data
dir.create("data")
setwd("data")

# téléchargement 
url1 <- "http://data.montpellier3m.fr/sites/default/files/ressources/VilleMTP_MTP_BureauxVote.geojson"
url2 <- "http://data.montpellier3m.fr/sites/default/files/ressources/VilleMTP_MTP_Elections.csv"
file1 <- "VilleMTP_MTP_BureauxVote.geojson"
file2 <- "VilleMTP_MTP_Elections.csv"

#url1 <- "https://data.montpellier3m.fr/sites/default/files/ressources/MMM_MMM_Limites.zip"
#url2 <- "https://data.montpellier3m.fr/sites/default/files/ressources/MMM_MMM_ArretsBusUrb.zip"
#url3 <- "https://data.montpellier3m.fr/sites/default/files/ressources/MMM_MMM_ArretsTram.zip"

download.file(url1, file1)
download.file(url2, file2)

# plot
library(sp)
library(spplot)

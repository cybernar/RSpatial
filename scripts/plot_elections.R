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



# packages
library(geojsonio)
library(sp)
library(rgdal)
library(dplyr)

# -----------------
# ouverture donnees
# -----------------

# ouverture donnees : resultats elections (encodage CP1252)
resultats <- read.table(f_elections,fileEncoding = "CP1252")

# ouverture donnees : bureaux de vote, decoupage geographique
# avec geojsonio
gj <- geojson_read(f_bv, method="local", what="sp")
# ou avec rgdal
gj <- readOGR(dsn=f_bv, layer="VilleMTP_MTP_BureauxVote")
head(gj@data)

# colonnes du dataframe resultats
colnames(resultats)

# liste des elections
elections <- resultats %>% select("N..Election","Election") %>% distinct()
tail(elections)

# liste des bureaux de votes
bureaux <- resultats %>% select("N..Bureau","Bureau") %>% distinct()
head(bureaux)


# --------------------
# carto elections 2017
# --------------------

# filtrer Election présidentielle 2017-Avril-23, 1er tour (N..Election==282)
resultats_pres2017_t1 <- resultats %>% filter(N..Election==282)

# nb inscrits / nb votants / abstention par bureaux 
abstention_pres2017_t1 <- resultats_pres2017_t1 %>% 
  select("N..Election","N..Bureau","Inscrits","Votants") %>%
  distinct() %>% mutate(Abstention=100*(1-(Votants/Inscrits)))
#resultats_pres2017_t1 <- subset(resultats, resultats$Election==pres2017_1, select=c("Election","N..Bureau","Bureau","Inscrits","Votants"))

# jointure

# plot

# plot en Lambert 93
df_EPSG <- make_EPSG()
def_prj4_l93 <- subset(df_EPSG, df_EPSG$code==2154)[["prj4"]]
gj_l93 <- spTransform(gj, CRS(def_prj4_l93))

gj_l93@proj4string
gj_l93@data

plot(gj_l93,axes=T)

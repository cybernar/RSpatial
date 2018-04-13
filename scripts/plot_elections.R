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
library(tidyr)
library(viridis)

# -----------------
# ouverture donnees
# -----------------

# ouverture donnees : resultats elections (encodage CP1252)
resultats <- read.csv2(f_elections,fileEncoding = "Windows-1252")
# nettoyage > enlever espaces superflus
resultats <- mutate(resultats,
                    Election=trimws(Election),
                    Bureau=trimws(Bureau),
                    Candidat=trimws(Candidat))

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
resultats_pres2017_t1 <- resultats %>% filter(N..Election==282) %>% mutate(ID.Candidat=paste0("C",N..Candidat),
                                                                           P.Voix=100*Nombre.de.Voix/Exprimés)

# nb inscrits / nb votants / abstention par bureaux 
abstention_pres2017_t1 <- resultats_pres2017_t1 %>% 
  select("N..Election","N..Bureau","Inscrits","Votants") %>%
  distinct() %>% mutate(Abstention=100*(1-(Votants/Inscrits)))
#resultats_pres2017_t1 <- subset(resultats, resultats$Election==pres2017_1, select=c("Election","N..Bureau","Bureau","Inscrits","Votants"))

# liste des candidats
candidats <- resultats_pres2017_t1 %>% select("ID.Candidat","Candidat") %>% distinct()
candidats

# générer tableau row=NBureau,col=IDCandidat,val=%Voix
voix_pres2017_t1 <- resultats_pres2017_t1 %>% 
  select("N..Bureau","ID.Candidat","P.Voix") %>%
  spread(key="ID.Candidat", value="P.Voix")
voix_pres2017_t1 <- inner_join(voix_pres2017_t1,
                               abstention_pres2017_t1, 
                               by="N..Bureau")

# jointure spatial X data 
gj@data <- left_join(gj@data, voix_pres2017_t1, by=c("Bureau"="N..Bureau"))
pal <- magma(7,begin=0.2,direction=-1)
spplot(gj, 
       zcol=c("C11","C2","C3","C9"), 
       names.attr=c("F. Fillon","M. Le Pen","E. Macron","J-L. Mélenchon"), 
       main="Election présidentielle 2017, 1er tour",
       col.regions = pal, at = c(0,5,10,15,20,30,40,55), log="y", pretty=T)


spplot(gj, 
       zcol="Abstention", 
       main="Election présidentielle 2017, 1er tour",
       sub="Abstention",
       col.regions = pal, at = c(10,15,20,25,30,35,40,45), log="y", pretty=T)

# plot

# plot en Lambert 93
df_EPSG <- make_EPSG()
def_prj4_l93 <- subset(df_EPSG, df_EPSG$code==2154)[["prj4"]]
gj_l93 <- spTransform(gj, CRS(def_prj4_l93))

gj_l93@proj4string
gj_l93@data

plot(gj_l93,axes=T)

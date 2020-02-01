# Librairies
library(openxlsx)
library(rgdal)
library(dplyr)

# Choix du répertoire de travail
rep <- "/Users/Valentin/Foret/Travail/Pichery"
setwd(rep)

# ----- Import du classeur acheteurs :
# file <- file.choose()
file <- "/Users/Valentin/Foret/Travail/Pichery/Data/Excel/Coord_achet_2016_VD.xlsx"

df <- read.xlsx(file) %>% 
  mutate(Ville=gsub("Â", "A", Ville), # Ville=gsub("ST ", "SAINT ", Ville, fixed=T),
         Ville=gsub("S/", "SUR", Ville),
         Ville=gsub("'", " ", Ville),
         Ville=gsub("-", " ", Ville))

# ----- Import du classeur correspondance Code postal/Code INSEE :
# Codes <- file.choose()
Codes <- "/Users/Valentin/Foret/Travail/Pichery/Data/correspondance-code-insee-code-postal.xlsx"
df_Codes <- read.xlsx(Codes)
df_Codes2 <- select(df_Codes,
                    Code_INSEE, Code_Postal, Commune) %>% 
  mutate(Commune=gsub("-", " ", Commune),
         Commune=gsub("'", " ", Commune))

# ----- Fusion table Acheteur et Codes
t <- left_join(df, df_Codes2, by=c("Code_postal2"="Code_Postal", "Ville"="Commune")) %>% 
  distinct(t) # Attention vérifier doublon pour St Pierre d'Entremont. 1 code postal donne 2 INSEE

# ----- Import du fichier communes françaises :
# file_SHP <- file.choose()
# file_SHP <- "/Users/Valentin/Foret/Travail/Pichery/Data/communes-20160119-shp/communes-20160119.shp"
# Communes_SHP <- readOGR(file_SHP, "communes-20160119")
file_SHP <- "/Users/Valentin/Foret/Travail/Pichery/Data/SIG/communes-plus-20140630-100m-shp/communes-plus-20140630-100m.shp"
Communes_SHP <- readOGR(file_SHP, "communes-plus-20140630-100m")
# shape récupéré à l'adresse : https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/
Communes <- select(Communes_SHP@data,
                   insee, nom, lat_centro, lon_centro)

# ----- Fusion de la table Acheteurs et du shape des communes
# temp <- select(Communes_SHP@data, INSEE_COM, NOM_COM, X_CENTROID, Y_CENTROID) %>% 
#   mutate(INSEE_COM=as.character(INSEE_COM))
# temp$NOM_COM <- gsub("-", " ", temp$NOM_COM)

# t2 <- left_join(t, temp, by=c("Code_INSEE"="INSEE_COM"))
t2 <- merge(t, Communes, by.x="Code_INSEE", by.y="insee", all.x=T)
write.xlsx(t2, "Out/Localisation_Acheteurs.xlsx")

# ----- Lecture classeur contenant les noms et centroides corrigés :
file_FIN <- file.choose()
file_FIN <- "/Users/Valentin/Foret/Travail/Pichery/Out/Localisation_Acheteurs_Complet.xlsx"
df_FIN <- read.xlsx(file_FIN)

# ----- Conversion en shape de points par communes
# shape <- select(t2,
#               one_of(names(df), "X_CENTROID", "Y_CENTROID")) %>% 
#   rename_("X"="X_CENTROID",
#           "Y"="Y_CENTROID") %>% 
#   filter(!is.na(X) & !is.na(Y))
pos <- which(is.na(df_FIN$lon_centro))
shape <- rename(df_FIN,
                X=lon_centro,  # ATTENTION VÉRIFIER QUE LES X ET Y N'ONT PAS ÉTÉ INVERSÉS !!!!!!!!!!!!!!!
                Y=lat_centro)
shape$X <- as.numeric(shape$X)
shape$Y <- as.numeric(shape$Y)
coordinates(shape) <- ~ X + Y
proj4string(shape) <- CRS("+init=epsg:4326")
shape_L93 <- spTransform(shape, CRS("+init=epsg:2154"))
#   writeOGR(shape, "SIG/Vecteurs/shape", "shape",
#          driver="ESRI Shapefile", overwrite_layer=T)
dir.create("SIG", showWarnings = F)
writeOGR(shape_L93, dsn=paste0(rep,"/Out/SIG"), layer="AcheteursComplet_L93",
         driver="ESRI Shapefile", overwrite_layer=T)

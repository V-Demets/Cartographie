# ----- Chargement des librairies
library(easypackages)
packages("rgdal","sp","rgeos","dplyr","tcltk","openxlsx",
         "doBy","tools","argosfilter","raster")
# library(rgdal)
# library(sp)
# library(rgeos)
# library(dplyr)
# library(tcltk)
# library(openxlsx)
# library(doBy)
# library(tools)
# # library(geosphere)
# library(argosfilter)
# library(raster)



# ----- Choix du répertoire de travail
# repPP <- tk_choose.dir(default=getwd(),
#                        caption="Choix du répertoire de travail")
repPP <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein"
setwd(repPP)
# Création du répertoire de sortie :
dir.create("Out",showWarnings=F)
dir.create("Out/SIG",showWarnings=F)
dir.create("Out/SIG/Vecteurs",showWarnings=F)
dir.create("Out/SIG/Vecteurs/Placettes",showWarnings=F)
dir.create("Out/SIG/Vecteurs/Perimetres",showWarnings=F)



# ----- Chargement du shape des périmètres des dispositifs
# file_France <- tk_choose.files(caption="Choix du shape des régions françaises",
#                                multi=F, filters=matrix(c("shape","shape",".shp","shp"), ncol=2))
file_France <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Data/SIG/Vecteurs/Perimetres/nouvelles_regions.shp"
shp_France <- readOGR(file_France,
                      basename(file_path_sans_ext(file_France)),
                      verbose=F)

# file_Perim <- tk_choose.files(caption="Choix du shape des périmètres des dispositifs",
#                               multi=F, filters=matrix(c("shape","shape",".shp","shp"), ncol=2))
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Data/SIG/Vecteurs/Perimetres/Perim_Fleckenstein.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Data/SIG/Vecteurs/Perimetres/FLECK_ONF_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Data/SIG/Vecteurs/Perimetres/Perim_PSDRF1_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_Fleckenstein_GF_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Mask_Perim_GF.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_Fleckenstein_GF2_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_Fleckenstein_GF2Bis_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_GF3_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Data/SIG/Vecteurs/Perimetres/Perim_PSDRF2_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_PSDRF3_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_PSDRF4_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Mask_Perim_GF5.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_GF4_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_PSDRF5_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_GF5_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_GF7_L93.shp"
file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_GF_Fin_L93.shp"
file_Perim <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_PSDRF_Fin_L93.shp"
shp_Perim <- readOGR(file_Perim,
                     basename(file_path_sans_ext(file_Perim)),
                     verbose=F)
# -----

# ----- Suppression des polygones trop petits : -----
# plot(shp_Perim,col="red")
# SP <- shp_Perim
# areas <- sapply(SP@polygons[[1]]@Polygons, function(x) x@area)
# 
# Poly <- list()
# # Plyr <- c()
# for(i in 1:length(areas)){
#   if(!areas[[i]] < 0.01) Poly <- c(Poly, SP@polygons[[1]]@Polygons[[i]])
# }
# SP1 <- SpatialPolygons(Srl = list(Polygons(srl = Poly, ID="Global")))
# tab <- data.frame(Nom="Perim")
# row.names(tab) <- "Global"
# SPDF <- SpatialPolygonsDataFrame(Sr = SP1, data = tab)
# 
# shp_Perim <- SPDF
# shp_Perim@proj4string <- CRS("+init=epsg:2154")
# shp_Perim@data <- mutate(shp_Perim@data,
#                          Surface = gArea(shp_Perim, byid=T)/10000)
# # writeOGR(shp_Perim,
# #          paste0(getwd(),"/Out/SIG/Vecteurs/Perimetres"),
# #          layer="Perim_GF3_L93",
# #          driver="ESRI Shapefile",
# #          overwrite_layer=F)
# writeOGR(shp_Perim,
#          paste0(getwd(),"/Out/SIG/Vecteurs/Perimetres"),
#          layer="Perim_GF5_L93",
#          driver="ESRI Shapefile",
#          overwrite_layer=F)
# Couches obtenues :
# 1. en sélectionnant les zones à ne inventorier avec le protocole GF -> Mask_Perim_GF
# 2. en soustrayant la couche Mask_Perim_GF à la FLECK_ONF_L93 -> Perim_Fleckenstein_GF2_L93
# Problème : il reste des coquilles (erreurs de topo, non indiquées comme "hole" dans R)
# 3. en fusionnant les polygones de Mask_Perim_GF -> Mask2_Perim_GF
# 4. en soustrayant la couche Mask2_Perim_GF à la FLECK_ONF_L93 -> Perim_Fleckenstein_GF2Bis_L93
# A ce stade il ne reste que des micros polygones -> supprimés avec R (seuil = surface doit être > 0.01m2) -> Perim_GF3_L93
# ----- Suppression des trous dans les polygones : -----
# shp_Perim0 <- gUnaryUnion(shp_Perim)
# plot(shp_Perim0,col="red")
# SP <- shp_Perim0
# trous <- sapply(SP@polygons[[1]]@Polygons, function(x) x@hole)
# 
# Poly <- list()
# for(i in 1:length(trous)){
#   if(!trous[[i]]) Poly <- c(Poly, SP@polygons[[1]]@Polygons[[i]])
# }
# SP1 <- SpatialPolygons(Srl = list(Polygons(srl = Poly, ID="Global")))
# tab <- data.frame(Nom="Perim")
# row.names(tab) <- "Global"
# SPDF <- SpatialPolygonsDataFrame(Sr = SP1, data = tab)
# 
# shp_Perim <- SPDF
# shp_Perim@proj4string <- CRS("+init=epsg:2154")
# shp_Perim@data <- mutate(shp_Perim@data,
#                     Surface = gArea(shp_Perim, byid=T)/10000)
# 
# # writeOGR(shp_Perim,
# #          paste0(getwd(),"/Out/SIG/Vecteurs/Perimetres"),
# #          layer="Perim_Fleckenstein_L93",
# #          driver="ESRI Shapefile",
# #          overwrite_layer=F) # Réécriture
# # writeOGR(shp_Perim,
# #          paste0(getwd(),"/Out/SIG/Vecteurs/Perimetres"),
# #          layer="Mask2_Perim_GF",
# #          driver="ESRI Shapefile",
# #          overwrite_layer=F)
# # writeOGR(shp_Perim,
# #          paste0(getwd(),"/Out/SIG/Vecteurs/Perimetres"),
# #          layer="Perim_PSDRF3_L93",
# #          driver="ESRI Shapefile",
# #          overwrite_layer=F) # Réécriture
# # writeOGR(shp_Perim,
# #          paste0(getwd(),"/Out/SIG/Vecteurs/Perimetres"),
# #          layer="Perim_PSDRF5_L93",
# #          driver="ESRI Shapefile",
# #          overwrite_layer=F) # Réécriture
# writeOGR(shp_Perim,
#          paste0(getwd(),"/Out/SIG/Vecteurs/Perimetres"),
#          layer="Mask2_GF5_L93",
#          driver="ESRI Shapefile",
#          overwrite_layer=F) # Réécriture
# ----- %%%%%%%%%% -----


# ----- %%%%% Prévisualisation %%%%% -----

plot(shp_Perim,col="red")
plot(shp_France, add=T)

# Distance minimale au périmètre à laquelle placer les placettes
Dist=25



# ---------- Initiation de la boucle :
i=1
print(paste0("i = ",i))

# Création de la colonne Disp
shp_Perim@data$NumForet <- NA
shp_Perim@data$NumForet[i] <- as.character(i)

# Création de la colonne Dist_Plac
shp_Perim@data$Dist_Plac <- NA

# Création de la colonne Az_N
shp_Perim@data$Az_N <- NA

# Création de la colonne Az_E
shp_Perim@data$Az_E <- NA

shp_temp <- shp_Perim[i,]
plot(shp_temp)
df_temp <- shp_temp@data


# -- Définition du nombre de placettes à insérer en fonction de la surface 
# (si surface > 6 ha alors passer de 10 à 12 plac)
# nb_Plac <- ifelse(df_temp$SURFACE > 6,
#                   12,10)
# nb_Plac <- 131
# nb_Plac <- 20 # PSDRF
nb_Plac <- 28 # PSDRF
# nb_Plac <- 111 # GF


# -- Création du grid de placettes
shp_Plac <- spsample(shp_temp, n=nb_Plac, "regular")
mat_Dist <- gDistance(shp_Plac, as(shp_temp, "SpatialLines"), byid = TRUE)
min_Dist <- min(mat_Dist)
# Contrôle qu'on ait bien le bon nombre de placettes + respect de la Dist minimale
if (dim(shp_Plac@coords)[1] != nb_Plac | min_Dist < Dist) {
  count=0
  while(dim(shp_Plac@coords)[1] != nb_Plac | min_Dist < Dist) {
    shp_Plac <- spsample(shp_temp, n=nb_Plac, "regular")
    mat_Dist <- gDistance(shp_Plac, as(shp_temp, "SpatialLines"), byid = TRUE)
    min_Dist <- min(mat_Dist)
    count=count+1
    # print(paste0("Essai ",count))
    if (count >= 100) {
      # Answer <- tk_messageBox(type="ok",
      #                         message="100 essais réalisés. Arrêter ?")
      # stop("Essais infructueux")
      # Answer <- tk_messageBox(type="ok",
      #                         message="100 essais réalisés. Diminution de la distance minimale de 1m")
      Dist=Dist-1
      print(paste0("Distance : ",Dist))
      count=0
    }
  }
}

# rangement des coordonnées par ordre croissant
shp_Plac <- shp_Plac[order(shp_Plac@coords[,1],shp_Plac@coords[,2]),]
shp_Plac <- SpatialPointsDataFrame(shp_Plac,
                            data.frame(NumDisp=as.character(round(rep(i,dim(shp_Plac@coords)[1]),0)),
                                       NumPlac=as.character(1:dim(shp_Plac@coords)[1]),
                                       DistanceMin=as.character(rep(Dist,dim(shp_Plac@coords)[1])),
                                       stringsAsFactors=F),
                            match.ID=T)
# ----- Calcul de la distance entre les points
dist <- c()
for (j in 2:nb_Plac) {
  dist <- c(dist,round(gDistance(shp_Plac[j-1,],shp_Plac[j,]),1))
}

df_FreqDist <- table(dist)
dist <- names(df_FreqDist)[df_FreqDist==max(df_FreqDist)] # on prend la distance inter-point 
# la plus fréquente dans la maille
dist <- unique(dist)

if (length(dist) > 1) {
  stop("Erreur : distance entre les points est multiple")
}
# Enregistrement de la distance
# shp_Plac$Dist_Plac <- dist
shp_Perim@data$Dist_Plac[i] <- as.character(dist)

# ----- Calcul de l'azimut entre les points (N et E).
# conversion en WGS84 pour pouvoir utiliser la fonction bearing
shp_Plac <- spTransform(shp_Plac, CRS("+proj=longlat +datum=WGS84"))

# Calcul des différents azimut pour repérer celui du nord et celui du sud
# bearing <- c()
# for (j in 2:nb_Plac) {
#   bearing <- c(bearing,round(bearing(shp_Plac[j-1,],shp_Plac[j,])*100/90,1))
# }
# bearing_test <- bearingTrack(coordinates(shp_Plac)[,2], # alternative pour avoir N=0 ?
#                              coordinates(shp_Plac)[,1])


# bearing <- unique(bearing)
# bearing <- bearing[which(bearing > 0)]

# Enregistrement des valeurs
# shp_Perim$Az_N <- min(bearing)
# shp_Plac$Az_E <- shp_Plac$Az_N+100
# shp_Perim@data$Az_N[i] <- as.character(min(bearing))
# shp_Perim@data$Az_E[i] <- as.character(min(bearing) + 100)

# ----- Réécriture du shape
# changement du CRS :
shp_Plac <- spTransform(shp_Plac, CRS("+init=epsg:2154"))


# writeOGR(shp_Plac,
#          paste0(getwd(),"/Out/SIG/Vecteurs/Placettes"),
#          layer="Plac_Fleckenstein_L93",
#          driver="ESRI Shapefile",
#          overwrite_layer=T)
# writeOGR(shp_Plac,
#          paste0(getwd(),"/Out/SIG/Vecteurs/Placettes"),
#          layer="Plac_PSDRF1_L93",
#          driver="ESRI Shapefile",
#          overwrite_layer=T)
# writeOGR(shp_Plac,
#          paste0(getwd(),"/Out/SIG/Vecteurs/Placettes"),
#          layer="Plac_GF3_L93",
#          driver="ESRI Shapefile",
#          overwrite_layer=T)
# writeOGR(shp_Plac,
#          paste0(getwd(),"/Out/SIG/Vecteurs/Placettes"),
#          layer="Plac_PSDRF5_L93",
#          driver="ESRI Shapefile",
#          overwrite_layer=T)
# writeOGR(shp_Plac,
#          paste0(getwd(),"/Out/SIG/Vecteurs/Placettes"),
#          layer="Plac_GF5_L93",
#          driver="ESRI Shapefile",
#          overwrite_layer=T)
# writeOGR(shp_Plac,
#          paste0(getwd(),"/Out/SIG/Vecteurs/Placettes"),
#          layer="Plac_GF7_L93",
#          driver="ESRI Shapefile",
#          overwrite_layer=T)
writeOGR(shp_Plac,
         paste0(getwd(),"/Out/SIG/Vecteurs/Placettes"),
         layer="Plac_PSDRF_Fin2_L93",
         driver="ESRI Shapefile",
         overwrite_layer=T)
# writeOGR(shp_Plac,
#          paste0(getwd(),"/Out/SIG/Vecteurs/Placettes"),
#          layer="Plac_GF_Fin_L93",
#          driver="ESRI Shapefile",
#          overwrite_layer=T)

# ----- %%%%%%%%%% -----


# ----- %%%%% Traitement des points GPS %%%%% -----
repPP <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein"
setwd(repPP)

# -- Campagne 1 -- #
ListFiles1 <- tk_choose.files(caption="Choix des fichiers GPS - Campagne1")
# ListFiles2 <- tk_choose.files(caption="Choix des fichiers GPS - Campagne2")
# ListFiles <- c(ListFiles1,ListFiles2)
ListFiles <- ListFiles1

Gps_SHP <- readOGR(ListFiles[1],
                   layer="waypoints",
                   verbose=F)
for (i in 2:length(ListFiles)) {
  Gps_FILE <- ListFiles[i]
  temp_SHP <- readOGR(Gps_FILE,
                      layer="waypoints",
                      verbose=F)
  
  Gps_SHP <- rbind(temp_SHP,
                   Gps_SHP)
}
Gps_SHP <- spTransform(Gps_SHP,
                       CRSobj = CRS("+init=epsg:2154"))

dir.create("Out/SIG/GPS",showWarnings=F)
dir.create("Out/SIG/GPS/Campagne1",showWarnings=F)

writeOGR(obj=Gps_SHP,
         dsn="Out/SIG/GPS/Campagne1/",
         layer="Plac_GPS1_L93",
         driver="ESRI Shapefile",
         overwrite_layer=T)


# -- Campagne 2 -- #
ListFiles2 <- tk_choose.files(caption="Choix des fichiers GPS - Campagne2")
# ListFiles <- c(ListFiles1,ListFiles2)
ListFiles <- ListFiles2

file_GPX <- readOGR(ListFiles[1],
                    layer="waypoints",
                    verbose=F)
Gps_SHP <- readOGR(ListFiles[1],
                   layer="waypoints",
                   verbose=F)
for (i in 2:length(ListFiles)) {
  Gps_FILE <- ListFiles[i]
  temp_SHP <- readOGR(Gps_FILE,
                      layer="waypoints",
                      verbose=F)
  
  Gps_SHP <- rbind(temp_SHP,
                   Gps_SHP)
}
Gps_SHP <- spTransform(Gps_SHP,
                       CRSobj = CRS("+init=epsg:2154"))

dir.create("Out/SIG/GPS/Campagne2",showWarnings=F)
writeOGR(obj=Gps_SHP,
         dsn="Out/SIG/GPS/Campagne2/",
         layer="Plac_GPS2_L93",
         driver="ESRI Shapefile",
         overwrite_layer=T)

# -- Toutes les campagnes -- #
ListFiles <- c(ListFiles1,ListFiles2)

Gps_SHP <- readOGR(ListFiles[1],
                   layer="waypoints",
                   verbose=F)
Gps_SHP$wptx1_WaypointExtension <- NA
for (i in 2:length(ListFiles)) {
  # print(i)
  Gps_FILE <- ListFiles[i]
  temp_SHP <- readOGR(Gps_FILE,
                      layer="waypoints",
                      verbose=F)
  
  Gps_SHP <- rbind(temp_SHP,
                   Gps_SHP)
}

# crop par rapport au périmètre d'inventaire
Perim_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Data/SIG/Vecteurs/Perimetres/Perim_Fleckenstein_L93.shp"
Perim_SHP <- readOGR(Perim_FILE,
                     basename(file_path_sans_ext(Perim_FILE)),
                     verbose=F)
Perim_SHP <- spTransform(Perim_SHP,
                         CRSobj = CRS("+init=epsg:2154"))
Gps_SHP <- spTransform(Gps_SHP,
                       CRSobj = CRS("+init=epsg:2154"))
plot(Perim_SHP)
plot(Perim_SHP)
plot(Gps_SHP,
     col="red",add=T)
# Gps_SHP <- crop(Gps_SHP,
#                  extent(Perim_SHP))
# Gps_SHP <- gIntersection(Gps_SHP,
#                          Perim_SHP)
plot(Gps_SHP,
     col="blue",add=T)

dir.create("Out/SIG/GPS",showWarnings=F)

writeOGR(obj=Gps_SHP,
         dsn="Out/SIG/GPS/",
         layer="Plac_GPS_L93",
         driver="ESRI Shapefile",
         overwrite_layer=T)

# -- Distinction PSDRF-GF -- #
# crop par rapport au périmètre d'inventaire du protocole GF
# Perim_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_PSDRF_Fin_L93.shp"
# Perim_SHP <- readOGR(Perim_FILE,
#                      basename(file_path_sans_ext(Perim_FILE)),
#                      verbose=F)
# Gps_SHP <- spTransform(Gps_SHP,
#                        CRSobj = CRS("+init=epsg:2154"))
# plot(Perim_SHP)
# plot(Gps_SHP,
#      col="red",add=T)
# Gps_SHP <- crop(Gps_SHP,
#                 extent(Perim_SHP))


# crop par rapport au périmètre d'inventaire du PSDRF
Perim_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_PSDRF_Fin_L93.shp"
Perim_SHP <- readOGR(Perim_FILE,
                     basename(file_path_sans_ext(Perim_FILE)),
                     verbose=F)
Perim_SHP <- spTransform(Perim_SHP,
                         CRSobj = CRS("+init=epsg:2154"))
Gps_SHP <- spTransform(Gps_SHP,
                       CRSobj = CRS("+init=epsg:2154"))
plot(Perim_SHP)
plot(Gps_SHP,
     col="red",add=T)
# Gps_SHP2 <- gIntersection(Gps_SHP,
#                          Perim_SHP,
#                          byid=T)
Intersect_DF <- gIntersects(Gps_SHP,
                            Perim_SHP,
                            byid=T)
GpsPSDRF_DF <- as.data.frame(which(Intersect_DF==TRUE, arr.ind=T))
names(GpsPSDRF_DF) <- c("Pos_Perim","Pos_Gps")
GpsPSDRF_DF <- arrange(GpsPSDRF_DF,
                       Pos_Gps)
row.names(GpsPSDRF_DF) <- 1:dim(GpsPSDRF_DF)[1]

Pos_Gps_PSDRF <- GpsPSDRF_DF$Pos_Gps # position des points PSDRF - à soustraire à Gps_SHP pour avoir position des points GF
GpsPSDRF_DF <- cbind(GpsPSDRF_DF,
                     Gps_SHP@data[GpsPSDRF_DF$Pos_Gps,],
                     Perim_SHP@data[GpsPSDRF_DF$Pos_Perim,],
                     Gps_SHP@coords[GpsPSDRF_DF$Pos_Gps,]) %>%
  dplyr::select(name,time,coords.x1,coords.x2) %>%
  rename_("Xgps"="coords.x1",
          "Ygps"="coords.x2") %>% 
  mutate(Xgps=as.numeric(Xgps),
         Ygps=as.numeric(Ygps))

GpsPSDRF_SHP <- GpsPSDRF_DF
coordinates(GpsPSDRF_SHP) <- ~ Xgps + Ygps
proj4string(GpsPSDRF_SHP) <- CRS("+init=epsg:2154")

plot(GpsPSDRF_SHP,
     col="blue",add=T)

dir.create("Out/SIG/GPS",showWarnings=F)

writeOGR(GpsPSDRF_SHP,
         dsn="Out/SIG/GPS/",
         layer="Plac_GPS-PSDRF_L93",
         driver="ESRI Shapefile",
         overwrite_layer=T,
         verbose=F,
         encoding="UTF-8")

# Récupération des points du protocole GF (autres par défaut)
GpsGF_SHP <- Gps_SHP[-Pos_Gps_PSDRF,c("name","time")]

# plot(GpsPSDRF_SHP,
#      col="blue",add=T)
# plot(GpsGF_SHP,
#      col="green",add=T)

writeOGR(GpsGF_SHP,
         dsn="Out/SIG/GPS/",
         layer="Plac_GPS-GF_L93",
         driver="ESRI Shapefile",
         overwrite_layer=T,
         verbose=F,
         encoding="UTF-8")

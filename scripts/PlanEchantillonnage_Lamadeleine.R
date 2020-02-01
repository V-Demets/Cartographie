########## 0/ Initialisation ##########
# -- packages
# library(easypackages)
# packages(
#   "sf", "dplyr", "tcltk", "openxlsx", 
#          "doBy", "tools", "argosfilter", "raster"
#   )
library(openxlsx)
library(sf)
library(stats) # TODO : à charger avant dplyr (fonction 'filter' et 'lag' masquées sinon)
library(dplyr)
library(tcltk)
library(nngeo) # fonction st_remove_holes

# -- choix du répertoire de travail
# repPP <- tk_choose.dir(
#   default = getwd(), 
#   caption = "Choix du répertoire de travail"
# )
repPP <- "/Users/Valentin/Travail/Outils/Cartographie/dossiers/Forestallia/Lamadeleine"
setwd(repPP)


# -- Chargement du shape des périmètres des dispositifs
# # file_France <- tk_choose.files(caption = "Choix du shape des régions françaises", 
# #                                multi = F, filters = matrix(c("shape", "shape", ".shp", "shp"), ncol = 2))
# file_France <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Data/SIG/vecteurs/perimetres/nouvelles_regions.shp"
# shp_France <- readOGR(file_France, 
#                       basename(file_path_sans_ext(file_France)), 
#                       verbose = F)

# perim_file <- tk_choose.files(
#   caption = "Choix du shape du périmètre du domaine d'étude",
#   multi = F,
#   filters = matrix(c("shape", "shape", ".shp", "shp"), ncol = 2)
# )
perim_file <- "/Users/Valentin/Travail/Outils/Cartographie/dossiers/Forestallia/Lamadeleine/data/SIG/vecteurs/perimetres/perim_foret_L93.shp"

# -- création du répertoire de sortie :
save_path <- "out/SIG/vecteurs/perimetres" # chemin
dir.create(
  save_path, showWarnings = F, recursive = T
)

# -- extraction du chemin relatif
perim_file <- sub(
  paste0(repPP, "/"), "", perim_file
)

# -- lecture du classeur excel
perim <- st_read(
  file.path(repPP, "/", perim_file),
  stringsAsFactors = FALSE, quiet = T
) %>% 
  st_transform(crs = 2154)
# plot(st_geometry(perim))

# -- Union des polygones et calculs des surfaces
# ---- solution 1
# perim_union2 <- 
#   perim %>% 
#   st_union() %>% 
#   st_remove_holes() %>% 
#   st_sf()

# ---- solution 2
perim_union <-
  perim %>%
  summarise(Surface = sum(Surface)) %>% 
  st_remove_holes()

# perim_union <- 
#   perim %>% 
#   st_union()
# x <- perim_union[[1]][2]
# plot(x)
# y <- st_multipolygon(x)
# plot(y)
# z <- y[[1]][1]
# w <- st_polygon(z)
# plot(w, add=T, col = "red")

# perim_union <- 
#   perim %>% 
#   summarise(Surface = sum(Surface))
# class(perim_union)

# plot(st_geometry(c[1]))
#   # Faire en sorte d'extraire les micropolygones (pour les visualiser sous QGIS ?)
#   st_buffer(dist = 0) %>% 
#   st_combine() %>% 
#   # st_as_sf()
#   st_union()
# # plot(st_geometry(perim_union))
#   plot(st_geometry(perim_union))

# -- réécriture du shape de périmètre :
st_write(
  perim_union, dsn = save_path, layer = "perim_foret_L93", 
  driver = "ESRI Shapefile", quiet = T,
  update = TRUE, delete_layer = TRUE
)

# -- nettoyage du parcellaire :
# parcellaire <- st_sym_difference(perim_union, perim) %>%
  parcellaire <- perim %>% 
  filter(Parcelles %in% c("12C", "12B")) %>% 
  st_remove_holes()
test <- parcellaire %>% 
  st_union()
parcellaire <- st_intersection(perim, perim_union) %>% 
  st_union()
plot(st_geometry(parcellaire))
plot(test)
st_is_valid(parcellaire) # que faire des erreurs topologiques dans le parcellaire ?

########## / \ ##########

# ----- Suppression des polygones trop petits : -----
# plot(shp_Perim, col = "red")
# SP <- shp_Perim
# areas <- sapply(SP@polygons[[1]]@Polygons, function(x) x@area)
# 
# Poly <- list()
# # Plyr <- c()
# for(i in 1:length(areas)){
#   if(!areas[[i]] < 0.01) Poly <- c(Poly, SP@polygons[[1]]@Polygons[[i]])
# }
# SP1 <- SpatialPolygons(Srl = list(Polygons(srl = Poly, ID = "Global")))
# tab <- data.frame(Nom = "Perim")
# row.names(tab) <- "Global"
# SPDF <- SpatialPolygonsDataFrame(Sr = SP1, data = tab)
# 
# shp_Perim <- SPDF
# shp_Perim@proj4string <- CRS("+init = epsg:2154")
# shp_Perim@data <- mutate(shp_Perim@data, 
#                          Surface = gArea(shp_Perim, byid = T)/10000)
# # writeOGR(shp_Perim, 
# #          paste0(getwd(), "/out/SIG/vecteurs/perimetres"), 
# #          layer = "Perim_GF3_L93", 
# #          driver = "ESRI Shapefile", 
# #          overwrite_layer = F)
# writeOGR(shp_Perim, 
#          paste0(getwd(), "/out/SIG/vecteurs/perimetres"), 
#          layer = "Perim_GF5_L93", 
#          driver = "ESRI Shapefile", 
#          overwrite_layer = F)
# Couches obtenues :
# 1. en sélectionnant les zones à ne inventorier avec le protocole GF -> Mask_Perim_GF
# 2. en soustrayant la couche Mask_Perim_GF à la FLECK_ONF_L93 -> Perim_Fleckenstein_GF2_L93
# Problème : il reste des coquilles (erreurs de topo, non indiquées comme "hole" dans R)
# 3. en fusionnant les polygones de Mask_Perim_GF -> Mask2_Perim_GF
# 4. en soustrayant la couche Mask2_Perim_GF à la FLECK_ONF_L93 -> Perim_Fleckenstein_GF2Bis_L93
# A ce stade il ne reste que des micros polygones -> supprimés avec R (seuil = surface doit être > 0.01m2) -> Perim_GF3_L93
# ----- Suppression des trous dans les polygones : -----
# shp_Perim0 <- gUnaryUnion(shp_Perim)
# plot(shp_Perim0, col = "red")
# SP <- shp_Perim0
# trous <- sapply(SP@polygons[[1]]@Polygons, function(x) x@hole)
# 
# Poly <- list()
# for(i in 1:length(trous)){
#   if(!trous[[i]]) Poly <- c(Poly, SP@polygons[[1]]@Polygons[[i]])
# }
# SP1 <- SpatialPolygons(Srl = list(Polygons(srl = Poly, ID = "Global")))
# tab <- data.frame(Nom = "Perim")
# row.names(tab) <- "Global"
# SPDF <- SpatialPolygonsDataFrame(Sr = SP1, data = tab)
# 
# shp_Perim <- SPDF
# shp_Perim@proj4string <- CRS("+init = epsg:2154")
# shp_Perim@data <- mutate(shp_Perim@data, 
#                     Surface = gArea(shp_Perim, byid = T)/10000)
# 
# # writeOGR(shp_Perim, 
# #          paste0(getwd(), "/out/SIG/vecteurs/perimetres"), 
# #          layer = "Perim_Fleckenstein_L93", 
# #          driver = "ESRI Shapefile", 
# #          overwrite_layer = F) # Réécriture
# # writeOGR(shp_Perim, 
# #          paste0(getwd(), "/out/SIG/vecteurs/perimetres"), 
# #          layer = "Mask2_Perim_GF", 
# #          driver = "ESRI Shapefile", 
# #          overwrite_layer = F)
# # writeOGR(shp_Perim, 
# #          paste0(getwd(), "/out/SIG/vecteurs/perimetres"), 
# #          layer = "Perim_PSDRF3_L93", 
# #          driver = "ESRI Shapefile", 
# #          overwrite_layer = F) # Réécriture
# # writeOGR(shp_Perim, 
# #          paste0(getwd(), "/out/SIG/vecteurs/perimetres"), 
# #          layer = "Perim_PSDRF5_L93", 
# #          driver = "ESRI Shapefile", 
# #          overwrite_layer = F) # Réécriture
# writeOGR(shp_Perim, 
#          paste0(getwd(), "/out/SIG/vecteurs/perimetres"), 
#          layer = "Mask2_GF5_L93", 
#          driver = "ESRI Shapefile", 
#          overwrite_layer = F) # Réécriture
# ----- %%%%%%%%%% -----


# ----- %%%%% Prévisualisation %%%%% -----

plot(st_geometry(perim_union), border = "red")
# plot(shp_France, add = T)

# Distance minimale au périmètre à laquelle placer les placettes
dist = 15



# # ---------- Initiation de la boucle :
# i = 1
# print(paste0("i = ", i))
# 
# # Création de la colonne Disp
# perim_union <- 
#   perim_union %>% 
#   mutate(NumForet = character(),
#          dist_plac = NA,
#          az_N = NA,
#          az_E = NA)
# perim_union@data$NumForet <- NA
# perim_union@data$NumForet[i] <- as.character(i)
# 
# # Création de la colonne Dist_Plac
# perim_union@data$Dist_Plac <- NA
# 
# # Création de la colonne Az_N
# perim_union@data$Az_N <- NA
# 
# # Création de la colonne Az_E
# perim_union@data$Az_E <- NA
# 
# shp_temp <- perim_union[i, ]
# plot(shp_temp)
# df_temp <- shp_temp@data


# -- Définition du nombre de placettes à insérer en fonction de la surface 
# plac_nb <- 120 # Lamadeleine - V1
plac_nb <- 110 # Lamadeleine - V2

perim_inv <- perim_union %>% st_buffer(-dist)
plot(st_geometry(perim_union))
plot(st_geometry(perim_inv), add = T, border = "red")

# -- création du grid de placettes
# test <- 0
# count <- 0
plot_grid <- c()
while (length(plot_grid) != plac_nb & count < 100) {
  # plac_nb <- plac_nb + 1
  plot_grid <- st_sample(
    perim_inv, 
    size = plac_nb, 
    type = "regular"
  )
  # test <- length(plot_grid)[1]
  count = count + 1
  # print(count) # debug
  # print(paste0("plot", length(plot_grid))) # debug
}
plot(plot_grid, add = T, col = "green")

plot_grid <- 
  plot_grid %>% 
  st_sf() %>% 
  mutate(
    X = st_coordinates(plot_grid)[, 1],
    Y = st_coordinates(plot_grid)[, 2]
  ) %>% 
  arrange(desc(Y), X) %>% 
  mutate(NumPlac = 1:plac_nb) %>% 
  select(NumPlac, X, Y, geometry)

# -- création du répertoire de sortie :
save_path <- sub("perimetres", "placettes", save_path) # chemin
dir.create(
  save_path, showWarnings = F, recursive = T
)

# -- écriture du shape des placettes :
st_write(
  plot_grid, 
  dsn = save_path, 
  layer = paste0("grid_", plac_nb, "_placettes_L93"), 
  driver = "ESRI Shapefile", quiet = T,
  update = TRUE, delete_layer = TRUE
)
# mat_Dist <- gDistance(shp_Plac, as(shp_temp, "SpatialLines"), byid = TRUE)
# min_Dist <- min(mat_Dist)
# # Contrôle qu'on ait bien le bon nombre de placettes + respect de la Dist minimale
# if (dim(shp_Plac@coords)[1] != plac_nb | min_Dist < Dist) {
#   count = 0
#   while(dim(shp_Plac@coords)[1] != plac_nb | min_Dist < Dist) {
#     shp_Plac <- spsample(shp_temp, n = plac_nb, "regular")
#     mat_Dist <- gDistance(shp_Plac, as(shp_temp, "SpatialLines"), byid = TRUE)
#     min_Dist <- min(mat_Dist)
#     count = count+1
#     # print(paste0("Essai ", count))
#     if (count > =  100) {
#       # Answer <- tk_messageBox(type = "ok", 
#       #                         message = "100 essais réalisés. Arrêter ?")
#       # stop("Essais infructueux")
#       # Answer <- tk_messageBox(type = "ok", 
#       #                         message = "100 essais réalisés. Diminution de la distance minimale de 1m")
#       Dist = Dist-1
#       print(paste0("Distance : ", Dist))
#       count = 0
#     }
#   }
# }

# rangement des coordonnées par ordre croissant
shp_Plac <- shp_Plac[order(shp_Plac@coords[, 1], shp_Plac@coords[, 2]), ]
shp_Plac <- SpatialPointsDataFrame(shp_Plac, 
                            data.frame(NumDisp = as.character(round(rep(i, dim(shp_Plac@coords)[1]), 0)), 
                                       NumPlac = as.character(1:dim(shp_Plac@coords)[1]), 
                                       DistanceMin = as.character(rep(Dist, dim(shp_Plac@coords)[1])), 
                                       stringsAsFactors = F), 
                            match.ID = T)
# ----- Calcul de la distance entre les points
dist <- c()
for (j in 2:plac_nb) {
  dist <- c(dist, round(gDistance(shp_Plac[j-1, ], shp_Plac[j, ]), 1))
}

df_FreqDist <- table(dist)
dist <- names(df_FreqDist)[df_FreqDist == max(df_FreqDist)] # on prend la distance inter-point 
# la plus fréquente dans la maille
dist <- unique(dist)

if (length(dist) > 1) {
  stop("Erreur : distance entre les points est multiple")
}
# Enregistrement de la distance
# shp_Plac$Dist_Plac <- dist
perim_union@data$Dist_Plac[i] <- as.character(dist)

# ----- Calcul de l'azimut entre les points (N et E).
# conversion en WGS84 pour pouvoir utiliser la fonction bearing
shp_Plac <- spTransform(shp_Plac, CRS("+proj = longlat +datum = WGS84"))

# Calcul des différents azimut pour repérer celui du nord et celui du sud
# bearing <- c()
# for (j in 2:plac_nb) {
#   bearing <- c(bearing, round(bearing(shp_Plac[j-1, ], shp_Plac[j, ])*100/90, 1))
# }
# bearing_test <- bearingTrack(coordinates(shp_Plac)[, 2], # alternative pour avoir N = 0 ?
#                              coordinates(shp_Plac)[, 1])


# bearing <- unique(bearing)
# bearing <- bearing[which(bearing > 0)]

# Enregistrement des valeurs
# shp_Perim$Az_N <- min(bearing)
# shp_Plac$Az_E <- shp_Plac$Az_N+100
# shp_Perim@data$Az_N[i] <- as.character(min(bearing))
# shp_Perim@data$Az_E[i] <- as.character(min(bearing) + 100)

# ----- Réécriture du shape
# changement du CRS :
shp_Plac <- spTransform(shp_Plac, CRS("+init = epsg:2154"))


# writeOGR(shp_Plac, 
#          paste0(getwd(), "/out/SIG/vecteurs/placettes"), 
#          layer = "Plac_Fleckenstein_L93", 
#          driver = "ESRI Shapefile", 
#          overwrite_layer = T)
# writeOGR(shp_Plac, 
#          paste0(getwd(), "/out/SIG/vecteurs/placettes"), 
#          layer = "Plac_PSDRF1_L93", 
#          driver = "ESRI Shapefile", 
#          overwrite_layer = T)
# writeOGR(shp_Plac, 
#          paste0(getwd(), "/out/SIG/vecteurs/placettes"), 
#          layer = "Plac_GF3_L93", 
#          driver = "ESRI Shapefile", 
#          overwrite_layer = T)
# writeOGR(shp_Plac, 
#          paste0(getwd(), "/out/SIG/vecteurs/placettes"), 
#          layer = "Plac_PSDRF5_L93", 
#          driver = "ESRI Shapefile", 
#          overwrite_layer = T)
# writeOGR(shp_Plac, 
#          paste0(getwd(), "/out/SIG/vecteurs/placettes"), 
#          layer = "Plac_GF5_L93", 
#          driver = "ESRI Shapefile", 
#          overwrite_layer = T)
# writeOGR(shp_Plac, 
#          paste0(getwd(), "/out/SIG/vecteurs/placettes"), 
#          layer = "Plac_GF7_L93", 
#          driver = "ESRI Shapefile", 
#          overwrite_layer = T)
writeOGR(shp_Plac, 
         paste0(getwd(), "/out/SIG/vecteurs/placettes"), 
         layer = "Plac_PSDRF_Fin2_L93", 
         driver = "ESRI Shapefile", 
         overwrite_layer = T)
# writeOGR(shp_Plac, 
#          paste0(getwd(), "/out/SIG/vecteurs/placettes"), 
#          layer = "Plac_GF_Fin_L93", 
#          driver = "ESRI Shapefile", 
#          overwrite_layer = T)

# ----- %%%%%%%%%% -----


# ----- %%%%% Traitement des points GPS %%%%% -----
repPP <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein"
setwd(repPP)

# -- Campagne 1 -- #
ListFiles1 <- tk_choose.files(caption = "Choix des fichiers GPS - Campagne1")
# ListFiles2 <- tk_choose.files(caption = "Choix des fichiers GPS - Campagne2")
# ListFiles <- c(ListFiles1, ListFiles2)
ListFiles <- ListFiles1

Gps_SHP <- readOGR(ListFiles[1], 
                   layer = "waypoints", 
                   verbose = F)
for (i in 2:length(ListFiles)) {
  Gps_FILE <- ListFiles[i]
  temp_SHP <- readOGR(Gps_FILE, 
                      layer = "waypoints", 
                      verbose = F)
  
  Gps_SHP <- rbind(temp_SHP, 
                   Gps_SHP)
}
Gps_SHP <- spTransform(Gps_SHP, 
                       CRSobj = CRS("+init = epsg:2154"))

dir.create("out/SIG/GPS", showWarnings = F)
dir.create("out/SIG/GPS/Campagne1", showWarnings = F)

writeOGR(obj = Gps_SHP, 
         dsn = "out/SIG/GPS/Campagne1/", 
         layer = "Plac_GPS1_L93", 
         driver = "ESRI Shapefile", 
         overwrite_layer = T)


# -- Campagne 2 -- #
ListFiles2 <- tk_choose.files(caption = "Choix des fichiers GPS - Campagne2")
# ListFiles <- c(ListFiles1, ListFiles2)
ListFiles <- ListFiles2

file_GPX <- readOGR(ListFiles[1], 
                    layer = "waypoints", 
                    verbose = F)
Gps_SHP <- readOGR(ListFiles[1], 
                   layer = "waypoints", 
                   verbose = F)
for (i in 2:length(ListFiles)) {
  Gps_FILE <- ListFiles[i]
  temp_SHP <- readOGR(Gps_FILE, 
                      layer = "waypoints", 
                      verbose = F)
  
  Gps_SHP <- rbind(temp_SHP, 
                   Gps_SHP)
}
Gps_SHP <- spTransform(Gps_SHP, 
                       CRSobj = CRS("+init = epsg:2154"))

dir.create("out/SIG/GPS/Campagne2", showWarnings = F)
writeOGR(obj = Gps_SHP, 
         dsn = "out/SIG/GPS/Campagne2/", 
         layer = "Plac_GPS2_L93", 
         driver = "ESRI Shapefile", 
         overwrite_layer = T)

# -- Toutes les campagnes -- #
ListFiles <- c(ListFiles1, ListFiles2)

Gps_SHP <- readOGR(ListFiles[1], 
                   layer = "waypoints", 
                   verbose = F)
Gps_SHP$wptx1_WaypointExtension <- NA
for (i in 2:length(ListFiles)) {
  # print(i)
  Gps_FILE <- ListFiles[i]
  temp_SHP <- readOGR(Gps_FILE, 
                      layer = "waypoints", 
                      verbose = F)
  
  Gps_SHP <- rbind(temp_SHP, 
                   Gps_SHP)
}

# crop par rapport au périmètre d'inventaire
Perim_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Data/SIG/vecteurs/perimetres/Perim_Fleckenstein_L93.shp"
Perim_SHP <- readOGR(Perim_FILE, 
                     basename(file_path_sans_ext(Perim_FILE)), 
                     verbose = F)
Perim_SHP <- spTransform(Perim_SHP, 
                         CRSobj = CRS("+init = epsg:2154"))
Gps_SHP <- spTransform(Gps_SHP, 
                       CRSobj = CRS("+init = epsg:2154"))
plot(Perim_SHP)
plot(Perim_SHP)
plot(Gps_SHP, 
     col = "red", add = T)
# Gps_SHP <- crop(Gps_SHP, 
#                  extent(Perim_SHP))
# Gps_SHP <- gIntersection(Gps_SHP, 
#                          Perim_SHP)
plot(Gps_SHP, 
     col = "blue", add = T)

dir.create("out/SIG/GPS", showWarnings = F)

writeOGR(obj = Gps_SHP, 
         dsn = "out/SIG/GPS/", 
         layer = "Plac_GPS_L93", 
         driver = "ESRI Shapefile", 
         overwrite_layer = T)

# -- Distinction PSDRF-GF -- #
# crop par rapport au périmètre d'inventaire du protocole GF
# Perim_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/out/SIG/vecteurs/perimetres/Perim_PSDRF_Fin_L93.shp"
# Perim_SHP <- readOGR(Perim_FILE, 
#                      basename(file_path_sans_ext(Perim_FILE)), 
#                      verbose = F)
# Gps_SHP <- spTransform(Gps_SHP, 
#                        CRSobj = CRS("+init = epsg:2154"))
# plot(Perim_SHP)
# plot(Gps_SHP, 
#      col = "red", add = T)
# Gps_SHP <- crop(Gps_SHP, 
#                 extent(Perim_SHP))


# crop par rapport au périmètre d'inventaire du PSDRF
Perim_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/out/SIG/vecteurs/perimetres/Perim_PSDRF_Fin_L93.shp"
Perim_SHP <- readOGR(Perim_FILE, 
                     basename(file_path_sans_ext(Perim_FILE)), 
                     verbose = F)
Perim_SHP <- spTransform(Perim_SHP, 
                         CRSobj = CRS("+init = epsg:2154"))
Gps_SHP <- spTransform(Gps_SHP, 
                       CRSobj = CRS("+init = epsg:2154"))
plot(Perim_SHP)
plot(Gps_SHP, 
     col = "red", add = T)
# Gps_SHP2 <- gIntersection(Gps_SHP, 
#                          Perim_SHP, 
#                          byid = T)
Intersect_DF <- gIntersects(Gps_SHP, 
                            Perim_SHP, 
                            byid = T)
GpsPSDRF_DF <- as.data.frame(which(Intersect_DF == TRUE, arr.ind = T))
names(GpsPSDRF_DF) <- c("Pos_Perim", "Pos_Gps")
GpsPSDRF_DF <- arrange(GpsPSDRF_DF, 
                       Pos_Gps)
row.names(GpsPSDRF_DF) <- 1:dim(GpsPSDRF_DF)[1]

Pos_Gps_PSDRF <- GpsPSDRF_DF$Pos_Gps # position des points PSDRF - à soustraire à Gps_SHP pour avoir position des points GF
GpsPSDRF_DF <- cbind(GpsPSDRF_DF, 
                     Gps_SHP@data[GpsPSDRF_DF$Pos_Gps, ], 
                     Perim_SHP@data[GpsPSDRF_DF$Pos_Perim, ], 
                     Gps_SHP@coords[GpsPSDRF_DF$Pos_Gps, ]) %>%
  dplyr::select(name, time, coords.x1, coords.x2) %>%
  rename_("Xgps" = "coords.x1", 
          "Ygps" = "coords.x2") %>% 
  mutate(Xgps = as.numeric(Xgps), 
         Ygps = as.numeric(Ygps))

GpsPSDRF_SHP <- GpsPSDRF_DF
coordinates(GpsPSDRF_SHP) <- ~ Xgps + Ygps
proj4string(GpsPSDRF_SHP) <- CRS("+init = epsg:2154")

plot(GpsPSDRF_SHP, 
     col = "blue", add = T)

dir.create("out/SIG/GPS", showWarnings = F)

writeOGR(GpsPSDRF_SHP, 
         dsn = "out/SIG/GPS/", 
         layer = "Plac_GPS-PSDRF_L93", 
         driver = "ESRI Shapefile", 
         overwrite_layer = T, 
         verbose = F, 
         encoding = "UTF-8")

# Récupération des points du protocole GF (autres par défaut)
GpsGF_SHP <- Gps_SHP[-Pos_Gps_PSDRF, c("name", "time")]

# plot(GpsPSDRF_SHP, 
#      col = "blue", add = T)
# plot(GpsGF_SHP, 
#      col = "green", add = T)

writeOGR(GpsGF_SHP, 
         dsn = "out/SIG/GPS/", 
         layer = "Plac_GPS-GF_L93", 
         driver = "ESRI Shapefile", 
         overwrite_layer = T, 
         verbose = F, 
         encoding = "UTF-8")

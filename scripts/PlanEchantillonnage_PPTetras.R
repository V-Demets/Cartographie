# ----- Chargement des librairies
library(rgdal)
library(sp)
library(rgeos)
library(dplyr)
library(tcltk)
library(openxlsx)
library(doBy)
library(tools)
library(geosphere)
# library(GISTools)

# ----- Choix du répertoire de travail
# repPP <- tk_choose.dir(default=getwd(),
#                        caption="Choix du répertoire de travail")
repPP <- "/Users/Valentin/Foret/Travail/Placettes_Tetras"
setwd(repPP)

# ----- Chargement du shape des périmètres des dispositifs
# file_France <- tk_choose.files(caption="Choix du shape des régions françaises",
#                                multi=F, filters=matrix(c("shape","shape",".shp","shp"), ncol=2))
file_France <- "/Users/Valentin/Foret/Travail/Placettes_Tetras/Data/SIG/nouvelles_regions.shp"
# file_Perim <- tk_choose.files(caption="Choix du shape des périmètres des dispositifs",
#                               multi=F, filters=matrix(c("shape","shape",".shp","shp"), ncol=2))
# file_Perim <- "/Users/Valentin/Foret/Travail/Placettes_Tetras/Data/SIG/Vecteurs/Perim_DispTetras_L93.shp"
# file_Perim <- "/Users/Valentin/Foret/Travail/Placettes_Tetras/Data/SIG/Changements/Perim_DispTetrasAdd_L93.shp"
file_Perim <- "/Users/Valentin/Foret/Travail/Placettes_Tetras/Out/SIG/Perim_DispTetrasAdd_L93.shp"
shp_France <- readOGR(file_France, basename(file_path_sans_ext(file_France)))
shp_Perim <- readOGR(file_Perim, basename(file_path_sans_ext(file_Perim)))

# Prévisualisation
plot(shp_Perim)
plot(shp_France, add=T)

# Distance minimale au périmètre à laquelle placer les placettes
Dist=25

# ---------- Initiation de la boucle :
i=1
print(paste0("i = ",i))

# Création de la colonne Disp
shp_Perim@data$NumDisp <- NA
shp_Perim@data$NumDisp[i] <- as.character(i)

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
nb_Plac <- ifelse(df_temp$SURFACE > 6,
                  12,10)
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
bearing <- c()
for (j in 2:nb_Plac) {
  bearing <- c(bearing,round(bearing(shp_Plac[j-1,],shp_Plac[j,])*100/90,1))
}
bearing <- unique(bearing)
bearing <- bearing[which(bearing > 0)]

# Enregistrement des valeurs
# shp_Perim$Az_N <- min(bearing)
# shp_Plac$Az_E <- shp_Plac$Az_N+100
shp_Perim@data$Az_N[i] <- as.character(min(bearing))
shp_Perim@data$Az_E[i] <- as.character(min(bearing) + 100)

# ----- Réécriture du shape
# changement du CRS :
shp_Plac <- spTransform(shp_Plac, CRS("+init=epsg:2154"))

a <- shp_Perim@data

######################################################################################################


# ---------- Ecriture des autres dispositifs
for (i in 2:dim(shp_Perim)[1]) { #dim(shp_Perim)[1]
  # Distance minimale au périmètre à laquelle placer les placettes
  Dist=25
  
  print(paste0("i = ",i))
  shp_Perim@data$NumDisp[i] <- as.character(i)
  
  shp_temp <- shp_Perim[i,]
  df_temp <- shp_temp@data
  # -- Définition du nombre de placettes à insérer en fonction de la surface 
  # (si surface > 6 ha alors passer de 10 à 12 plac)
  nb_Plac <- ifelse(df_temp$SURFACE > 6,
                    12,10)
  # -- Création du grid de placettes
  shp_Plac_temp <- spsample(shp_temp, n=nb_Plac, "regular")
  mat_Dist <- gDistance(shp_Plac_temp, as(shp_temp, "SpatialLines"), byid = TRUE)
  min_Dist <- min(mat_Dist)
  # Contrôle qu'on ait bien le bon nombre de placettes + respect de la Dist minimale
  if (dim(shp_Plac_temp@coords)[1] != nb_Plac | min_Dist < Dist) {
    count=0
    while(dim(shp_Plac_temp@coords)[1] != nb_Plac | min_Dist < Dist) {
      shp_Plac_temp <- spsample(shp_temp, n=nb_Plac, "regular")
      mat_Dist <- gDistance(shp_Plac_temp, as(shp_temp, "SpatialLines"), byid = TRUE)
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
    # rangement des coordonnées par ordre croissant
    shp_Plac_temp <- shp_Plac_temp[order(shp_Plac_temp@coords[,1],shp_Plac_temp@coords[,2]),]
    shp_Plac_temp <- SpatialPointsDataFrame(shp_Plac_temp,
                                       data.frame(NumDisp=as.character(round(rep(i,dim(shp_Plac_temp@coords)[1]),0)),
                                                  NumPlac=as.character(1:dim(shp_Plac_temp@coords)[1]),
                                                  DistanceMin=as.character(rep(Dist,dim(shp_Plac_temp@coords)[1])),
                                                  stringsAsFactors=F),
                                       match.ID=T)
  } else {
    # mat_Dist <- gDistance(shp_Plac_temp, as(shp_temp, "SpatialLines"), byid = TRUE)
    # min_Dist <- min(mat_Dist)
    # if (min_Dist < Dist) {
    #   count=0
    #   while (min_Dist < Dist) {
    #     
    #   }
    # } else {
    # rangement des coordonnées par ordre croissant
    shp_Plac_temp <- shp_Plac_temp[order(shp_Plac_temp@coords[,1],shp_Plac_temp@coords[,2]),]
    shp_Plac_temp <- SpatialPointsDataFrame(shp_Plac_temp,
                                            data.frame(NumDisp=as.character(round(rep(i,dim(shp_Plac_temp@coords)[1]),0)),
                                                       NumPlac=as.character(1:dim(shp_Plac_temp@coords)[1]),
                                                       DistanceMin=as.character(rep(Dist,dim(shp_Plac_temp@coords)[1])),
                                                       stringsAsFactors=F),
                                            match.ID=T)
  }
    # ----- Calcul de la distance entre les points
    dist <- c()
    for (j in 2:nb_Plac) {
      # print(j)
      dist <- c(dist,round(gDistance(shp_Plac_temp[j-1,],shp_Plac_temp[j,]),1))
      # print(dist)
    }
    # plot(shp_Plac_temp)
    df_FreqDist <- table(dist)
    dist <- names(df_FreqDist)[df_FreqDist==max(df_FreqDist)] # on prend la distance inter-point 
    # la plus fréquente dans la maille
    dist <- unique(dist)
    
    if (length(dist) > 1) {
      stop("Erreur : distance entre les points est multiple")
    }
    # Enregistrement de la distance
    # shp_Plac_temp$Dist_Plac <- dist
    shp_Perim@data$Dist_Plac[i] <- as.character(dist)
    
    # ----- Calcul de l'azimut entre les points (N et E).
    # conversion en WGS84 pour pouvoir utiliser la fonction bearing
    shp_Plac_temp <- spTransform(shp_Plac_temp, CRS("+proj=longlat +datum=WGS84"))
    
    # Calcul des différents azimut pour repérer celui du nord et celui du sud
    bearing <- c()
    for (j in 2:nb_Plac) {
      # print(j)
      bearing <- c(bearing,round(bearing(shp_Plac_temp[j-1,],shp_Plac_temp[j,])*100/90,1))
      # print(round(bearing(shp_Plac_temp[j-1,],shp_Plac_temp[j,])*100/90,1))
    }
    bearing <- unique(bearing)
    bearing <- bearing[which(bearing > 0)]
    
    # Enregistrement des valeurs
    # shp_Plac_temp$Az_N <- min(bearing)
    # shp_Plac_temp$Az_E <- shp_Plac_temp$Az_N+100
    shp_Perim@data$Az_N[i] <- as.character(min(bearing))
    shp_Perim@data$Az_E[i] <- as.character(min(bearing) + 100)
    
    # ----- Réécriture du shape
    # changement du CRS :
    shp_Plac_temp <- spTransform(shp_Plac_temp, CRS("+init=epsg:2154"))
    
    
    # ----- Rassemblement du shape temporaire avec le shape général :
    # return(shp_Plac@data)
    shp_Plac <- rbind(shp_Plac,shp_Plac_temp)
}


writeOGR(shp_Plac, paste0(getwd(),"/Out/SIG"), layer="PlacTetrasAdd_L93", driver="ESRI Shapefile",
         overwrite_layer=T)
# writeOGR(shp_Perim, paste0(getwd(),"/Out/SIG"), layer="Perim_DispTetrasAdd_L93", driver="ESRI Shapefile",
#          overwrite_layer=T)



# shp_Plac45 <- shp_Plac
# shp_Plac <- shp_Plac45

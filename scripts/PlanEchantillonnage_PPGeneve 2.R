# ----- Chargement des librairies
library(easypackages)
packages("rgdal","sp","rgeos","dplyr","tcltk","openxlsx","doBy",
        "tools","argosfilter","raster")
# library(geosphere)



# ----- Choix du répertoire de travail
# repPP <- tk_choose.dir(default=getwd(),
#                        caption="Choix du répertoire de travail")
repPP <- "/Users/Valentin/Foret/Travail/Leforestier/Geneve/FD_Geneve"
setwd(repPP)
# Création du répertoire de sortie :
dir.create("Out",showWarnings=F)
dir.create("Out/SIG",showWarnings=F)
dir.create("Out/SIG/Vecteurs",showWarnings=F)
dir.create("Out/SIG/Vecteurs/Placettes",showWarnings=F)
dir.create("Out/SIG/Vecteurs/Perimetres",showWarnings=F)



# ----- Chargement du shape des périmètres des placettes
# file_France <- tk_choose.files(caption="Choix du shape des régions françaises",
#                                multi=F, filters=matrix(c("shape","shape",".shp","shp"), ncol=2))
file_France <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Data/SIG/Vecteurs/Perimetres/nouvelles_regions.shp"
shp_France <- readOGR(file_France,
                      basename(file_path_sans_ext(file_France)),
                      verbose=F)

file_Perim <- tk_choose.files(caption="Choix du shape des périmètres des placettes",
                              multi=F, filters=matrix(c("shape","shape",".shp","shp"), ncol=2))
file_Perim <- "/Users/Valentin/Foret/Travail/Leforestier/Geneve/FD_Geneve/Data/SIG/Vecteurs/Perimetres/Foret_Privee/foret_privee_secteurs_L93.shp"
shp_Perim <- readOGR(file_Perim,
                     basename(file_path_sans_ext(file_Perim)),
                     verbose=F)
# -----

shp <- shp_Perim

# Souci sur le shape originel (même après réenregistré sous QGIS) -> impossible de faire buffer (-25m,-20m) sur la partie centrale
# Désagrégation du shape -> reconstruction avec surface de chaque parcelle
shp_ToFill <- buffer(shp_Perim,width=0,dissolve=F)
shp1 <- gUnaryUnion(shp_ToFill)

shp2 <- disaggregate(shp1)
df <- data.frame(Surface=gArea(shp2, byid=T)/10000)
row.names(df) <- 1:dim(df)[1]
shp2 <- SpatialPolygonsDataFrame(shp2,
                                 data=df)

shp3 <- spTransform(shp2, CRS("+init=epsg:2154"))
writeOGR(shp3,
         paste0(getwd(),"/Out/SIG/Vecteurs/Perimetres"),
         layer="PerimTest_L93",
         driver="ESRI Shapefile",
         overwrite_layer=T) # Réécriture -> réenregistré sous QGIS sous le nom "Perim_GenevePRIV_L93"

# ---------- construction de la maille :
# ----- Test Buffer 25m
# file.choose()
# file_PerimPlac <- "/Users/Valentin/Foret/Travail/Leforestier/Geneve/FD_Geneve/Data/SIG/Vecteurs/Perimetres/Foret_Privee/Perim_GenevePlacPRIV_L93.shp"
file_PerimPlac <- "/Users/Valentin/Foret/Travail/Leforestier/Geneve/FD_Geneve/Data/SIG/Vecteurs/Perimetres/Foret_Privee/PerimPlacPRIV_Buffer25m_L93.shp"
shp_PerimPlac <- readOGR(file_PerimPlac,
                     basename(file_path_sans_ext(file_PerimPlac)),
                     verbose=F)
shp_Plac <- spsample(shp_PerimPlac, type="regular", cellsize=c(160,160))

# plot(shp_Plac)
# --- Rangement des coordonnées par ordre croissant
shp_Plac <- shp_Plac[order(shp_Plac@coords[,1],shp_Plac@coords[,2]),]
shp_Plac <- SpatialPointsDataFrame(shp_Plac,
                                   data.frame(NumForet=as.character(round(rep(1,dim(shp_Plac@coords)[1]),0)),
                                              NumPlac=as.character(1:dim(shp_Plac@coords)[1]),
                                              # DistanceMin=as.character(rep(Dist,dim(shp_Plac@coords)[1])),
                                              stringsAsFactors=F),
                                   match.ID=T)
# --- Réécriture du shape
# -- Changement du CRS :
shp_Plac <- spTransform(shp_Plac, CRS("+init=epsg:2154"))

writeOGR(shp_Plac,
         paste0(getwd(),"/Out/SIG/Vecteurs/Placettes"),
         layer="Plac_GenevePRIV_L93",
         driver="ESRI Shapefile",
         overwrite_layer=T)

# ----- test Buffer 20m
# file_PerimPlac2 <- "/Users/Valentin/Foret/Travail/Leforestier/Geneve/FD_Geneve/Data/SIG/Vecteurs/Perimetres/Foret_Privee/PerimPlacPRIV_Buffer20m_L93.shp"
# shp_PerimPlac2 <- readOGR(file_PerimPlac2,
#                          basename(file_path_sans_ext(file_PerimPlac2)),
#                          verbose=F)
# shp_Plac2 <- spsample(shp_PerimPlac, type="regular", cellsize=c(160,160))
# 
# plot(shp_Plac)
##### ----- Tests Fleckenstein ----- #####

library(sf)
library(tcltk)

# ----- Répertoire de travail
# rep <- tk_choose.dir(caption = "Choix du shape à découper")
rep <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein"
setwd(rep)

# ----- Import des fichiers d'entrée
# shp1_FILE <- tk_choose.files(caption = "Choix du shape à découper")
shp1_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes/Stations/StationsONF.shp"
shp1 <- st_read(shp1_FILE)

# shp2_FILE <- tk_choose.files(caption = "Choix du shape de découpage")
shp2_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/PP_Fleckenstein/Out/SIG/Vecteurs/Perimetres/Perim_PSDRF_Fin_L93.shp"
shp2 <- st_read(shp2_FILE)

# shp1 <- st_transform(shp1, st_crs(shp2))
shp1 <- st_intersection(shp1, shp2)
plot(shp2)
plot(shp1)

# ----- Ecriture du résultat de l'intersection
out_DIR <- paste0(rep,"/Out/SIG/Vecteurs/Perimetres/")
st_write(shp1,
         dsn=out_DIR,
         layer="Milieux",
         driver="ESRI Shapefile",
         update=T,
         delete_layer=T,
         quiet =T)
##### ----- #####


##### ----- Travail J-L BUGNOT ----- #####
# ----- Récupération des données pour étude Yzeron-A89
library(sf)
library(tcltk)
library(elevatr)
library(raster)

# ----- Répertoire de travail
# rep <- tk_choose.dir(caption = "Choix du répertoire de travail")
# rep <- "/Users/Valentin/Foret/Travail/Bugnot"
rep <- "/Users/Valentin/Travail/Outils/Cartographie/dossiers/Bugnot/ASLGF"
setwd(rep)

# ----- Paramètres initiaux
buffer = 50
zoom = 14
epsg=NULL

# ----- Import des fichiers d'entrée
# shp1_FILE <- tk_choose.files(caption = "Choix du shape à découper")
# shp1_FILE <- "/Users/Valentin/Foret/Travail/Bugnot/Data/SIG/Vecteurs/Perimetres/Perim_Projets.shp"
shp1_FILE <- "/Users/Valentin/Travail/Outils/Cartographie/dossiers/Bugnot/ASLGF/data/SIG/vecteurs/Martelage/parcelles_martelage2019_L93.shp"
shp1 <- st_read(shp1_FILE, quiet = T)
shp0 <- rgdal::readOGR(shp1_FILE)

if (is.numeric(buffer) & is.numeric(zoom)) {
  shp2 <- st_buffer(shp1, dist = buffer)
  zoom = max(zoom, 9)
  x <- get_elev_raster(as(shp2, "Spatial"), z = zoom, src = "aws")
  ras2 <- x
  if (!is.null(epsg)) {
    x <- projectRaster(x, crs = CRS(paste0('+init=EPSG:', epsg)))
  }
  return(x)
} else {
  print("buffer et zoom doivent être des entiers.")
}

# ----- Clip du raster en fonction des polygones des périmètres
# shp00 <- buffer(shp0, width = buffer) # on applique le buffer à shp0 (SPDF)
# ras3 <- mask(x, shp00) # on extrait uniquement la zone intéressante.
shp00 <- st_buffer(shp1, dist = buffer) # on applique le buffer à shp1 (sf object)
shp00 <- shp00[32,]
ras3 <- mask(x, shp00) # on extrait uniquement la/les zone(s) intéressante(s).

# ----- Ecriture du résultat de l'extraction
out_DIR <- paste0(rep, "/out/SIG/rasters/")
dir.create(out_DIR, showWarnings = F, recursive = T)
writeRaster(
  # ras3,
  x,
  filename = paste0(out_DIR, "MNT_BIG.tif"),
  verbose = F,
  format = "GTiff",
  overwrite = T
)


# ----- Récupération indice réflectance de la végétation (NDVI : Normalized Difference Vegetation Index)
# --- Library
# library(getSpatialData) # package ‘getSpatialData’ is not available (for R version 3.3.3)
# devtools::install_github("16EAGLE/getSpatialData")
library(getSpatialData)
library(sf)
library(tidyverse)
library(tools)
library(velox)
library(httr) # attention library indispensable !!
library(xml2) # attention library indispensable !!
library(dplyr)
library(tcltk)

# --- Paramètres
shp1_FILE <- "/Users/Valentin/Foret/Travail/Bugnot/Data/SIG/Vecteurs/Perimetres/Perim_Projets.shp"
shp0 <- rgdal::readOGR(shp1_FILE)

shp0_FILE <- "/Users/Valentin/Foret/Travail/Bugnot/Data/SIG/Vecteurs/Perimetres/Emprise_Projets.shp"
shp0 <- rgdal::readOGR(shp0_FILE)

start = "2017-05-01"
end = "2018-05-26"
platform = "Sentinel-2"
user = "vdemets"
# shp=as(perim46, "Spatial")
# shp=as(shp0, "Spatial")
# shp=raster::buffer(shp0, width=2500) #[1,]
shp <- spTransform(shp0,
                   CRS("+init=epsg:4326")) # conversion en WGS84
# shp <- rgeos::gUnaryUnion(shp)
# shp=st_as_sfc(shp0)
out = "/Users/Valentin/Foret/Travail/Bugnot/Out"

time_range <-  c(start, end)
set_aoi(shp) # définir la plage
# set_aoi(matrix(raster::extent(shp),
#                nrow=2, ncol=2, byrow=F)) # définir la plage
login_CopHub(username = user) # password perso = "Rahte9poSEN"
dir.create("TempSentinel", showWarnings = F)
set_archive(paste(getwd(), "TempSentinel", sep="/"))
# set_archive(out)

Photos <- getSentinel_query(time_range = time_range,
                            platform = "Sentinel-2", username=user, hub="operational")
PhotosFilt <- Photos %>%
  mutate(cloudcoverpercentage = as.numeric(cloudcoverpercentage)) %>%
  filter(processinglevel == "Level-1C" & cloudcoverpercentage <= 30)
ListChoix <- PhotosFilt %>%
  mutate(Date = as.Date(substr(datatakesensingstart, 1,10), "%Y-%m-%d"),
         Heure = substr(datatakesensingstart, 12,19),
         ID = paste(Date, "---", "Cover",round(cloudcoverpercentage,0),"%")) %>%
  dplyr::select(Date,Heure,ID) %>%
  distinct() %>%
  arrange(desc(Date))
Choix <- tk_select.list(ListChoix$ID, title="Choisir une date de passage", multiple=T)
pos <- which(ListChoix$ID %in% Choix)

## Download some datasets
files <- getSentinel_data(records = PhotosFilt[pos,])






# ----- Extraction des données NDVI
ext=200
band=1
# rep <- tk_choose.dir()
rep <- "/Users/Valentin/Foret/Travail/Bugnot/TempSentinel/get_data/SENTINEL"

fichs <- list.files(rep, full.names =T, pattern="\\.tif$")
noms <- substring(file_path_sans_ext(basename(fichs)), 54)
if(all(c("B11","B12","B2","B3","B4","B5","B6","B7","B8","B8A") %in% noms)){
  e <- extent(as(shp, "Spatial")) + ext
  # la bande B2 ayant la plus basse résolution va servir de référence pour l'extension et la résolution
  pos = which(noms == "B2")
  r1 <- raster(fichs[pos])# fichs[3] = B2
  r1 <- crop(r1, e) # on découpe les rasters nationaux à notre échelle
  s <- stack(r1)
  for(i in fichs[-pos]) { # boucle sur les autres bandes
    r <- raster(i)
    r <- crop(r, e)
    if (sum(res(r) == c(10,10)) < 2) {
      r <- resample(r, r1, method='bilinear')
    }
    s <- stack(s, r) # les différentes couches de raster sont ajouté à s
  }
  names(s) <- noms
  #------------------ Calcul des variables de synthèse ------------------
  SR           <- s$B8 / s$B4 # Simple Ratio Index
  SAVI         <- 1.5*(s$B8 - s$B4)/(s$B8 + s$B4 + 0.5) # Soil Adjusted Vegetation Index
  S2REP        <- 705 + 35*(0.5*(s$B7 + s$B4) - s$B5)/(s$B6 - s$B5) # Sentinel-2 Red-Edge Position
  RENDWI       <- (s$B3 - s$B5)/(s$B3 + s$B5) # Red Edge - Normalized Difference Water Index
  RedEdgeNDVI  <- (s$B8 - s$B6)/(s$B8 + s$B6)  # Red edge NDVI
  PSRI         <- (s$B4 - s$B2)/s$B5 # Plant Senescence Reflectance Index
  PSRINRI      <- (s$B4 - s$B2)/s$B8 # Plant Senescence Reflectance Index - Near Infra-red
  NDWI         <- (s$B3 - s$B8)/(s$B3 + s$B8) # Normalized Difference Water Index
  NDVI         <- (s$B8 -s$B4)/(s$B8 + s$B4) # Normalized Difference Vegetation Index
  NDVIgreen    <- s$B3*NDVI  # Normalized Difference Vegetation Index - Green
  NDVI705      <- (s$B6 - s$B5)/(s$B6 + s$B5) # NDVI705
  NDII         <- (s$B8 - s$B11)/(s$B8 + s$B11) # increase with increasing water content;forest canopy monitoring, and stressed vegetation detection
  NDI45        <- (s$B5 - s$B4)/(s$B5 + s$B4)# NDI45
  NDBI         <- (s$B11 - s$B8)/(s$B11 + s$B8) # Building or watershed runoff predictions and land-use planning
  MTCI         <- (s$B6 - s$B5)/(s$B5 - s$B4)# MERIS Terrestrial Chlorophyll Index
  
  s1 <- stack(SR, SAVI,S2REP,RENDWI,RedEdgeNDVI,PSRI,PSRINRI,NDWI,NDVI,NDVIgreen,
              NDVI705,NDII,NDI45,NDBI,MTCI) # création d'une variable s1 contennant les indices
  noms1 <- c("SR","SAVI","S2REP","RENDWI","RedEdgeNDVI","PSRI","PSRINRI","NDWI","NDVI",
             "NDVIgreen","NDVI705","NDII","NDI45","NDBI", "MTCI")  # et d'un autre avec les noms associés
  names(s1) <- noms1
  
  # Complément1
  if(band >1) {
    PNDVI        <- (s$B8 - (s$B3 + s$B4 + s$B2)) / (s$B8 + (s$B3 + s$B4 + s$B2))
    GI           <- s$B3/s$B4
    EVI          <- 2.5*(s$B8 - s$B4) / (s$B8 + 6*s$B4 - 7.5*s$B2 + 1)
    EVI2         <- 2.5*(s$B8 - s$B4) / (s$B8  + 2.4 * s$B4 + 1)
    CCCI         <- ((s$B8 - s$B5) / (s$B8 + s$B5)) / ((s$B8 - s$B4) / (s$B8 + s$B4))
    CHLREDEDGE   <- s$B5 / s$B8
    ARI1         <- (1 / s$B3) - (1 / s$B5)
    s1 <- stack(s1, PNDVI,GI,EVI,EVI2, CHLREDEDGE,CCCI, ARI1)
    noms2 <- c(noms1, "PNDVI","GI","EVI","EVI2","CHLREDEDGE", "CCCI","ARI1")
    names(s1) <- noms2
  }
  
  # Complément2
  if(band >2) {
    NBRraw       <- (s$B8 - s$B12)/(s$B8 + s$B12) # Burn Surface
    MSI          <- (s$B11 / s$B8)
    MSAVI2       <- (s$B8 + 1) - 0.5 * sqrt((2 * s$B8 - 1) ^ 2 + 8 * s$B4)
    MNDWI        <- (s$B3 - s$B11)/(s$B3 + s$B11)
    MCARI        <- 1 - 0.2 * (s$B5 - s$B3) / (s$B5 - s$B4)
    IRECI        <- (s$B7 - s$B4) * s$B6 / s$B5
    GRVI1        <- (s$B4 - s$B3) / (s$B4 + s$B3)
    GNDVI        <- (s$B8 - s$B3) / (s$B8 + s$B3)
    CRI2         <- (1 / s$B2) - (1 / s$B5)
    CRI1         <-  (1 / s$B2) - (1 / s$B3)
    BAI          <- 1 / ((0.1 - s$B4)^2 + (0.06 - s$B8)^2)
    ARVI         <- (s$B8 - (2*s$B4 - s$B2)) /(s$B8 + (2*s$B4 - s$B2))
    ARI2         <- (s$B8 / s$B2) - (s$B8 / s$B3)
    BI           <- ((s$B4 + s$B2)-s$B3)/((s$B4 + s$B2)+s$B3)
    AVI          <- ((s$B4+1)*(256-s$B3)*(s$B4-s$B3))^(1/3)
    s1 <- stack(s1, NBRraw,MSI,MSAVI2,MNDWI,MCARI,IRECI,GRVI1,GNDVI,CRI2,CRI1,BAI,ARVI,ARI2,BI,AVI)
    noms3 <- c(noms2,"NBRraw","MSI","MSAVI2","MNDWI","MCARI","IRECI","GRVI1","GNDVI",
               "CRI2","CRI1","BAI","ARVI","ARI2","BI","AVI")
    names(s1) <- noms3
  }
  
  return(s1)
} else {
  print(paste("Le répertoire ne contient pas les rasters",noms)) # permet la vérification
}


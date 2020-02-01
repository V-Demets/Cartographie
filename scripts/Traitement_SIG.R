
##### Etude Juillet 2018 #####
# ----- Import des library
library(easypackages)
packages("sf","dplyr","tcltk","openxlsx","tools")

# ----- Choix du répertoire de travail
rep <- "/Users/Valentin/Foret/Travail/Bugnot"
setwd(rep)

# ----- Intersection de périmètres
# -- Périmètre d'étude global :
Perim_FILE <- "/Users/Valentin/Foret/Travail/Bugnot/Data/SIG/Vecteurs/Perimetres/Perim_Projets_L93.shp"
Perim_SHP <- st_read(Perim_FILE,
                     quiet=T)
Perim2_SHP <- st_buffer(Perim_SHP,
                        dist=0)
st_write(Perim_SHP,
         dsn=paste0(rep,"/Data/SIG/Vecteurs/Perimetres/A89/Foret_New_L93.shp"),
         driver="ESRI shapefile",
         layer="Foret_New_L93")


# -- Périmètre forêt :
# Foret_FILE <- "/Users/Valentin/Foret/Travail/Bugnot/Data/SIG/Vecteurs/Perimetres/A89/Perim_Forets_A89.shp"
# Foret_FILE <- "/Users/Valentin/Foret/Travail/Bugnot/Data/SIG/Vecteurs/Perimetres/A89/Foret_New_L93.shp"
Foret_FILE <- file.choose()
Foret_SHP <- st_read(Foret_FILE,
                     quiet=T)
Foret2_SHP <- st_buffer(Foret_SHP,
                       dist=0)
st_write(Foret2_SHP,
         dsn=paste0(rep,"/Data/SIG/Vecteurs/Perimetres/A89/Foret_New_L93.shp"),
         driver="ESRI shapefile",
         layer="Foret_New_L93",
         delete_dsn=T,
         quiet=T)

# -- Intersection
inter_SHP <- st_intersection(Perim_SHP,
                             Foret2_SHP)

# -- Ecriture du shape intersection
st_write(inter_SHP,
         dsn=paste0(rep,"/Data/SIG/Vecteurs/Perimetres/A89/Perim_Foret2_L93.shp"),
         driver="ESRI shapefile",
         layer="Perim_Foret2_L93",
         delete_dsn=T,
         quiet=T)

# -- Nettoyages autres erreurs topo
Foret_FILE <- "/Users/Valentin/Foret/Travail/Bugnot/Data/SIG/Vecteurs/Perimetres/A89/Perim_Foret2_L93.shp"
Foret_SHP <- st_read(Foret_FILE,
                     quiet=T)
Foret2_SHP <- st_buffer(Foret_SHP,
                        dist=0)

st_write(Foret2_SHP,
         dsn=paste0(rep,"/Data/SIG/Vecteurs/Perimetres/A89/Perim_Foret2_L93.shp"),
         driver="ESRI shapefile",
         layer="Perim_Foret2_L93",
         delete_dsn=T,
         quiet=T)

# -- Union des polygones et calculs des surfaces
Foret_FILE <- "/Users/Valentin/Foret/Travail/Bugnot/Data/SIG/Vecteurs/Perimetres/A89/Perim_Foret2_L93.shp"
Foret_SHP <- st_read(Foret_FILE,
                     quiet=T)
Foret2_SHP <- st_buffer(Foret_SHP,
                        dist=0)

Foret3_SHP <- st_combine(Foret2_SHP) %>% 
  st_union()
Foret3_SHP <- st_transform(Foret3_SHP,
                           crs="+init=epsg:3857")
t <- st_area(Foret3_SHP)

# Foret3_SHP <- mutate(Foret2_SHP,
#                      group="Commun") %>% 
#   group_by(group) %>%
#   summarise(geometry = st_union(geometry), do_union = F) %>% 
#   ungroup() %>% 
#   st_as_sfc()
x <- st_area(Foret3_SHP)
  mutst_cast("POLYGON")
st_combine(Foret2_SHP)


# st_is_valid(st_sfc(Foret3_SHP))
polys_sf            <- st_cast(Foret3_SHP, "MULTIPOLYGON")
polys_sf$vals       <- st_area(polys_sf)
  mutate(Surface_ha=st_area(.))


  # Bilan : problème dans le système d'unités
  
  ##### \ / #####
  
  
  
  
  ##### Cartographie Jerphanion #####
  library(sp)
  # -- Périmètre d'étude global :
  Perim_FILE <- "/Users/Valentin/Foret/Travail/Bugnot/Jerphanion/Data/SIG/Vecteurs/Perimetres/Avenant_ASLGF MCL_2018_v3_region.shp"
  Perim_SHP <- sf::st_read(Perim_FILE,
                       quiet=T)
  # invalid_LIST <- sf::st_make_valid(Perim_SHP)
  Perim_SHP <- rgdal::readOGR(Perim_FILE)
  Perim_SHP2 <- rgeos::gBuffer(Perim_SHP, width=0)
  Perim_SHP <- spTransform(Perim_SHP,
                           CRS("+init=epsg:2154"))
  plot(Perim_SHP, add=T)
  plot(Perim_SHP[184,], colour="red")
  shp <- Perim_SHP[185,]
  shp2 <- broom::tidy(shp)
  # shp3 <- ~ long + lat
  p1 <- Polygon(coords = shp2[,c(1,2)], hole = F)
  p2 <- Polygons(srl = list(p1), ID = "PolygA")
  SP <- SpatialPolygons(Srl = list(p2))
  df1 <- shp@data
  rownames(df1) <- "PolygA"
  SPDF <- SpatialPolygonsDataFrame(Sr = SP, data = df1)
  rgdal::writeOGR(SPDF,
                  dsn=dirname(Perim_FILE),
                  layer="Loc_ErreurTopo2_L93",
                  driver="ESRI Shapefile",
                  overwrite_layer=T)
  
  
  shp3 <- SpatialPolygonsDataFrame(shp2,
                                   proj4string=CRS("+init=epsg:2154"),
                                   data=shp2@data)
  writeOGR(Perim_SHP,
           )
  rgdal::writeOGR(Perim_SHP[184,],
           dsn=dirname(Perim_FILE),
           layer="Loc_ErreurTopo_L93",
           driver="ESRI Shapefile",
           overwrite_layer=T)


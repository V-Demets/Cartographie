########## Fusion de shapes ##########
# -- Import des library
library(easypackages)
packages("sf", "dplyr", "tcltk", "openxlsx", "tools", "units", "readxl")

# -- Choix des shapes :
shp1_FILE <- tk_choose.files(default = "", caption = "Choix du 1er shape")
shp2_FILE <- tk_choose.files(default = "", caption = "Choix du 2ème shape")

# -- Lecture des shapes :
shp1 <- st_read(shp1_FILE, quiet = T)
shp2 <- st_read(shp2_FILE, quiet = T)

# # -- Attributs utilisés pour la fusion des shapes
merge_ATTRS <- 
  c("id", "commune", "prefixe", "section", "numero", "created", "updated", "geometry")
Mieux vaut attendre le retour de J-L avec le propriétaire manquant
Manque aussi Saint-Sorbin

shp3 <- shp1 %>% st_join(shp2)
########## / ----- \ ##########


########## Archive d'un shape au format .rda ##########
# -- Import des library
library(easypackages)
packages("sf", "dplyr", "tcltk", "openxlsx", "tools", "units", "readxl")

# -- Choix du shape :
shp_FILE <- tk_choose.files(default = "", caption = "Choix du shape à archiver")

# -- Lecture du shape :
shp <- st_read(shp_FILE, quiet = T)

# -- Sauvegarde au format.rda :
Communes_SHP <- shp
save(
  Communes_SHP, 
  file = file.path("tables", gsub(".shp", ".rda", basename(shp_FILE)))
)
########## / ----- \ ##########

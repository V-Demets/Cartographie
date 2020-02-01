########## Extraction cadastre :
# ----- Import des library
library(easypackages)
packages("sf", "dplyr", "tcltk", "openxlsx", "tools", "units", "readxl")

# ----- Choix du répertoire de travail
# rep <- tk_choose.dir(caption="Choix du r\u00E9pertoire de travail")
# rep <- "/Users/Valentin/Foret/Travail/Bugnot/Lescure"
rep <- "dossiers/Bugnot/ASLGF"

##### --- 1/ Import du cadastre départemental --- #####
# cad_FILE <-
#   tk_choose.files(default="", caption = "Choix du/des shape(s) du cadastre")
# # cad_FILE <- c(
# #   "/Users/Valentin/Foret/Travail/Bugnot/Lescure/Data/SIG/Vecteurs/Perimetres/Cadastre/Parcelles_42.shp",
# #   "/Users/Valentin/Foret/Travail/Bugnot/Lescure/Data/SIG/Vecteurs/Perimetres/Cadastre/Parcelles_69.shp",
# #   "/Users/Valentin/Foret/Travail/Bugnot/Lescure/Data/SIG/Vecteurs/Perimetres/Cadastre/Parcelles_69_2Add.shp"
# # )
cad_FILE <- c(
  "data/SIG/vecteurs/cadastre/cadastre-42-parcelles-shp/parcelles_L93.shp",
  "data/SIG/vecteurs/cadastre/cadastre-69-parcelles-shp/parcelles_L93.shp"
)
print(cad_FILE)

# Import - construction d'un shape cad_SHP
# cad_SHP <- data_frame()
for (i in 1:length(cad_FILE)) {
  # print(i)
  cad_TEMP <- st_read(cad_FILE[i], quiet = T)
  cad_SHP <- 
  if (i==1) {cad_TEMP} else {cad_SHP <- rbind(cad_SHP, cad_TEMP)}
}

# Mise en forme 
# cad_SHP0 <- cad_SHP # debug
cad_SHP <- 
  cad_SHP %>% 
  mutate(
    commune = as.character(commune),
    section = as.character(section),
    numero = as.numeric(as.character(numero))
  )
##### --- /\ --- #####



# (N.B) test extraction sur les shapes du département 69. Préférer sélection manuelle
# sur GIS
# cad_SHP <- st_read(cad_FILE)
# save(cad_SHP,
#      file="Tables/Cadastre/shape_cadastre_69.RData")


##### --- 2/ Import de la liste des parcelles (Proprietaire - commune - section - numero) --- #####
# -- Choix du classeur contenant la liste des sections=numéros à extraire :
# ext_FILE <- 
#   tk_choose.files(
#     default = "", 
#     caption = "Choix du classeur contenant la liste des sections-num\u00E9ros \u00E0 extraire"
#   )
# # ext_FILE <- 
# #   "/Users/Valentin/Foret/Travail/Bugnot/Lescure/Data/Excel/Cadastre/Cadastre_Lescure.xlsx"
ext_FILE <- "dossiers/Bugnot/ASLGF/data/excel/cadastre/Cadastre_ASLGF.xlsx"

print(ext_FILE)
# Import

ext_DF <- 
  # read_xlsx(ext_FILE, sheet = "Cadastre_Foret") %>% 
  read_xlsx(ext_FILE, sheet = "Cadastre_Base") %>%
  rename(
    Proprietaire = "Propri\u00E9taire",
    commune = Commune,
    section = Section,
    numero = "Num\u00E9ro"
  ) %>% 
  mutate(
    commune = as.character(commune),
    section = as.character(section),
    numero = as.numeric(as.character(numero))
  )
##### --- /\ --- #####


##### --- 3/ Import du fichier des communes et récupération du numéro INSEE --- #####
# N.B.1 : penser à corriger les noms de commune dans le fichier du 2/

# Chargement de l'archive Communes : 
load("tables/Communes_2019_L93.rda")
Communes_DF <- 
  Communes_SHP_2016@data %>% 
  mutate(
    NOM_COM = as.character(NOM_COM),
    INSEE_COM = as.character(INSEE_COM)
  )
# Jonction avec le df des parcelles de propriétaire : 
get_INSEE <- 
  ext_DF %>% 
  mutate(Nom_Commune = toupper(Nom_Commune)) %>% 
  left_join(Communes_DF, by = c("Nom_Commune" = "NOM_COM")) %>% 
  mutate(commune = ifelse(!is.na(ID_GEOFLA), INSEE_COM, commune)) %>% 
  select(Proprietaire, Nom_Commune, commune, section, numero, Surface)

write.xlsx(get_INSEE, file = file.path(rep,"data/excel/cadastre/get_INSEE_return.xlsx"))
# N.B.2 : Vérifier qu'il y a bien un numéro INSEE pour toutes les communes. Sinon cf N.B.1

# (N.B) ; str <- names(ext_DF)[1] ; str <- "Propri\u00E9taire" ; stri_replace_all_fixed(str,pattern="é",replacement="e")
##### --- /\ --- #####


##### --- 4/ Jonction du df des parcelles de propriétaires (une fois les Num INSEE récupérés)  --- #####
# Import de la liste des parcelles de propriétaires
ext_DF <- 
  read_xlsx(ext_FILE, sheet = "Cadastre_Base_INSEE") %>%
  rename(
    Proprietaire = "Propri\u00E9taire",
    commune = Commune,
    section = Section,
    numero = "Num\u00E9ro"
  ) %>% 
  mutate(
    commune = as.character(commune),
    section = as.character(section),
    numero = as.numeric(as.character(numero))
  )

# Jonction de tables - extraction des parcelles depuis le cadastre global
# cad_SHP <- cad_SHP0 # debug - Sauvegarde
cad_ext_SHP <- 
  cad_SHP %>% 
  right_join(ext_DF)
# N.B.3 : Vérifier qu'on a bien une parcelle pour chaque numéro INSEE. Sinon, cela signifie qu'il y a eut un changement dans le cadastre -> contacter le commanditaire

# Export du parcellaire au format .shp
cad_ext_SHP <- # correction minime
  cad_ext_SHP %>%
  filter(!(Proprietaire == "FURNION Pascal" & contenance == 7770))
dir.create(
  # path = "Out/SIG/Vecteurs/Perimetres/Cadastre", showWarnings = F, recursive = T
  path = file.path(rep, "out/SIG/vecteurs/perimetres/cadastre"), 
  showWarnings = F, recursive = T
)


#!! ATTENTION !!# : changer le nom de la forêt / du projet !!
st_write(
  # cad_ext_SHP, dsn = "Out/SIG/Vecteurs/Perimetres/Cadastre",
  cad_ext_SHP, 
  dsn = file.path(rep, "out/SIG/vecteurs/perimetres/cadastre"),
  layer = "parcelles_ASLGF_L93.shp", driver="ESRI Shapefile", 
  delete_layer = T, quiet = T, update = T
)
##### --- /\ --- #####

##### --- 5/ Création du périmètre de la forêt à partir des parcelles cadastrales --- #####

# Simplification
perim_SHP <- 
  cad_ext_SHP %>% 
  mutate(parcelle = paste0(section, "-", numero)) %>% 
  group_by(commune, Nom_Commune, Proprietaire) %>% 
  summarise(
    contenance = sum(contenance),
    parcelles = paste0(parcelle, collapse = ", ")
  ) %>% 
  ungroup()

#!! ATTENTION !!# : multipolygones créés !!
perim_DF <- 
  perim_SHP %>% 
  st_set_geometry(NULL) %>% 
  select(commune, Nom_Commune, section, numero, Proprietaire, contenance)# %>% 
# perim_SHP <- st_union(cad_ext_SHP)

# Export du périmètre de la forêt au format .shp 
dir.create(
  path = file.path(rep, "out/SIG/vecteurs/perimetres"), showWarnings = F, recursive = T
)
st_write(
  perim_SHP, 
  dsn = file.path(rep, "out/SIG/vecteurs/perimetres"), 
  layer = "Perim_Foret_L93.shp", driver = "ESRI Shapefile", 
  delete_layer = T, quiet = T, update = T
)

# -- Export du parcellaire au format Excel
# load("/Users/Valentin/Foret/Valentin_Demets/Tables/General/Communes_2016.rda")
# communes_SHP <- Communes_SHP_2016 %>% 
#   st_as_sf()
# save(communes_SHP,
#      file="/Users/Valentin/Foret/Valentin_Demets/Tables/General/Communes.rda")
load("/Users/Valentin/Foret/Valentin_Demets/Tables/General/Shape_Communes.rda")
communes_DF <- 
  communes_SHP %>% 
  st_set_geometry(NULL) %>% 
  select(CODE_COM, INSEE_COM, NOM_COM) %>% 
  rename(
    code = CODE_COM, 
    commune = INSEE_COM, 
    nom = NOM_COM
  ) %>% 
  mutate(
    code = as.numeric(as.character(code)), 
    commune = as.character(commune), 
    nom = as.character(nom)
  )
cad_ext_DF <- 
  cad_ext_SHP %>% 
  st_set_geometry(NULL) %>% 
  left_join(communes_DF) %>% 
  select(Proprietaire, nom, commune, section, numero, contenance) %>% 
  mutate(contenance = contenance / 10000) %>% 
  rename(
    "Propri\u00E9taire" = Proprietaire, 
    Commune = nom, 
    Code_INSEE = commune, 
    "Num\u00E9ro" = numero, 
    Section = section, 
    Surface_ha = contenance
  )

perim_DF <- 
  perim_SHP %>% 
  st_set_geometry(NULL) %>% 
  left_join(communes_DF) %>% 
  select(Proprietaire, nom, commune, contenance) %>% 
  mutate(contenance = contenance / 10000) %>% 
  rename(
    "Propri\u00E9taire" = Proprietaire, 
    Commune = nom, 
    Code_INSEE = commune, 
    Surface_ha = contenance
  )

# -- Edition du classeur d'export
row_END <- dim(cad_ext_DF)[1]
wb <- createWorkbook()
addWorksheet(wb, "Parcellaire")
writeData(wb, "Parcellaire", cad_ext_DF)
writeData(wb, "Parcellaire", perim_DF, startRow = row_END + 3)

dir.create(path = "Out/Excel/Cadastre", showWarnings = F, recursive = T)
saveWorkbook(wb, "Out/Excel/Cadastre/Cadastre_Lescure_Rendu.xlsx", overwrite = T)

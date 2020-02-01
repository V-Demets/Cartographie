##### fonction de mise en forme (du classeur "cadastre.xlsx") #####
style_title <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, textDecoration = NULL, 
  # fond cellule
  fgFill = "gray", 
  # bordure
  border =  "TopBottomLeftRight", 
  # alignement
  valign = "center", halign = "center", 
  # rotation
  textRotation = 0 
)
style_general <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, textDecoration = NULL, 
  # fond cellule
  fgFill = NULL, 
  # bordure
  border = "LeftRight", 
  # alignement
  valign = "center", halign = "center", 
  # rotation
  textRotation = 0 
)
style_separate_row <- createStyle(
  # bordure
  border = "Bottom"
)

styles_list <- c(style_title, style_general)
names(styles_list) <- c("style_title", "style_general")

layout_wb <- function(wb, styles, styles_distribution) {
  ##### 1/ Tests de sécurité #####
  # -- sécurité : présence dans le wb des feuilles mentionnées dans styles_distribution
  # test
  all_wb_sheets <- sheets(wb)
  test_sheet <- 
    with(styles_distribution, which(!sheets %in% all_wb_sheets))
  
  # message
  if (length(test_sheet) > 0) {
    missing_sheets <- paste0(
      unique( styles_distribution$sheets[test_sheet] ), 
      collapse = ", "
    )
    stop(
      "Certaines feuilles de la table '", substitute(styles_distribution),
      "' n'existent pas dans le classeur excel \u00E0 mettre en forme : ",
      missing_sheets
    )
  }
  # -- sécurité : présence en arguments des styles mentionnés dans styles_distribution
  # test
  all_arg_styles <- names(styles_list)
  test_style <- 
    with(styles_distribution, which(!styles %in% all_arg_styles))
  
  # message
  if (length(test_style) > 0) {
    missing_styles <- paste0(
      unique( styles_distribution$styles[test_style] ), 
      collapse = ", "
    )
    stop(
      "Certaines styles de la table '", substitute(styles_distribution),
      "' sont introuvables dans les arguments : ",
      missing_styles
    )
  }
  ##### /\ #####
  
  
  ##### 2/ Mise en forme du wb #####
  # -- liste des feuilles du wb
  sheet_name_list <- unique(styles_distribution$sheets)
  
  for (sheet_name in sheet_name_list) {
    # sheet_name <- sheet_name_list[1] # debug
    
    tmp_sheet <- styles_distribution %>% filter(sheets == sheet_name)
    # -- liste des styles
    style_name_list <- tmp_sheet$styles
    
    for (style_name in style_name_list) {
      # style_name <- style_name_list[1] # debug
      tmp_style <- tmp_sheet %>% filter(styles == style_name)
      
      # -- récupération du style
      style <- styles_list[[which(names(styles_list) == style_name)]]
      
      # -- paramètres
      rows <- with(tmp_style, start_row:end_row)
      cols <- with(tmp_style, start_col:end_col)
      gridExpand <- tmp_style$gridExpand
      sep_var <- tmp_style$separate_var
      
      # -- ajout du style
      addStyle(wb, sheet_name, style, rows, cols, gridExpand)
      
      # -- séparation des lignes
      if (!is.na(sep_var)) {
        df <- readWorkbook(wb, sheet_name)
        rows <- which(!duplicated(df[sep_var]))
        addStyle(
          wb, sheet_name, style_separate_row, 
          rows = c(rows[-1], dim(df)[1] + 1), 
          cols = 1:dim(df)[2], 
          gridExpand = T, stack = T
        )
      }
      
      # # -- widths et heights
      # removeColWidths(wb, sheet_name, cols)
      # removeRowHeights(wb, sheet_name, rows)
      # setColWidths(wb, sheet_name, cols, widths = "auto")
      # setRowHeights(wb, sheet_name, rows, heights = "auto")
      
    } # end of loop style_name_list
  } # end of loop sheet_name_list
  ##### /\ #####
  
  # retour de la fonction layout_wb
}



##### fonction d'extraction du cadastre #####
##### 1/ Initialisation #####
# -- import des library
library(easypackages)
packages("sf", "dplyr", "tcltk", "openxlsx", "tools", "units", "readxl")

# -- choix du répertoire de travail
rep <- tk_choose.dir(caption = "Choix du r\u00E9pertoire de travail")
name <- basename(rep)
##### /\ #####

##### 2/ Préparation des données #####
# -- 1.1/ remplissage du classeur Excel modèle avec les noms de communes, 
# les parcelles, les sections et les préfixes (s'il y en a)

# -- 1.2/ sélection (voire téléchargement si nécessaire) des communes sur Etalab
##### /\ #####

##### 3/ import du shape présélectionné #####
# -- sélection du/des shape(s)
cad_FILE <- tk_choose.files(
  default = "", 
  caption = "Choix du/des shape(s) du cadastre",
  filters = matrix(c(".shp", ".shp"), 1, 2)
)
print(cad_FILE) # debug
# TODO : connexion à cadastre etalab avec choix des communes ?

# -- import
cad_SHP <- c()
for (i in 1:length(cad_FILE)) {
  # print(i) # debug
  # lecture des shapes
  cad_TEMP <- st_read(cad_FILE[i], quiet = T)
  
  # sauvegarde
  cad_SHP <- cad_SHP %>% rbind(cad_TEMP)
}

# -- mise en forme 
cad_SHP <- 
  cad_SHP %>% 
  mutate(
    code_insee = as.character(commune), 
    section = as.character(section), 
    numero = as.numeric(as.character(numero)), 
    prefixe = as.character(prefixe)
  )
##### --- /\ --- #####


##### 4/ Import de la liste des parcelles (proprietaire - commune - section - numero - prefixe) --- #####
# -- choix du classeur contenant la liste des sections, numéros, ... à extraire :
ext_FILE <-
  tk_choose.files(
    default = "", 
    caption = "Choix du classeur contenant la liste des sections-num\u00E9ros \u00E0 extraire",
    filters = matrix(c(".xlsx", ".xlsx"), 1, 2)
  )
print(ext_FILE)
rep_data <- dirname(ext_FILE)

# -- import + mise en forme 
ext_DF <- 
  read_xlsx(ext_FILE, sheet = "parcelles") %>% # read.xlsx ?
  rename(
    proprietaire = "Propri\u00E9taire", 
    nom_commune = "Nom Commune", 
    section = "Section", 
    numero = "Num\u00E9ro",
    pref = "Pr\u00E9fixe"
  ) %>% 
  # mise en forme des colonnes pour jonction avec shape cadastre
  mutate(
    nom_commune = toupper( as.character(nom_commune) ), 
    section = as.character(section), 
    numero = as.numeric(as.character(numero)),
    pref = as.character(pref), 
    id_parc = 1:dim(ext_DF)[1]
    id_parc = as.integer(id_parc)
  )

# -- autres éléments d'import
nb_parcelles <- dim(ext_DF)[1]
wb <- loadWorkbook(ext_FILE)
##### /\ #####


##### 5/ Import du fichier des communes et récupération du numéro INSEE --- #####
# -- chargement de l'archive Communes 
# -> permet d'apposer le code INSEE au nom de la commune dans le classeur Excel 
# => permet de faire la jonction entre le nom de la commune (classeur Excel) et 
# le shape du cadastre (code INSEE)
load("tables/Communes_2019_L93.rda")
# communes_DF <- communes_SHP %>% 
#   st_set_geometry(NULL)
com_SHP <- 
  Communes_SHP %>% 
  mutate(
    nom_commune = as.character(nom_commun),
    code_insee = as.character(insee_comm)
  ) %>% 
  select(nom_commune, code_insee)

# -- jonction avec le df des parcelles de propriétaire : 
get_INSEE <-
  ext_DF %>%
  left_join(com_SHP, by = "nom_commune") %>%
  select(proprietaire, nom_commune, code_insee, section, numero, pref, id_parc)

# -- sécurité sur le nombre de parcelles reconnues
# N.B.3 : Vérifier qu'on a bien une parcelle pour chaque numéro INSEE. Sinon, cela signifie qu'il y a eut un changement dans le cadastre -> contacter le commanditaire
if (dim(get_INSEE)[1] != nb_parcelles) {
  stop("le nombre de parcelles r\u00E9cup\u00E9r\u00E9es apr\u00E8s jonction avec les communes (com_SHP) ne correspond pas avec le classeur import\u00E9 (ext_DF)")
  return(get_INSEE)
}

# -- insertion de la feuille dans le wb du cadastre : 
# N.B : rajouter feuilles parcelles, parcelles-insee puis matrice_cadastrale
# wb <- loadWorkbook(ext_FILE) # debug
# wb <- loadWorkbook(file.path(rep_data, "cadastre.xlsx")) # debug

if (!"parcelles-insee" %in% sheets(wb)) {
  get_INSEE <- 
    get_INSEE %>% 
    rename(
      "Propri\u00E9taire" = proprietaire,
      "Nom Commune"= nom_commune,
      "Code INSEE" = code_insee,
      "Section" = section,
      "Num\u00E9ro" = numero,
      "Pr\u00E9fixe" = pref
    )
  # paramètre : attribution des styles
  styles_distribution <- data.frame(
    sheets = c(
      rep("parcelles", 2),
      rep("parcelles-insee", 2)
    ),
    styles = c("style_title", "style_general"),
    start_row = c(
      c(1, 2),
      c(1, 2)
    ),
    end_row = c(
      c(1, nb_parcelles + 1),
      c(1, nb_parcelles + 1)
    ),
    start_col = c(
      c(1, 1),
      c(1, 1)
    ),
    end_col = c(
      c(5, 5), # dim(ext_DF)[2]
      c(7, 7) # dim(ext_DF)[2]
    ),
    gridExpand = c(TRUE, TRUE),
    separate_var = c(
      c(NA, 2), #"Nom.Commune"
      c(NA, 2) #"nom_commune"
    ),
    stringsAsFactors = F
  )
  
  # wb <- loadWorkbook(ext_FILE) # debug
  # rajout d'une feuille
  addWorksheet(wb, sheet = "parcelles-insee")
  # écriture des données
  writeData(wb, sheet = "parcelles-insee", get_INSEE)
   # mise en forme
  layout_wb(wb, styles = styles_list, styles_distribution)
   # sauvegarde du classeur
  saveWorkbook(wb, file = file.path(rep_data, "cadastre.xlsx"), overwrite = T)
}


##### 6/ Obtention du parcellaire = jonction du classeur avec les parcelles + code INSEE avec le shape des parcelles de la commune #####
# -- import de la liste des parcelles de propriétaires
ext_DF <-
  read_xlsx(ext_FILE, sheet = "parcelles-insee") %>%
  rename(
    proprietaire = "Propri\u00E9taire", 
    nom_commune = "Nom Commune", 
    code_insee = "Code INSEE",
    section = "Section", 
    numero = "Num\u00E9ro",
    pref = "Pr\u00E9fixe"
  )

# -- jonction de tables - extraction des parcelles depuis le cadastre global
cad_ext_SHP <- 
  cad_SHP %>% 
  right_join(ext_DF, by = c("code_insee", "section", "numero")) %>% 
  mutate(test_prefixe = ifelse(is.na(pref), prefixe, pref)) %>% 
  filter(prefixe == test_prefixe) %>% 
  mutate(
    prefixe = test_prefixe,
    test_prefixe = NULL,
    pref = NULL
  )

# -- sécurité si doublon (car 2 préfixes pour une même parcelle) : 
dupl_df <- 
  cad_ext_SHP %>% 
  add_count(id_parc) %>% 
  filter(n > 1) %>% 
  select(commune, prefixe, section, numero)
if (dim(dupl_df)[1] > 0) {
  # warning
  warning(
    "Attention : doublon de parcelle. Voir le classeur 'doublons_parcelles.xlsx' : vérifier les préfixes et surfaces et corriger dans le classeur cadastre.xlsx", 
    immediate. = TRUE
  )
  
  # édition des erreurs au format Excel
  doublons_parcelles <- 
    cad_ext_SHP %>% 
    right_join(dupl_df, by = c("commune", "prefixe", "section", "numero")) %>% 
    st_set_geometry(NULL)
  write.xlsx(
    doublons_parcelles, 
    file = file.path(rep, "doublons_parcelles.xlsx")
  )
}

# -- création du répertoire de sortie
output_dir <- file.path(rep, "out/SIG/vecteurs/perimetres")
dir.create(path = output_dir, showWarnings = F, recursive = T)

# -- écriture du shape (L93)
st_write(
  cad_ext_SHP, 
  dsn = file.path(output_dir, "cadastre"), 
  layer = paste0("parcelles_", name, "_L93.shp"), 
  driver = "ESRI Shapefile", 
  delete_layer = T, quiet = T, update = T
)
##### /\ #####

##### 7/ Création du périmètre de la forêt (fusion des parcelles cadastrales) #####
# -- simplification des périmètres
perim_SHP <- 
  cad_ext_SHP %>% 
  mutate(
    parcelle = paste0(section, "-", numero),
    parcelle = ifelse(
      prefixe == "000", #is.na(prefixe) | 
      parcelle, 
      paste0(parcelle, "-", prefixe)
    )
  ) %>% 
  group_by(code_insee, nom_commune, proprietaire) %>% 
  summarise(
    contenance = sum(contenance), 
    parcelles = paste0(parcelle, collapse = ", ")
  ) %>% 
  ungroup() %>% 
  st_as_sf()

# -- export du périmètre de la forêt au format .shp 
st_write(
  perim_SHP, 
  dsn = output_dir, 
  layer = paste0("perim_", name, "_L93.shp"),
  driver = "ESRI Shapefile", 
  delete_layer = T, quiet = T, update = T
)
msg <- tk_messageBox(
  type = "ok",
  message = "Shapes des parcelles et du p\u00E9rim\u00E8tre de la for\u00EAt \u00E9dit\u00E9s"
)

# -- export du parcellaire au format Excel
matrice_cadastrale <- 
  cad_ext_SHP %>% 
  st_set_geometry(NULL) %>%
  select(
    proprietaire, nom_commune, code_insee, 
    numero, section, prefixe, contenance
    ) %>% 
  rename(
    "Propri\u00E9taire" = proprietaire,
    "Nom Commune"= nom_commune,
    "Code INSEE" = code_insee,
    "Section" = section,
    "Num\u00E9ro" = numero,
    "Pr\u00E9fixe" = prefixe,
    "Surface (m2)" = contenance
  )

# paramètre : attribution des styles
styles_distribution <- data.frame(
  sheets = rep("matrice_cadastrale", 2),
  styles = c("style_title", "style_general"),
  start_row = c(1, 2),
  end_row = c(1, nb_parcelles + 1),
  start_col = c(1, 1),
  end_col = c(7, 7),
  gridExpand = c(TRUE, TRUE),
  separate_var = c(NA, 2),
  stringsAsFactors = F
)

# wb <- loadWorkbook(ext_FILE)
wb <- createWorkbook()
# rajout d'une feuille
addWorksheet(wb, sheet = "matrice_cadastrale")
# écriture des données
writeData(wb, sheet = "matrice_cadastrale", matrice_cadastrale)
# mise en forme
layout_wb(wb, styles = styles_list, styles_distribution)
# sauvegarde du classeur
saveWorkbook(wb, file = file.path(rep_data, "matrice_cadastrale.xlsx"), overwrite = T)

msg <- tk_messageBox(
  type = "ok",
  message = "Matrice cadastrale \u00E9dit\u00E9e dans le fichier 'data/excel/cadastre/matrice_cadastrale.xlsx'"
)

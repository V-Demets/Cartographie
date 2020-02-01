# -- set up
# rm(list = ls())

source("scripts/dissolve_shape.R")

# libraries
library(easypackages)
packages("sf", "dplyr", "tcltk", "openxlsx", "tools", "nngeo")

# wd
rep <- "/Users/Valentin/Travail/Outils/Cartographie/dossiers/AFI"
rep <- tk_choose.dir(
  default = getwd(),
  caption = "Choix du répertoire de travail"
)
setwd(rep)




##### fonction pour échantillonner des placettes dans un périmètre #####
create_PP_grid <- function(
  rep = NULL, sf_path = NULL, buffer_dist = NULL# nb_plot = NULL,
) {
  # -- set up
  # perimeter_sf <- sf # debug
  # nb_plot <- 10 # debug
  
  # -- read sf
  # path
  if (is.null(sf_path)) {
    sf_path <- tk_choose.files(
      caption = "Choix du shape", 
      multi = T, 
      filters = matrix(c("fichier shape", ".shp"), 1, 2, byrow = T)
    )
    sf_path <- gsub(paste0(rep, "/"), "", sf_path)
  } # end of cond 'is.null(sf_path)'
  # read
  perimeter_sf <- sf_path %>% st_read(quiet = T)
  # area
  perim_area <- sum(st_area(perimeter_sf)) / 10000
  
  # -- plot number
  # build reminder str for select message
  CV_hypothetical <- c(0.15, 0.30, 0.50)
  nb_plot_recommended_min <- ceiling( (2 * CV_hypothetical / 0.05) ^ 2 ) # t estimated for 100 plots
  nb_plot_recommended_max <- ceiling( (2 * CV_hypothetical / 0.1) ^ 2 )
  nb_plot_recommended <- ceiling( (2 * 0.3 / 0.05) ^ 2 ) # recommanded for Er = 8 % and CV = 30 %
  nb_plot_recommended_str <- paste0(
    "CV estim\u00E9 : ", CV_hypothetical * 100, " \u0025 \u002D\u003E ",
    "entre ", nb_plot_recommended_max, " (Er \u003D 10 \u0025) et ", nb_plot_recommended_min, " (Er \u003D 5 \u0025) placettes"
  )
  # choose number of plots
  nb_plot <- tk_select.list(
    choices = 1:200,
    preselect = nb_plot_recommended,
    multiple = F,
    title = paste0(
      "Choix du nombre de placettes (surface totale du domaine d'inventaire = ", 
      round(perim_area, 4), " ha)\n\nRappel :\n", 
      paste0(nb_plot_recommended_str, collapse = "\n")
    )
  )
  
  # -- buffer dist
  # buffer_dist <- 23 debug
  if (is.null(buffer_dist)) {
    buffer_dist <- tk_select.list(
      choices = seq(5, 35, 5),
      preselect = 25,
      multiple = F,
      title = "Choix d'une distance buffer"
    )
  } # end of cond 'is.null(buffer_dist)'
  
  # -- apply buffer dist
  buff_perimeter_sf <- 
    perimeter_sf %>% 
    sf_dissolve(buffer_dist = 10) %>% 
    st_buffer(-buffer_dist) %>% 
    st_union() # TODO : comprendre pourquoi nécessaire d'appliquer st_union (cas pour audenge)
  
  plot(st_geometry(perimeter_sf)) # debug
  plot(st_geometry(buff_perimeter_sf), add = T, border = "red") # debug
  
  # -- création du grid de placettes
  count <- 0
  plot_grid <- c()
  while (length(plot_grid) != nb_plot & count < 100) {
    # nb_plot <- nb_plot + 1
    plot_grid <- st_sample(
      buff_perimeter_sf, 
      size = nb_plot, 
      type = "regular"
    )
    # test <- length(plot_grid)[1]
    count = count + 1
    print(count) # debug
    # print(paste0("plot", length(plot_grid))) # debug
  }
  
  # get dist btw plots
  dist_plot <- c()
  for (i in 1:(length(plot_grid) - 1)) {
    dist_tmp <- st_distance(plot_grid[i], plot_grid[i + 1], by_element = TRUE)
    dist_plot <- min(dist_plot, dist_tmp)
  }
  
  # -- build plot_grid and intersect
  answ <- tk_messageBox(
    type = "yesno",
    message = paste0(
      "les placettes sont espacées de ", 
      dist_plot,
      " m\n\nSouhaitez-vous ajuster le grid de placettes en paramétrant la taille de la maille (distance entre les placettes) ?"
    )
  )
  
  while (answ == "yes") {
    dist_plot_recommanded <- floor(dist_plot / 5 + 0.5) * 5
    dist_plot <- tk_select.list(
      choices = seq(
        floor(dist_plot_recommanded * 0.75 / 5 + 0.5) * 5, 
        floor(dist_plot_recommanded * 1.25 / 5 + 0.5) * 5, 
        5
      ),
      preselect = dist_plot_recommanded,
      multiple = F,
      title = "Choix de la taille du maillage"
    )
    # plot(st_geometry(perimeter_sf)) # debug
    # plot(st_geometry(buff_perimeter_sf), add = T, border = "red") # debug
    
    plot_grid <- st_make_grid(
      buff_perimeter_sf,
      cellsize = c(dist_plot, dist_plot),
      what = "centers",
      offset = st_bbox(buff_perimeter_sf)[c("xmin", "ymin")] + runif(2, -10, 10)
    ) %>% 
      st_intersection(buff_perimeter_sf)
    plot(plot_grid, col = "green", add = T)
    
    # get nb plot
    nb_plot <- length(plot_grid)
    
    # get dist btw plots
    dist_plot <- c()
    for (i in 1:(length(plot_grid) - 1)) {
      dist_tmp <- st_distance(plot_grid[i], plot_grid[i + 1], by_element = TRUE)
      dist_plot <- min(dist_plot, dist_tmp)
    }
    
    # -- new answer needed
    answ <- tk_messageBox(
      type = "yesno",
      message = paste0(
        "Le grid créé comporte ", nb_plot, " placettes, espacées de ", 
        dist_plot,
        " m\n\nSouhaitez-vous r\u00E9ajuster le grid de placettes en paramétrant la taille de la maille (distance entre les placettes) ?"
      )
    )
  } # end of cond 'answ == "yes"'
  
  # -- final format
  plot_grid <- 
    plot_grid %>% 
    st_sf() %>% 
    mutate(
      X = st_coordinates(plot_grid)[, 1],
      Y = st_coordinates(plot_grid)[, 2]
    ) %>% 
    arrange(desc(Y), X) %>% 
    mutate(NumPlac = 1:nb_plot) %>% 
    select(NumPlac, X, Y, geometry)
  
  # -- writing grid
  # directory path
  save_path <- gsub("perimetres", "placettes", dirname(sf_path)) # gsub(rep, "", dirname(sf_path))
  save_path <- gsub("data", "out", save_path)
  dir.create(
    save_path, showWarnings = F, recursive = T
  )
  
  # writing
  st_write(
    plot_grid, 
    dsn = save_path, 
    layer = paste0("grid_", nb_plot, "_2suppr_placettes_L93"), 
    driver = "ESRI Shapefile", quiet = T,
    update = TRUE, delete_layer = TRUE
  )
  
  # -- ending message
  msg <- tk_messageBox(
    type = "ok",
    message = paste0(
      "Le grid créé (grid_", 
      nb_plot, 
      "_2suppr_placettes_L93) a été enregistré à l'emplacement : ", 
      save_path
    ),
    icon = "info"
  )
}

create_PP_grid(rep)

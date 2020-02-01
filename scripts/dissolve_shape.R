# -- set up
# libraries
library(easypackages)
packages("sf", "dplyr", "tcltk", "openxlsx", "tools")

# wd
# rep <- "/Users/Valentin/Travail/Outils/Cartographie/dossiers/AFI"
# setwd(rep)

##### function to dissolve pultipolygon #####
sf_dissolve <- function(
  sf = NULL, buffer_dist = 1, rewrite = FALSE
) { #, rep = NULL
  # # -- set wd
  # setwd(rep)
  
  # -- sf file
  if (is.null(sf)) {
    # choose file path
    sf_path <- tk_choose.files(
      caption = "Choix du shape", 
      multi = T, 
      filters = matrix(c("fichier shape", ".shp"), 1, 2, byrow = T)
    )
    # read
    sf <- 
      sf_path %>% 
      st_read(quiet = T)
    # sf_path <- gsub(paste0(rep, "/"), "", sf_path)
  }
  # sf_path <- "data/SIG/vecteurs/perimetres/140-unknown/140-unknown.shp" # debug
  
  # -- dissolution
  # plot(st_geometry(sf, NULL)) # debug
  sf <- 
    sf %>% 
    st_buffer(dist = buffer_dist) %>% 
    st_union() %>% 
    # st_combine() %>% 
    st_buffer(dist = -buffer_dist) %>% 
    st_cast("POLYGON")
  # plot(st_geometry(sf, NULL))
  
  # TODO : AUTRE SOLUTION (avec buffer_dist = 0)
  # sf <- 
  #   sf_path %>% 
  #   st_read(quiet = T) %>% 
  #   st_buffer(dist = buffer_dist) %>% 
  #   st_union() %>% 
  #   nngeo::st_remove_holes()
  
  
  # -- rewrite shape
  if (rewrite == T) {
    # parameters
    sf_dsn_path <- dirname(sf_path)
    sf_layer <- gsub(".shp", "_dissolved", basename(sf_path))
    # call
    st_write(
      sf,
      dsn = sf_dsn_path,
      layer = sf_layer, 
      driver = "ESRI shapefile",
      update = T, 
      # quiet = T, 
      delete_layer = T
    )
  } # end of cond 'rewrite == T'
  
  # -- end message
  # cat("Shape '", sf_layer, "' written in '", sf_dsn_path, "'", sep = "")
  
  # -- return from sf_dissolve function
  return(sf)
}

# sf_dissolve(rep) # call

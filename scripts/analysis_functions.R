require(tidyverse)
require(lubridate)
require(sf)


#' @description A simple function that will take a dataframe, filter by records inside ANP, and return a
#' cleaned dataframe. IMPORTANT: This function only work for lat long data seperated
#' in two different columns (one for lat and one for long).
#'
#' @param df Name of the dataframe you have read in.
#' @param park The quoted name of the national park/monument that you want to filter records by. REQUIRES
#' name format to be exact. Find a list of the 427 park names at this link: https://rpubs.com/klima21/filternps.
#' @param lat The quoted column name that is your latitude data.
#' @param long The quoted column name that is your longitude data.
#'
#' @return Returns a dataframe of the same structure, but filtered to records inside
#' the specified park/monument. Some column names may change.
#'
#' @example
#'
#' # Read in data from working directory
#' bird.dat <- read.csv("ebird_mappingloc_20220217.csv")
#'
#' # Use filter_nps function to filter the bird.dat dataframe to records inside Acadia National Park
#' bird.anp <- filter_nps(bird.dat, "Acadia National ParK", lat = "y", long = "x")
#'
#' @export

filter_nps <- function(dat, park, lat, long) {
  
  sf::sf_use_s2(FALSE)
  
  if (park == "Acadia National Park") {
    
    
    acad.bounds <- sf::read_sf("data/acad_boundary/acad_feeboundary_polygon.shp")
    
     
    dat2 <- dat %>% 
      rename(x = paste(long), y = paste(lat)) %>% 
      mutate(longitude.keep = x,
             latitude.keep = y) %>% 
      sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(acad.bounds))
    
    
    output <- sf::st_join(dat2, acad.bounds, left = F) %>% 
      st_set_geometry(., NULL) %>% 
      dplyr::select(-c(FID)) %>% 
      dplyr::select(everything(), latitude = latitude.keep, longitude = longitude.keep)
    
  } else {
    
    nps.bounds <- sf::read_sf("data/nps_boundary/nps_boundary.shp") %>% 
      st_transform(4326) %>% 
      filter(UNIT_NAME == paste(park))
    
    
    if (length(nps.bounds) < 1) {
      stop("Function returned a park with 0 polygons. The park name does not exist. Go to https://rpubs.com/klima21/filternps for a list of valid park names.")
    }
    
    
    dat2 <- dat %>% 
      rename(x = paste(long), y = paste(lat)) %>% 
      mutate(longitude.keep = x,
             latitude.keep = y) %>% 
      sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(nps.bounds))
    
    
    # dat2 %>% 
    #   mutate(intersect = as.integer(st_intersects(geometry, nps.bounds))) %>% 
    #   filter(!is.na(intersect))
    
    
    output <- sf::st_join(dat2, nps.bounds, left = F) %>% 
      st_set_geometry(., NULL) %>%
      dplyr::select(-c(OBJECTID:Shape_Area)) %>% 
      dplyr::select(everything(), latitude = latitude.keep, longitude = longitude.keep)
  }
  
  return(output)
}




#' @description A simple function that will take a data frame, filter by records inside ANP, and return a
#' cleaned data frame. IMPORTANT: This function only work for lat long data separated
#' in two different columns (one for lat and one for long).
#'
#' @param df Name of the data frame you have read in.
#' @param lat The quoted column name that is your latitude data.
#' @param long The quoted column name that is your longitude data.
#'
#' @return Returns a data frame of the same structure, but filtered to records inside
#' the specified park/monument. Some column names may change.
#'
#' @example
#'
#' # Read in data from working directory
#' bird.dat <- read.csv("ebird_mappingloc_20220217.csv")
#'
#' # Use filter_nps function to filter the bird.dat data frame to records inside Acadia National Park
#' bird.anp <- filter_nps(bird.dat, lat = "y", long = "x")
#'
#' @export

filter_acad_gbif <- function(dat, lat, long) {
  
  sf::sf_use_s2(FALSE)
  
  
  acad.bounds <- sf::read_sf("data/acad_boundary/ACAD_ParkBoundary_PY_202004.shp") %>% 
    st_transform(4326)
  
  
  dat2 <- dat %>% 
    rename(x = paste(long), y = paste(lat)) %>% 
    mutate(longitude.keep = x,
           latitude.keep = y) %>% 
    sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(acad.bounds))
  
  
  dat2 %>% 
    mutate(intersect = as.integer(st_intersects(geometry, acad.bounds))) %>% 
    filter(!is.na(intersect))
  
  
  output <- sf::st_join(dat2, acad.bounds, left = F) %>% 
    st_set_geometry(., NULL) %>% 
    select(-c(CLASS, Acres, Hectares, SHAPE_Leng, SHAPE_Area)) %>% 
    select(everything(), latitude = latitude.keep, longitude = longitude.keep)
  
  
  return(output)
  
}




#' @description A simple function that will take a dataframe, filter by records inside a given
#' park, and return a cleaned dataframe. This function only work for lat long data separated
#' in two different columns (one for lat and one for long).
#'
#' @param df Name of the dataframe you have read in.
#' @param park The quoted name of the national park/monument that you want to filter records by. REQUIRES
#' name format to be exact. Find a list of the 427 park names at this link: https://rpubs.com/klima21/filternps.
#' @param lat The quoted column name that is your latitude data.
#' @param long The quoted column name that is your longitude data.
#'
#' @return Returns a dataframe of the same structure, but filtered to records inside
#' the specified park/monument. Some column names may change.
#'
#' @export

filter_gbif_to_park <- function(dat, park, lat, long) {
  
  if (!file.exists("data/nps_boundary")) {
    download('https://irma.nps.gov/DataStore/DownloadFile/673366', destfile = "data/nps_boundary.zip")
  }
  
  
  if (!file.exists("data/nps_boundary")) {
    unzip("data/nps_boundary.zip", exdir = "data/nps_boundary")
  }
  

  nps.bounds <- readOGR("data/nps_boundary/nps_boundary.shp", verbose = FALSE)
  
  
  select.bounds <- nps.bounds[nps.bounds@data$UNIT_NAME==paste(park), ]
  
  
  df <- df %>% rename(latitude = paste(lat), longitude = paste(long))
  
  df$"long" <- df$longitude
  df$"lat" <- df$latitude
  
  coordinates(df) <- c("long", "lat")
  
  slot(df, "proj4string") <- slot(select.bounds, "proj4string")
  
  output <- over(select.bounds, df, returnList = TRUE)
  
  output.df <- data.frame(output) %>% 
    rename_with(~str_replace(., "X15.", ""), everything())
  
  return(output.df)
  
}




#' Function summarizes iNaturalist observations for watchlist species
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and the path for the outputs. It creates .csv
#' files of all the species in the data frame listed as non-native and invasive as well 
#' as some summary statistics. This is done at both the federal and state levels. Data is 
#' written out to the provided directory.
#'
#' @inheritParams None
#' @return A dataframe of recent iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @param output.path: The path you want the summary statistic tables to be written to.
#' @seealso None
#' @export
#' @examples  
#' watchlist_species(inat_lastweek, "outputs/te_species")

watchlist_species <- function(x, output.path) {
  
  ## Check to make sure that parameter inputs are correct
  # output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  # Stop this output from showing
  options(readr.show_col_types = FALSE)
  
  
  # Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  ### THREATENED/ENDANGERED
  ## Federal
  # Read in the file and filter for the T, E, and SC species
  fed_te_sp <- read_csv("data/federal_list_maine.csv") %>% 
    rename_with(tolower, everything()) %>% 
    dplyr::select(scientific.name = "scientific name", common.name = "common name",
           listing.status = "esa listing status") %>% 
    mutate(level = "federal",
           listing.status = tolower(listing.status),
           listing.status = paste0("federally ", listing.status)) %>% 
    dplyr::select(-level)
  
  
  ## State
  # Read in the file and filter for the T, E, and SC species
  state_te_sp <- read_csv("data/maine_thrt_end_list.csv") %>% 
    mutate(level = "state",
           listing.status = tolower(listing.status),
           listing.status = paste0("state ", listing.status)) %>% 
    dplyr::select(-level)
  
  
  # All T, E species from the last week
  te_specieslist_federal <- x %>% 
    filter(scientific.name %in% fed_te_sp$scientific.name) %>% 
    dplyr::select(scientific.name, common.name, observed.on, place.guess, latitude, longitude) %>% 
    left_join(fed_te_sp, by = "scientific.name") %>% 
    dplyr::select(scientific.name, common.name = common.name.x, observed.on, 
           location = place.guess, latitude, longitude, listing.status)
  
  
  # All T, E species from the last week
  te_specieslist_state <- x %>% 
    filter(scientific.name %in% state_te_sp$scientific.name) %>% 
    dplyr::select(scientific.name, common.name, observed.on, place.guess, latitude, longitude) %>% 
    left_join(state_te_sp, by = "scientific.name") %>% 
    dplyr::select(scientific.name, common.name = common.name.x, observed.on, 
           location = place.guess, latitude, longitude, listing.status) %>% 
    filter(scientific.name != "Sterna dougallii")
  
  # Combine and export
  all_te_sp <- dplyr::bind_rows(te_specieslist_federal, te_specieslist_state) #%>% 
  # mutate(link = paste0("<a href= ", "'", url, "' target='_blank'>view record here</a>")) %>% 
  # dplyr::select(-url)
  
  write.csv(all_te_sp, paste(output.path, "te_specieslist.csv", sep = "/"), row.names = F)
  
  
  
  ## RARE, INVASIVE, PESTS
  # Rare native species list
  listsp <- read_excel("data/acad_watchlist_species.xlsx", .name_repair = custom_name_repair) 
  
  rares <- listsp %>% 
    filter(status == "rare native" | status == "insect")
  
  invasive_ne <- listsp %>% 
    filter(status == "invasive not established" |
           status == "invasive established" |
           status == "pest disease")
  
  
  # Native but rare
  rares_obs <- x %>% 
    filter(scientific.name %in% rares$scientific.name) %>% 
    arrange(desc(observed.on))
  
  
  # Invasives and pests
  invasive_sp <- x %>% 
    filter(scientific.name %in% invasive_ne$scientific.name)
  
  inv2 <- listsp %>% 
    filter(grepl("spp.$", scientific.name)) %>% 
    mutate(genus = str_remove(scientific.name, "\\s\\w*\\."))
  
  invasive_gen <-  x %>% 
    mutate(genus = str_remove(scientific.name, "\\s\\w*")) %>% 
    filter(genus %in% inv2$genus) %>% 
    dplyr::select(-genus) %>% 
    filter(scientific.name != "Lonicera canadensis")
  
  invasive_obs <- rbind(invasive_sp, invasive_gen) %>% 
    arrange(desc(observed.on))
  
  # Export
  write.csv(rares_obs, paste(output.path, "rare_specieslist.csv", sep = "/"), row.names = F)
  write.csv(invasive_obs, paste(output.path, "invasive_pestslist.csv", sep = "/"), row.names = F)
  
}


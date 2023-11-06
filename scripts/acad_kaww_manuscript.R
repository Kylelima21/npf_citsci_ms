### Acadia National Park (ACAD) and Katahdin Woods and Waters National Monumnet (KAWW)
### iNaturalist and eBird synthesis script for manuscript
### Schoodic Institute at Acadia National Park, 2023


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(data.table)
library(auk)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(webshot2)
library(sf)
library(ggmap)
library(lwgeom)
library(purrr)
library(directlabels)
library(cowplot)
library(readxl)
library(raster)
library(geosphere)
library(png)
library(grid)

source("functions/analysis_functions.R")




#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

### ACAD data import ###

## Read, format, filter to ACAD, and clean the iNaturalist data
inatA <- tibble(read.csv("data/acad_inat_obs.csv")) %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude") %>% 
  mutate(year = year(observed_on),
         month = month(observed_on)) %>% 
  rename_with(~str_replace_all(., "_", "."), .cols = everything()) %>% 
  dplyr::select(-c(id, observed.on.string, created.at, updated.at, 
                   license:num.identification.disagreements, 
                   oauth.application.id, private.place.guess:species.guess)) %>% 
  mutate(taxon = str_extract(scientific.name, "^\\w*"),
         species = str_remove(scientific.name, "^\\w*"),
         species = str_trim(species, "both"),
         subspecies = str_extract(species, "\\s\\w*"),
         subspecies = str_trim(subspecies, "both"),
         species = str_extract(species, "[^\\s]+"),
         species = ifelse(species == "×", NA, species),
         park = "ACAD") %>% 
  dplyr::select(common.name, scientific.name, taxon, species, subspecies, 
                iconic.taxon.name, observed.on, year, month, quality.grade, latitude, 
                longitude, user.login, everything()) %>% 
  mutate(scientific.name = ifelse(scientific.name == "Heterocampa umbrata", "Heterocampa pulverea", scientific.name),
         species = ifelse(species == "umbrata", "pulverea", species))


## Read, format, filter to ACAD, and clean the eBird data
ebdA <- tibble(read.delim("data/ebd_US-ME_relFeb-2023.txt", header = T, quote = "")) %>% 
  dplyr::select(c('COMMON.NAME', 'SCIENTIFIC.NAME', 'CATEGORY', 'OBSERVATION.DATE', 'OBSERVATION.COUNT', 
                  'DURATION.MINUTES', 'SAMPLING.EVENT.IDENTIFIER', 'OBSERVER.ID', 'NUMBER.OBSERVERS',
                  'PROTOCOL.TYPE', 'ALL.SPECIES.REPORTED', 'EFFORT.DISTANCE.KM', 'LOCALITY', 'COUNTY', 
                  'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'checklist.id'='SAMPLING.EVENT.IDENTIFIER', 'latitude'='LATITUDE', 'longitude'='LONGITUDE',
         'observer.id'='OBSERVER.ID', 'category'='CATEGORY', 'county'='COUNTY', 
         'protocol'='PROTOCOL.TYPE', 'all.species.reported'='ALL.SPECIES.REPORTED', 
         'duration.min'='DURATION.MINUTES', 'num.observers'='NUMBER.OBSERVERS', 
         'distance.km'='EFFORT.DISTANCE.KM') %>% 
  mutate(park = "ACAD") %>% 
  filter(obs.date <= "2022-12-31") %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude")


## Read in the ACAD basemap for figures
acad.bm <- sf::read_sf("data/acad_boundary/formapping.shp")

## Read in the ACAD boundary layer
acad.bounds <- sf::read_sf("data/acad_boundary/acad_feeboundary_polygon.shp")



#------------------------------------------------#

### KAWW data import ###

## Read, format, filter to KAWW, and clean the iNaturalist data
inatK <- tibble(read.csv("data/kaww_inat_obs_tax.csv")) %>% 
  filter_nps(., "Katahdin Woods and Waters National Monument", "latitude", "longitude") %>% 
  mutate(year = year(observed_on),
         month = month(observed_on)) %>% 
  rename_with(~str_replace_all(., "_", "."), .cols = everything()) %>% 
  dplyr::select(-c(id, observed.on.string, created.at, updated.at, 
                   license:num.identification.disagreements, 
                   oauth.application.id, private.place.guess:species.guess)) %>% 
  mutate(taxon = str_extract(scientific.name, "^\\w*"),
         species = str_remove(scientific.name, "^\\w*"),
         species = str_trim(species, "both"),
         subspecies = str_extract(species, "\\s\\w*"),
         subspecies = str_trim(subspecies, "both"),
         species = str_extract(species, "[^\\s]+"),
         species = ifelse(species == "×", NA, species),
         park = "KAWW") %>% 
  dplyr::select(common.name, scientific.name, taxon, species, subspecies, 
                iconic.taxon.name, observed.on, year, month, quality.grade, latitude, 
                longitude, user.login, everything()) %>%
  filter(observed.on <= "2022-12-31")


## Read, format, filter to KAWW, and clean the eBird data
ebdK <- tibble(read.delim("data/ebd_US-ME_relFeb-2023.txt", header = T, quote = "")) %>% 
  dplyr::select(c('COMMON.NAME', 'SCIENTIFIC.NAME', 'CATEGORY', 'OBSERVATION.DATE', 'OBSERVATION.COUNT', 
                  'DURATION.MINUTES', 'SAMPLING.EVENT.IDENTIFIER', 'OBSERVER.ID', 'NUMBER.OBSERVERS',
                  'PROTOCOL.TYPE', 'ALL.SPECIES.REPORTED', 'EFFORT.DISTANCE.KM', 'LOCALITY', 'COUNTY', 
                  'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'checklist.id'='SAMPLING.EVENT.IDENTIFIER', 'latitude'='LATITUDE', 'longitude'='LONGITUDE',
         'observer.id'='OBSERVER.ID', 'category'='CATEGORY', 'county'='COUNTY', 
         'protocol'='PROTOCOL.TYPE', 'all.species.reported'='ALL.SPECIES.REPORTED', 
         'duration.min'='DURATION.MINUTES', 'num.observers'='NUMBER.OBSERVERS', 
         'distance.km'='EFFORT.DISTANCE.KM') %>% 
  mutate(park = "KAWW") %>% 
  filter_nps(., "Katahdin Woods and Waters National Monument", "latitude", "longitude") %>% 
  filter(obs.date <= "2022-12-31")


## Read in the KAWW boundary layer
kaww.bounds <- sf::read_sf("data/kww_boundary/kww_boundary_polyg.shp")



#------------------------------------------------#

### Figure additions ###

readPNG("")


#------------------------------------------------#
####             Study area maps              ####
#------------------------------------------------#

### Context map ###

## Read in US map
states <- map_data("state")


## Plot context map
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = "gray50", show.legend = F) + 
  coord_fixed(1.3) +
  lims(x = c(-80, -66), y = c(38, 48)) +
  theme_nothing()


## Export figure
ggsave("outputs/forpub/study_area_new_england.png", height = 5.28, width = 5.28, units = "in", dpi = 350)



#------------------------------------------------#

### ACAD map ###

## Create ACAD leaflet
acadmap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addMapPane("polygons", zIndex = 201) %>%
  addMapPane("labels", zIndex = 300) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = providerTileOptions(pane = "labels")) %>% 
  addPolygons(data = acad.bounds, color = "black", fill = T, fillColor = "forestgreen", opacity = 1, fillOpacity = 0.9,
              weight = .5, options = pathOptions(pane = "polygons"))


## View map
acadmap


## Export figure
saveWidget(acadmap, "outputs/temp.html", selfcontained = FALSE)
webshot("outputs/temp.html", file = "outputs/forpub/acad_study_area.png",
        vwidth = 700, vheight = 500,
        cliprect = "viewport")



#------------------------------------------------#

### KAWW map ###

## Define bounds for leaflet map
maxLong = -68.55836 + 0.04
maxLat = 46.12548 + 0.04
minLong = -68.82463 - 0.04
minLat = 45.82705 - 0.04


## Create KAWW leaflet
kawwmap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addMapPane("polygons", zIndex = 201) %>%
  addMapPane("labels", zIndex = 300) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = providerTileOptions(pane = "labels")) %>% 
  addPolygons(data = kaww.bounds, color = "black", fill = T, fillColor = "darkorange", opacity = 1, fillOpacity = 0.9,
              weight = .5, options = pathOptions(pane = "polygons")) %>% 
  fitBounds(minLong, minLat, maxLong, maxLat)


## View map
kawwmap


## Export figure
saveWidget(acadmap, "outputs/temp.html", selfcontained = FALSE)
webshot("outputs/temp.html", file = "outputs/forpub/kaww_study_area.png",
        vwidth = 700, vheight = 500,
        cliprect = "viewport")




#------------------------------------------------#
####         Data Set Summary Stats           ####
#------------------------------------------------#

### Total iNaturalist obs ###

## Combine KAWW and ACAD data sets
inat <- bind_rows(inatA, inatK)

## All observations
length(inatA$common.name) # 51,146
length(inatK$common.name) # 2,100
length(inat$common.name)  # 53,246


## Average submissions per observer
length(inatA$common.name)/length(unique(inatA$user.login)) # 11.12353
length(inatK$common.name)/length(unique(inatK$user.login)) # 23.86364
length(inat$common.name)/length(unique(inat$user.login))   # 11.44337


## Get all obs that are research grade
rgA <- inatA %>% 
  filter(quality.grade == "research") 
length(rgA$common.name)                # 27,566
rgK <- inatK %>% 
  filter(quality.grade == "research") 
length(rgK$common.name)                # 1,276
rg <- inat %>% 
  filter(quality.grade == "research") 
length(rg$common.name)                 # 28,842


## Percentage of observations that are research grade
paste0(round(length(rgA$species)/length(inatA$species)*100, digits = 2), "% of all observations are research grade.")
paste0(round(length(rgK$species)/length(inatK$species)*100, digits = 2), "% of all observations are research grade.")
paste0(round(length(rg$species)/length(inat$species)*100, digits = 2), "% of all observations are research grade.")


### Total species from iNaturalist
## Manipulate the data
inatA_splist <- inatA %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(sci.name = paste(taxon, species, sep = " ")) %>% 
  dplyr::select(sci.name) %>% 
  distinct() %>% 
  rename(scientific.name = sci.name) %>% 
  arrange(scientific.name)
inatK_splist <- inatK %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(sci.name = paste(taxon, species, sep = " ")) %>% 
  dplyr::select(sci.name) %>% 
  distinct() %>% 
  rename(scientific.name = sci.name) %>% 
  arrange(scientific.name)
inat_splist <- inat %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(sci.name = paste(taxon, species, sep = " ")) %>% 
  dplyr::select(sci.name) %>% 
  distinct() %>% 
  rename(scientific.name = sci.name) %>% 
  arrange(scientific.name)


## Determine number of species
paste0("There have been ", length(inatA_splist$scientific.name), " species recorded from iNaturalist research grade observations")
paste0("There have been ", length(inatK_splist$scientific.name), " species recorded from iNaturalist research grade observations")
paste0("There have been ", length(inat_splist$scientific.name), " species recorded from iNaturalist research grade observations")



#------------------------------------------------#

### Total eBird obs ###

## Combine KAWW and ACAD data sets
ebd <- bind_rows(ebdA, ebdK)


## All observations
length(ebdA$common.name) # 472,362
length(ebdK$common.name) # 12,458
length(ebd$common.name)  # 484,820


## Total checklists
ebird_chkA <- ebdA %>% 
  distinct(checklist.id)
ebird_chkK <- ebdK %>% 
  distinct(checklist.id)
ebird_chk <- ebd %>% 
  distinct(checklist.id)

paste0("There have been ", length(ebird_chkA$checklist.id), " checklists submitted by eBird users.")
paste0("There have been ", length(ebird_chkK$checklist.id), " checklists submitted by eBird users.")
paste0("There have been ", length(ebird_chk$checklist.id), " checklists submitted by eBird users.")


## Average checklists per observer
length(ebird_chkA$checklist.id)/length(unique(ebdA$observer.id)) # 6.954525
length(ebird_chkK$checklist.id)/length(unique(ebdK$observer.id)) # 5.161137
length(ebird_chk$checklist.id)/length(unique(ebd$observer.id)).  # 7.053639


## Get all complete checklists
ebirdcompA <- ebdA %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  distinct(checklist.id)
ebirdcompK <- ebdK %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  distinct(checklist.id)
ebirdcomp <- ebd %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  distinct(checklist.id)

length(ebirdcompA$checklist.id) # 35,133
length(ebirdcompK$checklist.id) # 897
length(ebirdcomp$checklist.id)  # 36,030


## Percentage of checklists that are complete
paste0(round(length(ebirdcompA$checklist.id)/length(ebird_chkA$checklist.id)*100, digits = 2), "% of all checklists are complete.")
paste0(round(length(ebirdcompK$checklist.id)/length(ebird_chkK$checklist.id)*100, digits = 2), "% of all checklists are complete.")
paste0(round(length(ebirdcomp$checklist.id)/length(ebird_chk$checklist.id)*100, digits = 2), "% of all checklists are complete.")


### Total species from eBird
## Manipulate the data
ebird_splistA <- ebdA %>% 
  filter(category == "species") %>% 
  dplyr::select(scientific.name) %>% 
  distinct() %>% 
  arrange(scientific.name)
ebird_splistK <- ebdK %>% 
  filter(category == "species") %>% 
  dplyr::select(scientific.name) %>% 
  distinct() %>% 
  arrange(scientific.name)
ebird_splist <- ebd %>% 
  filter(category == "species") %>% 
  dplyr::select(scientific.name) %>% 
  distinct() %>% 
  arrange(scientific.name)

## Determine number of species
paste0("There have been ", length(ebird_splistA$scientific.name), " species recorded by eBird users.")
paste0("There have been ", length(ebird_splistK$scientific.name), " species recorded by eBird users.")
paste0("There have been ", length(ebird_splist$scientific.name), " species recorded by eBird users.")



#------------------------------------------------#

### Park specific science quality data ###

## Calculate percent of ACAD science-ready data
compobsA <- ebdA %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental")

(length(compobsA$common.name) + length(rgA$common.name))/ length(bind_rows(inatA, ebdA)$common.name)
# 0.8438457

## Calculate percent of KAWW science-ready data
compobsK <- ebdK %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental")

(length(compobsK$common.name) + length(rgK$common.name))/ length(bind_rows(inatK, ebdK)$common.name)
# 0.9015661



#------------------------------------------------#

### Total citizen science data set stats ###

## Total species across both data sets
bind_rows(ebird_splist, inat_splist) %>% 
  distinct(scientific.name)               # 2,174 species


## Total number of observations
length(bind_rows(inat, ebd)$common.name) # 538,066




#------------------------------------------------#
####          Cumulative Observers            ####
#------------------------------------------------#

### iNaturalist ###


## Calculate ACAD cumulative observers
cumulativeobA <- inatA %>% 
  group_by(user.login) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year) %>% 
  summarise(observers = length(user.login)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         park = "ACAD") %>% 
  dplyr::select(year, cumsum, park)


## Calculate KAWW cumulative observers
cumulativeobK <- inatK %>% 
  group_by(user.login) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year) %>% 
  summarise(observers = length(user.login)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         park = "KAWW") %>% 
  dplyr::select(year, cumsum, park)


## Bind these data for plotting
inatcumob <- bind_rows(cumulativeobA, cumulativeobK)


## Plot
inatcumob %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(cumulativeobA, year == 2022), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  geom_dl(data = subset(cumulativeobK, year == 2022), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Count (n)") +
  scale_x_continuous(limits = c(1995, 2026), breaks = seq(1990, 2024, by = 5)) +
  theme(legend.position = c(0.13, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual(values = c("ACAD" = "gray60", "KAWW" = "black")) +
  scale_linetype_manual(values = c("ACAD" = 1, "KAWW" = 6))


## Export figure
ggsave(paste0("outputs/forpub/final_cumulative_inat_", str_replace_all(today(), "-", ""), ".png"),
       height = 5.28, width = 8, units = "in", dpi = 500)



#------------------------------------------------#

### eBird ###


## Calculate ACAD cumulative observers
cumulativeobeA <- ebdA %>% 
  group_by(observer.id) %>% 
  filter(obs.date == min(obs.date)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(year = year(obs.date)) %>% 
  group_by(year) %>% 
  summarise(observers = length(observer.id)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         park = "ACAD") %>% 
  dplyr::select(year, cumsum, park)


## Calculate KAWW cumulative observers
cumulativeobeK <- ebdK %>% 
  group_by(observer.id) %>% 
  filter(obs.date == min(obs.date)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(year = year(obs.date)) %>% 
  group_by(year) %>% 
  summarise(observers = length(observer.id)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         park = "KAWW") %>% 
  dplyr::select(year, cumsum, park)


## Bind these data for plotting
ebdcumob <- bind_rows(cumulativeobeA, cumulativeobeK)


## Plot
ebdcumob %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(cumulativeobeA, year == 2022), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  geom_dl(data = subset(cumulativeobeK, year == 2022), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Count (n)") +
  scale_x_continuous(limits = c(1960, 2026), breaks = seq(1960, 2026, by = 10)) +
  theme(legend.position = c(0.13, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual(values = c("ACAD" = "gray60", "KAWW" = "black")) +
  scale_linetype_manual(values = c("ACAD" = 1, "KAWW" = 6))


## Export figure
ggsave(paste0("outputs/forpub/final_cumulative_ebd_", str_replace_all(today(), "-", ""), ".png"),
       height = 5.28, width = 8, units = "in", dpi = 500)




#------------------------------------------------#
####           Watch List Species             ####
#------------------------------------------------#

### ACAD data
## Create full data set with only research-grade grade observations
map_inatA <- inat %>%
  filter(quality.grade == "research")

mapdat <- bind_rows(map_inat, ebd) %>% 
  dplyr::select(common.name, scientific.name, observed.on, place.guess, latitude, longitude) %>% 
  mutate(cat = "All observations")

alldatmap <- bind_rows(inat, ebd) %>% 
  dplyr::select(common.name, scientific.name, observed.on, place.guess, latitude, longitude) %>% 
  mutate(cat = "All observations")


## Run watch list function to get rare, pest, and T&E species
watchlist_species(mapdat, "outputs")


### Load in the watch list csv files
## Pest species
pests <- read.csv("outputs/invasive_pestslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Invasive species observations")

## Rare species
rare <- read.csv("outputs/rare_specieslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Rare native species observations")

## T&E species
tande <- read.csv("outputs/te_specieslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Threatened/endangered species observations")





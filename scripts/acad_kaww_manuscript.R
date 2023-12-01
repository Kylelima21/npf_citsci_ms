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
library(scales)

source("scripts/analysis_functions.R")




#------------------------------------------------#
####        Data Import and Cleaning          ####
#------------------------------------------------#

### ACAD data import ###

## Read, format, filter to ACAD, and clean the iNaturalist data
inatA <- tibble(read.csv("data/acad_inat_obs_20231107.csv")) %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude") %>% 
  filter(observed_on <= "2022-12-31") %>% 
  mutate(year = year(observed_on),
         month = month(observed_on)) %>% 
  rename_with(~str_replace_all(., "_", "."), .cols = everything()) %>% 
  dplyr::select(id, observed.on, time.observed.at, user.id, user.login,
                   quality.grade:positional.accuracy, coordinates.obscured,
                   species.guess:month) %>% 
  mutate(park = "ACAD") %>% 
  dplyr::select(common.name, scientific.name = taxon.species.name, taxon.species.name, taxon.subspecies.name, 
                iconic.taxon.name, observed.on, year, month, quality.grade, latitude, 
                longitude, user.login, everything(), full.scientific.name = scientific.name) %>% 
  rename(kingdom = taxon.kingdom.name, phylum = taxon.phylum.name,
         class = taxon.class.name, order = taxon.order.name,
         family = taxon.family.name, genus = taxon.genus.name,
         subspecies = taxon.subspecies.name)


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
  filter(checklist.id != "S56409710") %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude")


## Read in the ACAD basemap for figures
acad.bm <- sf::read_sf("data/acad_boundary/formapping.shp")


## Read in the ACAD boundary layer
acad.bounds <- sf::read_sf("data/acad_boundary/acad_feeboundary_polygon.shp")



#------------------------------------------------#

### KAWW data import ###

## Read, format, filter to KAWW, and clean the iNaturalist data
inatK <- tibble(read.csv("data/kaww_inat_obs_20231108.csv")) %>% 
  filter_nps(., "Katahdin Woods and Waters National Monument", "latitude", "longitude") %>% 
  filter(observed_on <= "2022-12-31") %>% 
  mutate(year = year(observed_on),
         month = month(observed_on)) %>% 
  rename_with(~str_replace_all(., "_", "."), .cols = everything()) %>% 
  dplyr::select(id, observed.on, time.observed.at, user.id, user.login,
                quality.grade:positional.accuracy, coordinates.obscured,
                species.guess:month) %>% 
  mutate(park = "KAWW") %>% 
  dplyr::select(common.name, scientific.name = taxon.species.name, taxon.species.name, taxon.subspecies.name, 
                iconic.taxon.name, observed.on, year, month, quality.grade, latitude, 
                longitude, user.login, everything(), full.scientific.name = scientific.name) %>% 
  rename(kingdom = taxon.kingdom.name, phylum = taxon.phylum.name,
         class = taxon.class.name, order = taxon.order.name,
         family = taxon.family.name, genus = taxon.genus.name,
         subspecies = taxon.subspecies.name)


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




#------------------------------------------------#
####             Study Area Maps              ####
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
# ggsave("outputs/forpub/pptx_and_subfigs/study_area_new_england.png", height = 5.28, width = 5.28, units = "in", dpi = 500)



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
# saveWidget(acadmap, "outputs/temp.html", selfcontained = FALSE)
# webshot("outputs/temp.html", file = "outputs/forpub/pptx_and_subfigs/study_area_acad.png",
#         vwidth = 700, vheight = 500,
#         cliprect = "viewport")



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
# saveWidget(acadmap, "outputs/temp.html", selfcontained = FALSE)
# webshot("outputs/temp.html", file = "outputs/forpub/pptx_and_subfigs/study_area_kaww.png",
#         vwidth = 700, vheight = 500,
#         cliprect = "viewport")




#------------------------------------------------#
####        High Level Summary Stats          ####
#------------------------------------------------#

inat <- bind_rows(inatA, inatK)


### Total species from iNaturalist
## Manipulate the data
inatA_splist <- inatA %>% 
  filter(scientific.name != "" & quality.grade == "research" & !is.na(scientific.name)) %>% 
  dplyr::select(scientific.name) %>% 
  distinct() %>% 
  arrange(scientific.name)
inatK_splist <- inatK %>% 
  filter(scientific.name != "" & quality.grade == "research" & !is.na(scientific.name)) %>% 
  dplyr::select(scientific.name) %>% 
  distinct() %>% 
  arrange(scientific.name)
inat_splist <- inat %>%
  filter(scientific.name != "" & quality.grade == "research" & !is.na(scientific.name)) %>% 
  dplyr::select(scientific.name) %>% 
  distinct() %>% 
  arrange(scientific.name)


## Determine number of species
paste0("There have been ", length(inatA_splist$scientific.name), " species recorded from iNaturalist research grade observations")
paste0("There have been ", length(inatK_splist$scientific.name), " species recorded from iNaturalist research grade observations")
paste0("There have been ", length(inat_splist$scientific.name), " species recorded from iNaturalist research grade observations")



#------------------------------------------------#

ebd <- bind_rows(ebdA, ebdK)

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

### Total citizen science data set stats ###

## Total species across both data sets
bind_rows(ebird_splist, inat_splist) %>% 
  filter(!is.na(scientific.name)) %>% 
  distinct(scientific.name)               # 2,217 species


## Total number of observations
length(bind_rows(inat, ebd)$common.name) # 538,625




#------------------------------------------------#
####                Observers                 ####
#------------------------------------------------#

### iNaturalist ###


## Calculate ACAD cumulative observers
cumulativeobA <- inatA %>% 
  group_by(user.id) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year) %>% 
  summarise(observers = length(user.id)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         park = "ACAD") %>% 
  dplyr::select(year, cumsum, park)


## Calculate KAWW cumulative observers
cumulativeobK <- inatK %>% 
  group_by(user.id) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year) %>% 
  summarise(observers = length(user.id)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         park = "KAWW") %>% 
  dplyr::select(year, cumsum, park)


## Bind these data for plotting
inatcumob <- bind_rows(cumulativeobA, cumulativeobK)


## Plot
inatone <- inatcumob %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(cumulativeobA, year == 2022), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 1.8), "last.points")) +
  geom_dl(data = subset(cumulativeobK, year == 2022), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y + 0.4, x = x - 0.2), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "iNaturalist observers") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1995, 2023), breaks = seq(1990, 2023, by = 5)) +
  theme(legend.position = "none", #c(0.18, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_text(face = "bold", size = 17),
        legend.text = element_text(color = "black", size = 17,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 17),
        axis.title = element_text(color = "black", size = 17),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual("NPS Unit", values = c("ACAD" = "gray60", "KAWW" = "black")) +
  scale_linetype_manual("NPS Unit", values = c("ACAD" = 1, "KAWW" = 6))


## Export figure
# ggsave(paste0("outputs/forpub/final_cumulative_inat_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 500)



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
ebdone <- ebdcumob %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(cumulativeobeA, year == 2022), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 1.8), "last.points")) +
  geom_dl(data = subset(cumulativeobeK, year == 2022), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y + 0.4, x = x - 0.5), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "eBird observers") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1960, 2026), breaks = seq(1960, 2026, by = 10)) +
  theme(legend.position = c(0.18, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_text(face = "bold", size = 17),
        legend.text = element_text(color = "black", size = 17,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 17),
        axis.title = element_text(color = "black", size = 17),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual("NPS Unit", values = c("ACAD" = "gray60", "KAWW" = "black")) +
  scale_linetype_manual("NPS Unit", values = c("ACAD" = 1, "KAWW" = 6))


## Export figure
# ggsave(paste0("outputs/forpub/final_cumulative_ebd_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 500)



#------------------------------------------------#

### Total observers ###

## Calculate unique observers IDs for iNaturalist across both parks
inatobstot <- bind_rows(inatA, inatK) %>% 
  dplyr::select(user.id) %>% 
  distinct()

# 4,745


## Calculate unique observers IDs for eBird across both parks
ebdobstot <- bind_rows(ebdA, ebdK) %>% 
  dplyr::select(observer.id) %>% 
  distinct()

# 6,636


## Sum
length(inatobstot$user.id) + length(ebdobstot$observer.id) # 11,381




#------------------------------------------------#
####      Observations Observers Figure       ####
#------------------------------------------------#

### Creating the observations part of this four panel figure
## Calculate cumulative eBird observations
totalebd <- bind_rows(ebdA, ebdK) %>% 
  mutate(year = year(obs.date)) %>% 
  group_by(park, year) %>% 
  summarise(cumsum = length(scientific.name)) %>% 
  mutate(cumsum = cumsum(cumsum))


## Plot
totebd <- totalebd %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(totalebd, year == 2022 & park == "ACAD"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 2.5), "last.points")) +
  geom_dl(data = subset(totalebd, year == 2022 & park == "KAWW"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y + 0.4, x = x - 0.8), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "eBird observations") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1960, 2027), breaks = seq(1960, 2027, by = 10)) +
  theme(legend.position = "none", #c(0.18, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_text(face = "bold", size = 17),
        legend.text = element_text(color = "black", size = 17,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 17),
        axis.title = element_text(color = "black", size = 17),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual("NPS Unit", values = c("ACAD" = "gray60", "KAWW" = "black")) +
  scale_linetype_manual("NPS Unit", values = c("ACAD" = 1, "KAWW" = 6))


## Calculate cumulative inat observations
totalinat <- bind_rows(inatA, inatK) %>% 
  group_by(park, year) %>% 
  summarise(cumsum = length(scientific.name)) %>% 
  mutate(cumsum = cumsum(cumsum))


## Plot
totinat <- totalinat %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(totalinat, year == 2022 & park == "ACAD"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 2.1), "last.points")) +
  geom_dl(data = subset(totalinat, year == 2022 & park == "KAWW"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y + 0.4, x = x - 0.7), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "iNaturalist observations") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1995, 2025), breaks = seq(1990, 2024, by = 5)) +
  theme(legend.position = "none", #c(0.18, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_text(face = "bold", size = 17),
        legend.text = element_text(color = "black", size = 17,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 17),
        axis.title = element_text(color = "black", size = 17),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual("NPS Unit", values = c("ACAD" = "gray60", "KAWW" = "black")) +
  scale_linetype_manual("NPS Unit", values = c("ACAD" = 1, "KAWW" = 6))



## Combine to make a four panel figure
plot_grid(ebdone, inatone, totebd, totinat, nrow = 2, labels = c('a)', 'b)', 'c)', 'd)'), align = "h", label_size = 18)


## Save
# ggsave(paste0("outputs/forpub/figure_observations_observers.png"),
#                height = 10, width = 13.5, units = "in", dpi = 700)




#------------------------------------------------#
####           ACAD Observations              ####
#------------------------------------------------#

### eBird ###

### Data set summaries
## All observations
length(ebdA$common.name) # 472,360


## Total checklists
ebird_chkA <- ebdA %>% 
  distinct(checklist.id)

paste0("There have been ", length(ebird_chkA$checklist.id), " checklists submitted by eBird users.")


## Average checklists per observer
length(ebird_chkA$checklist.id) / length(unique(ebdA$observer.id)) # 6.95543


## Get all complete checklists
ebirdcompA <- ebdA %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  distinct(checklist.id)

length(ebirdcompA$checklist.id) # 35,133


## Percentage of checklists that are complete
paste0(round(length(ebirdcompA$checklist.id) / length(ebird_chkA$checklist.id) * 100, digits = 2), "% of all checklists are complete.")



### Monthly observations
## Calculate number of complete checklists/month and format
ck_compA <- ebdA %>% 
  mutate(month = month(obs.date),
         year = year(obs.date)) %>% 
  filter(year > 1957) %>%
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>%
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(unique(checklist.id))) %>% 
  arrange(date) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs),
         data = "Complete checklists")


## Calculate total number of checklists/month and format
tempckA <- ebdA %>% 
  mutate(month = month(obs.date),
         year = year(obs.date)) %>% 
  filter(year > 1957) %>%
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(unique(checklist.id))) %>% 
  arrange(date) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs),
         data = "Total checklists")


## Bind the data sets for plotting
ckcombA <- bind_rows(tempckA, ck_compA)


## Plot 
ckcombA %>% 
  ggplot(aes(x = date, y = tot.obs, color = data, alpha = data, linetype = data)) + 
  geom_line(linewidth = 0.8) +
  theme_classic() +
  labs(x = "Year", y = "Number of checklists") +
  scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2022-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2004-01-01"), as.Date("2022-12-31"))) +
  theme(legend.position = c(0.18, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total checklists" = "gray50", "Complete checklists" = "black")) +
  scale_alpha_manual(values = c("Total checklists" = 0.7, "Complete checklists" = 1)) +
  scale_linetype_manual(values = c("Total checklists" = 1, "Complete checklists" = 1))


## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_ebird_acad", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 500)



### Monthly summary stats
## Create full date sequence to add zeros into the data
datesebirdA <- tibble(date = seq(as.Date("1958/1/1"), as.Date("2022/12/1"), by = "month"))


## Create data frame for calculations
ebirdavgA <- datesebirdA %>% 
  full_join(tempckA) %>% 
  dplyr::select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Calculate average checklists/month to 2010
earlyeA <- ebirdavgA %>% 
  filter(date <= "2010-01-01" & date >= "2000-01-01")
mean(earlyeA$tot.obs)
sd(earlyeA$tot.obs) / sqrt(length(earlyeA$tot.obs))


## Calculate average checklists/month to current
toteA <- ebirdavgA %>% 
  filter(date >= "2010-01-01")
mean(toteA$tot.obs)
sd(toteA$tot.obs) / sqrt(length(toteA$tot.obs))


## Summer months avg 2018 - 2022
summereA <- ebirdavgA %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2018-06-01" & date <= "2022-08-31") %>% 
  filter(month == 6 | month == 7 | month == 8)
mean(summereA$tot.obs)
sd(summereA$tot.obs) / sqrt(length(summereA$tot.obs))


## Winter months avg 2018 - 2022
wintereA <- ebirdavgA %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2017-12-01" & date <= "2022-02-28") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(wintereA$tot.obs)
sd(wintereA$tot.obs) / sqrt(length(wintereA$tot.obs))


## Calculate mean +- SE species per complete checklist
avg.chA <- ebdA %>% 
  filter(category == "species") %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  group_by(checklist.id) %>% 
  summarise(richness = length(scientific.name))
mean(avg.chA$richness)
sd(avg.chA$richness) / sqrt(length(avg.chA$richness))



#------------------------------------------------#

### iNaturalist ###

### Data set summaries
## All observations
length(inatA$common.name) # 51,703


## Average submissions per observer
length(inatA$common.name) / length(unique(inatA$user.login)) # 11.01705


## Get all obs that are research grade
rgA <- inatA %>% 
  filter(quality.grade == "research") 
length(rgA$common.name)                # 28,779


## Percentage of observations that are research grade
paste0(round(length(rgA$scientific.name) / length(inatA$scientific.name) * 100, digits = 2), "% of all observations are research grade.")



### Monthly observations
## Calculate number of ACAD research grade obs/month and format
rgtempA <- inatA %>% 
  filter(quality.grade == "research") %>%
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>% 
  arrange(date) %>% 
  mutate(data = "Research grade observations",
         tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Calculate total number of obs/month and format
alltempA <- inatA %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>%
  arrange(date) %>% 
  mutate(data = "Total observations",
         tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Bind the data sets for plotting
tempcoA <- bind_rows(alltempA, rgtempA)


## Plot 
tempcoA %>% 
  ggplot(aes(x = date, y = tot.obs, color = data, alpha = data, linetype = data)) + 
  geom_line(linewidth = 0.8) +
  theme_classic() +
  labs(x = "Year", y = "Number of observations") +
  scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2022-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2004-01-01"), as.Date("2022-12-31"))) +
  theme(legend.position = c(0.23, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total observations" = "gray50", "Research grade observations" = "black")) +
  scale_alpha_manual(values = c("Total observations" = 0.7, "Research grade observations" = 1)) +
  scale_linetype_manual(values = c("Total observations" = 1, "Research grade observations" = 1))


## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_inat_acad", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 500)



### Monthly summary stats
## Create full date sequence to add zeros into the data
datesinatA <- tibble(date = seq(as.Date("1976/1/1"), as.Date("2022/12/1"), by = "month"))


## Create data frame for calculations
inatavgA <- datesinatA %>% 
  full_join(alltempA) %>% 
  dplyr::select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Calculate average obs/month to 2014
earlyiA <- inatavgA %>% 
  filter(date <= "2014-01-01" & date >= "2000-01-01") 
mean(earlyiA$tot.obs)
sd(earlyiA$tot.obs)/sqrt(length(earlyiA$tot.obs))


## Calculate average obs/month to current
totiA <- inatavgA %>% 
  filter(date >= "2014-01-01")
mean(totiA$tot.obs)
sd(totiA$tot.obs)/sqrt(length(totiA$tot.obs))


## Summer months avg 2018 - 2022
summeriA <- inatavgA %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2018-06-01" & date <= "2022-08-31") %>% 
  filter(month == 6 | month == 7 | month == 8)
mean(summeriA$tot.obs)
sd(summeriA$tot.obs)/sqrt(length(summeriA$tot.obs))


## Winter months avg 2018 - 2022
winteriA <- inatavgA %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2017-12-01" & date <= "2022-02-28") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(winteriA$tot.obs)
sd(winteriA$tot.obs)/sqrt(length(winteriA$tot.obs))



#------------------------------------------------#

### Total park data set ###

## Total number of ACAD citsci observations
length(bind_rows(inatA, ebdA)$common.name) # 524,063


## Percent of ACAD obs that are from iNaturalist
length(inatA$common.name) / length(bind_rows(inatA, ebdA)$common.name) * 100 # 10%


## Percent of ACAD obs that are from eBird
length(ebdA$common.name) / length(bind_rows(inatA, ebdA)$common.name) * 100 # 90%


## Calculate percent of ACAD science-ready data
compobsA <- ebdA %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental")

(length(compobsA$common.name) + length(rgA$common.name)) / length(bind_rows(inatA, ebdA)$common.name) * 100
# 84.52667




#------------------------------------------------#
####           KAWW Observations              ####
#------------------------------------------------#

### eBird ###

### Data set summaries
## All observations
length(ebdK$common.name) # 12,458


## Total checklists
ebird_chkK <- ebdK %>% 
  distinct(checklist.id)

paste0("There have been ", length(ebird_chkK$checklist.id), " checklists submitted by eBird users.")


## Average checklists per observer
length(ebird_chkK$checklist.id)/length(unique(ebdK$observer.id)) # 5.161137


## Get all complete checklists
ebirdcompK <- ebdK %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  distinct(checklist.id)

length(ebirdcompK$checklist.id) # 897


## Percentage of checklists that are complete
paste0(round(length(ebirdcompK$checklist.id)/length(ebird_chkK$checklist.id)*100, digits = 2), "% of all checklists are complete.")



### Monthly observations
## Calculate number of complete checklists/month and format
ck_compK <- ebdK %>% 
  mutate(month = month(obs.date),
         year = year(obs.date)) %>% 
  filter(year > 1957) %>%
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(unique(checklist.id))) %>% 
  arrange(date) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs),
         data = "Complete checklists")


## Calculate total number of checklists/month and format
tempckK <- ebdK %>% 
  mutate(month = month(obs.date),
         year = year(obs.date)) %>% 
  filter(year > 1957) %>%
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(unique(checklist.id))) %>% 
  arrange(date) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs),
         data = "Total checklists")


## Bind the data sets for plotting
ckcombK <- bind_rows(tempckK, ck_compK)


## Plot 
ckcombK %>% 
  ggplot(aes(x = date, y = tot.obs, color = data, alpha = data, linetype = data)) + 
  geom_line(linewidth = 0.8) +
  theme_classic() +
  labs(x = "Year", y = "Number of checklists") +
  scale_x_date(breaks = seq(as.Date("2014-01-01"), as.Date("2022-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2014-01-01"), as.Date("2022-12-31"))) +
  theme(legend.position = c(0.18, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total checklists" = "gray50", "Complete checklists" = "black")) +
  scale_alpha_manual(values = c("Total checklists" = 0.7, "Complete checklists" = 1)) +
  scale_linetype_manual(values = c("Total checklists" = 1, "Complete checklists" = 1))


## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_ebird_kaww", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 500)



### Monthly summary stats
## Create full date sequence to add zeros into the data
datesebirdK <- tibble(date = seq(as.Date("1958/1/1"), as.Date("2022/12/1"), by = "month"))


## Create data frame for calculations
ebirdavgK <- datesebirdK %>% 
  full_join(tempckK) %>% 
  dplyr::select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Calculate average checklists/month to 2010
earlyeK <- ebirdavgK %>% 
  filter(date <= "2010-01-01" & date >= "2000-01-01")
mean(earlyeK$tot.obs)
sd(earlyeK$tot.obs) / sqrt(length(earlyeK$tot.obs))


## Calculate average checklists/month to current
toteK <- ebirdavgK %>% 
  filter(date >= "2016-01-01")
mean(toteK$tot.obs)
sd(toteK$tot.obs) / sqrt(length(toteK$tot.obs))


## Summer months avg 2018 - 2022
summereK <- ebirdavgK %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2018-06-01" & date <= "2022-08-31") %>% 
  filter(month == 6 | month == 7 | month == 8)
mean(summereK$tot.obs)
sd(summereK$tot.obs) / sqrt(length(summereK$tot.obs))


## Winter months avg 2018 - 2022
wintereK <- ebirdavgK %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2017-12-01" & date <= "2022-02-01") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(wintereK$tot.obs)
sd(wintereK$tot.obs) / sqrt(length(wintereK$tot.obs))


## Calculate mean +- SE species per complete checklist
avg.chK <- ebdK %>% 
  filter(category == "species") %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  group_by(checklist.id) %>% 
  summarise(richness = length(scientific.name))
mean(avg.chK$richness)
sd(avg.chK$richness) / sqrt(length(avg.chK$richness))



#------------------------------------------------#

### iNaturalist ###

### Data set summaries
## All observations
length(inatK$common.name) # 2,104


## Average submissions per observer
length(inatK$common.name) / length(unique(inatK$user.login)) # 24.18391


## Get all obs that are research grade
rgK <- inatK %>% 
  filter(quality.grade == "research") 
length(rgK$common.name)                # 1,288


## Percentage of observations that are research grade
paste0(round(length(rgK$scientific.name) / length(inatK$scientific.name) * 100, digits = 2), "% of all observations are research grade.")



### Monthly observations
## Calculate number of KAWW research grade obs/month and format
rgtempK <- inatK %>% 
  filter(quality.grade == "research") %>%
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>% 
  arrange(date) %>% 
  mutate(data = "Research grade observations",
         tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Calculate total number of obs/month and format
alltempK <- inatK %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>%
  arrange(date) %>% 
  mutate(data = "Total observations",
         tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Bind the data sets for plotting
tempcoK <- bind_rows(alltempK, rgtempK)


## Plot 
tempcoK %>% 
  ggplot(aes(x = date, y = tot.obs, color = data, alpha = data, linetype = data)) + 
  geom_line(linewidth = 0.8) +
  theme_classic() +
  labs(x = "Year", y = "Number of observations") +
  scale_x_date(breaks = seq(as.Date("2014-01-01"), as.Date("2022-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2014-01-01"), as.Date("2022-12-31"))) +
  theme(legend.position = c(0.23, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total observations" = "gray50", "Research grade observations" = "black")) +
  scale_alpha_manual(values = c("Total observations" = 0.7, "Research grade observations" = 1)) +
  scale_linetype_manual(values = c("Total observations" = 1, "Research grade observations" = 1))


## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_inat_kaww", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 500)



### Monthly summary stats
## Create full date sequence to add zeros into the data
datesinatK <- tibble(date = seq(as.Date("1976/1/1"), as.Date("2022/12/1"), by = "month"))

## Create data frame for calculations
inatavgK <- datesinatK %>% 
  full_join(alltempK) %>% 
  dplyr::select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))

## Calculate average obs/month to current
totiK <- inatavgK %>% 
  filter(date >= "2016-01-01")
mean(totiK$tot.obs)
sd(totiK$tot.obs)/sqrt(length(totiK$tot.obs))


## Summer months avg 2018 - 2022
summeriK <- inatavgK %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2018-06-01" & date <= "2022-08-31") %>% 
  filter(month == 6 | month == 7 | month == 8)
mean(summeriK$tot.obs)
sd(summeriK$tot.obs)/sqrt(length(summeriK$tot.obs))


## Winter months avg 2018 - 2022
winteriK <- inatavgK %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2017-12-01" & date <= "2022-02-28") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(winteriK$tot.obs)
sd(winteriK$tot.obs)/sqrt(length(winteriK$tot.obs))



#------------------------------------------------#

### Total park data set ###

## Total number of KAWW citsci observations
length(bind_rows(inatK, ebdK)$common.name) # 14,562


## Percent of KAWW obs that are from iNaturalist
length(inatK$common.name) / length(bind_rows(inatK, ebdK)$common.name) * 100 # 14%


## Percent of KAWW obs that are from eBird
length(ebdK$common.name) / length(bind_rows(inatK, ebdK)$common.name) * 100 # 86%


## Calculate percent of KAWW science-ready data
compobsK <- ebdK %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental")

(length(compobsK$common.name) + length(rgK$common.name)) / length(bind_rows(inatK, ebdK)$common.name)
# 0.9021426




#------------------------------------------------#
####            Spatial Coverage              ####
#------------------------------------------------#

### ACAD ### 

## Combine all data
griddatA <- bind_rows(inatA, ebdA) %>% 
  dplyr::select(common.name, scientific.name, observed.on, place.guess, latitude, longitude)


## Specify min/max for grid
xmnA = min(griddatA$longitude) - 0.01
xmxA = max(griddatA$longitude) + 0.01
ymnA = min(griddatA$latitude) - 0.01
ymxA = max(griddatA$latitude) + 0.01


## Create grid
rA = raster(matrix(1:8649, 93, 93), xmx = xmxA, xmn = xmnA, ymx = ymxA, ymn = ymnA)


## Format points
ptsA = griddatA %>% 
  dplyr::select(longitude, latitude) %>% 
  rename(x = longitude, y = latitude) %>% 
  as.data.frame()


# Make a raster of zeroes like the input
r2A = rA
r2A[] = 0


# Get the cell index for each point and make a table
countsA = table(cellFromXY(rA, ptsA))


# Fill in the raster with the counts from the cell index
r2A[as.numeric(names(countsA))] = countsA


## Change raster into data frame
r3A <- as.data.frame(r2A, xy = TRUE) %>% 
  rename(count = layer) %>% 
  mutate(count2 = as.numeric(ifelse(count == 0, "NA", count)))


## Read in the fee boundary shape file
acad.fee <- sf::read_sf("data/acad_boundary/acad_feeboundary_polygon.shp") %>% 
  st_transform(4326)


## Plot
ggplot() +
  geom_sf(fill = "gray", data = acad.bm) +
  geom_tile(aes(x = x, y = y, fill = count2),
            data = r3A %>% filter(!is.na(count2))) +
  geom_sf(color = "white", fill = "transparent", linewidth = 0.3,
          data = acad.fee) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(fill = "Observations") +
  lims(x = c(-68.48, -67.99), y = c(44.17, 44.48)) +
  scale_fill_viridis_b(breaks = c(1, 250, 500, 1000, 5000, 10000, 20000, 30000)) +
  theme_minimal() +
  theme(
    legend.position = c(0.112, 0.818),
    legend.margin = margin(c(5,5,10,6)),
    legend.background = element_rect(color = "black", fill = "white", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = "transparent", linewidth = 0.5),
    plot.background = element_rect(color = "white"),
    plot.margin = margin(8,13,4,10),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())


## Save plot
# ggsave("outputs/forpub/heatmap_acad_mdi.png", dpi = 700, width = 6, height = 5.4)


## Plot Isle Au Haut
ggplot() +
  geom_sf(fill = "gray", data = acad.bm) +
  geom_tile(aes(x = x, y = y, fill = count2),
            data = r3A %>% filter(!is.na(count2))) +
  geom_sf(color = "white", fill = "transparent", linewidth = 0.3,
          data = acad.fee) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(fill = "Observations") +
  lims(x = c(-68.7099, -68.42), y = c(43.95, 44.12)) +
  scale_fill_viridis_b(breaks = c(1, 250, 500, 1000, 2500, 5000, 10000, 20000, 30000)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = "transparent"),
    plot.background = element_rect(color = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())


## Save plot
# ggsave("outputs/forpub/heatmap_acad_isleauhaut.png", dpi = 700, width = 6, height = 6)



### Spatial stats
## Calculate grid size
head(r3A)
distm(c(-68.66641, 44.43629), c(-68.65944, 44.43629), fun = distHaversine)


## Calculate percent of cells with observations
cobsA <- r3A %>% 
  mutate(longitude.keep = x,
         latitude.keep = y) %>% 
  sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(acad.bounds))


## Filter to those cells that intersect with the KAWW polygon
filtcobsA <- sf::st_join(cobsA, acad.bounds, left = F) %>% 
  st_set_geometry(., NULL) %>% 
  dplyr::select(everything(), latitude = latitude.keep, longitude = longitude.keep)


## Calculate the percentage of cells with observations
length((filtcobsA %>% filter(count > 0))$count) / length(filtcobsA$count) * 100 # 95.96929



#------------------------------------------------#

### KAWW ###

## Combine all data
griddatK <- bind_rows(inatK, ebdK) %>% 
  dplyr::select(common.name, scientific.name, observed.on, place.guess, latitude, longitude)


## Specify min/max for grid
xmnK = min(griddatK$longitude) - 0.01
xmxK = max(griddatK$longitude) + 0.005
ymnK = min(griddatK$latitude) - 0.004
ymxK = max(griddatK$latitude) + 0.008


## Create grid
rK = raster(matrix(1:2204, 58, 38), xmx = xmxK, xmn = xmnK, ymx = ymxK, ymn = ymnK)


## Format points
ptsK = griddatK %>% 
  dplyr::select(longitude, latitude) %>% 
  rename(x = longitude, y = latitude) %>% 
  as.data.frame()


## Make a raster of zeroes like the input
r2K = rK
r2K[] = 0


## Get the cell index for each point and make a table
countsK = table(cellFromXY(rK,ptsK))


## Fill in the raster with the counts from the cell index
r2K[as.numeric(names(countsK))] = countsK


## Change raster into data frame
r3K <- as.data.frame(r2K, xy = TRUE) %>% 
  rename(count = layer) %>% 
  mutate(count2 = as.numeric(ifelse(count == 0, "NA", count)))


## Read in the fee boundary shape file
kww.b <- sf::read_sf("data/kww_boundary/kww_boundary_polygon.shp") %>% 
  st_transform(4326)


## Plot
ggplot() +
  geom_sf(color = "black", fill = "white", linewidth = 0.7,
          data = kww.b) +
  geom_tile(aes(x = x, y = y, fill = count2),
            data = r3K %>% filter(!is.na(count2))) +
  geom_sf(color = "white", fill = "transparent", linewidth = 0.3,
          data = kww.b) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(fill = "Observations") +
  lims(x = c(-68.965, -68.47), y = c(45.82, 46.13)) +
  scale_fill_viridis_b(breaks = c(1, 50, 100, 250, 500, 1000, 1500)) +
  theme_minimal() +
  theme(
    legend.position = c(0.112, 0.818),
    legend.margin = margin(c(5,5,10,6)),
    legend.background = element_rect(color = "black", fill = "white", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = "transparent", linewidth = 0.5),
    plot.background = element_rect(color = "white"),
    plot.margin = margin(8,13,4,10),
    panel.background = element_rect(fill = "gray"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


## Save plot
# ggsave("outputs/forpub/heatmap_kaww.png", dpi = 700, width = 6, height = 5.4)



### Spatial stats
## Calculate grid size
head(r3K)
distm(c(-68.82437, 46.13063), c(-68.81715, 46.13063), fun = distHaversine)


## Calculate percent of cells with observations
cobsK <- r3K %>% 
  mutate(longitude.keep = x,
         latitude.keep = y) %>% 
  sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(kaww.bounds))


## Filter to those cells that intersect with the KAWW polygon
filtcobsK <- sf::st_join(cobsK, kaww.bounds, left = F) %>% 
  st_set_geometry(., NULL) %>% 
  dplyr::select(everything(), latitude = latitude.keep, longitude = longitude.keep)


## Calculate the percentage of cells with observations
length((filtcobsK %>% filter(count > 0))$count) / length(filtcobsK$count) * 100 # 28.57143




#------------------------------------------------#
####          ACAD Taxonomic Summary          ####
#------------------------------------------------#

### eBird taxonomy stats ###

## Read in the eBird taxonomy for merging with ebd
etax <- read.csv("data/ebird_taxonomy_v2022.csv") %>% 
  dplyr::select(scientific.name = SCI_NAME, order = ORDER1, family = FAMILY,
                species.group = SPECIES_GROUP)


## Join the two 
ebdtaxA <- left_join(ebdA, etax, by = "scientific.name")


## Total species
ebdtaxA %>% 
  filter(category == "species") %>% 
  dplyr::select(scientific.name) %>% 
  distinct()


## Determine how many orders were recorded
unique(ebdtaxA$order) # 19


## Determine the percent of data made up by each order
e_ordersA <- ebdtaxA %>% 
  filter(order != "") %>% 
  group_by(order) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(count = round((count / length(ebdA$scientific.name)) * 100, digits = 2))


## Calculate frequency of obs for each species
ebdA %>% 
  mutate(count = ifelse(count == "X", 1, count),
         count = as.numeric(count)) %>% 
  group_by(common.name, scientific.name) %>% 
  summarize(frequency = round((length(scientific.name) / length(unique(ebdA$checklist.id)) * 100), 2)) %>% 
  arrange(-frequency)



#------------------------------------------------#


### iNaturalist taxonomy stats

## Total species
i_sppA <- inatA %>% 
  filter(scientific.name != "" & quality.grade == "research") %>% 
  dplyr::select(scientific.name) %>% 
  distinct()


## Total orders
i_ordersA <- inatA %>% 
  filter(order != "" & quality.grade == "research") %>% 
  dplyr::select(order) %>% 
  distinct()


## Total obs per kingdom
i_kingdoms_obsA <- inatA %>% 
  filter(kingdom != "") %>% 
  group_by(kingdom) %>% 
  summarise(count = length(kingdom)) %>% 
  arrange(-count)
  

## Total species per kingdom
i_kingdoms_sppA <- inatA %>% 
  filter(scientific.name != "" & quality.grade == "research") %>% 
  dplyr::select(scientific.name, kingdom) %>% 
  distinct() %>% 
  group_by(kingdom) %>% 
  summarise(count = length(kingdom)) %>% 
  arrange(-count)


## Total research-grade obs per kingdom
i_kingdoms_rgA <- inatA %>% 
  filter(quality.grade == "research") %>% 
  group_by(kingdom) %>% 
  summarise(rg.count = length(scientific.name)) %>% 
  arrange(-rg.count)

bind_cols(i_kingdoms_rgA, i_kingdoms_obsA) %>% 
  dplyr::select(kingdom = `kingdom...1`, rg.count, count) %>% 
  mutate(prop = 100 * (rg.count / count)) %>% 
  arrange(-prop)


## Totals orders per kingdom
# i_kingdoms_ordA <- inatA %>% 
#   filter(order != "" & quality.grade == "research") %>% 
#   dplyr::select(order, kingdom) %>% 
#   distinct() %>% 
#   group_by(kingdom) %>% 
#   summarise(count = length(kingdom)) %>% 
#   arrange(-count)


## Total obs per order Animalia
inatA %>% 
  filter(kingdom == "Animalia" & quality.grade == "research" & order != "") %>% 
  group_by(order) %>%
  summarise(count = length(order)) %>% 
  arrange(-count)


## Total species per order Animalia
inatA %>% 
  filter(kingdom == "Animalia" & quality.grade == "research" & order != "") %>% 
  dplyr::select(order, scientific.name) %>% 
  distinct() %>% 
  group_by(order) %>%
  summarise(count = length(order)) %>% 
  arrange(-count)


## Total obs per order Plantae
inatA %>% 
  filter(kingdom == "Plantae" & quality.grade == "research" & order != "") %>% 
  group_by(order) %>%
  summarise(count = length(order)) %>% 
  arrange(-count)


## Total species per order Plantae
inatA %>% 
  filter(kingdom == "Plantae" & quality.grade == "research" & order != "") %>% 
  dplyr::select(order, scientific.name) %>% 
  distinct() %>% 
  group_by(order) %>%
  summarise(count = length(order)) %>% 
  arrange(-count)


## Total species level ids
spidsA <- inatA %>% 
  filter(scientific.name != "")


length(spidsA$common.name) # 37,086
length(spidsA$common.name) / length(inatA$common.name) * 100 # 71.73 %
spidsA %>% filter(quality.grade == "research") # 28,522


## Total observations per species
sptotsA <- inatA %>% 
  filter(scientific.name != "" & quality.grade == "research") %>% 
  group_by(scientific.name) %>% 
  summarise(total = length(scientific.name)) %>% 
  arrange(-total)




#------------------------------------------------#
####          KAWW Taxonomic Summary          ####
#------------------------------------------------#

### eBird taxonomy stats ###

## Join the tax data set from earlier with KAWW
ebdtaxK <- left_join(ebdK, etax, by = "scientific.name")


## Total species
ebdtaxK %>% 
  filter(category == "species") %>% 
  dplyr::select(scientific.name) %>% 
  distinct()


## Determine how many orders were recorded
unique(ebdtaxK$order) # 17


## Determine the percent of data made up by each order
e_ordersK <- ebdtaxK %>% 
  filter(order != "") %>% 
  group_by(order) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(count = round((count / length(ebdK$scientific.name)) * 100, digits = 2))

# write.csv(e_ordersA, "outputs/forpub/ebird_orders_table_kaww.csv", row.names = F)


## Calculate frequency of obs for each species
ebdK%>% 
  mutate(count = ifelse(count == "X", 1, count),
         count = as.numeric(count)) %>% 
  group_by(common.name, scientific.name) %>% 
  summarize(frequency = round((length(scientific.name) / length(unique(ebdK$checklist.id)) * 100), 2)) %>% 
  arrange(-frequency)



#------------------------------------------------#


### iNaturalist taxonomy stats

## Total species
i_sppK <- inatK %>% 
  filter(scientific.name != "" & quality.grade == "research") %>% 
  dplyr::select(scientific.name) %>% 
  distinct()


## Total orders
i_ordersK <- inatK %>% 
  filter(order != "" & quality.grade == "research") %>% 
  dplyr::select(order) %>% 
  distinct()


## Total obs per kingdom
i_kingdoms_obsK <- inatK %>% 
  filter(kingdom != "") %>% 
  group_by(kingdom) %>% 
  summarise(count = length(kingdom)) %>% 
  arrange(-count)


## Total species per kingdom
i_kingdoms_sppK <- inatK %>% 
  filter(scientific.name != "" & quality.grade == "research") %>% 
  dplyr::select(scientific.name, kingdom) %>% 
  distinct() %>% 
  group_by(kingdom) %>% 
  summarise(count = length(kingdom)) %>% 
  arrange(-count)


## Total research-grade obs per kingdom
i_kingdoms_rgK <- inatK %>% 
  filter(quality.grade == "research") %>% 
  group_by(kingdom) %>% 
  summarise(rg.count = length(scientific.name)) %>% 
  arrange(-rg.count)

bind_cols(i_kingdoms_rgK, i_kingdoms_obsK %>% filter(kingdom != "Protozoa" & kingdom != "Viruses")) %>% 
  dplyr::select(kingdom = `kingdom...1`, rg.count, count) %>% 
  mutate(prop = 100 * (rg.count / count)) %>% 
  arrange(-prop)


## Total obs per order Animalia
inatK %>% 
  filter(kingdom == "Animalia" & quality.grade == "research" & order != "") %>% 
  group_by(order) %>%
  summarise(count = length(order)) %>% 
  arrange(-count)


## Total species per order Animalia
inatA %>% 
  filter(kingdom == "Animalia" & quality.grade == "research" & order != "") %>% 
  dplyr::select(order, scientific.name) %>% 
  distinct() %>% 
  group_by(order) %>%
  summarise(count = length(order)) %>% 
  arrange(-count)


## Total obs per order Plantae
inatK %>% 
  filter(kingdom == "Plantae" & quality.grade == "research" & order != "") %>% 
  group_by(order) %>%
  summarise(count = length(order)) %>% 
  arrange(-count)

## Total species per order Plantae
inatK %>% 
  filter(kingdom == "Plantae" & quality.grade == "research" & order != "") %>% 
  dplyr::select(order, scientific.name) %>% 
  distinct() %>% 
  group_by(order) %>%
  summarise(count = length(order)) %>% 
  arrange(-count)


## Total species level ids
spidsK <- inatK %>% 
  filter(scientific.name != "")

length(spidsK$common.name) # 1,555
length(spidsK$common.name) / length(inatK$common.name) * 100 # 73.91 %
spidsK %>% filter(quality.grade == "research") # 1,277


## Total observations per species
sptotsK <- inatK %>% 
  filter(scientific.name != "" & quality.grade == "research") %>% 
  group_by(scientific.name) %>% 
  summarise(total = length(scientific.name)) %>% 
  arrange(-total)




#------------------------------------------------#
####         Total Taxonomic Summary          ####
#------------------------------------------------#

### ACAD orders ###

## Total orders in both ACAD data sets
oeA <- ebdtaxA %>% 
  dplyr::select(scientific.name, order)


oiA <- inatA %>% 
  filter(quality.grade == "research") %>% 
  dplyr::select(scientific.name, order)


oeiA <- bind_rows(oeA, oiA) 


allordersA <- oeiA %>% 
  filter(order != "") %>% 
  group_by(order) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(percent = round((count / length(oeiA$scientific.name)) * 100, digits = 2))


# write.csv(allorders, "outputs/forpub/table_all_orders.csv", row.names = F) ## THIS DOES WORK



#------------------------------------------------#

### KAWW orders ###

## Total orders in both KAWW data sets
oeK <- ebdtaxK %>% 
  dplyr::select(scientific.name, order)


oiK <- inatK %>% 
  filter(quality.grade == "research") %>% 
  dplyr::select(scientific.name, order)


oeiK <- bind_rows(oeK, oiK) 


allordersK <- oeiK %>% 
  filter(order != "") %>% 
  group_by(order) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(percent = round((count / length(oeiK$scientific.name)) * 100, digits = 2))


# write.csv(allorders, "outputs/forpub/all_orders_table.csv", row.names = F)



#------------------------------------------------#

### Total orders ###

## Bind rows from ACAD and KAWW and calculate
bind_rows(oeiA, oeiK) %>% 
  group_by(order) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(percent = round((count / length(oeiK$scientific.name)) * 100, digits = 2))

# 207 total orders



#------------------------------------------------#

### Total species by park figure ###

## Filter data sets to necessary info
iKcumsp <- inatK %>% 
  filter(quality.grade == "research" & scientific.name != "") %>% 
  dplyr::select(scientific.name, observed.on, park)

eKcumsp <- ebdK %>% 
  filter(category == "species") %>% 
  dplyr::select(scientific.name, observed.on = obs.date, park)

iAcumsp <- inatA %>% 
  filter(quality.grade == "research" & scientific.name != "") %>% 
  dplyr::select(scientific.name, observed.on, park)

eAcumsp <- ebdA %>% 
  filter(category == "species") %>% 
  dplyr::select(scientific.name, observed.on = obs.date, park)


## Calculate cumulative species totals for iNat in both parks
icumulativespp <- bind_rows(iKcumsp, iAcumsp) %>% 
  group_by(scientific.name, park) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year, park) %>% 
  summarise(tot.obs = length(scientific.name)) %>%
  ungroup() %>% 
  arrange(park, year) %>% 
  group_by(park) %>% 
  mutate(cumsum = cumsum(tot.obs)) %>% 
  dplyr::select(year, cumsum, park)


## Plot 
icumplot <- icumulativespp %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(icumulativespp, year == 2022 & park == "ACAD"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.3, dl.trans(y = y, x = x - 1.7), "last.points")) +
  geom_dl(data = subset(icumulativespp, year == 2022 & park == "KAWW"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.3, dl.trans(y = y + 0.4, x = x - 0.5), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Cumulative iNaturalist species") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1975, 2027), breaks = seq(1970, 2027, by = 10)) +
  theme(legend.position = "none", #c(0.18, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_text(face = "bold", size = 15),
        legend.text = element_text(color = "black", size = 15,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual("NPS Unit", values = c("ACAD" = "gray60", "KAWW" = "black")) +
  scale_linetype_manual("NPS Unit", values = c("ACAD" = 1, "KAWW" = 6))



## Calculate cumulative species totals for eBird in both parks
ecumulativespp <- bind_rows(eKcumsp, eAcumsp) %>% 
  group_by(scientific.name, park) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year, park) %>% 
  summarise(tot.obs = length(scientific.name)) %>%
  ungroup() %>% 
  arrange(park, year) %>% 
  group_by(park) %>% 
  mutate(cumsum = cumsum(tot.obs)) %>% 
  dplyr::select(year, cumsum, park)


## Plot
ecumplot <- ecumulativespp %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(ecumulativespp, year == 2021 & park == "ACAD"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.3, dl.trans(y = y + 0.4, x = x - 0.5), "last.points")) +
  geom_dl(data = subset(ecumulativespp, year == 2022 & park == "KAWW"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.3, dl.trans(y = y + 0.4, x = x - 0.5), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Cumulative eBird species") +
  scale_y_continuous(labels = comma, limits = c(0, 350)) +
  scale_x_continuous(limits = c(1955, 2027), breaks = seq(1950, 2027, by = 10)) +
  theme(legend.position = c(0.18, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_text(face = "bold", size = 15),
        legend.text = element_text(color = "black", size = 15,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual("NPS Unit", values = c("ACAD" = "gray60", "KAWW" = "black")) +
  scale_linetype_manual("NPS Unit", values = c("ACAD" = 1, "KAWW" = 6))


## Combine to make a two panel figure
plot_grid(ecumplot, icumplot, nrow = 2, labels = c('a)', 'b)'), align = "h", label_size = 15)


## Save
# ggsave(paste0("outputs/forpub/figure_species_accumulation.png"),
#        height = 9, width = 6, units = "in", dpi = 700)



#------------------------------------------------#

### Research grade iNaturalist observations by kingdom ###

## Table for ACAD
kingtabA <- inatA %>% 
  filter(kingdom != "") %>% 
  group_by(kingdom) %>% 
  summarize(total.obs = length(scientific.name),
            rg.obs = length(which(quality.grade == "research"))) %>% 
  mutate(percent.rg = round(100*(rg.obs/total.obs), digits = 0),
         park = "ACAD")


## Table for KAWW 
kingtabK <- inatK %>% 
  filter(kingdom != "") %>% 
  group_by(kingdom) %>% 
  summarize(total.obs = length(scientific.name),
            rg.obs = length(which(quality.grade == "research"))) %>% 
  mutate(percent.rg = round(100*(rg.obs/total.obs), digits = 0),
         park = "KAWW")


## Combine tables and format
rgkingtab <- bind_rows(kingtabA, kingtabK) %>% 
  arrange(park, -percent.rg)


## Write out
# write.csv(rgkingtab, "outputs/forpub/table_rg_kingdoms.csv", row.names = F)



#------------------------------------------------#

### Taxonomic diversity ###

taxtabeA <- ebdtaxA %>% 
  dplyr::select(common.name, scientific.name, category, order, family) %>% 
  mutate(family = str_replace(family, "\\s\\(.*$", ""),
         kingdom = "Animalia",
         phylum = "Chordata",
         class = "Aves") %>% 
  dplyr::select(common.name:category, kingdom:class, order, family)


taxtabiA <- inatA %>% 
  filter(quality.grade == "research") %>% 
  dplyr::select(common.name, scientific.name, kingdom:family)


taxtabA <- bind_rows(taxtabeA, taxtabiA)


length(unique((taxtabA %>% filter(kingdom != ""))$kingdom))
length(unique((taxtabA %>% filter(phylum != ""))$phylum))
length(unique((taxtabA %>% filter(class != ""))$class))
length(unique((taxtabA %>% filter(order != ""))$order))
length(unique((taxtabA %>% filter(family != ""))$family))



## Create data for phylogram
# testdat <- inatK %>% 
#   filter(kingdom != "") %>% 
#   group_by(kingdom) %>% 
#   mutate(kingdom.id = cur_group_id()) %>% 
#   group_by(phylum) %>% 
#   mutate(phylum.id = cur_group_id()) %>% 
#   group_by(class) %>% 
#   mutate(class.id = cur_group_id()) %>% 
#   group_by(order) %>% 
#   mutate(order.id = cur_group_id()) %>% 
#   group_by(family) %>% 
#   mutate(family.id = cur_group_id()) %>%
#   group_by(genus) %>% 
#   mutate(genus.id = cur_group_id()) %>% 
#   ungroup() %>% 
#   filter(kingdom == "Fungi" | kingdom == "Animalia") %>% 
#   dplyr::select(family, kingdom.id:family.id) %>% 
#   distinct() %>% 
#   filter(family != "") %>% 
#   column_to_rownames(var = "family")
#   
#   
#   
#   dplyr::select(scientific.name, kingdom.id:genus.id) %>% 
#   distinct() %>% 
#   filter(scientific.name != "") %>% 
#   column_to_rownames(var = "scientific.name")


## Compute distances and hierarchical clustering
# dd <- dist(scale(testdat), method = "euclidean")
# hc <- hclust(dd, method = "ward.D2")
# 
# plot(as.phylo(hc), type = "fan")




#------------------------------------------------#
####           Watch List Species             ####
#------------------------------------------------#

### ACAD data

## Create full data set with only research-grade grade observations
map_inatA <- inatA %>%
  filter(quality.grade == "research")

mapdatA <- bind_rows(map_inatA, ebdA) %>% 
  dplyr::select(common.name, scientific.name, observed.on, place.guess, latitude, longitude) %>% 
  mutate(cat = "All observations")


## Run watch list function to get rare, pest, and T&E species
watchlist_species(mapdatA, "outputs/watchlist_acad")


### Load in the watch list csv files
## Pest species
pestsA <- read.csv("outputs/watchlist_acad/invasive_pestslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Invasive species observations")

ptabA <- tibble(pestsA) %>% 
  group_by(scientific.name) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(category = "pest/invasive")

length(ptabA$scientific.name)


## Rare species
rareA <- read.csv("outputs/watchlist_acad/rare_specieslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Rare native species observations")

rtabA <- tibble(rareA) %>% 
  group_by(scientific.name) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(category = "rare native")

length(rtabA$scientific.name)


## T&E species
tandeA <- read.csv("outputs/watchlist_acad/te_specieslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Threatened/endangered species observations")

tetabA <- tibble(tandeA) %>% 
  group_by(scientific.name) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(category = "threatened/endangered")

length(tetabA$scientific.name)



## Create table of the watchlist species
ptr_tableA <- bind_rows(ptabA, tetabA, rtabA)

# write.csv(ptr_tableA, "outputs/forpub/table_watchlist_acad.csv", row.names = F)



## What percent of watchlist species have been detected?
wldetA <- read_excel("data/acad_watchlist_species.xlsx") %>% 
  filter(in.anp == "P" | in.anp == "Y") %>% 
  mutate(citsci.detect = ifelse(scientific.name %in% ptr_tableA$scientific.name, "yes", "no"))


nrow(wldetA %>% filter(citsci.detect == "yes")) / nrow(wldetA)


nparkA <- read_excel("data/acad_watchlist_species.xlsx") %>% 
  filter(in.anp == "N") %>% 
  mutate(citsci.detect = ifelse(scientific.name %in% ptr_tableA$scientific.name, "yes", "no")) %>% 
  filter(citsci.detect == "yes")



#------------------------------------------------#

### KAWW data

## Create full data set with only research-grade grade observations
map_inatK <- inatK %>%
  filter(quality.grade == "research")

mapdatK <- bind_rows(map_inatK, ebdK) %>% 
  dplyr::select(common.name, scientific.name, observed.on, place.guess, latitude, longitude) %>% 
  mutate(cat = "All observations")


## Run watch list function to get rare, pest, and T&E species
watchlist_species(mapdatK, "outputs/watchlist_kaww")


### Load in the watch list csv files
## Pest species
pestsK <- read.csv("outputs/watchlist_kaww/invasive_pestslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Invasive species observations")

ptabK <- tibble(pestsK) %>% 
  group_by(scientific.name) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(category = "pest/invasive")

length(ptabK$scientific.name)


## Rare species
rareK <- read.csv("outputs/watchlist_kaww/rare_specieslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Rare native species observations")

rtabK <- tibble(rareK) %>% 
  group_by(scientific.name) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(category = "rare native")

length(rtabK$scientific.name)


## T&E species
tandeK <- read.csv("outputs/watchlist_kaww/te_specieslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Threatened/endangered species observations")

tetabK <- tibble(tandeK) %>% 
  group_by(scientific.name) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(category = "threatened/endangered")

length(tetabK$scientific.name)



## Create table of the watchlist species
ptr_tableK <- bind_rows(ptabK, tetabK, rtabK)

# write.csv(ptr_tableK, "outputs/forpub/table_watchlist_kaww.csv", row.names = F)



## What percent of watchlist species have been detected?
wldetK <- read_excel("data/acad_watchlist_species.xlsx") %>% 
  mutate(citsci.detect = ifelse(scientific.name %in% ptr_tableK$scientific.name, "yes", "no"))


nrow(wldetK %>% filter(citsci.detect == "yes")) / nrow(wldetK)










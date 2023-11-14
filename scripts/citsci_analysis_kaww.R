### KWW iNaturalist and eBird summaries
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

source("scripts/analysis_functions.R")



#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

## Read, format, filter to KWW, and clean the iNaturalist data
inat <- tibble(read.csv("data/kww_inat_obs_tax.csv")) %>% 
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
         species = ifelse(species == "×", NA, species)) %>% 
  dplyr::select(common.name, scientific.name, taxon, species, subspecies, 
                iconic.taxon.name, observed.on, year, month, quality.grade, latitude, 
                longitude, user.login, everything()) %>%
  filter(observed.on <= "2022-12-31")
  # mutate(scientific.name = ifelse(scientific.name == "Heterocampa umbrata", "Heterocampa pulverea", scientific.name),
  #        species = ifelse(species == "umbrata", "pulverea", species))

## Read, format, filter to KWW, and clean the eBird data
ebd <- tibble(read.delim("data/ebd_US-ME_relFeb-2023.txt", header = T, quote = "")) %>% 
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
  filter_nps(., "Katahdin Woods and Waters National Monument", "latitude", "longitude") %>% 
  filter(obs.date <= "2022-12-31")


## Read in the basemapfor figures
kww.bm <- sf::read_sf("data/nps_boundary/kww_boundary/formapping.shp")

## Read in the Acadia boundary layer
# acad.bounds <- sf::read_sf("data/acad_boundary/ACAD_ParkBoundary_PY_202004.shp") %>% 
#   st_transform(4326)
kww.bound <- sf::read_sf("data/kww_boundary/kww_boundary_polyg.shp")




#------------------------------------------------#
####             Study area maps              ####
#------------------------------------------------#

### Study area map
## Read in US map
states <- map_data("state")


## Plot context map
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = "gray50", show.legend = F) + 
  coord_fixed(1.3) +
  lims(x = c(-80, -66), y = c(38, 48)) +
  theme_nothing()


## Export figure
# ggsave(paste0("outputs/context_map_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 5.28, units = "in", dpi = 350)


maxLong = -68.55836 + 0.04
maxLat = 46.12548 + 0.04
minLong = -68.82463 - 0.04
minLat = 45.82705 - 0.04


## Create Acadia bounds map
acadmap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addMapPane("polygons", zIndex = 201) %>%
  addMapPane("labels", zIndex = 300) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = providerTileOptions(pane = "labels")) %>% 
  addPolygons(data = kww.bound, color = "black", fill = T, fillColor = "darkorange", opacity = 1, fillOpacity = 0.9,
              weight = .5, options = pathOptions(pane = "polygons")) %>% 
  # addLegend("bottomright", values = ~kww.bound,
  #           colors = c("darkorange"),
  #           labels = c("Katahdin Woods and Waters National Monument"),
  #           opacity = 1) %>% 
  fitBounds(minLong,minLat,maxLong,maxLat)

acadmap


## Export figure
saveWidget(acadmap, "outputs/temp.html", selfcontained = FALSE)
webshot("outputs/temp.html", file = "outputs/kwwplot.png",
        vwidth = 700, vheight = 500,
        cliprect = "viewport")




#------------------------------------------------#
####             4 panel map fig              ####
#------------------------------------------------#

## Create full data set
map_inat <- inat %>%
  filter(quality.grade == "research")

mapdat <- bind_rows(map_inat, ebd) %>% 
  dplyr::select(common.name, scientific.name, observed.on, place.guess, latitude, longitude) %>% 
  mutate(cat = "All observations")


alldatmap <- bind_rows(inat, ebd) %>% 
  dplyr::select(common.name, scientific.name, observed.on, place.guess, latitude, longitude) %>% 
  mutate(cat = "All observations")


## Run t&e list function to get rare, pest, and te species
telist_species(mapdat)


### Load in the watchlist csv files
## T&E species
tande <- read.csv("outputs/te_specieslist.csv") %>% 
  dplyr::select(common.name, scientific.name, observed.on, latitude, longitude) %>% 
  mutate(cat = "Threatened/endangered species observations")
  

## All obs
all <- ggplot() +
  geom_sf(fill = "forestgreen", data = kww.bound, alpha = 1, color = 'black',
          linewidth = 0.5) +
  geom_sf(fill = "transparent", data = kww.bound, color = 'white',
          linewidth = 0.1) +
  geom_point(aes(x = longitude, y = latitude),
             shape = 21, size = 1.5, color = "white", stroke = 0.05,
             fill = "black", alpha = 1, data = alldatmap) +
  geom_text(aes(x = -68.54, y = 45.81), label = "All observations",
            size = 3, fontface = "bold") +
  geom_text(aes(x = -68.54, y = 45.79), label = "n = 14,558",
            size = 3) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  lims(y = c(45.82705 - 0.04, 46.12548 + 0.04), x = c(-68.82463 - 0.12, -68.55836 + 0.12)) +
  theme_bw() +
  theme(#plot.margin = margin(0.3,0.4,0.3,1.5, unit = "cm"),
        panel.background = element_rect(fill = "gray"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

all


## T&E species
te <- ggplot() +
  geom_sf(fill = "forestgreen", data = kww.bound, alpha = 1, color = 'black',
          linewidth = 0.5) +
  geom_sf(fill = "transparent", data = kww.bound, color = 'white',
          linewidth = 0.1) +
  geom_point(aes(x = longitude, y = latitude),
             shape = 21, size = 2.2, color = "white", stroke = 0.2,
             fill = "black", data = tande) +
  geom_text(aes(x = -68.54, y = 45.825), label = "Threatened and",
            size = 3, fontface = "bold") +
  geom_text(aes(x = -68.54, y = 45.81), label = "endangered species",
            size = 3, fontface = "bold") +
  geom_text(aes(x = -68.54, y = 45.79), label = "n = 3",
            size = 3) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  lims(y = c(45.82705 - 0.04, 46.12548 + 0.04), x = c(-68.82463 - 0.12, -68.55836 + 0.12)) +
  theme_bw() +
  theme(#plot.margin = margin(0.3,0.4,0.3,1.5, unit = "cm"),
        panel.background = element_rect(fill = "gray"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

te


## Cowplot it
plot_grid(all, te, nrow = 1, labels = c("a)", "b)"), align = "h", label_size = 13, 
          label_x = c(0.05, 0.05), label_y = c(0.88, 0.88))

## Save
ggsave(paste0("outputs/forpub/two_map_figure_", str_replace_all(today(), "-", ""), ".png"),
       height = 4.5, width = 7, units = "in", dpi = 700)




tetab <- tibble(tande) %>% 
  group_by(scientific.name) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(category = "threatened/endangered")

tetab




#------------------------------------------------#
####             Summary Stats                ####
#------------------------------------------------#

### Total iNaturalist obs
## All observations
length(inat$common.name)

## Average submissions per observer
length(inat$common.name)/length(unique(inat$user.login))

## Get all obs that are research grade
rg <- inat %>% 
  filter(quality.grade == "research") 

length(rg$common.name)

## Percentage of observations that are research grade
paste0(round(length(rg$species)/length(inat$species)*100, digits = 2), "% of all observations are research grade.")


### Total species from iNaturalist
## Manipulate the data
inat_splist <- inat %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(sci.name = paste(taxon, species, sep = " ")) %>% 
  dplyr::select(sci.name) %>% 
  distinct() %>% 
  rename(scientific.name = sci.name) %>% 
  arrange(scientific.name)

## Determine number of species
paste0("There have been ", length(inat_splist$scientific.name), " species recorded from iNaturalist research grade observations")


#------------------------------------------------#


### Total eBird obs
length(ebd$common.name) 

## Total checklists
ebird_chk <- ebd %>% 
  distinct(checklist.id)

paste0("There have been ", length(ebird_chk$checklist.id), " checklists submitted by eBird users.")

## Average checklists per observer
length(ebird_chk$checklist.id)/length(unique(ebd$observer.id))

## Get all complete checklists
ebirdcomp <- ebd %>% 
  filter(protocol == "Traveling" | protocol == "Stationary" & duration.min >= 5 & 
           all.species.reported == 1) %>% 
  distinct(checklist.id)

length(ebirdcomp$checklist.id)

## Percentage of checklists that are complete
paste0(round(length(ebirdcomp$checklist.id)/length(ebird_chk$checklist.id)*100, digits = 2), "% of all checklists are complete.")


### Total species from eBird
## Manipulate the data
ebird_splist <- ebd %>% 
  filter(category == "species") %>% 
  dplyr::select(scientific.name) %>% 
  distinct() %>% 
  arrange(scientific.name)

## Determine number of species
paste0("There have been ", length(ebird_splist$scientific.name), " species recorded by eBird users.")


#------------------------------------------------#


## Total species
bind_rows(ebird_splist, inat_splist) %>% 
  distinct(scientific.name)




#------------------------------------------------#
####        Total Observations Plots          ####
#------------------------------------------------#

### Plotting iNat observations over time
## Calculate number of research grade obs/month and format
rgtemp <- inat %>% 
  filter(quality.grade == "research") %>%
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>% 
  arrange(date) %>% 
  mutate(data = "Research grade observations",
         tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Calculate total number of obs/month and format
alltemp <- inat %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>%
  arrange(date) %>% 
  mutate(data = "Total observations",
         tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Bind the data sets for plotting
tempco <- bind_rows(alltemp, rgtemp)


## Plot 
tempco %>% 
  ggplot(aes(x = date, y = tot.obs, color = data, alpha = data, linetype = data)) + 
  geom_line(size = 0.8) +
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
# ggsave(paste0("outputs/forpub/monthly_obs_inat_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 350)



### Summary stats
## Create full date sequence to add zeros into the data
datesinat <- tibble(date = seq(as.Date("1976/1/1"), as.Date("2022/12/1"), by = "month"))

## Create data frame for calculations
inatavg <- datesinat %>% 
  full_join(alltemp) %>% 
  dplyr::select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))

## Calculate average obs/month to current
toti <- inatavg %>% 
  filter(date >= "2016-01-01")
mean(toti$tot.obs)
sd(toti$tot.obs)/sqrt(length(toti$tot.obs))

## Winter months avg 2018 - 2022
winteri <- inatavg %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2016-12-01" & date <= "2022-02-01") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(winteri$tot.obs)
sd(winteri$tot.obs)/sqrt(length(winteri$tot.obs))


#------------------------------------------------#


### Plotting eBird observations over time
## Calculate number of complete checklists/month and format
ck_comp <- ebd %>% 
  mutate(month = month(obs.date),
         year = year(obs.date)) %>% 
  filter(year > 1957) %>%
  filter(protocol == "Traveling" | protocol == "Stationary" & duration.min >= 5 &
           all.species.reported == 1) %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(unique(checklist.id))) %>% 
  arrange(date) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs),
         data = "Complete checklists")


## Calculate total number of checklists/month and format
tempck <- ebd %>% 
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
ckcomb <- bind_rows(tempck, ck_comp)


## Plot 
ckcomb %>% 
  ggplot(aes(x = date, y = tot.obs, color = data, alpha = data, linetype = data)) + 
  geom_line(size = 0.8) +
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
# ggsave(paste0("outputs/forpub/monthly_obs_ebird_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 350)



### Summary stats
## Create full date sequence to add zeros into the data
datesebird <- tibble(date = seq(as.Date("1958/1/1"), as.Date("2022/12/1"), by = "month"))

## Create data frame for calculations
ebirdavg <- datesebird %>% 
  full_join(tempck) %>% 
  dplyr::select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))

## Calculate average checklists/month to 2010
earlye <- ebirdavg %>% 
  filter(date <= "2010-01-01" & date >= "2000-01-01")
mean(earlye$tot.obs)
sd(earlye$tot.obs)/sqrt(length(earlye$tot.obs))

## Calculate average checklists/month to current
tote <- ebirdavg %>% 
  filter(date >= "2016-01-01")
mean(tote$tot.obs)
sd(tote$tot.obs)/sqrt(length(tote$tot.obs))


## Winter months avg 2018 - 2022
wintere <- ebirdavg %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2016-12-01" & date <= "2022-02-01") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(wintere$tot.obs)
sd(wintere$tot.obs)/sqrt(length(wintere$tot.obs))




#------------------------------------------------#
####      Cumulative sp/observers Plots       ####
#------------------------------------------------#

### Plot cumulative species and observers from iNat data
## Calculate cumulative species (rg only)
cumulativesp <- inat %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(sci.name = paste(taxon, species, sep = " "),
         observed.on = as.Date(observed.on)) %>% 
  group_by(sci.name) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  #mutate(date = ym(paste0(year, "-", month))) %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year) %>% 
  summarise(tot.obs = length(common.name)) %>%
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(tot.obs), 0, tot.obs),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Species") %>% 
  dplyr::select(year, cumsum, data)


## Calculate cumulative observers
cumulativeob <- inat %>% 
  group_by(user.login) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  #mutate(date = ym(paste0(year, "-", month))) %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year) %>% 
  summarise(observers = length(user.login)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Observers") %>% 
  dplyr::select(year, cumsum, data)


## Bind these data for plotting
cumdata <- bind_rows(cumulativesp, cumulativeob)


## Plot
cumdata %>% 
  ggplot(aes(x = year, y = cumsum, color = data, alpha = data, linetype = data)) + 
  geom_line(size = 0.8) +
  geom_dl(data = subset(cumulativeob, year == 2022), aes(label = cumsum), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  geom_dl(data = subset(cumulativesp, year == 2022), aes(label = cumsum), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Count (n)") +
  scale_x_continuous(limits = c(2014, 2024), breaks = seq(1990, 2024, by = 3)) +
  theme(legend.position = c(0.13, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual(values = c("Observers" = "gray60", "Species" = "black")) +
  scale_alpha_manual(values = c("Observers" = 1, "Species" = 1)) +
  scale_linetype_manual(values = c("Observers" = 1, "Species" = 6))


## Export figure
# ggsave(paste0("outputs/forpub/monthly_cumulative_inat_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 350)


#------------------------------------------------#


### Plot cumulative species and observers from eBird data
## Calculate cumulative species
cumulativespe <- ebd %>%
  filter(category == "species") %>% 
  group_by(scientific.name) %>% 
  filter(obs.date == min(obs.date)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  # mutate(date = ym(paste0(year(obs.date), "-", month(obs.date)))) %>%
  mutate(year = year(obs.date)) %>% 
  group_by(year) %>% 
  summarise(tot.obs = length(scientific.name)) %>%
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(tot.obs), 0, tot.obs),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Species") %>% 
  dplyr::select(year, cumsum, data)


## Calculate cumulative observers
cumulativeobe <- ebd %>% 
  group_by(observer.id) %>% 
  filter(obs.date == min(obs.date)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  # mutate(date = ym(paste0(year(obs.date), "-", month(obs.date)))) %>% 
  mutate(year = year(obs.date)) %>% 
  group_by(year) %>% 
  summarise(observers = length(observer.id)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Observers") %>% 
  dplyr::select(year, cumsum, data)


## Bind these data for plotting
cumedata <- bind_rows(cumulativespe, cumulativeobe)


## Plot
cumedata %>% 
  ggplot(aes(x = year, y = cumsum, color = data, alpha = data, linetype = data)) + 
  geom_line(size = 0.8) +
  geom_dl(data = subset(cumulativeobe, year == 2022), aes(label = cumsum), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  geom_dl(data = subset(cumulativespe, year == 2022), aes(label = cumsum), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Count (n)") +
  scale_x_continuous(limits = c(2014, 2024), breaks = seq(1990, 2024, by = 3)) +
  theme(legend.position = c(0.13, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual(values = c("Observers" = "gray60", "Species" = "black")) +
  scale_alpha_manual(values = c("Observers" = 1, "Species" = 1)) +
  scale_linetype_manual(values = c("Observers" = 1, "Species" = 6))


## Export figure
# ggsave(paste0("outputs/forpub/monthly_cumulative_ebird_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 350)




#------------------------------------------------#
####              Taxonomy stats              ####
#------------------------------------------------#

### eBird taxonomy stats
## Read in the eBird taxonomy for merging with ebd
tax <- read.csv("data/ebird_taxonomy_v2022.csv") %>% 
  dplyr::select(scientific.name = SCI_NAME, order = ORDER1, family = FAMILY,
                species.group = SPECIES_GROUP)


## Join the two 
ebdtax <- left_join(ebd, tax, by = "scientific.name")


## Determine how many orders were recorded
unique(ebdtax$order)


## Determine the percent of data made up by each order
e_orders <- ebdtax %>% 
  filter(order != "") %>% 
  group_by(order) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(count = round((count/length(ebd$scientific.name))*100, digits = 2))

# write.csv(e_orders, "outputs/forpub/ebird_orders_table.csv", row.names = F)


## Calculate frequency of obs for each species
ebd %>% 
  mutate(count = ifelse(count == "X", 1, count),
         count = as.numeric(count)) %>% 
  group_by(common.name, scientific.name) %>% 
  summarize(frequency = round((length(scientific.name)/length(unique(ebd$checklist.id))*100), 2)) %>% 
  arrange(-frequency)


## Calculate mean +- SE species per complete checklist
avg.ch <- ebd %>% 
  filter(category == "species") %>% 
  filter(protocol == "Traveling" | protocol == "Stationary" & duration.min >= 5) %>% 
  group_by(checklist.id) %>% 
  summarise(richness = length(scientific.name))
mean(avg.ch$richness)
sd(avg.ch$richness)/sqrt(length(avg.ch$richness))


#------------------------------------------------#


### iNaturalist taxonomy stats
## Format data for summaries
sptots <- inat %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(scientific.name = paste(taxon, species, sep = " ")) 


## Total observations per species
sptots %>% 
  group_by(scientific.name) %>% 
  summarise(total = length(scientific.name)) %>% 
  arrange(-total) 


## Totals species per kingdom
i_kingdoms_table <- inat %>% 
  filter(taxon.kingdom.name != "" & taxon.kingdom.name != "Viruses") %>% 
  group_by(taxon.kingdom.name) %>% 
  summarise(count = length(taxon.kingdom.name)) %>% 
  arrange(-count)

# write.csv(i_kingdoms_table, "outputs/forpub/inat_kingdoms_table.csv", row.names = F)


## Total species per order Plantae
sptots %>% 
  filter(taxon.kingdom.name == "Plantae") %>% 
  group_by(taxon.order.name) %>%
  summarise(count = length(taxon.order.name)) %>% 
  arrange(-count)


## Total obs per order Plantae
inat %>% 
  dplyr::select(scientific.name, observed.on:positional.accuracy, 
                taxon.kingdom.name, taxon.order.name) %>% 
  filter(taxon.kingdom.name == "Plantae") %>% 
  group_by(taxon.order.name) %>%
  summarise(count = length(taxon.order.name)) %>% 
  arrange(-count)


## Total species per order Animalia
sptots %>% 
  filter(taxon.kingdom.name == "Animalia") %>% 
  group_by(taxon.order.name) %>%
  summarise(count = length(taxon.order.name)) %>% 
  arrange(-count)


## Total obs per order Animalia
inat %>% 
  dplyr::select(scientific.name, observed.on:positional.accuracy,
                taxon.kingdom.name, taxon.order.name) %>% 
  filter(taxon.kingdom.name == "Animalia") %>% 
  group_by(taxon.order.name) %>%
  summarise(count = length(taxon.order.name)) %>% 
  arrange(-count)


## Total orders
i_orders <- sptots %>% 
  filter(taxon.order.name != "") %>% 
  group_by(taxon.order.name) %>% 
  summarise(length = length(taxon.order.name)) %>% 
  arrange(-length) %>% 
  mutate(count = round((length/length(inat$scientific.name))*100, digits = 2))

length(i_orders$taxon.order.name)



#------------------------------------------------#


## Total orders in both data sets
oe <- ebdtax %>% 
  dplyr::select(scientific.name, order)


oi <- sptots %>% 
  rename(order = taxon.order.name) %>% 
  dplyr::select(scientific.name, order)


oei <- bind_rows(oe, oi) 

allorders <- oei %>% 
  group_by(order) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(percent = round((count/length(oei$scientific.name))*100, digits = 2)) %>% 
  filter(order != "")


write.csv(allorders, "outputs/forpub/all_orders_table.csv", row.names = F)




#------------------------------------------------#
####           Observation Heat Map           ####
#------------------------------------------------#

## Combine all data
griddat <- bind_rows(inat, ebd) %>% 
  dplyr::select(common.name, scientific.name, observed.on, place.guess, latitude, longitude)

## Specify min/max for grid
xmn = min(griddat$longitude) - 0.15
xmx = max(griddat$longitude) + 0.15
ymn = min(griddat$latitude) - 0.05
ymx = max(griddat$latitude) + 0.05

## Create grid
r = raster(matrix(1:1600, 40, 40), xmx = xmx, xmn = xmn, ymx = ymx, ymn = ymn)


## Format points
pts = griddat %>% 
  dplyr::select(longitude, latitude) %>% 
  rename(x = longitude, y = latitude) %>% 
  as.data.frame()

# Make a raster of zeroes like the input
r2 = r
r2[] = 0

# Get the cell index for each point and make a table
counts = table(cellFromXY(r,pts))

# Fill in the raster with the counts from the cell index
r2[as.numeric(names(counts))] = counts

## Change raster into dataframe
r3 <- as.data.frame(r2, xy = TRUE) %>% 
  rename(count = layer) %>% 
  mutate(count2 = as.numeric(ifelse(count == 0, "NA", count)))

## Calculate grid size
distm(c(-68.66255, 44.46557), c(-68.66983, 44.46557), fun = distHaversine)

## Read in the fee boundary shapefile
kww.b <- sf::read_sf("data/kww_boundary/kww_boundary_polygon.shp") %>% 
  st_transform(4326)


## Plot
ggplot() +
  #geom_sf(fill = "gray", data = acad.bm) +
  geom_sf(color = "black", fill = "white", linewidth = 0.7,
          data = kww.b) +
  geom_tile(aes(x = x, y = y, fill = count2),
            data = r3 %>% filter(!is.na(count2))) +
  geom_sf(color = "white", fill = "transparent", linewidth = 0.3,
          data = kww.b) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(fill = "Observations") +
  lims(x = c(-68.985, -68.385), y = c(45.78, 46.16)) +
  scale_fill_viridis_b(breaks = c(1, 100, 300, 500, 1000, 1500)) +
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
ggsave("outputs/forpub/obs_heatmap.png", dpi = 700, width = 6, height = 5.4)









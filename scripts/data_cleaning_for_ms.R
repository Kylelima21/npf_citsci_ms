### Acadia National Park (ACAD) and Katahdin Woods and Waters National Monument (KAWW)
### iNaturalist and eBird synthesis script for data formatting and cleaning
### Schoodic Institute at Acadia National Park, 2025


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

## Call packages
library(tidyverse)


## Source the function script
source("scripts/analysis_functions.R")




#------------------------------------------------#
####        Data Import and Cleaning          ####
#------------------------------------------------#

### ACAD data import ###

## Read, format, filter to ACAD, and clean the iNaturalist data
inatA <- tibble(read.csv("data/acad_inat_obs_20251218.csv")) %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude") %>% 
  filter(observed_on <= "2024-12-31") %>% 
  mutate(year = year(observed_on),
         month = month(observed_on)) %>% 
  rename_with(~str_replace_all(., "_", "."), .cols = everything()) %>% 
  select(id, observed.on, time.observed.at, user.id, user.login,
         quality.grade:positional.accuracy, coordinates.obscured,
         species.guess:month) %>% 
  mutate(park = "ACAD",
         genus = str_extract(scientific.name, "^\\w*")) %>% 
  select(common.name, scientific.name = taxon.species.name, taxon.species.name, taxon.subspecies.name, 
         iconic.taxon.name, observed.on, year, month, quality.grade, latitude, 
         longitude, user.login, id:taxon.family.name, genus, 
         full.scientific.name = scientific.name, everything()) %>% 
  rename(kingdom = taxon.kingdom.name, phylum = taxon.phylum.name,
         class = taxon.class.name, order = taxon.order.name,
         family = taxon.family.name,
         subspecies = taxon.subspecies.name)

## Save
write.csv(inatA, "data/clean_data_for_ms/iNaturalist_acadia.csv", row.names = F)



## Read, format, filter to ACAD, and clean the eBird data
ebdA <- tibble(read.delim("data/ebd_US-ME_relNov-2025.txt", header = T, quote = "")) %>% 
  select(c('COMMON.NAME', 'SCIENTIFIC.NAME', 'CATEGORY', 'OBSERVATION.DATE', 'OBSERVATION.COUNT', 
           'DURATION.MINUTES', 'SAMPLING.EVENT.IDENTIFIER', 'OBSERVER.ID', 'NUMBER.OBSERVERS',
           'ALL.SPECIES.REPORTED', 'EFFORT.DISTANCE.KM', 'LOCALITY', 'COUNTY', 'PROTOCOL.NAME',
           'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'checklist.id'='SAMPLING.EVENT.IDENTIFIER', 'latitude'='LATITUDE', 'longitude'='LONGITUDE',
         'observer.id'='OBSERVER.ID', 'category'='CATEGORY', 'county'='COUNTY', 
         'all.species.reported'='ALL.SPECIES.REPORTED', 'protocol'='PROTOCOL.NAME',
         'duration.min'='DURATION.MINUTES', 'num.observers'='NUMBER.OBSERVERS', 
         'distance.km'='EFFORT.DISTANCE.KM') %>% 
  mutate(park = "ACAD") %>% 
  filter(obs.date <= "2024-12-31") %>% 
  filter(checklist.id != "S56409710") %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude")


## Save
write.csv(ebdA, "data/clean_data_for_ms/eBird_acadia.csv", row.names = F)



#------------------------------------------------#

### KAWW data import ###

## Read, format, filter to KAWW, and clean the iNaturalist data
inatK <- tibble(read.csv("data/kaww_inat_obs_20251218.csv")) %>% 
  filter_nps(., "Katahdin Woods and Waters National Monument", "latitude", "longitude") %>% 
  filter(observed_on <= "2024-12-31") %>% 
  mutate(year = year(observed_on),
         month = month(observed_on)) %>% 
  rename_with(~str_replace_all(., "_", "."), .cols = everything()) %>% 
  select(id, observed.on, time.observed.at, user.id, user.login,
         quality.grade:positional.accuracy, coordinates.obscured,
         species.guess:month) %>% 
  mutate(park = "KAWW") %>% 
  select(common.name, scientific.name = taxon.species.name, taxon.species.name, taxon.subspecies.name, 
         iconic.taxon.name, observed.on, year, month, quality.grade, latitude, 
         longitude, user.login, everything(), full.scientific.name = scientific.name) %>% 
  rename(kingdom = taxon.kingdom.name, phylum = taxon.phylum.name,
         class = taxon.class.name, order = taxon.order.name,
         family = taxon.family.name, genus = taxon.genus.name,
         subspecies = taxon.subspecies.name)


## Save
write.csv(inatK, "data/clean_data_for_ms/iNaturalist_katahdin.csv", row.names = F)



## Read, format, filter to KAWW, and clean the eBird data
ebdK <- tibble(read.delim("data/ebd_US-ME_relNov-2025.txt", header = T, quote = "")) %>% 
  select(c('COMMON.NAME', 'SCIENTIFIC.NAME', 'CATEGORY', 'OBSERVATION.DATE', 'OBSERVATION.COUNT', 
           'DURATION.MINUTES', 'SAMPLING.EVENT.IDENTIFIER', 'OBSERVER.ID', 'NUMBER.OBSERVERS',
           'ALL.SPECIES.REPORTED', 'EFFORT.DISTANCE.KM', 'LOCALITY', 'COUNTY', 'PROTOCOL.NAME',
           'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'checklist.id'='SAMPLING.EVENT.IDENTIFIER', 'latitude'='LATITUDE', 'longitude'='LONGITUDE',
         'observer.id'='OBSERVER.ID', 'category'='CATEGORY', 'county'='COUNTY', 
         'all.species.reported'='ALL.SPECIES.REPORTED', 'protocol'='PROTOCOL.NAME',
         'duration.min'='DURATION.MINUTES', 'num.observers'='NUMBER.OBSERVERS', 
         'distance.km'='EFFORT.DISTANCE.KM') %>% 
  mutate(park = "KAWW") %>% 
  filter_nps(., "Katahdin Woods and Waters National Monument", "latitude", "longitude") %>% 
  filter(obs.date <= "2024-12-31") %>% 
  filter(checklist.id != "S2371044")


## Save
write.csv(ebdK, "data/clean_data_for_ms/eBird_katahdin.csv", row.names = F)




#------------------------------------------------#

### Visitation data ### 

## Read in and format visitation data
visits <- tibble(read.csv("data/acad_kaww_visits_data.csv")) %>% 
  rename(ACAD = acad.visits, KAWW = kaww.visits) %>% 
  pivot_longer(cols = c(ACAD, KAWW), values_to = "visits", names_to = "park") %>% 
  filter(!is.na(visits)) %>% 
  arrange(park, year)


## Save
write.csv(visits, "data/clean_data_for_ms/park_visitation_data.csv", row.names = F)






#------------------------------------------------#

### Fixing managers watchlist ACAD ### 

itac <- tibble(read.csv("data/taxa.csv")) %>% 
  select(kingdom:specificEpithet, scientificName) %>% 
  rename(scientific.name = scientificName)


naw <- acad.watch %>% 
  left_join(itac, by = "scientific.name") %>% 
  distinct() %>% 
  arrange(status, scientific.name) %>% 
  filter(specificEpithet != "")


adx <- read_excel("data/acad_watchlist_species.xlsx") %>% 
  mutate(present = ifelse(in.anp == "P", "Y", in.anp)) %>% 
  select(scientific.name, present) %>% 
  distinct()
  

naw2 <- naw %>% 
  left_join(adx, by = "scientific.name")


write.csv(naw2, "data/clean_data_for_ms/acad_watchlist.csv", row.names = F)




### Fixing managers watchlist KAWW ### 
kdx <- tibble(read_excel("data/kaww_watchlist_species.xlsx")) %>% 
  rename(present = "in.kaww") %>% 
  left_join(itac, by = "scientific.name") %>% 
  arrange(status, scientific.name) %>% 
  filter(specificEpithet != "") %>% 
  arrange(status, scientific.name)


write.csv(kdx, "data/clean_data_for_ms/kaww_watchlist.csv", row.names = F)



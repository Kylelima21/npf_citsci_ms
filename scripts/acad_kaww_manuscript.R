### Acadia National Park (ACAD) and Katahdin Woods and Waters National Monument (KAWW)
### iNaturalist and eBird synthesis script for manuscript
### Schoodic Institute at Acadia National Park, 2025


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

## Call packages
library(tidyverse)
library(sf)
library(leaflet)
library(directlabels)
library(scales)
library(cowplot)
library(conflicted)
library(raster) 
library(geosphere)


## Source the function script
source("scripts/analysis_functions.R")


## Specify the functions from dplyr
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)




#------------------------------------------------#
####             Read in the data             ####
#------------------------------------------------#

### ACAD data ###

## Read in the iNaturalist data
inatA <- tibble(read.csv("data/clean_data_for_ms/iNaturalist_acadia.csv"))

## Read in the eBird data
ebdA <- tibble(read.csv("data/clean_data_for_ms/eBird_acadia.csv"))

## Read in the ACAD base map for figures
acad.bm <- sf::read_sf("data/acad_boundary/formapping.shp")

## Read in the ACAD boundary layer
acad.bounds <- sf::read_sf("data/acad_boundary/acad_feeboundary_polygon.shp")

## Read in the fee boundary shape file
acad.fee <- sf::read_sf("data/acad_boundary/acad_feeboundary_polygon.shp") %>% 
  st_transform(4326)

## Read in watchlist
acad.watch <- tibble(read.csv("data/clean_data_for_ms/acad_watchlist.csv"))



#------------------------------------------------#

### KAWW data ###

## Read in the iNaturalist data
inatK <- tibble(read.csv("data/clean_data_for_ms/iNaturalist_katahdin.csv"))

## Read in the eBird data
ebdK <- tibble(read.csv("data/clean_data_for_ms/eBird_katahdin.csv"))

# ## Read in the KAWW boundary layer
kaww.bounds <- sf::read_sf("data/kaww_boundary/kaww_bounds.shp") %>%
  st_transform(., crs = '+proj=longlat +datum=WGS84')

## Read in the fee boundary shape file
kww.b <- sf::read_sf("data/kaww_boundary/kaww_bounds.shp") %>% 
  st_transform(4326)

## Read in watchlist
kaww.watch <- tibble(read.csv("data/clean_data_for_ms/kaww_watchlist.csv"))



#------------------------------------------------#

### Visitation and taxonomic data ###

## Read in visitation data
visits <- tibble(read.csv("data/clean_data_for_ms/park_visitation_data.csv"))


## Read in the eBird taxonomy for merging with ebd
etax <- read.csv("data/ebird_taxonomy_v2025.csv") %>% 
  select(scientific.name = SCI_NAME, order = ORDER, family = FAMILY,
         species.group = SPECIES_GROUP)




#------------------------------------------------#
####          Quantity - Observers            ####
#------------------------------------------------#

### Total observers ###

## Calculate unique observers for ACAD
nrow(inatA %>% distinct(user.id)) + nrow(ebdA %>% distinct(observer.id))


## Calculate unique observers for KAWW
nrow(inatK %>% distinct(user.id)) + nrow(ebdK %>% distinct(observer.id))


## Get cumulative eBird observers and observations for each year and park 
## and merge with visit data
ebdv <- bind_rows(ebdA, ebdK) %>% 
  mutate(year = year(obs.date)) %>% 
  group_by(year, park) %>% 
  summarise(ebird.observers = length(unique(observer.id)),
            ebird.observations = length(common.name),
            .groups = "drop") %>% 
  arrange(park, year) %>% 
  left_join(visits)


## Get cumulative iNat observers and observations for each year and park 
## and merge with visit data
inatv <- bind_rows(inatA, inatK) %>% 
  group_by(year, park) %>% 
  summarise(inat.observers = length(unique(user.id)),
            inat.observations = length(id),
            .groups = "drop") %>% 
  arrange(park, year) %>% 
  left_join(visits)


## Combine eBird and iNat data and filter to years 2020 and beyond since that 
## is when KAWW started collecting visitation data. Then calculate park based 
## annual observers per visit and observations per visit.
visdat <- left_join(ebdv, inatv, by = c("year", "park", "visits")) %>% 
  filter(year >= 2020) %>% 
  mutate(observers = ebird.observers + inat.observers,
         observations = ebird.observations + inat.observations) %>% 
  select(year, park, observers, observations, visits) %>% 
  mutate(observers.vis = observers/visits,
         observations.vis = observations/visits) 


## Calculate mean and SE observers per visit
visdat %>% 
  group_by(park) %>%
  summarise(mean.observers = mean(observers.vis),
            se.observers = sd(observers.vis)/sqrt(length(observers.vis)))


## Test difference between parks
wilcox.test(observers.vis ~ park, data = visdat)


### Observer increase over the last decade ###
left_join(ebdv, inatv, by = c("year", "park", "visits")) %>% 
  select(year, park, ebird.observers, inat.observers) %>% 
  group_by(park, year) %>% 
  summarise(cum.observers = sum(ebird.observers, inat.observers, na.rm = T)) %>% 
  filter(year == 2014 | year == 2024)



#------------------------------------------------#

### Cumulative Observers Figure ###

### iNaturalist cumulative observers figure
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
  select(year, cumsum, park)


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
  select(year, cumsum, park)


## Bind these data for plotting
inatcumob <- bind_rows(cumulativeobA, cumulativeobK)


## Plot
inatone <- inatcumob %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(cumulativeobA, year == 2024), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 1.8), "last.points")) +
  geom_dl(data = subset(cumulativeobK, year == 2024), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y + 0.4, x = x - 0.2), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "iNaturalist observers") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1960, 2026), breaks = seq(1960, 2026, by = 10)) +
  theme(legend.position = "none",
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_text(face = "bold", size = 17),
        legend.text = element_text(color = "black", size = 17,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 17),
        axis.title = element_text(color = "black", size = 17),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
  scale_color_manual("NPS Unit", values = c("ACAD" = "gray60", "KAWW" = "black")) +
  scale_linetype_manual("NPS Unit", values = c("ACAD" = 1, "KAWW" = 6))

inatone


## Export figure
# ggsave(paste0("outputs/forpub/final_cumulative_inat_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 700)



### eBird cumulative observers figure
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
  select(year, cumsum, park)


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
  select(year, cumsum, park)


## Bind these data for plotting
ebdcumob <- bind_rows(cumulativeobeA, cumulativeobeK)


## Plot
ebdone <- ebdcumob %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(cumulativeobeA, year == 2024), 
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 1.8), "last.points")) +
  geom_dl(data = subset(cumulativeobeK, year == 2024), 
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

ebdone


## Export figure
# ggsave(paste0("outputs/forpub/final_cumulative_ebd_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 700)





#------------------------------------------------#
####   Quantity - Observations and Taxonomy   ####
#------------------------------------------------#

### Observations per visit ###

## Calculate mean and SE observations per visit
visdat %>% 
  group_by(park) %>%
  summarise(mean.observations = mean(observations.vis),
            se.observations = sd(observations.vis)/sqrt(length(observations.vis)))


## Test difference between parks
wilcox.test(observations.vis ~ park, data = visdat)




#------------------------------------------------#
#------------------------------------------------#

# ├ ACAD Observations ----


### Total park dataset ###
## Total number of ACAD citsci observations
length(bind_rows(inatA, ebdA)$common.name) # 725,908


## Percent of ACAD obs that are from eBird
length(ebdA$common.name) / length(bind_rows(inatA, ebdA)$common.name) * 100 # 89%


## Percent of ACAD obs that are from iNaturalist
length(inatA$common.name) / length(bind_rows(inatA, ebdA)$common.name) * 100 # 11%



#------------------------------------------------#

### eBird ###
## All observations
length(ebdA$common.name) # 644,090


## Total checklists
ebird_chkA <- ebdA %>% 
  distinct(checklist.id)

paste0("There have been ", length(ebird_chkA$checklist.id), " checklists submitted by eBird users.")


## Average checklists per observer
length(ebird_chkA$checklist.id) / length(unique(ebdA$observer.id)) # 6.97



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
  labs(x = "Year", y = "Number of eBird checklists") +
  scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2024-12-31"), by = "4 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2004-01-01"), as.Date("2024-12-31"))) +
  theme(legend.position = c(0.18, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 13,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(color = "black", size = 13),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total checklists" = "gray50", "Complete checklists" = "black")) +
  scale_alpha_manual(values = c("Total checklists" = 0.7, "Complete checklists" = 1)) +
  scale_linetype_manual(values = c("Total checklists" = 1, "Complete checklists" = 1))


## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_ebird_acad_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 700)



### Monthly summary stats
## Create full date sequence to add zeros into the data
datesebirdA <- tibble(date = seq(as.Date("1958/1/1"), as.Date("2024/12/1"), by = "month"))


## Create data frame for calculations
ebirdavgA <- datesebirdA %>% 
  full_join(tempckA) %>% 
  select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Summer months avg 2020 - 2024
summereA <- ebirdavgA %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2020-06-01" & date <= "2024-08-31") %>% 
  filter(month == 6 | month == 7 | month == 8)
mean(summereA$tot.obs)
sd(summereA$tot.obs) / sqrt(length(summereA$tot.obs))


## Winter months avg 2020 - 2024
wintereA <- ebirdavgA %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2019-12-01" & date <= "2024-02-28") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(wintereA$tot.obs)
sd(wintereA$tot.obs) / sqrt(length(wintereA$tot.obs))



#------------------------------------------------#

### iNaturalist ###
## All observations
length(inatA$common.name) # 81,818


## Average submissions per observer
length(inatA$common.name) / length(unique(inatA$user.login)) # 11.75



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
  labs(x = "Year", y = "Number of iNaturalist observations") +
  scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2024-12-31"), by = "4 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2004-01-01"), as.Date("2024-12-31"))) +
  theme(legend.position = c(0.23, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 13,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(color = "black", size = 13),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total observations" = "gray50", "Research grade observations" = "black")) +
  scale_alpha_manual(values = c("Total observations" = 0.7, "Research grade observations" = 1)) +
  scale_linetype_manual(values = c("Total observations" = 1, "Research grade observations" = 1))


## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_inat_acad_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 700)



### Monthly summary stats
## Create full date sequence to add zeros into the data
datesinatA <- tibble(date = seq(as.Date("1976/1/1"), as.Date("2024/12/1"), by = "month"))


## Create data frame for calculations
inatavgA <- datesinatA %>% 
  full_join(alltempA) %>% 
  select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Summer months avg 2020 - 2024
summeriA <- inatavgA %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2020-06-01" & date <= "2024-08-31") %>% 
  filter(month == 6 | month == 7 | month == 8)
mean(summeriA$tot.obs)
sd(summeriA$tot.obs)/sqrt(length(summeriA$tot.obs))


## Winter months avg 2020 - 2024
winteriA <- inatavgA %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2019-12-01" & date <= "2024-02-28") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(winteriA$tot.obs)
sd(winteriA$tot.obs)/sqrt(length(winteriA$tot.obs))




#------------------------------------------------#
#------------------------------------------------#

# ├ KAWW Observations ----


### Total park dataset
## Total number of KAWW citsci observations
length(bind_rows(inatK, ebdK)$common.name) # 19,626


## Percent of KAWW obs that are from eBird
length(ebdK$common.name) / length(bind_rows(inatK, ebdK)$common.name) * 100 # 80%


## Percent of KAWW obs that are from iNaturalist
length(inatK$common.name) / length(bind_rows(inatK, ebdK)$common.name) * 100 # 20%



#------------------------------------------------#

### eBird ###
## All observations
length(ebdK$common.name) # 15,621


## Total checklists
ebird_chkK <- ebdK %>% 
  distinct(checklist.id)

paste0("There have been ", length(ebird_chkK$checklist.id), " checklists submitted by eBird users.")


## Average checklists per observer
length(ebird_chkK$checklist.id)/length(unique(ebdK$observer.id)) # 4.92



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
  labs(x = "Year", y = "Number of eBird checklists") +
  scale_x_date(breaks = seq(as.Date("2014-01-01"), as.Date("2024-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2014-01-01"), as.Date("2024-12-31"))) +
  theme(legend.position = c(0.18, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 13,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(color = "black", size = 13),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total checklists" = "gray50", "Complete checklists" = "black")) +
  scale_alpha_manual(values = c("Total checklists" = 0.7, "Complete checklists" = 1)) +
  scale_linetype_manual(values = c("Total checklists" = 1, "Complete checklists" = 1))


## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_ebird_kaww_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 700)



### Monthly summary stats
## Create full date sequence to add zeros into the data
datesebirdK <- tibble(date = seq(as.Date("1958/1/1"), as.Date("2024/12/1"), by = "month"))


## Create data frame for calculations
ebirdavgK <- datesebirdK %>% 
  full_join(tempckK) %>% 
  select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Summer months avg 2020 - 2024
summereK <- ebirdavgK %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2020-06-01" & date <= "2024-08-31") %>% 
  filter(month == 6 | month == 7 | month == 8)
mean(summereK$tot.obs)
sd(summereK$tot.obs) / sqrt(length(summereK$tot.obs))


## Winter months avg 2020 - 2024
wintereK <- ebirdavgK %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2019-12-01" & date <= "2024-02-01") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(wintereK$tot.obs)
sd(wintereK$tot.obs) / sqrt(length(wintereK$tot.obs))



#------------------------------------------------#

### iNaturalist ###
## All observations
length(inatK$common.name) # 4,005


## Average submissions per observer
length(inatK$common.name) / length(unique(inatK$user.login)) # 28.2



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
  labs(x = "Year", y = "Number of iNaturalist observations") +
  scale_x_date(breaks = seq(as.Date("2014-01-01"), as.Date("2024-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2014-01-01"), as.Date("2024-12-31"))) +
  theme(legend.position = c(0.23, 0.85),
        legend.background = element_rect(color = "black", linewidth = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 13,  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(color = "black", size = 13),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total observations" = "gray50", "Research grade observations" = "black")) +
  scale_alpha_manual(values = c("Total observations" = 0.7, "Research grade observations" = 1)) +
  scale_linetype_manual(values = c("Total observations" = 1, "Research grade observations" = 1))


## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_inat_kaww_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 700)



### Monthly summary stats
## Create full date sequence to add zeros into the data
datesinatK <- tibble(date = seq(as.Date("1976/1/1"), as.Date("2024/12/1"), by = "month"))


## Create data frame for calculations
inatavgK <- datesinatK %>% 
  full_join(alltempK) %>% 
  select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Summer months avg 2020 - 2024
summeriK <- inatavgK %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2020-06-01" & date <= "2024-08-31") %>% 
  filter(month == 6 | month == 7 | month == 8)
mean(summeriK$tot.obs)
sd(summeriK$tot.obs)/sqrt(length(summeriK$tot.obs))


## Winter months avg 2020 - 2024
winteriK <- inatavgK %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2019-12-01" & date <= "2024-02-28") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(winteriK$tot.obs)
sd(winteriK$tot.obs)/sqrt(length(winteriK$tot.obs))




#------------------------------------------------#
#------------------------------------------------#

# ├ Observers Observations Figure ----

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
  geom_dl(data = subset(totalebd, year == 2024 & park == "ACAD"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 2.5), "last.points")) +
  geom_dl(data = subset(totalebd, year == 2024 & park == "KAWW"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y + 0.4, x = x - 0.8), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "eBird observations") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1960, 2027), breaks = seq(1960, 2027, by = 10)) +
  theme(legend.position = "none",
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
  geom_dl(data = subset(totalinat, year == 2024 & park == "ACAD"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y, x = x - 2.1), "last.points")) +
  geom_dl(data = subset(totalinat, year == 2024 & park == "KAWW"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.45, dl.trans(y = y + 0.4, x = x - 0.7), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "iNaturalist observations") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(1960, 2027), breaks = seq(1960, 2027, by = 10)) +
  theme(legend.position = "none",
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
#------------------------------------------------#

# ├ ACAD Taxonomy ----

### eBird ###

## Join ebd with correct ebird taxonomy info
ebdtaxA <- left_join(ebdA, etax, by = "scientific.name")


## Total species
ebdtaxA %>% 
  filter(category == "species" | category == "domestic" | category == "issf" |
           category == "form") %>% 
  select(scientific.name) %>% 
  distinct()


## Determine how many orders were recorded
unique(ebdtaxA$order) # 20



#------------------------------------------------#

### iNaturalist ###

## Total rg species
inatA %>% 
  filter(scientific.name != "" & quality.grade == "research") %>% 
  select(scientific.name) %>% 
  distinct()


## Total rg kingdoms
inatA %>% 
  filter(kingdom != "" & quality.grade == "research") %>% 
  group_by(kingdom) %>% 
  summarise(count = length(kingdom)) %>% 
  arrange(-count)


## Total rg orders
inatA %>% 
  filter(order != "" & quality.grade == "research") %>% 
  select(order) %>% 
  distinct()


## Total obs per kingdom
i_kingdoms_obsA <- inatA %>% 
  filter(kingdom != "") %>% 
  group_by(kingdom) %>% 
  summarise(count = length(kingdom)) %>% 
  arrange(-count)

i_kingdoms_obsA


## Total rg species per kingdom
inatA %>%
  filter(scientific.name != "" & quality.grade == "research") %>%
  select(scientific.name, kingdom) %>%
  distinct() %>%
  group_by(kingdom) %>%
  summarise(count = length(kingdom)) %>%
  arrange(-count)


## Total rg obs per kingdom and proportion of rg obs to total obs
i_kingdoms_rgA <- inatA %>% 
  filter(quality.grade == "research") %>% 
  group_by(kingdom) %>% 
  summarise(rg.count = length(scientific.name)) %>% 
  arrange(-rg.count)

bind_cols(i_kingdoms_rgA, i_kingdoms_obsA) %>% 
  select(kingdom = `kingdom...1`, rg.count, count) %>% 
  mutate(prop = 100 * (rg.count / count)) %>% 
  arrange(-prop)




#------------------------------------------------#
#------------------------------------------------#

# ├ KAWW Taxonomy ----

### eBird ###

## Join ebd with correct ebird taxonomy info
ebdtaxK <- left_join(ebdK, etax, by = "scientific.name")


## Total species
ebdtaxK %>% 
  filter(category == "species" | category == "domestic" | category == "issf" |
           category == "form") %>% 
  select(scientific.name) %>% 
  distinct()


## Determine how many orders were recorded
unique(ebdtaxK$order) # 18



#------------------------------------------------#

### iNaturalist ###

## Total rg species
inatK %>% 
  filter(scientific.name != "" & quality.grade == "research") %>% 
  select(scientific.name) %>% 
  distinct()


## Total rg kingdoms
inatK %>% 
  filter(kingdom != "" & quality.grade == "research") %>% 
  group_by(kingdom) %>% 
  summarise(count = length(kingdom)) %>% 
  arrange(-count)


## Total rg orders
inatK %>% 
  filter(order != "" & quality.grade == "research") %>% 
  select(order) %>% 
  distinct()


## Total obs per kingdom
i_kingdoms_obsK <- inatK %>% 
  filter(kingdom != "") %>% 
  group_by(kingdom) %>% 
  summarise(count = length(kingdom)) %>% 
  arrange(-count)

i_kingdoms_obsK


## Total rg species per kingdom
inatK %>% 
  filter(scientific.name != "" & quality.grade == "research") %>% 
  select(scientific.name, kingdom) %>% 
  distinct() %>% 
  group_by(kingdom) %>% 
  summarise(count = length(kingdom)) %>% 
  arrange(-count)


## Total rg obs per kingdom and proportion of rg obs to total obs
i_kingdoms_rgK <- inatK %>% 
  filter(quality.grade == "research") %>% 
  group_by(kingdom) %>% 
  summarise(rg.count = length(scientific.name)) %>% 
  arrange(-rg.count)

bind_cols(i_kingdoms_rgK, i_kingdoms_obsK %>% filter(kingdom != "Viruses")) %>% 
  select(kingdom = `kingdom...1`, rg.count, count) %>% 
  mutate(prop = 100 * (rg.count / count)) %>% 
  arrange(-prop)




#------------------------------------------------#
#------------------------------------------------#

# ├ Species Accumulation Figure ----

## Filter data sets to proper species data
iKcumsp <- inatK %>% 
  filter(quality.grade == "research" & scientific.name != "") %>% 
  select(scientific.name, observed.on, park)

eKcumsp <- ebdK %>% 
  filter(category == "species" | category == "domestic" | category == "issf" |
           category == "form") %>% 
  select(scientific.name, observed.on = obs.date, park)

iAcumsp <- inatA %>% 
  filter(quality.grade == "research" & scientific.name != "") %>% 
  select(scientific.name, observed.on, park)

eAcumsp <- ebdA %>% 
  filter(category == "species" | category == "domestic" | category == "issf" |
           category == "form") %>% 
  select(scientific.name, observed.on = obs.date, park)


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
  select(year, cumsum, park)


## Plot 
icumplot <- icumulativespp %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(icumulativespp, year == 2024 & park == "ACAD"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.3, dl.trans(y = y, x = x - 1.7), "last.points")) +
  geom_dl(data = subset(icumulativespp, year == 2024 & park == "KAWW"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.3, dl.trans(y = y + 0.55, x = x - 0.5), "last.points")) +
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
  select(year, cumsum, park)


## Plot
ecumplot <- ecumulativespp %>% 
  ggplot(aes(x = year, y = cumsum, color = park, linetype = park)) + 
  geom_line(linewidth = 0.8) +
  geom_dl(data = subset(ecumulativespp, year == 2024 & park == "ACAD"),
          aes(label = format(cumsum, big.mark = ",", scientific = FALSE)), color = "black",
          method = list(cex = 1.3, dl.trans(y = y + 0.4, x = x - 0.5), "last.points")) +
  geom_dl(data = subset(ecumulativespp, year == 2024 & park == "KAWW"),
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

### Research grade iNaturalist observations by kingdom

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
  arrange(park, -percent.rg) %>% 
  select(park, kingdom, percent.rg, total.obs, rg.obs)


## Write out
# write.csv(rgkingtab, "outputs/forpub/supptable_rg_kingdoms.csv", row.names = F)






#------------------------------------------------#
####      Quantity - Spatial Coverage         ####
#------------------------------------------------#

### ACAD ### 

## Combine all data but first remove general hotspot for park
scebdA <- ebdA %>% 
  filter(locality != "Acadia NP (Please use more specific location if possible)")

griddatA <- bind_rows(inatA, scebdA) %>% 
  select(common.name, scientific.name, observed.on, place.guess, latitude, longitude)


## Specify min/max for grid
xmnA = min(griddatA$longitude) - 0.01
xmxA = max(griddatA$longitude) + 0.01
ymnA = min(griddatA$latitude) - 0.01
ymxA = max(griddatA$latitude) + 0.01


## Create grid
rA = raster(matrix(1:8649, 93, 93), xmx = xmxA, xmn = xmnA, ymx = ymxA, ymn = ymnA)


## Format points
ptsA = griddatA %>% 
  select(longitude, latitude) %>% 
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


## Plot
ggplot() +
  geom_sf(fill = "white", data = acad.bm) +
  geom_tile(aes(x = x, y = y, fill = count2),
            data = r3A %>% filter(!is.na(count2))) +
  geom_sf(color = "black", fill = "transparent", linewidth = 1,
          data = acad.fee) +
  geom_sf(color = "white", fill = "transparent", linewidth = 0.3,
          data = acad.fee) +
  labs(fill = "Observations") +
  lims(x = c(-68.48, -67.99), y = c(44.17, 44.48)) +
  scale_fill_viridis_b(breaks = c(1, 100, 250, 500, 1000, 5000, 10000, 20000, 30000)) +
  theme_minimal() +
  theme(
    legend.position = c(0.112, 0.818),
    legend.margin = margin(5,5,10,6),
    legend.background = element_rect(color = "black", fill = "white", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = "transparent", linewidth = 0.5),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "gray"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())


## Save plot
# ggsave("outputs/forpub/heatmap_acad_mdi_20260123.png", dpi = 700, width = 6, height = 5.4)


## Plot Isle Au Haut
ggplot() +
  geom_sf(fill = "white", data = acad.bm) +
  geom_tile(aes(x = x, y = y, fill = count2),
            data = r3A %>% filter(!is.na(count2))) +
  geom_sf(color = "black", fill = "transparent", linewidth = 1.2,
          data = acad.fee) +
  geom_sf(color = "white", fill = "transparent", linewidth = 0.4,
          data = acad.fee) +
  labs(fill = "Observations") +
  lims(x = c(-68.7099, -68.42), y = c(43.95, 44.12)) +
  scale_fill_viridis_b(breaks = c(1, 100, 250, 500, 1000, 5000, 10000, 20000, 30000)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = "transparent", linewidth = 0.5),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "gray"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())


## Save plot
# ggsave("outputs/forpub/heatmap_acad_isleauhaut_20260123.png", dpi = 700, width = 6, height = 6)



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
  select(everything(), latitude = latitude.keep, longitude = longitude.keep)


## Calculate the percentage of cells with observations
length((filtcobsA %>% filter(count > 0))$count) / length(filtcobsA$count) * 100 # 96.32%



#------------------------------------------------#

### KAWW ###

## Combine all data but first remove general hotspot for park
scebdK <- ebdK %>% 
  mutate(locality = ifelse(checklist.id == "S113870051", "KAWW", locality)) %>% 
  filter(locality != "Katahdin Woods and Waters National Monument")

griddatK <- bind_rows(inatK, scebdK) %>% 
  select(common.name, scientific.name, observed.on, place.guess, latitude, longitude)


## Specify min/max for grid
xmnK = min(griddatK$longitude) - 0.01
xmxK = max(griddatK$longitude) + 0.005
ymnK = min(griddatK$latitude) - 0.004
ymxK = max(griddatK$latitude) + 0.008


## Create grid
rK = raster(matrix(1:2204, 58, 38), xmx = xmxK, xmn = xmnK, ymx = ymxK, ymn = ymnK)


## Format points
ptsK = griddatK %>% 
  select(longitude, latitude) %>% 
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


## Plot
ggplot() +
  geom_sf(color = "black", fill = "white", linewidth = 0.7, data = kww.b) +
  geom_tile(aes(x = x, y = y, fill = count2),
            data = r3K %>% filter(!is.na(count2))) +
  geom_sf(color = "white", fill = "transparent", linewidth = 0.3,
          data = kww.b) +
  labs(fill = "Observations") +
  lims(x = c(-68.965, -68.47), y = c(45.82, 46.13)) +
  scale_fill_viridis_b(breaks = c(1, 100, 250, 500, 1000, 5000, 10000, 20000, 30000)) +
  theme_minimal() +
  theme(
    legend.position = c(0.112, 0.818),
    legend.margin = margin(5,5,10,6),
    legend.background = element_rect(color = "black", fill = "white", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = "transparent", linewidth = 0.5),
    plot.background = element_rect(color = "white"),
    panel.background = element_rect(fill = "gray"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())


## Save plot
# ggsave("outputs/forpub/heatmap_kaww_20260123.png", dpi = 700, width = 6, height = 5.4)



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
  select(everything(), latitude = latitude.keep, longitude = longitude.keep)


## Calculate the percentage of cells with observations
length((filtcobsK %>% filter(count > 0))$count) / length(filtcobsK$count) * 100 # 36.94




#------------------------------------------------#
####    Quality - Scientific Applicability    ####
#------------------------------------------------#

### ACAD ###

## Get all complete checklists
ebirdcompA <- ebdA %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  distinct(checklist.id)

length(ebirdcompA$checklist.id) # 48,918


## Percentage of checklists that are complete
paste0(round(length(ebirdcompA$checklist.id) / length(ebird_chkA$checklist.id) * 100, digits = 2), "% of all checklists are complete.")


## Get all obs that are research grade
rgA <- inatA %>% 
  filter(quality.grade == "research") 
length(rgA$common.name)                # 48,488


## Percentage of observations that are research grade
paste0(round(length(rgA$scientific.name) / length(inatA$scientific.name) * 100, digits = 2), "% of all observations are research grade.")


## Total species level IDs
spidsA <- inatA %>% 
  filter(scientific.name != "")

nrow(spidsA) # 61,791
length(spidsA$common.name) / length(inatA$common.name) * 100 # 76 %


## Species level IDs that are research grade
nrow(spidsA %>% filter(quality.grade == "research")) # 48,014



#------------------------------------------------#

### KAWW ###

## Get all complete checklists
ebirdcompK <- ebdK %>% 
  filter(duration.min >= 5 & all.species.reported == 1 & protocol != "Incidental") %>% 
  distinct(checklist.id)

length(ebirdcompK$checklist.id) # 1,119


## Percentage of checklists that are complete
paste0(round(length(ebirdcompK$checklist.id)/length(ebird_chkK$checklist.id)*100, digits = 2), "% of all checklists are complete.")


## Get all obs that are research grade
rgK <- inatK %>% 
  filter(quality.grade == "research") 
length(rgK$common.name)                # 2,560


## Percentage of observations that are research grade
paste0(round(length(rgK$scientific.name) / length(inatK$scientific.name) * 100, digits = 2), "% of all observations are research grade.")


## Total species level IDs
spidsK <- inatK %>% 
  filter(scientific.name != "")

nrow(spidsK) # 3,343
length(spidsK$common.name) / length(inatK$common.name) * 100 # 83 %


## Species level IDs that are research grade
nrow(spidsK %>% filter(quality.grade == "research")) # 2,539





#------------------------------------------------#
####    Quality - Management Applicability    ####
#------------------------------------------------#

### ACAD ###

## How many species on watchlist?
nrow(acad.watch)


## Create full data set with only research-grade grade observations
map_inatA <- inatA %>%
  filter(quality.grade == "research")

map_ebdA <- ebdA %>% 
  mutate(positional.accuracy = NA) %>% 
  rename(observed.on = obs.date, place.guess = locality)

mapdatA <- bind_rows(map_inatA, map_ebdA) %>% 
  select(common.name, scientific.name, observed.on, place.guess, latitude, longitude, positional.accuracy, url) %>% 
  mutate(cat = "All observations")


## Filter to watchlist species to get rare, invasive, and T&E species
acad.wl.full <- mapdatA %>% 
  filter(scientific.name %in% acad.watch$scientific.name) %>% 
  left_join(acad.watch, by = "scientific.name")


## What percent of species known to be in the park have been documented by citsci
nrow(acad.wl.full %>% filter(present == "Y") %>% distinct(scientific.name))/
  nrow(acad.watch %>% filter(present == "Y")) * 100
# 121 / 174


## Number of state and federal T&E observations
acad.wl.full %>% 
  group_by(status) %>% 
  summarise(n.obs = n())


## Number of state and federal T&E species
acad.wl.full %>% 
  filter(status == "federal/state TE") %>% 
  distinct(scientific.name)


## Exploring observations of T&E species
acad.wl.full %>% 
  filter(status == "federal/state TE") %>% 
  group_by(scientific.name, kingdom, phylum, class) %>% 
  summarise(num.obs = n()) %>% 
  arrange(-num.obs)


## Number of invasive observations
acad.wl.full %>% 
  group_by(status) %>% 
  summarise(n.obs = n())


## Number of invasive species
a.inv <- acad.wl.full %>% 
  filter(status == "invasive/disease") %>% 
  distinct(scientific.name, kingdom)

a.inv


## Percentage of obs for invasive plants and insects/nematodes
acad.wl.full %>% 
  filter(status == "invasive/disease") %>% 
  group_by(kingdom) %>% 
  summarise(count = n()) %>% 
  mutate(perc = 100* count/nrow(acad.wl.full %>% 
                             filter(status == "invasive/disease")))


## Number of rare/native observations
acad.wl.full %>% 
  group_by(status) %>% 
  summarise(n.obs = n())


## Number of rare/native species
acad.wl.full %>% 
  filter(status == "rare/native") %>% 
  distinct(scientific.name)



#------------------------------------------------#

### KAWW ###

## How many species on watchlist?
nrow(kaww.watch)


## Create full data set with only research-grade grade observations
map_inatK <- inatK %>%
  filter(quality.grade == "research")

map_ebdK <- ebdK %>% 
  mutate(positional.accuracy = NA) %>% 
  rename(observed.on = obs.date, place.guess = locality)

mapdatK <- bind_rows(map_inatK, map_ebdK) %>% 
  select(common.name, scientific.name, observed.on, place.guess, latitude, longitude, positional.accuracy, url)


## Filter to watchlist species to get rare, invasive, and T&E species
kaww.wl.full <- mapdatK %>% 
  filter(scientific.name %in% kaww.watch$scientific.name) %>% 
  left_join(kaww.watch, by = "scientific.name")


## What percent of species known to be in the park have been documented by citsci
nrow(kaww.wl.full %>% filter(present == "Y") %>% distinct(scientific.name))/
  nrow(kaww.watch %>% filter(present == "Y")) * 100
# 27 / 39


## Number of state and federal T&E observations
kaww.wl.full %>% 
  group_by(status) %>% 
  summarise(n.obs = n())


## Number of state and federal T&E species
kaww.wl.full %>% 
  filter(status == "federal/state TE") %>% 
  distinct(scientific.name)


## Exploring observations of T&E species
kaww.wl.full %>% 
  filter(status == "federal/state TE") %>% 
  group_by(scientific.name, kingdom, phylum, class) %>% 
  summarise(num.obs = n()) %>% 
  arrange(-num.obs)


## Number of invasive observations
kaww.wl.full %>% 
  group_by(status) %>% 
  summarise(n.obs = n())


## Number of invasive species
k.inv <- kaww.wl.full %>% 
  filter(status == "invasive/disease") %>% 
  distinct(scientific.name, kingdom)

k.inv


## Percentage of obs for invasive plants
kaww.wl.full %>% 
  filter(status == "invasive/disease") %>% 
  group_by(scientific.name) %>% 
  summarise(count = n()) %>% 
  arrange(-count)


## Number of rare/native observations
kaww.wl.full %>% 
  group_by(status) %>% 
  summarise(n.obs = n())


## Number of rare/native species
kaww.wl.full %>% 
  filter(status == "rare/native") %>% 
  distinct(scientific.name)





#------------------------------------------------#
####             Accessibility                ####
#------------------------------------------------#

### How has the early detection tool done? ###

## Get watchlist observations that have been reported since the tool was 
# implemented on January 30, 2023
m.rep <- acad.wl.full %>% 
  filter(observed.on >= "2023-01-30")


## Calculate total observations
nrow(m.rep)


## Calculate total species
m.rep %>% 
  distinct(scientific.name)


## See rare/native group obs
m.rep %>% 
  filter(status == "rare/native")


## See invasive/disease group obs
m.rep %>% 
  filter(status == "invasive/disease")


## See federal/state TE group obs
m.rep %>% 
  filter(status == "federal/state TE")





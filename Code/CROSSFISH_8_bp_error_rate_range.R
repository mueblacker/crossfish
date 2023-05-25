# Library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(pasta)
library(purrr)

# Load data ---------------------------------------------------------------
# If not available go to CROSSFISH_12_Segmented first

total.euds <- read_csv(
  "~/Data/subset/EU/total.euds.csv", guess_max = 6000)

total.usds <- read_csv(
  "~/Data/subset/US/total.usds.csv")


feow.sites <- read_csv(
  "~/Data/subset/feow_site_density.csv") 

# Load look up table ------------------------------------------------------
ld.site.criteria <- read_csv(
  "~/Data/subset/Lkpt/ld_site_criteria.csv")

metrics.criteria <- read_csv(
  "~/Data/subset/Lkpt/fish_metrics_look_up_table.csv") 

# Threshold tables --------------------------------------------------------

seg.bp <- read.csv(
  "~/Publication/Output/Segmented/break_point.csv") 

seg.bp$continent <- as.character(seg.bp$continent)
seg.bp$size <- as.character(seg.bp$size)
seg.bp$metric <- as.character(seg.bp$metric)
seg.bp$stressor <- as.character(seg.bp$stressor)
seg.bp$catchment <- as.character(seg.bp$catchment)

titan.bp <- read.csv(
  "~/Publiacation/Output/TITAN/titan_bp2.csv")


# Create a list with ecoregions and fish metrics --------------------------

eu.ecoregion <-ld.site.criteria %>% filter(continent == "EU") %>% .$FEOW_Code %>% unique()
us.ecoregion <-ld.site.criteria %>% filter(continent == "US") %>% .$FEOW_Code %>% unique()

eu.metrics <-  metrics.criteria  %>% filter(continent == "EU") %>% .$metric %>% unique()
us.metrics <-  metrics.criteria  %>% filter(continent == "US") %>% .$metric %>% unique()

hs <- c("pop", "roadx", "urban", "agri", "pasture")

total.euds %<>% 
  mutate_at(.vars = vars(starts_with("lu")), .funs = funs(.*100))

total.usds %<>% 
  mutate_at(.vars = vars(starts_with("lu")), .funs = funs(.*100))


eu.summary <- total.euds %>% 
  filter(eco_feow_code %in% eu.ecoregion) %>% 
  mutate(size = ifelse(geo_nc_area <= 100, "creek", "river")) %>% 
  select(size, starts_with("pop"), starts_with("inf"), starts_with("lu")) %>% 
  rename(hs_lc_roadx = inf_lc_roadx_dens,
         hs_nc_roadx = inf_nc_roadx_dens,
         hs_lc_pop = pop_lc_dens,
         hs_nc_pop = pop_nc_dens) %>% 
  gather(., key = "key", value = "ERR", -size) %>% 
  separate(key, c("hs","catchment","stressor"), sep = "_") %>% 
  select(-hs) %>% 
  group_by(size, catchment, stressor) %>% 
  dplyr::summarise(ERR = (max(ERR)-min(ERR))/100) %>% 
  mutate(ERR = ifelse(stressor == "pasture" |stressor == "agri", ERR*7.5, ERR*5),
         continent = "EU") %>% 
  filter(stressor %in% hs)


us.summary <- total.usds %>% 
  filter(eco_feow_code %in% us.ecoregion) %>% 
  mutate(size = ifelse(geo_nc_area <= 100, "creek", "river")) %>% 
  select(size, starts_with("pop"), starts_with("inf"), starts_with("lu")) %>% 
  rename(hs_lc_roadx = inf_lc_roadx_dens,
         hs_nc_roadx = inf_nc_roadx_dens,
         hs_lc_pop = pop_lc_dens,
         hs_nc_pop = pop_nc_dens) %>% 
  gather(., key = "key", value = "ERR", -size) %>% 
  separate(key, c("hs","catchment","stressor"), sep = "_") %>% 
  select(-hs) %>% 
  group_by(size, catchment, stressor) %>% 
  dplyr::summarise(ERR = (max(ERR)-min(ERR))/100) %>% 
  mutate(ERR = ifelse(stressor == "pasture" |stressor == "agri", ERR*7.5, ERR*5),
         continent = "US") %>% 
  filter(stressor %in% hs)

summary <- eu.summary %>% 
  bind_rows(us.summary)
  

break.point <- full_join(seg.bp, titan.bp , by =c ("continent", "size", "catchment", "stressor", "metric"))
  
  




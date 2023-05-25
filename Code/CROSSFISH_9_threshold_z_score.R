
# Library -----------------------------------------------------------------

library(dplyr)
library(magrittr)
library(readr)
library(tidyr)


# Data --------------------------------------------------------------------

# Load table with thresholds


total.euds <- read_csv(
  "~/Data/subset/EU/total.euds.csv", guess_max = 6000)

total.usds <- read_csv(
  "~/Data/subset/US/total.usds.csv")


break.point <- read.csv(
  "~/Output/Significant_threshold/sig_bp_hs_005.csv")


# Calculation of the z-score of the threshold value -----------------------

eu.lu <- total.euds %>%
  mutate(size = ifelse(geo_nc_area <= 100, "creek", "river")) %>% 
  rename(dens_lc_pop = pop_lc_dens,
         dens_nc_pop = pop_nc_dens,
         dens_lc_roadx = inf_lc_roadx_dens,
         dens_nc_roadx = inf_nc_roadx_dens,
        ecoregion = eco_feow_code) %>% 
  select(id_site_code, ecoregion, size, starts_with("lu_"), starts_with("dens")) %>% 
  gather(., key = "stressor", value = "value", - id_site_code, - ecoregion, -size) %>% 
  separate(., stressor, c("x", "catchment", "stressor"), sep = "_") %>% 
  select(-x, -id_site_code) %>% 
  group_by(ecoregion, size, catchment, stressor) %>% 
  mutate(value = ifelse(stressor == "urban" | stressor == "pasture"| stressor == "agri", value*100, value)) %>% 
  summarise(mean_stressor =  mean(value), sd_stressor = sd(value))

us.lu <- total.usds %>%
  mutate(size = ifelse(geo_nc_area <= 100, "creek", "river")) %>% 
  rename(dens_lc_pop = pop_lc_dens,
         dens_nc_pop = pop_nc_dens,
         dens_lc_roadx = inf_lc_roadx_dens,
         dens_nc_roadx = inf_nc_roadx_dens,
         ecoregion = eco_feow_code) %>% 
  select(id_site_code, ecoregion, size, starts_with("lu_"), starts_with("dens")) %>% 
  gather(., key = "stressor", value = "value", - id_site_code, - ecoregion, -size) %>% 
  separate(., stressor, c("x", "catchment", "stressor"), sep = "_") %>% 
  select(-x, -id_site_code) %>% 
  group_by(ecoregion, size, catchment, stressor) %>%
  mutate(value = ifelse(stressor == "urban" | stressor == "pasture"| stressor == "agri", value*100, value)) %>% 
  summarise(mean_stressor = mean(value),  sd_stressor = sd(value))

stat.lu <- eu.lu %>% 
  bind_rows(us.lu)

bp.z.score <- inner_join(break.point, stat.lu, by = c("ecoregion", "size", "catchment", "stressor")) %>% 
  mutate(bp_z_score = (bp_value - mean_stressor)/sd_stressor) 
  
test <- bp.z.score  %>% 
  filter(threshold == "yes") %>% 
  group_by(size, catchment, stressor, metric) %>% 
  #mutate(pr_bp_zscore = (bp_z_score-min(bp_z_score))/(max(bp_z_score)-min(bp_z_score))) %>% 
  select(continent, ecoregion, size, catchment, stressor, metric, pr_bp_zscore)

bp.z.score  <- full_join(break.point, test, by = c("continent", "ecoregion", "size", "catchment", "stressor", "metric"))

 
write_csv(bp.z.score , "~/Output/Significant_threshold/sig_bp_hs_005.csv")
  

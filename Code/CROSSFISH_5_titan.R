# Library -----------------------------------------------------------------
library(TITAN2)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(pasta)
library(purrr)

# Load data ---------------------------------------------------------------

# original dataset with human stressor variables
total.euds <- read_csv(
  "~/Data/subset/EU/total.euds.csv", guess_max = 6000)

total.usds <- read_csv(
  "~/Data/subset/US/total.usds.csv")

# Predicted fish metrics values from the BRT analysis
eu.brt.output <- readRDS(
  "~/Output/BRT/brt_output_eu"%.%"RDS")

us.brt.output <- readRDS(
  "~/Output/BRT/brt_output_us"%.%"RDS")


# Load intermediate results if already exsiting ---------------------------
# If already available

# Nested list for the TITAN analysis - if not, go to: Create nested list 
# for TITAN threshold analysis
tot.list <- readRDS("~/Output/TITAN/tot_list.RDS")

# Previous TITAN models 
fit.titan <- readRDS("~/Output/TITAN/titan_model.RDS")


# Load look up table ------------------------------------------------------
ld.site.criteria <- read_csv(
  "~/Data/subset/Lkpt/ld_site_criteria.csv")

metrics.criteria <- read_csv(
  "~/Data/subset/Lkpt/fish_metrics_look_up_table.csv") 

ecoregion.use <- read_csv(
  "~/Data/subset/Lkpt/use_of_ecoregion_TITAN.csv")


# Create a list with ecoregions, fish metrics and human stressors  --------

eu.ecoregion <-ld.site.criteria %>% filter(continent == "EU") %>% .$FEOW_Code %>% unique()
us.ecoregion <-ld.site.criteria %>% filter(continent == "US") %>% .$FEOW_Code %>% unique()

eu.metrics <-  metrics.criteria  %>% filter(continent == "EU") %>% .$metric %>% unique()
us.metrics <-  metrics.criteria  %>% filter(continent == "US") %>% .$metric %>% unique()

hs <- c("pop", "roadx", "urban", "agri", "pasture", "forest", "shrub", "noveg")

# Create nested list for TITAN threshold analysis -------------------------

total.euds %<>% 
  mutate_at(.vars = vars(starts_with("lu")), .funs = funs(.*100))

total.usds %<>% 
  mutate_at(.vars = vars(starts_with("lu")), .funs = funs(.*100))


temp.list <- list()

for(i.con in c("EU","US")){
  if(i.con == "EU") total.ds   <- total.euds
  if(i.con == "US") total.ds   <- total.usds
  if(i.con == "EU") brt.output <- eu.brt.output
  if(i.con == "US") brt.output <- us.brt.output
  if(i.con == "EU") ecoregion  <- eu.ecoregion
  if(i.con == "US") ecoregion  <- us.ecoregion
  if(i.con == "EU") metrics    <- eu.metrics
  if(i.con == "US") metrics    <- us.metrics
  
  for (i.eco in ecoregion){
    for(i.sz in c("creek", "river")){
      for (i.ctch in c("lc", "nc")){
        for (i.hs in hs){ 
         for (i.met in metrics){  
           
           metric.usability <- metrics.criteria %>% 
             filter(ecoregion == i.eco, metric == i.met)
           
           metric.usability <- as.data.frame(metric.usability)
           
           if(metric.usability$BRT == "NO") next

          
          inputname <- i.met%_%i.eco
          
          if(i.sz == "creek"){
            
            temp <- total.ds %>% 
              filter(eco_feow_code == i.eco, geo_nc_area <= 100) %>% 
              select(id_site_code, contains(i.ctch)) %>% 
              select(id_site_code, contains(i.hs)) 
            
            temp.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]][[i.met]] <- list()
            
            
            temp.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]][[i.met]] <- brt.output[[inputname]] %>% 
              select(id_site_code, ends_with("_pre")) %>% 
              inner_join(., temp, by ="id_site_code") %>% 
              rename_(human_stressor = names(.)[3])
            
            }else{  
              
              temp <- total.ds %>% 
                filter(eco_feow_code == i.eco, geo_nc_area > 100) %>% 
                select(id_site_code, contains(i.ctch)) %>% 
                select(id_site_code, contains(i.hs))
              
              
              temp.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]][[i.met]] <- list()
              
              
              temp.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]][[i.met]] <- brt.output[[inputname]] %>% 
                select(id_site_code, ends_with("_pre")) %>% 
                inner_join(., temp, by ="id_site_code") %>% 
                rename_(human_stressor = names(.)[3])
            
            }
          }
        }
      }
    }
  }
}

# to modify the depth of the nested list the 'purrr' package is needed
tot.list <- modify_depth(temp.list, -3, bind_cols) %>%
  modify_depth(., -2, ~ select(.x, "id_site_code", "human_stressor", starts_with("ft_")) %>% as_tibble)

# Save nested data list for TITAN analysis
saveRDS(tot.list , "~/Output/TITAN/tot_list_20170508.RDS")


# Threshold analysis with TITAN -------------------------------------------

fit.titan <- list()

system.time({
for(i.con in c("US", "EU")){
  if(i.con == "EU") ecoregion  <- eu.ecoregion
  if(i.con == "US") ecoregion  <- us.ecoregion

  for (i.eco in ecoregion){
    for(i.sz in c("creek", "river")){
      for(i.ctch in c("lc", "nc")){
        for (i.hs in hs){ 
            
          eco.usability <- ecoregion.use %>% 
            filter(ecoregion == i.eco)
          
          eco.usability <- as.data.frame(eco.usability)
          
          if(eco.usability$TITAN == "NO") next
          
          
              # Select human stressor
              stressor <- tot.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]] %>% 
                dplyr::select(human_stressor)
              
              # Select fish metrics
              metrics <- tot.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]] %>%
                dplyr::select(starts_with("ft_")) 
              
              # TITAN calculation
              fit.titan.temp <- try(titan(stressor, metrics, minSplt = 5, imax = FALSE,
                                     numPerm = 1000, boot = FALSE))
              
              if(class(fit.titan.temp) == "list")
                
                # Save the model
                fit.titan[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]] <- list()
                fit.titan[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]] <- fit.titan.temp

     
              
        }
      }
    }
  }
}
})
 
# Save nested list with all models             
saveRDS(fit.titan, "~/Output/TITAN/titan_model.RDS") 


# Result summary table for TITAN ---------------------------------------


first <- TRUE

for(i.con in c("EU", "US")){
  if(i.con == "EU") ecoregion  <- eu.ecoregion
  if(i.con == "US") ecoregion  <- us.ecoregion

  
  for (i.eco in ecoregion){
    for(i.sz in c("creek", "river")){
      for (i.ctch in c("lc", "nc")){
        for (i.hs in hs){ 
 
            
            eco.usability <- ecoregion.use %>% 
              filter(ecoregion == i.eco)
            
            eco.usability <- as.data.frame(eco.usability)
            
            if(eco.usability$TITAN == "NO") next
           
            
            if(first == TRUE){
              
              titan.bp <- as.data.frame(fit.titan[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]]$sppmax) %>% 
                tibble::rownames_to_column(., var = "trait") %>% 
                mutate(continent = i.con, ecoregion = i.eco, size = i.sz, catchment = i.ctch, stressor = i.hs )
                
            first <- FALSE

            }else{
              
              titan.bp <- bind_rows(titan.bp,
              as.data.frame(fit.titan[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]]$sppmax) %>% 
                tibble::rownames_to_column(., var = "trait") %>% 
                mutate(continent = i.con, ecoregion = i.eco, size = i.sz, catchment = i.ctch, stressor = i.hs ))
            }
            
        }
      }
    }
  }
}

titan.bp %<>% 
 separate(trait, c("ft", "group", "metric", "pr"), "_") %>% 
  mutate(metric = toupper(metric)) %>% 
  select(continent, ecoregion, size, metric, stressor, catchment, zenv.cp, obsiv.prob, zscore)

# Save table with the TITAN break point values
write.csv(titan.bp, "~/Output/TITAN/titan_bp.csv", row.names = FALSE)

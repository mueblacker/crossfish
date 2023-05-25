# Library -----------------------------------------------------------------
library(TITAN2)
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

eu.brt.output <- readRDS(
  "~/Output/BRT/brt_output_eu_20180601"%.%"RDS")

us.brt.output <- readRDS(
  "~/Output/BRT/brt_output_us_20171019"%.%"RDS")

fit.titan <- readRDS("~/Output/TITAN/titan_model.RDS")

tot.list <- readRDS("~/Output/TITAN/tot_list.RDS")

# Load look up table ------------------------------------------------------
ld.site.criteria <- read_csv(
  "~/Data/subset/Lkpt/ld_site_criteria.csv")

metrics.criteria <- read_csv(
  "~/Data/subset/Lkpt/fish_metrics_look_up_table.csv") 

metrics.use <- read_csv(
  "~/Data/subset/Lkpt/use_of_ecoregion_TITAN.csv")

# Create a list with ecoregions and fish metrics --------------------------

eu.ecoregion <-ld.site.criteria %>% filter(continent == "EU") %>% .$FEOW_Code %>% unique()
us.ecoregion <-ld.site.criteria %>% filter(continent == "US") %>% .$FEOW_Code %>% unique()

eu.metrics <-  metrics.criteria  %>% filter(continent == "EU") %>% .$metric %>% unique()
us.metrics <-  metrics.criteria  %>% filter(continent == "US") %>% .$metric %>% unique()

hs <- c("pop", "roadx", "urban", "agri", "pasture")

# Create tables for TITAN threshold analysis ------------------------------

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

tot.list <- modify_depth(temp.list, -3, bind_cols) %>%
  modify_depth(., -2, ~ select(.x, "id_site_code", "human_stressor", starts_with("ft_")) %>% as_tibble)

saveRDS(tot.list , "~/Output/TITAN/tot_list.RDS")


# Threshold analysis with TITAN -------------------------------------------
library(foreach)
library(doParallel)
library(parallel)


cl<-makeCluster(4) #change the 2 to your number of CPU cores
registerDoParallel(cl)

system.time({
  
fit.titan <- foreach(i = 1:4, .packages=c("TITAN2", "dplyr", "tibble", "pasta")) %dopar%{
  fit.titan.i <- list()
        for(i.con in c("US")){
          #if(i.con == "EU") ecoregion  <- eu.ecoregion
          if(i.con == "US") ecoregion  <- us.ecoregion
          
          for (i.eco in ecoregion){
            for(i.sz in c("creek","river")){
              for(i.ctch in c("lc", "nc")){
                for (i.hs in hs){ 
                  
                  metric.usability <- metrics.use %>% 
                    filter(ecoregion == i.eco)
                  
                  metric.usability <- as.data.frame(metric.usability)
                  
                  if(metric.usability$TITAN == "NO" | metric.usability$ecoregion == "EstTxsGlf" |
                     metric.usability$ecoregion == "WstFlrGlf" ) next
                  
                  
                  
                  stressor <- tot.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]] %>% 
                    dplyr::select(human_stressor)
                  
                  metrics <- tot.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]] %>%
                    dplyr::select(starts_with("ft_")) 
                  
                  
                  fit.titan.temp <- try(titan(stressor, metrics, minSplt = 5, imax = FALSE,
                                              numPerm = 1000, boot = FALSE))
                  
                  if(class(fit.titan.temp) == "list")
                    
                    fit.titan.i[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]] <- list()
                    fit.titan.i[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.hs]] <- as.data.frame(fit.titan.temp$sppmax) %>% 
                    tibble::rownames_to_column(., var = "trait") %>% 
                    dplyr::select(1:8)
                  
          
          
          }
        }
      }
    }
        }
  saveRDS(fit.titan.i, "~/Output/TITAN/save_increment/us_titan"%_%i%.%"rds") 
  return(fit.titan.i)
}

stopCluster(cl)
})


saveRDS(fit.titan, "~/Output/TITAN/save_increment/us_titan_model_4.RDS") 


# Threshold summary table for TITAN ---------------------------------------


first <- TRUE

for(i.con in c("EU", "US")){
  if(i.con == "EU") ecoregion  <- eu.ecoregion
  if(i.con == "US") ecoregion  <- us.ecoregion
  
  
  for (i.eco in ecoregion){
    for(i.sz in c("creek", "river")){
      for (i.ctch in c("lc", "nc")){
        for (i.hs in hs){ 
          
          
          metric.usability <- metrics.use %>% 
            filter(ecoregion == i.eco)
          
          metric.usability <- as.data.frame(metric.usability)
          
          if(metric.usability$TITAN == "NO" | metric.usability$ecoregion == "EstTxsGlf" |
             metric.usability$ecoregion == "WstFlrGlf" ) next
          
          
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

write.csv(titan.bp, "~/Output/TITAN/titan_bp.csv", row.names = FALSE)

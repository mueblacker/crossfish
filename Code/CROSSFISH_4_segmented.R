# Library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(pasta)
library(segmented) 
library(plotly)


# Load data ---------------------------------------------------------------

# original dataset with human stressor variables
total.euds <- read_csv(
  "~/Data/subset/EU/total.euds.csv", guess_max = 6000)

total.usds <- read_csv(
  "~/Data/subset/US/total.usds.csv", guess_max = 6000)

# Predicted fish metrics values from the BRT analysis
eu.brt.output <- readRDS(
  "~/Output/BRT/brt_output_eu.RDS")

us.brt.output <- readRDS(
  "~/Output/BRT/brt_output_us.RDS")

# Load look up table ------------------------------------------------------

ld.site.criteria <- read_csv(
  "~/Data/subset/Lkpt/ld_site_criteria.csv")

metrics.criteria <- read_csv(
  "~/Data/subset/Lkpt/fish_metrics_look_up_table.csv") 

# If not existing go first to - Create look up table with the starting point 
psi.stressor <- read_csv(
  "~/Data/subset/Lkpt/psi_stressor.csv") 


# Load intermediate results if already exsiting ---------------------------

# If not go to "Create tables for threshold analysis"
ta.list <- readRDS(
  "~/Output/Segmented/ta_list.RDS")

# If not go "Threshold analysis with Segmented"
fit.seg <- readRDS(
  "~/Output/Segmented/seg_model.RDS")

# If not go "Create result table (break point and p-value)"
break.point <- read.csv(
          "~/Output/Segmented/break_point.csv")

# Create a list with ecoregions and fish metrics --------------------------

eu.ecoregion <-ld.site.criteria %>% filter(continent == "EU") %>% .$FEOW_Code %>% unique() 
us.ecoregion <-ld.site.criteria %>% filter(continent == "US") %>% .$FEOW_Code %>% unique()

eu.metrics <-  metrics.criteria  %>% filter(continent == "EU") %>% .$metric %>% unique()
us.metrics <-  metrics.criteria  %>% filter(continent == "US") %>% .$metric %>% unique()

hs <- c("pop", "roadx", "urban", "agri", "pasture" , "forest", "shrub", "noveg")


# Create a nested list for the piecewise linear regression analysis ------
# with the predicted values from the BRTs analysis

total.euds %<>% 
  mutate_at(.vars = vars(starts_with("lu")), .funs = funs(.*100))

total.usds %<>% 
  mutate_at(.vars = vars(starts_with("lu")), .funs = funs(.*100))

ta.list <- list()

for(i.con in c("EU", "US")){ # continent
  if(i.con == "EU") total.ds   <- total.euds
  if(i.con == "US") total.ds   <- total.usds
  if(i.con == "EU") brt.output <- eu.brt.output
  if(i.con == "US") brt.output <- us.brt.output
  if(i.con == "EU") ecoregion  <- eu.ecoregion
  if(i.con == "US") ecoregion  <- us.ecoregion
  if(i.con == "EU") metrics    <- eu.metrics
  if(i.con == "US") metrics    <- us.metrics
  
  for (i.eco in ecoregion){ # ecoregion
    for(i.sz in c("creek", "river")){ # stream size
     for (i.ctch in c("lc", "nc")){ # local or network catchment
      for (i.met in metrics){  # fish metric
       for (i.hs in hs){ # human stressor
      
        # Check if the fish metric should be used in the analysis  
        metric.usability <- metrics.criteria %>% 
          filter(ecoregion == i.eco, metric == i.met)
        
        metric.usability <- as.data.frame(metric.usability)
        
        if(metric.usability$BRT == "NO") next
        
        inputname <- i.met%_%i.eco
      
      if(i.sz == "creek"){
      
        # Select human stressor variables for creek sites
        temp <- total.ds %>% 
        filter(eco_feow_code == i.eco, geo_nc_area <= 100) %>% 
        select(id_site_code, contains(i.ctch)) %>% 
        select(id_site_code, contains(i.hs))
        
      
        ta.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]] <- list()
      
        # Join the human stressor variables with the predicted fish metric values
        ta.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]] <- brt.output[[inputname]] %>% 
        select(id_site_code, ends_with("_pre")) %>% 
        inner_join(., temp, by ="id_site_code") %>% 
        rename_(Y = names(.)[2],
                X = names(.)[3])
      
      }else{  
        
        # Select human stressor variables for river sites
        temp <- total.ds %>% 
        filter(eco_feow_code == i.eco, geo_nc_area > 100) %>% 
        select(id_site_code, contains(i.ctch)) %>% 
        select(id_site_code, contains(i.hs))
      
      
        ta.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]] <- list()
      
        # Join the human stressor variables with the predicted fish metric values
        ta.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]] <- brt.output[[inputname]] %>% 
        select(id_site_code, ends_with("_pre")) %>% 
        inner_join(., temp, by ="id_site_code") %>% 
        rename_(Y = names(.)[2],
                X = names(.)[3])
      }
      }
      }
     }
    }
  }
}

# Save nested data list for the piecewise linear regression analysis
saveRDS(ta.list, "~/Output/Segmented/ta_list.RDS")


# Create look up table with the starting point of the break point  ------
# Starting point = assumption of the break point

#first <- TRUE

for(i.con in c("US", "EU")){
  if(i.con == "EU") ecoregion  <- eu.ecoregion
  if(i.con == "US") ecoregion  <- us.ecoregion
  if(i.con == "EU") metrics    <- eu.metrics
  if(i.con == "US") metrics    <- us.metrics
  
  for (i.eco in ecoregion){
    for(i.sz in c("creek", "river")){
      for (i.ctch in c("lc", "nc")){
        for (i.met in metrics){ 
          for (i.hs in hs){ 
          
          metric.usability <- metrics.criteria %>% 
            filter(ecoregion == i.eco, metric == i.met)
          
          metric.usability <- as.data.frame(metric.usability)
          
          if(metric.usability$BRT == "NO") next
          
          if(first){

          psi.stressor <- tibble(continent  = i.con,
                                 ecoregion  = i.eco,
                                 size       = i.sz,
                                 catchment  = i.ctch,
                                 metric     = i.met,
                                 stressor   = i.hs,
                                 psi        = 1)
          
          first <- FALSE
          
          }else{
            psi.stressor <- add_row(psi.stressor,
                                    continent  = i.con,
                                    ecoregion  = i.eco,
                                    size       = i.sz,
                                    catchment  = i.ctch,
                                    metric     = i.met,
                                    stressor   = i.hs,
                                    psi        = 1)
            
 
          }
          psi.stressor %<>%
            mutate(psi = replace(psi, stressor == "pop", 100)) %>% 
            mutate(psi = replace(psi, stressor == "roadx", 0.1)) %>% 
            mutate(psi = replace(psi, stressor == "forest", 40))
          }
        }
      }
    }
  }
}

psi.stressor %<>% 
  group_by(ecoregion) %>% 
  arrange(., stressor) %>% 
  arrange(., metric) %>% 
  arrange(., size) %>% 
  arrange(., catchment) %>% 
  arrange(., ecoregion) %>% 
  arrange(., continent)

# Save table with the guessed psi values
write.csv(psi.stressor,
          "~/Data/subset/Lkpt/psi_stressor.csv", 
          row.names = FALSE)


# Threshold analysis with Segmented ---------------------------------------

#fit.seg <- list()

for(i.con in c("EU", "US")){
  if(i.con == "EU") ecoregion  <- eu.ecoregion
  if(i.con == "US") ecoregion  <- us.ecoregion
  if(i.con == "EU") metrics    <- eu.metrics
  if(i.con == "US") metrics    <- us.metrics
  
  for (i.eco in ecoregion){
    for(i.sz in c("creek", "river")){
    for (i.ctch in c("lc", "nc")){
      for (i.met in metrics){ 
        for (i.hs in hs){ 
          
          metric.usability <- metrics.criteria %>% 
            filter(ecoregion == i.eco, metric == i.met)
          
          metric.usability <- as.data.frame(metric.usability)
          
        if(metric.usability$BRT == "NO") next
        
        
          ExampleData <- ta.list[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]]
          
          # Linear regression model
          fit.temp  <- lm(Y ~ X, data  = ExampleData)
          
          # Select start value for break point from the look up table
          psi.temp <- psi.stressor %>% 
           filter(ecoregion == i.eco, catchment == i.ctch, size == i.sz, 
                  metric ==  i.met,  stressor == i.hs, catchment == i.ctch)
          
          # Piecewise linear regression model and break point calculation
          fit.seg.temp <- try(segmented(fit.temp, ~ X, psi.temp$psi)) #psi.temp$psi
  
        
          
          
        if(class(fit.seg.temp)[1] == "segmented"){
          
            fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]] <- list()
            
            fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]] <- fit.seg.temp 
            
            # Save summary of the segmented model
            summary.path <- "~/Output/Segmented/Summary"%//%i.eco%//%i.sz
            if(!dir.exists(summary.path)) dir.create(summary.path, recursive = TRUE)
            
            sink(summary.path%//%i.eco%_%i.sz%_%i.ctch%_%i.met%_%i.hs%.%"txt")
            print(summary(fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]]))
            sink()
            
            
            # Save a scatter plot with the piecewise linear model and the p-value
            figures.path <- "~/Output/Segmented/Figures"%//%i.eco%//%i.sz
            if(!dir.exists(figures.path)) dir.create(figures.path, recursive = TRUE)
            
            png(figures.path%//%i.eco%_%i.sz%_%i.ctch%_%i.met%_%i.hs%.%"png")
            plot(ExampleData$X, ExampleData$Y, xlab = i.hs, ylab = i.met)
            plot(fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]] , add = TRUE, link = FALSE, lwd = 2, col = 2:3, lty = c(1,3))
            lines(fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]] ,col = 2, pch = 19, bottom = FALSE, lwd = 2)
            title(main = "p_value ="%&&%summary(fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]])$coefficients[2, 4], cex.main = 1 )
            dev.off()
            
          
          }else{
            
            # If there was an error a scatter plot WITHOUT the piecewise linear model and the p-value will be saved
            # Check errors afterwards by screening the plots
            # Often the psi value has to be adjusted
            # Set a new psi value and rerun fish metrics and stressors with an error

            figures.path <- "~/Output/Segmented/Figures"%//%i.eco%//%i.sz
            if(!dir.exists(figures.path)) dir.create(figures.path, recursive = TRUE)
            
            png(figures.path%//%i.eco%_%i.sz%_%i.ctch%_%i.met%_%i.hs%.%"png")
            plot(ExampleData$X, ExampleData$Y,  xlab = i.hs, ylab = i.met)
            dev.off() 
            
           
          } 
          }
        }
      }
    }
  }
}

# Save Segmented models
saveRDS(fit.seg, "~/Output/Segmented/seg_model.RDS")

# Create result table (break point and p-value) ---------------------------

first <- TRUE

for(i.con in c("EU", "US")){
  if(i.con == "EU") ecoregion  <- eu.ecoregion
  if(i.con == "US") ecoregion  <- us.ecoregion
  if(i.con == "EU") metrics    <- eu.metrics
  if(i.con == "US") metrics    <- us.metrics
  
  for (i.eco in ecoregion){
    for(i.sz in c("creek", "river")){
    for (i.ctch in c("lc", "nc")){
      for (i.met in metrics){ 
        for (i.hs in hs){ 
          
          metric.usability <- metrics.criteria %>% 
            filter(ecoregion == i.eco, metric == i.met)
          
          metric.usability <- as.data.frame(metric.usability)
          
          if(metric.usability$BRT == "NO") next
          
          if(class(fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]])[1] == "segmented"){
            
          if(first){
            
            break.point <- tibble(continent  = i.con,
                                  ecoregion  = i.eco,
                                  size       = i.sz,
                                  metric     = i.met,
                                  stressor   = i.hs,
                                  catchment  = i.ctch,
                                  bp_est     = fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]]$psi[2],
                                  st_err     = fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]]$psi[3],
                                  p_value    = summary(fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]])$coefficients[2, 4])
                                   
            
            first <- FALSE
            
          }else{
            break.point <- add_row(break.point,
                                   continent  = i.con,
                                   ecoregion  = i.eco,
                                   size       = i.sz,
                                   metric     = i.met,
                                   stressor   = i.hs,
                                   catchment  = i.ctch,
                                   bp_est     = fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]]$psi[2],
                                   st_err     = fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]]$psi[3],
                                   p_value    = summary(fit.seg[[i.con]][[i.eco]][[i.sz]][[i.ctch]][[i.met]][[i.hs]])$coefficients[2, 4])
            
          }
          }else{
          
          if(first){
            
            break.point <- tibble(continent  = i.con,
                                  ecoregion  = i.eco,
                                  size       = i.sz,
                                  metric     = i.met,
                                  stressor   = i.hs,
                                  catchment  = i.ctch,
                                  bp_est     = NA,
                                  st_err     = NA,
                                  p_value    = NA)
            
            first <- FALSE
            
          }else{
            break.point <- add_row(break.point,
                                   continent  = i.con,
                                   ecoregion  = i.eco,
                                   size       = i.sz,
                                   metric     = i.met,
                                   stressor   = i.hs,
                                   catchment  = i.ctch,
                                   bp_est     = NA,
                                   st_err     = NA,
                                   p_value    = NA)
          }
          }
          }
        }
      }
    }
  }
}
            
break.point %<>% 
  group_by(ecoregion) %>% 
  arrange(., stressor) %>% 
  arrange(., metric) %>% 
  arrange(., ecoregion) %>% 
  arrange(., continent)  


          
write.csv(break.point,
          "~/Output/Segmented/break_point1.csv", 
          row.names = FALSE)

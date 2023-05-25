
# Library -----------------------------------------------------------------

library(readr) # provides a fast way to read csv files
library(dplyr) # tool for working with data frames
library(tidyr) # easy way to reshuffle your data
library(magrittr) # a "pipe"-like operator 
library(dismo) # to grow BRTs
library(gbm) # to predict values from BRTs
library(pasta) # functions to paste strings together

#to install the 'pasta' package: 

#install.packages("devtools")
#library(devtools)
#install_github("chrisschuerz\pasta")

# Load data ---------------------------------------------------------------

# Load original data set
total.euds <- read_csv(
  "~/Data/subset/EU/total.euds.csv")

total.usds <- read_csv(
  "~/Data/subset/US/total.usds.csv")

# Load the data from the Eigenvector spatial filtering
total.euds.esf <- read_csv( 
  "~/Data/subset/ESF/total.euds.esf.csv")

total.usds.esf <- read_csv( 
  "~/Data/subset/ESF/total.usds.esf.csv")


# Load look up table ------------------------------------------------------

# Create a table with the criteria for least disturbed sites
# with seperate columns for each spatial unit and each criterion 
ld.site.criteria <- read_csv(
  "~/Data/subset/Lkpt/ld_site_criteria.csv")

# After you have tried several settings for the BRTs, create a table with 
# all spatial units, fish metrics and the best initial settings
# (learning rate (lr) , tree complexity (tc), bag.fraction (bf), n.trees (tr))

metrics.criteria <- read_csv(
  "~/Data/subset/Lkpt/fish_metrics_look_up_table.csv") 


# Load previouse saved data list and models -------------------------------

# If already available

# Read data list for analysis
# If not available yet, go to section "Create a nested list for BRT analysis"
brt.list <- readRDS("~/Output/BRT/brt_list.RDS") 

# If you already have saved some BRT models and prediciton output and want to save your current
# BRT models and prediciton ouput in the same list

# Read models 
brt.models <- readRDS(
  "~/Output/BRT/brt_models_us.RDS") 

# Read prediction output
brt.output <- readRDS(
  "~/Output/BRT/brt_output_eu.RDS") 



# Create a list with all ecoregions and fish metrics available ------------

# Create a list with all ecoregions
eu.ecoregion <-ld.site.criteria %>% filter(continent == "EU") %>% .$FEOW_Code %>% unique() 
us.ecoregion <-ld.site.criteria %>% filter(continent == "US") %>% .$FEOW_Code %>% unique()

# Create a list with all fish metrics
eu.metrics <-  metrics.criteria  %>% filter(continent == "EU") %>% .$metric %>% unique()
us.metrics <-  metrics.criteria  %>% filter(continent == "US") %>% .$metric %>% unique()

# Create a nested list for BRT analysis -----------------------------------

# Create a nested list for all ecoregions with "all" sampling sites and 
# the "least disturbed" sites (reference sites)
# Don't run this section if brt.list already exists 


brt.list <-  list() 

for(i.con in c("EU", "US")){
  if(i.con == "EU") total.ds      <- total.euds
  if(i.con == "US") total.ds      <- total.usds
  if(i.con == "EU") total.ds.esf  <- total.euds.esf
  if(i.con == "US") total.ds.esf  <- total.usds.esf
  if(i.con == "EU") ecoregion     <- eu.ecoregion
  if(i.con == "US") ecoregion     <- us.ecoregion
  if(i.con == "EU") metrics       <- eu.metrics
  if(i.con == "US") metrics       <- us.metrics
  
  for(i.site in c("ALL", "REF")){
    for(i.eco in ecoregion){
      for(i.met in metrics){
        
        # Builts a nested list e.g. brt.list$US$REF$UpperSnake$INTOL (see snapshot examples)
        brt.list[[i.con]][[i.site]][[i.eco]][[i.met]] <- list()
        
        # Filters the current ecoregion and fish metric from the look up table
        esf.correction <- metrics.criteria %>% 
          filter(ecoregion == i.eco, metric == i.met)
        
        # Verifies if the original data is used or if Eigenvector spatial filtering was applied
        if(esf.correction$moranI_greater_0.2 == "NO" | is.na(esf.correction$moranI_greater_0.2)){
          
          # If the original data will be used
          
          if(i.site == "ALL"){ #all sites
            
            # Filters the current ecoregion, selects the predictor (natural) variables,
            # and transfers it into the list
            brt.list[[i.con]][[i.site]][[i.eco]][[i.met]] <- total.ds %>% 
              filter(eco_feow_code == i.eco) %>%  
              dplyr::select(id_site_code, ends_with(i.met), starts_with("top_"), 
                            starts_with("cli_")) 
            
          }else{ #only least disturbed sites are selected
            
            # Criteria for the least disturbed sites are given by the look up table
            crit.creek <- ld.site.criteria %>% 
              filter(FEOW_Code == i.eco, stream_size == "CREEK")
            crit.river <- ld.site.criteria %>% 
              filter(FEOW_Code == i.eco, stream_size == "RIVER")
            
            brt.creek <- total.ds %>%
              # Filters the sites which fulfill the given criteria
              filter(eco_feow_code == i.eco, top_nc_area <= 100, lu_nc_agri < crit.creek$c_ag, 
                     lu_nc_pasture < crit.creek$c_pa, lu_nc_urban < crit.creek$c_ur) %>% 
              dplyr::select(id_site_code, ends_with(i.met), starts_with("top_"), 
                            starts_with("cli_")) %>% 
              filter(.[[2]] != 0) # Removes all sites, where the fish metrics value is zero
            
            brt.river <- total.ds %>%
              filter(eco_feow_code == i.eco, top_nc_area > 100, lu_nc_agri < crit.river$c_ag, 
                     lu_nc_pasture < crit.river$c_pa, lu_nc_urban < crit.river$c_ur) %>% 
              dplyr::select(id_site_code, ends_with(i.met), starts_with("top_"), 
                            starts_with("cli_")) %>% 
              filter(.[[2]] != 0)
            
            brt.list[[i.con]][[i.site]][[i.eco]][[i.met]] <- bind_rows(brt.creek, brt.river)
          }
         
        # If Eigenvector spatial filtering was applied
        }else{  
          if(i.site == "ALL"){ #all sites
            brt.list[[i.con]][[i.site]][[i.eco]][[i.met]] <- total.ds.esf %>% 
              filter(eco_feow_code == i.eco) %>% 
              dplyr::select(id_site_code, ends_with(i.met), starts_with("top_"), 
                            starts_with("cli_")) 
            
          }else{ # Only least disturbed sites are selected
            
            # Criteria for least disturbed sites are given by the look up table
            crit.creek <- ld.site.criteria %>% 
              filter(FEOW_Code == i.eco, stream_size == "CREEK")
            crit.river <- ld.site.criteria %>% 
              filter(FEOW_Code == i.eco, stream_size == "RIVER")
            
            brt.creek <- total.ds.esf %>%
              filter(eco_feow_code == i.eco, top_nc_area <= 100, lu_nc_agri < crit.creek$c_ag, 
                     lu_nc_pasture < crit.creek$c_pa, lu_nc_urban < crit.creek$c_ur) %>% 
              dplyr::select(id_site_code, ends_with(i.met), starts_with("top_"), 
                            starts_with("cli_")) %>% 
              filter(.[[2]] != 0)
            
            
            brt.river <- total.ds.esf %>%
              filter(eco_feow_code == i.eco, top_nc_area > 100, lu_nc_agri < crit.river$c_ag, 
                     lu_nc_pasture < crit.river$c_pa, lu_nc_urban < crit.river$c_ur) %>% 
              dplyr::select(id_site_code, ends_with(i.met), starts_with("top_"), 
                            starts_with("cli_")) %>% 
              filter(.[[2]] != 0)
            
            brt.list[[i.con]][[i.site]][[i.eco]][[i.met]] <- bind_rows(brt.creek, brt.river)
            
            
            
          }
        }
      }
    }
  }
}

# Save data list for BRT analysis
saveRDS(brt.list,"~/Output/BRT/brt_list.RDS") 

# BRT analysis ------------------------------------------------------------


# 1. Run the BRTs with different settings
# 2. Select the best initial setting from the summary table and/or by checking the 
# output plots
# 3. Create a look up table with the best initial setting for each ecoregion and fish metric
# 4. Rerun gmb.step() with the selected initial settings 

# Define initial settings you want to test with gbm.step() 
#tc <- c(3,5,7)
#lr <- c(0.005, 0.001, 0.0005)
#bf <- c(0.75)
#tr <- c(25, 50)

# Creates a list to save the models and the prediction
brt.models <- list() # Don't run if already existing
brt.output <- list() # Don't run if already existing

first <- TRUE #Only run this line if you want to generate a new mod.summary dataframe


for(i.con in c("EU")){
  if(i.con == "EU") ecoregion <- eu.ecoregion
  if(i.con == "US") ecoregion <- us.ecoregion
  if(i.con == "EU") metrics   <- eu.metrics
  if(i.con == "US") metrics   <- us.metrics
  
  for (i.eco in ecoregion){
    for (i.met in metrics){ 
      #Include for loops if you want to test different initial settings
      #for (i.tc in tc){
      #for (i.lr in lr){
      #for (i.bf in bf){
      #for (i.tr in tr){
      
      # The look up table indicats if the fish metric is used for the analysis 
      # and if already availabe it should include the selected initial settings 
      # for gbm.step()
      metric.usability <- metrics.criteria %>% 
        filter(ecoregion == i.eco, metric == i.met)
      
      metric.usability <- as.data.frame(metric.usability)
      
      if(metric.usability$BRT == "NO") next  # Checks if the metrics will be used or not
      
              
              
              inputname <- i.met%_%i.eco 

              
              # Selects the data 
              model.data <-as.data.frame(brt.list[[i.con]]$REF[[i.eco]][[i.met]])
              model.data[[2]] <-  model.data[[2]]/100
              
              
              
              # Runs BRTs with the least disturbed sites

              BuiltModel <-  try(gbm.step(data=model.data, 
                                          gbm.x = 3:ncol(model.data),
                                          gbm.y = 2,
                                          family = "gaussian",
                                          tree.complexity = metric.usability$tc, #i.tc
                                          learning.rate = metric.usability$lr,  #i.lr 
                                          bag.fraction = metric.usability$bf,  #i.bf
                                          n.trees = metric.usability$tr )) #i.tr
              
              
              # Creates a summary table of the models
              if(class(BuiltModel) == "gbm"){ #if gbm.step() successfully created a model
                if(first){
                  mod.summary <- tibble(in.name    = inputname,
                                        family     = "gaussian",
                                        tc         = metric.usability$tc,
                                        lr         = metric.usability$lr,
                                        bf         = metric.usability$bf,
                                        tr         = metric.usability$tr,
                                        best.trees = BuiltModel$gbm.call$best.trees,
                                        dev.mean   = BuiltModel$cv.statistics$deviance.mean,
                                        dev.se     = BuiltModel$cv.statistics$deviance.se,
                                        cv.corr    = BuiltModel$cv.statistics$correlation.mean,
                                        corr.se    = BuiltModel$cv.statistics$correlation.se,
                                        train.corr = BuiltModel$self.statistics$correlation)
                  first <- FALSE
                  
                } else {
                  mod.summary <- add_row(mod.summary, 
                                         in.name    = inputname,
                                         family     = "gaussian",
                                         tc         = metric.usability$tc,
                                         lr         = metric.usability$lr,
                                         bf         = metric.usability$bf,
                                         tr         = metric.usability$tr,
                                         best.trees = BuiltModel$gbm.call$best.trees,
                                         dev.mean   = BuiltModel$cv.statistics$deviance.mean,
                                         dev.se     = BuiltModel$cv.statistics$deviance.se,
                                         cv.corr    = BuiltModel$cv.statistics$correlation.mean,
                                         corr.se    = BuiltModel$cv.statistics$correlation.se,
                                         train.corr = BuiltModel$self.statistics$correlation)
                }
              } else {
                
                if(first){
                  mod.summary <- tibble(in.name    = inputname,
                                        family     = "gaussian",
                                        tc         = metric.usability$tc,
                                        lr         = metric.usability$lr,
                                        bf         = metric.usability$bf,
                                        tr         = metric.usability$tr ,
                                        best.trees = NA,
                                        dev.mean   = NA,
                                        dev.se     = NA,
                                        cv.corr    = NA,
                                        corr.se    = NA,
                                        train.corr = NA)
                  first <- FALSE
                  
                } else {  #if gbm.step() couldn't generate a BRT model, the columns will be filled with NAs
                  mod.summary <- add_row(mod.summary, 
                                         in.name    = inputname,
                                         family     = "gaussian",
                                         tc         = metric.usability$tc,
                                         lr         = metric.usability$lr,
                                         bf         = metric.usability$bf,
                                         tr         = metric.usability$tr,
                                         best.trees = NA,
                                         dev.mean   = NA,
                                         dev.se     = NA,
                                         cv.corr    = NA,
                                         corr.se    = NA,
                                         train.corr = NA)

            
          
                }
              }
                
    # Creates a nested list with all models
    brt.models[[inputname]] <- BuiltModel 
    
    
    if(!is.null(brt.models[[inputname]])){

    #Predicts the values for all sites from the BRT model:
    
    all.data <- as.data.frame(brt.list[[i.con]]$ALL[[i.eco]][[i.met]]) 
    all.data[[2]] <-  all.data[[2]]/100
    
    
    preds <- predict.gbm(BuiltModel, all.data, n.trees=BuiltModel$gbm.call$best.trees, type="response")
    
    varname <- names(all.data[2])%_%"pre"
    
    resid.data <- all.data %>% 
      dplyr::select(1, 2) %>% 
      mutate(!!varname := scales::rescale(all.data[[2]] - preds, to =c(0, 100)))
    
    # Creates a nested list with the predicted values
    brt.output[[inputname]] <- resid.data
    
    #}
    #}
    #}
    #}
    }
      
    }
  }
}  

# Saves the nested list with the BRT models and the prediciton
saveRDS(brt.models,"~/Output/BRT/brt_models_eu.RDS") 
saveRDS(brt.output, "~/Output/BRT/brt_output_eu.RDS") 


#Plots the functions and fitted values from the model	- Can be implemented in the for loop above

#find.int <- gbm.interactions(brt.models[[inputname]])
#rank.list <- as.data.frame(find.int$rank.list)

#tc <-  metric.usability$tc
#lr <-  metric.usability$lr
#tr <-  metric.usability$tr


#if(!is.null(brt.models[[inputname]])){
  #pdf("~/Output/BRT"%//%i.met%//%inputname%_%"tc"%&%tc%_%"lr"%&%lr%_%"tr"%&%tr%.%"pdf", 
      #width = 10, height = 8, title = inputname)
  #gbm.plot(brt.models[[inputname]], smooth=FALSE, n.plots=ncol(model.data)-2, write.title=TRUE,plot.layout=c(3, 3))
  #gbm.plot.fits(brt.models[[inputname]]) 
  #grid.table(rank.list)
  #dev.off()   }


  

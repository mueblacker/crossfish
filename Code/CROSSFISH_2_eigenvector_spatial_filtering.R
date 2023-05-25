# Library -----------------------------------------------------------------

library(readr)

library(dplyr)
library(tidyr)
library(magrittr)

library(pasta)

library(spdep) # For the calculation of the Moran I correlogramm
library(spmoran) # Eigenvector spatial filtering
library(sp)
library(maptools)

library(scales)
# Load data ---------------------------------------------------------------


total.euds <- read_csv(
  "~/Data/subset/EU/total.euds.csv")

total.usds <- read_csv(
  "~/Data/subset/US/total.usds.csv")

ld.site.criteria <- read_csv(
  "~/Data/subset/Lkpt/ld_site_criteria.csv")

eu.mean.metric<-  read_csv(
  "~/Data/subset/Lkpt/eu_metric_esf_look_up.csv") 

us.mean.metric <-  read_csv(
  "~/Data/subset/Lkpt/us_metric_esf_look_up.csv") 


# Load previous calculations (if available) ------------------------------

e.res.dist <- readRDS(
  "~/Output/ESF/eu_us_e_res_dist.RDS")

moran.cor.dist <- readRDS(
  "~/Output/ESF/us_eu_moran_cor_dist.RDS")


# Creat list of ecoregions and metrics for both continents ---------------

eu.ecoregion <- ld.site.criteria %>% 
  filter(continent == "EU") %>%
  .$FEOW_Code %>% unique() 

us.ecoregion <-ld.site.criteria %>% 
  filter(continent == "US") %>%
  .$FEOW_Code %>% unique()

eu.metrics <-  eu.mean.metric %>%
  .$metric %>% unique()

us.metrics <-  us.mean.metric %>%
  .$metric %>% unique()

metric.look.up <- eu.mean.metric %>% 
  bind_rows(., us.mean.metric) 

# Moran I correlogramm ----------------------------------------------------

moranI.test <- list()
moran.cor.dist  <- list()

for(i.con in c("EU")){
  if(i.con == "EU") total.ds  <- total.euds
  if(i.con == "US") total.ds  <- total.usds
  if(i.con == "EU") ecoregion <- eu.ecoregion
  if(i.con == "US") ecoregion <- us.ecoregion
  if(i.con == "EU") metrics   <- eu.metrics
  if(i.con == "US") metrics   <- us.metrics
  
  for(i.eco in ecoregion ){
    for(i.met in metrics ){
      
      # Select fish metrics with at least 36 non-zero values and some variation 
      metric.usability <- metric.look.up %>% 
        filter(ecoregion == i.eco, metric == i.met)
      
      if(metric.usability$n.sites < 36 | metric.usability$mean.ab == 0 | 
         metric.usability$mean.ab == 100) next
      
      # Select Ecoregion and fish metric of interest
      temp.df <- total.ds %>%
        filter(eco_feow_code == i.eco) %>%
        dplyr::select(id_site_code,geo_nc_area, loc_lat_wgs84, loc_long_wgs84, 
                      geo_lc_slope, geo_lc_elev, cli_lc_prec, cli_lc_temp, 
                      cli_nc_prec, cli_nc_temp, ends_with(i.met))  
      
      colnames(temp.df)[11] <- "ft_metric"
      
      # Define Coordinates (the R package spdep works with spatial data frames -
      # with the package sp it is possible to convert the data frame 
      # to a spatial point data frame)
      temp.coords <- temp.df[,c("loc_lat_wgs84", "loc_long_wgs84")]
      
      temp.spdf <- SpatialPointsDataFrame(coords = temp.coords, data = temp.df,
                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      
      coords <-  coordinates(temp.spdf)
      IDs<-row.names(as(temp.spdf, "data.frame"))
      
      # Define the distance (shortest distance at which each point has 
      # at least one neighbour) 
      # and the weigth matrix of nearst neighbours
      
      sids.kn1 <- knn2nb(knearneigh(coords, k=1), row.names = IDs)
      dsts <- unlist(nbdists(sids.kn1, coords))
      
      max_1nn<- max(dsts)
      
      temp_nb <- dnearneigh(coords, d1 = 0.004, d2 = max_1nn, row.names = IDs)
      #plot(temp_nb,coords, add=T) # plots the nearst neighbours
      sids_nbq_w<-nb2listw(temp_nb, zero.policy=T)
      
      # If you want to check the global Moran I
      #moranI.test[[i.con]][[i.eco]][[i.met]] <- list()
      #moranI.test[[i.con]][[i.eco]][[i.met]] <- moran.test(temp.df$ft_metric, listw=sids_nbq_w)
    
      
      # Calculation of the Moran I correlogram (Can be time consuming, 
      # depending on the number of samping sites)
      moran.cor.dist[[i.con]][[i.eco]][[i.met]] <- list()
      
      moran.cor.dist[[i.con]][[i.eco]][[i.met]] <- 
        sp.correlogram(temp_nb, temp.df$ft_metric, order = 6, method = "I",
                       style = "B", zero.policy=TRUE)

      
    }
  }
}

# Save Moran I correlograms
saveRDS(moran.cor.dist,
        "~/Output/ESF/us_eu_moran_cor_dist.RDS")



#temp.df <- total.usds %>%
  #filter(eco_feow_code == "AppPdm") %>%
  #dplyr::select(id_site_code,geo_nc_area, loc_lat_wgs84, loc_long_wgs84, 
                #geo_lc_slope, geo_lc_elev, cli_lc_prec, cli_lc_temp, 
                #cli_nc_prec, cli_nc_temp, starts_with("ft_")) 

#write.csv(temp.df, "~/Output/ESF/USSthPln.csv")



# Moran I Eigenvector spatial filtering ----------------------------------

e.res.dist <- list()

for(i.con in c("EU", "US")){
  if(i.con == "EU") total.ds  <- total.euds
  if(i.con == "US") total.ds  <- total.usds
  if(i.con == "EU") ecoregion <- eu.ecoregion
  if(i.con == "US") ecoregion <- us.ecoregion
  if(i.con == "EU") metrics   <- eu.metrics
  if(i.con == "US") metrics   <- us.metrics
  
  for(i.eco in ecoregion ){
    for(i.met in metrics ){
      
      # Select fish metrics with at least 36 non-zero values and some variation 
      metric.usability <- metric.look.up %>% 
        filter(ecoregion == i.eco, metric == i.met)
      
      if(metric.usability$n.sites < 36 | metric.usability$mean.ab == 0 | 
         metric.usability$mean.ab == 100) next
      
      # Select Ecoregion and fish metric of interest
      temp.df <- total.ds %>%
        filter(eco_feow_code == i.eco) %>%
        dplyr::select(id_site_code,geo_nc_area, loc_lat_wgs84, loc_long_wgs84, 
                      geo_lc_slope, geo_lc_elev, cli_lc_prec, cli_lc_temp, 
                      cli_nc_prec, cli_nc_temp, ends_with(i.met))  
      
      
      colnames(temp.df)[11] <- "ft_metric"
      
      # Define coordinates
      
      coords <- temp.df[,c("loc_lat_wgs84", "loc_long_wgs84")]
      
      # Define response variable
      
      y <- temp.df[,"ft_metric"]
      
      # Define predictor variables
      x <- temp.df[, c("geo_nc_area", "geo_lc_slope", "geo_lc_elev", 
                       "cli_lc_prec", "cli_lc_temp")]
      
      
      # Calculation of Eigenvector spatial filters, stepwise selection 
      # calculation of residuals (package spmoran)
      e.res.dist[[i.con]][[i.eco]][[i.met]] <- list()
      
      meig.dist <- meigen(coords = coords)
      e.res.dist[[i.con]][[i.eco]][[i.met]] <- esf(y=temp.df$ft_metric, 
                                                   x=x, meig= meig.dist,  fn= "aic")
      
    }
  }
}

# Save results of the ESF 
saveRDS(e.res.dist,
        "~/Output/ESF/eu_e_res_dist.RDS")


# Create table with ESF residuals -----------------------------------------

# Load look up table to select fish metrics where spatial autocorrelation occurs
esf.metric.look.up <-  read_csv(
  "~/Data/subset/Lkpt/fish_metrics_look_up_table.csv") 
 

# If you want check the residuals/ not necessary
check <- list()
for(i.con in c("EU", "US")){
  if(i.con == "EU") total.ds  <- total.euds
  if(i.con == "US") total.ds  <- total.usds
  if(i.con == "EU") ecoregion <- eu.ecoregion
  if(i.con == "US") ecoregion <- us.ecoregion
  if(i.con == "EU") metrics   <- eu.metrics
  if(i.con == "US") metrics   <- us.metrics
  
  for(i.eco in ecoregion ){
    
    for(i.met in metrics ){
      
      metric.usability <- esf.metric.look.up %>% 
        filter(ecoregion == i.eco, metric == i.met) %>% 
        mutate(moranI_greater_0.2 = ifelse(is.na(moranI_greater_0.2), "NO", moranI_greater_0.2 ))
      
      if(metric.usability$moranI_greater_0.2 =="NO" ) next
      
      temp.df <- total.ds %>%
        filter(eco_feow_code == i.eco) %>% 
        dplyr::select(ends_with(i.met))
      
      colnames(temp.df)[1] <- "ft_metric"

      check[[i.con]][[i.eco]][[i.met]] <- list()

      check[[i.con]][[i.eco]][[i.met]]  <- tibble(metrics     = temp.df$ft_metric,
                                                  esf_pred    = e.res.dist[[i.con]][[i.eco]][[i.met]]$pred,
                                                  esf_resid   = e.res.dist[[i.con]][[i.eco]][[i.met]]$resid,
                                                  check_esf   = temp.df$ft_metric - e.res.dist[[i.con]][[i.eco]][[i.met]]$pred)
      
    }
  }
}


# Bring list of ecoregions and fish metrics in one tibble
e.res.dist.rsc <- list()

first <-  TRUE

for(i.con in c("EU", "US")){
  if(i.con == "EU") total.ds  <- total.euds
  if(i.con == "US") total.ds  <- total.usds
  if(i.con == "EU") ecoregion <- eu.ecoregion
  if(i.con == "US") ecoregion <- us.ecoregion
  if(i.con == "EU") metrics   <- eu.metrics
  if(i.con == "US") metrics   <- us.metrics
  
  for(i.eco in ecoregion ){
    
    for(i.met in metrics ){
      
      
      
      # Select fish metrics 
      
      metric.usability <- esf.metric.look.up %>% 
        filter(ecoregion == i.eco, metric == i.met) %>% 
        mutate(moranI_greater_0.2 = ifelse(is.na(moranI_greater_0.2), "NO", moranI_greater_0.2 ))
      
      if(metric.usability$moranI_greater_0.2 =="NO" ) next
      
      temp.df <- total.ds %>%
        filter(eco_feow_code == i.eco) %>% 
        mutate(metrics = i.met)

      e.res.dist.rsc[[i.con]][[i.eco]][[i.met]] <- list()
      e.res.dist.rsc[[i.con]][[i.eco]][[i.met]] <- rescale(e.res.dist[[i.con]][[i.eco]][[i.met]]$resid, to=c(0,100))
      
      if(first){
        esf.residuals  <- tibble(continent     = i.con,
                                 id_site_code  = temp.df$id_site_code,
                                 ecoregion     = temp.df$eco_feow_code,
                                 metrics       = temp.df$metrics,
                                 esf_resid     = rescale(e.res.dist[[i.con]][[i.eco]][[i.met]]$resid, to =c(0,100)))
        
        first <- FALSE
        
      } else {
        esf.residuals  <- bind_rows(esf.residuals,
                                    tibble(continent     = i.con,
                                           id_site_code  = temp.df$id_site_code,
                                           ecoregion     = temp.df$eco_feow_code,
                                           metrics       = temp.df$metrics,
                                           esf_resid     = rescale(e.res.dist[[i.con]][[i.eco]][[i.met]]$resid, to =c(0,100))))
                                            
      }
      
    }
  }
}


eu.esf.residuals <- esf.residuals %>% 
  filter(continent == "EU") %>% 
  dplyr::select(-continent, -ecoregion) %>% 
  spread(metrics, esf_resid)

total.euds.esf <- total.euds %>% 
  dplyr::select(-starts_with("ft_")) %>% 
  left_join(.,eu.esf.residuals, by ="id_site_code")

total.euds.esf %<>% 
  rename( ft_hab_rheo   = RHEO,
          ft_troph_ins  = INS,
          ft_mig_mig   = MIG,
          ft_soc_salm   = SALM,
          ft_soc_game   = GAME,
          ft_soc_LC     = LC,
          ft_tol_intol  = INTOL)


us.esf.residuals <- esf.residuals %>% 
  filter(continent == "US") %>% 
  select(-continent, -ecoregion) %>% 
  spread(metrics, esf_resid)

total.usds.esf <- total.usds %>% 
  select(-starts_with("ft_")) %>% 
  left_join(.,us.esf.residuals, by ="id_site_code")

total.usds.esf %<>% 
  rename( ft_hab_rheo   = RHEO,
          ft_hab_lent   = LENT,
          ft_troph_detr = DETR,
          ft_troph_herb = HERB,
          #ft_troph_inv  = INV,
          ft_troph_plan = PLAN,
          #ft_troph_pisc = PISC,
          ft_rep_lith   = LITH,
          #ft_mig_mig   = MIG,
          ft_soc_salm   = SALM,
          #ft_soc_game   = GAME,
          #ft_soc_LC     = LC,
          #ft_soc_NT     = NT,
          ft_tol_tol    = TOL,
          ft_tol_intol  = INTOL)

# Save tibble of resiudals as csv-file
write.csv(total.euds.esf, "~/Data/subset/ESF/total.euds.esf.20180601.csv", row.names = FALSE)
write.csv(total.usds.esf, "~/Data/subset/ESF/total.usds.esf.20180601.csv", row.names = FALSE)

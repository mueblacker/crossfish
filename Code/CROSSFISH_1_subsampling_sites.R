# Library -----------------------------------------------------------------
library(readr)
library(dplyr)
library(magrittr)



# Load data ---------------------------------------------------------------

total.usds <- read_csv("./Data/subset/US/total.usds.csv") 


# Site reduction in Ecoregions with more than 2000 sites ------------------

rand.sub <- list()

# Select Ecoregion

eco.select <-  c( "NUSCAtlDrn", "LrnGrtLks", "UppMsss", "TysOldOhi", "MddMssr", "Clrd" )


FIRST_RUN <-  TRUE
for (i.eco in eco.select) {
  
 site.number <- total.usds %>% 
                  filter(eco_feow_code == i.eco) %>% 
                  count(eco_feow_code) %>% 
                  .[[2]]
 n <- 1/(1-2000/site.number)
                

  for(i.size in c("CREEK", "RIVER")){
    for(i.imp in c ("REF", "DIST")){
  
    if(i.size == "CREEK"){
      
      if(i.imp == "REF"){
        temp <- total.usds %>% 
          filter(eco_feow_code == i.eco, geo_nc_area < 100, lu_nc_agri < 0.1, 
                 lu_nc_pasture < 0.1, lu_nc_urban < 0.01) %>% 
          mutate(lu_nc_urag =  lu_nc_urban + lu_nc_agri) %>% 
          arrange(.,lu_nc_urag) %>% 
          select(id_site_code) }

    
      else{   
        temp <- total.usds %>% 
          filter(eco_feow_code == i.eco, geo_nc_area < 100 , lu_nc_agri >= 0.1 | 
                 lu_nc_pasture >= 0.1| lu_nc_urban >= 0.01) %>% 
          mutate(lu_nc_urag =  lu_nc_urban + lu_nc_agri) %>% 
          arrange(.,lu_nc_urag) %>% 
          select(id_site_code) } 
      
    }else{
        if(i.imp == "REF"){
          temp <- total.usds %>% 
            filter(eco_feow_code == i.eco, geo_nc_area >= 100, lu_nc_agri < 0.1, 
                   lu_nc_pasture < 0.1, lu_nc_urban < 0.01) %>% 
            mutate(lu_nc_urag =  lu_nc_urban + lu_nc_agri) %>% 
            arrange(.,lu_nc_urag) %>% 
            select(id_site_code) }
          
        else{   
          temp <- total.usds %>% 
            filter(eco_feow_code == i.eco, geo_nc_area >= 100,  lu_nc_agri >= 0.1 | 
                     lu_nc_pasture >= 0.1| lu_nc_urban >= 0.01) %>% 
            mutate(lu_nc_urag =  lu_nc_urban + lu_nc_agri) %>% 
            arrange(.,lu_nc_urag) %>% 
            select(id_site_code) }
    } 
        if(length(temp$id_site_code) > 0){
        rand.sub[[i.eco]][[i.size]][[i.imp]] <- list()
        rand.sub[[i.eco]][[i.size]][[i.imp]] <- temp[-seq(1, nrow(temp), n), ]
        }
      
      if(FIRST_RUN){
        
        subs.usds <- rand.sub[[i.eco]][[i.size]][[i.imp]] 
        
        FIRST_RUN <- FALSE
        
      }else{
        subs.usds <- subs.usds %>% 
         rbind(.,rand.sub[[i.eco]][[i.size]][[i.imp]])
      }
      
      
    }
  }
}


red.usds <-  total.usds %>% 
  inner_join(.,subs.usds, by = "id_site_code")  
  

total.usds <- total.usds %>% 
  filter(eco_feow_code != "NUSCAtlDrn" ,  eco_feow_code != "LrnGrtLks" ,
          eco_feow_code != "UppMsss", eco_feow_code !=  "TysOldOhi", eco_feow_code !=  "MddMssr", 
          eco_feow_code != "Clrd") %>% 
  bind_rows(., red.usds)


write.csv(total.usds,"./Data/subset/US/total.usds.csv", row.names = FALSE)


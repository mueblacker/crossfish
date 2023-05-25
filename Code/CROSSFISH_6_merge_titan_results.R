library(pasta)
library(purrr)
library(dplyr)
library(tibble)

eu.titan.path <- "~/Output/TITAN/save_increment/EU"
eu.titan.file <- "eu_titan"%_%1:100%.%"rds"
us.titan.path <- "~/Output/TITAN/save_increment/US"
us.titan.file <- "us_titan"%_%1:100%.%"rds"

eu.titan <- map(eu.titan.file, ~readRDS(eu.titan.path%//%.x)) %>% 
  set_names(., "run"%_%1:100)

us.titan <- map(us.titan.file, ~readRDS(us.titan.path%//%.x)) %>% 
  set_names(., "run"%_%1:100)

eu.str.name <- names(eu.titan[[1]]$EU$CanCst$creek$lc)
eu.ctc.name <- names(eu.titan[[1]]$EU$CanCst$creek)
eu.siz.name <- names(eu.titan[[1]]$EU$CanCst)
eu.eco.name <- map(eu.titan, ~names(.x$EU)) %>% 
  unlist() %>% 
  unique()

us.str.name <- names(us.titan[[1]]$US$AlsCndPcfCst$creek$lc)
us.ctc.name <- names(us.titan[[1]]$US$AlsCndPcfCst$creek)
us.siz.name <- names(us.titan[[1]]$US$AlsCndPcfCst)
us.eco.name <- map(us.titan, ~names(.x$US)) %>% 
  unlist() %>% 
  unique()

eu.titan.tbl <- modify_depth(.x = eu.titan, .depth = 5, ~map2_dfr(.x = .x, .y = eu.str.name, ~mutate(.x, stressor = .y))) %>% 
  modify_depth(., .depth = 4, ~map2_dfr(.x, .y = eu.ctc.name, ~mutate(.x, catchment = .y))) %>% 
  modify_depth(., .depth = 3, ~map2_dfr(.x, .y = eu.siz.name, ~mutate(.x, size = .y))) %>% 
  modify_depth(., .depth = 2, ~map2_dfr(.x, .y = eu.eco.name, ~mutate(.x, ecoregion = .y))) %>% 
  modify_depth(., .depth = 1, ~map2_dfr(.x, .y = "EU", ~mutate(.x, continent = .y))) %>% 
  map2_dfr(., "run"%_%1:100, ~mutate(.x, run = .y)) %>% 
  as_tibble(.)

us.titan.tbl <- modify_depth(.x = us.titan, .depth = 5, ~map2_dfr(.x = .x, .y = us.str.name, ~mutate(.x, stressor = .y))) %>% 
  modify_depth(., .depth = 4, ~map2_dfr(.x, .y = us.ctc.name, ~mutate(.x, catchment = .y))) %>% 
  modify_depth(., .depth = 3, ~map2_dfr(.x, .y = us.siz.name, ~mutate(.x, size = .y))) %>% 
  modify_depth(., .depth = 2, ~map2_dfr(.x, .y = us.eco.name, ~mutate(.x, ecoregion = .y))) %>% 
  modify_depth(., .depth = 1, ~map2_dfr(.x, .y = "US", ~mutate(.x, continent = .y))) %>% 
  map2_dfr(., "run"%_%1:100, ~mutate(.x, run = .y)) %>% 
  as_tibble(.)


titan.bp <- eu.titan.tbl %>% 
  bind_rows(us.titan.tbl) %>% 
  separate(trait, c("ft", "group", "metric", "pr"), "_") %>% 
  mutate(metric = toupper(metric)) %>% 
  select(continent, ecoregion, size, metric, stressor, catchment, run, zenv.cp, obsiv.prob, zscore)

write.csv(titan.bp,"~/Output/TITAN/titan_bp_100.csv", row.names = FALSE )

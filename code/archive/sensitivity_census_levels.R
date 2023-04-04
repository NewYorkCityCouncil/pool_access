source("code/00_load_dependencies.R")

########################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/3/2023
#
# Must be run after the 01_get_isochromes file!
########################################################################################


get_pop_overlap = function(pop, pools_walk_zone_15min, council_districts) { 
  # pull population info at block level for all relevant counties 
  pop = pop %>%
    st_transform(st_crs(4326))
  
  # figure out what blocks are within the pool isochrone
  block_intersection = st_intersection(pop, pools_walk_zone_15min) %>% 
    mutate(area_within_pool_zone = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    select(GEOID, area_within_pool_zone)
  
  # adjust the population to only include people not within a 15 minute walk of 
  #   a pool already (assuming uniform population dist over each block)
  pop = pop %>%
    merge(block_intersection, by = "GEOID", all.x = T) %>%
    mutate(total_area = as.numeric(st_area(.)), 
           area_within_pool_zone = replace_na(area_within_pool_zone, 0), 
           perc_area = (total_area - area_within_pool_zone)/total_area,
           
           # get rid of tiny measurement errors like -0.000000000001
           perc_area = ifelse(perc_area < 0, 0, perc_area), 
           
           # scale pop by (assumed) % not in zone
           pop_near_pool = value * (1 - perc_area),
           pop = value) %>%
    select(-total_area, -area_within_pool_zone, -value)
  
  
  # ------------------------------------------------------------------------------
  # pull population for each city owned "no use" land isochrone
  
  
  get_overlap_pop = function(x, population, col="pop") {
    # where x is list of ids from pop
    overlap_pop = population[x, ]
    overlap_pop = sum(overlap_pop %>% pull(col))
    return(overlap_pop)
  }
  council_block_intersection = st_intersects(council_districts, pop)
  council_districts$pop_near_pool = sapply(council_block_intersection,
                                           get_overlap_pop, 
                                           population=pop, col="pop_near_pool")
  council_districts$pop = sapply(council_block_intersection, get_overlap_pop, 
                                 population=pop, col="pop")
  council_districts$perc_near_pool = council_districts$pop_near_pool/council_districts$pop
  
  return(council_districts)
  
}

pools_walk_zone_15min = readRDS(file.path("data", "output", "pools_walk_zone_15min.RDS"))

council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326))


################################################################################
# block allocation
################################################################################

pop_b = get_decennial(geography = "block", 
                    state = "NY",
                    county = c("061", "081", "085", "005", "047"),
                    variables = "P1_001N", 
                    year = 2020, 
                    geometry = T)

b_allocation = get_pop_overlap(pop_b, pools_walk_zone_15min, council_districts)

################################################################################
# block group allocation
################################################################################

pop_bg = get_decennial(geography = "block group", 
                       state = "NY",
                       county = c("061", "081", "085", "005", "047"),
                       variables = "P1_001N", 
                       year = 2020, 
                       geometry = T)

bg_allocation = get_pop_overlap(pop_bg, pools_walk_zone_15min, council_districts)


################################################################################
# tract allocation
################################################################################

pop_tract = get_decennial(geography = "tract", 
                       state = "NY",
                       county = c("061", "081", "085", "005", "047"),
                       variables = "P1_001N", 
                       year = 2020, 
                       geometry = T)

tract_allocation = get_pop_overlap(pop_bg, pools_walk_zone_15min, council_districts)


################################################################################
# compare
################################################################################


cor(b_allocation$perc_near_pool, bg_allocation$perc_near_pool)

cor(b_allocation$perc_near_pool, tract_allocation$perc_near_pool)

cor(bg_allocation$perc_near_pool, tract_allocation$perc_near_pool)

ggplot() + 
  geom_point(aes(b_allocation$perc_near_pool, bg_allocation$perc_near_pool)) + 
  xlab("Block allocation") + ylab("Block group allocation")

ggplot() + 
  geom_point(aes(b_allocation$perc_near_pool, tract_allocation$perc_near_pool)) + 
  xlab("Block allocation") + ylab("Tract allocation")

ggplot() + 
  geom_point(aes(bg_allocation$perc_near_pool, tract_allocation$perc_near_pool)) + 
  xlab("Block group allocation") + ylab("Tract allocation")

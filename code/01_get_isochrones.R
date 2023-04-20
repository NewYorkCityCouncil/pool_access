source("code/00_load_dependencies.R")
source("../tokens.R")

########################################################################################
# Created by: Anne Driscoll
# Last edited on: 3/29/2023
#
# This file pulls in the pools location data, locations of city property and population
#   in order to prepare data for mapping
########################################################################################


################################################################################
# get 15 minute walk zones for existing parks
################################################################################

pools = st_read("https://data.cityofnewyork.us/api/geospatial/y5rm-wagw?method=export&format=GeoJSON") %>%
  st_transform(st_crs(4326)) %>%
  st_centroid() %>%
  # only keep one pool per property number (ie we don't want to count pools + 
  #   wading pools as separate when they're at the same location)
  group_by(gispropnum) %>%
  arrange(name) %>%
  filter(row_number()==1)

mb_access_token(mapbox_token, install = T, overwrite = T)
readRenviron("~/.Renviron")

get_isochrone = function(data, t = 15) {
  temp = mb_isochrone(location = as.vector(st_coordinates(data$geometry)),
                      profile = "walking",
                      time = t)
  Sys.sleep(0.25) # to force us not to go over the API limit
  return(temp)
}

# get the 15 minute walk zones for each pool, combine into shapefile, then union to make single shape
# SLOW: takes a minute
pools_walk_zone_15min = apply(pools, 1, FUN = get_isochrone)
pools_walk_zone_15min = bind_rows(pools_walk_zone_15min)
pools_walk_zone_15min = st_union(pools_walk_zone_15min)


################################################################################
# find (for each "no use" city property) how many people are within a 15 minute
#   walk that don't already have a pool within a 15 minute walk
################################################################################

# ------------------------------------------------------------------------------
# load in city property and figure out the isochrones for each potential location

city_property = st_read("https://data.cityofnewyork.us/api/geospatial/2mhq-um7h?method=export&format=GeoJSON") 

no_use = city_property %>%
  filter(usetype %in% c("NO USE", 
                        "NO USE-VACANT LAND", 
                        "NO USE-NON RESIDENTIAL STRUCTURE", 
                        "NO USE-RESIDENTIAL STRUCTURE", 
                        "UNDEVELOPED OPEN SPACE"))

# figure out which no use locations fall in our interest area
interest_area = st_read("https://data.cityofnewyork.us/api/geospatial/ykru-djh7?method=export&format=GeoJSON") %>%
  filter(ejdesignat %in% c("EJ Area", "Potential EJ Area")) %>%
  st_union() %>%
  st_difference(pools_walk_zone_15min)

# figure out which no_use locations are within the interest area and filter down
overlaps_area = st_intersects(no_use, interest_area)
overlaps_area = sapply(overlaps_area, length)
no_use = no_use[overlaps_area == 1, ]

# for each of the potential new pool locations, find 15 minute walk zone
# SLOW: takes several minutes since we are rate limited to 300 requests a minute
land_walk_zone_15min = apply(no_use, 1, FUN = get_isochrone)
land_walk_zone_15min = bind_rows(land_walk_zone_15min)


# ------------------------------------------------------------------------------
# get population from census and blank out any population within a pool isochrone

# load api key
#census_api_key(census_token, install = T)
#readRenviron("~/.Renviron")

# pull population info at block level for all relevant counties 
pop = get_decennial(geography = "block", 
                    state = "NY",
                    county = c("061", "081", "085", "005", "047"),
                    variables = "P1_001N", 
                    year = 2020, 
                    geometry = T)
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
         
         # get rid of tiny measurement errors like -0.000000001
         perc_area = ifelse(perc_area < 0, 0, perc_area), 
         
         # scale pop by (assumed) % not in zone
         pop = value * perc_area, 
         tot_pop = value) %>%
  select(-total_area, -area_within_pool_zone, -value)


# ------------------------------------------------------------------------------
# pull population for each city owned "no use" land isochrone

intersection = st_intersects(land_walk_zone_15min, pop)

get_overlap_pop = function(x, population) {
  # where x is list of ids from pop
  overlap_pop = population[x, ]
  overlap_pop = sum(overlap_pop$pop)
  return(overlap_pop)
}
no_use$new_users = sapply(intersection, get_overlap_pop, population=pop)


################################################################################
# save data for plotting
################################################################################

saveRDS(pop, file.path("data", "output", "block_population.RDS"))
saveRDS(pools, file.path("data", "output", "pools.RDS"))
saveRDS(pools_walk_zone_15min, file.path("data", "output", "pools_walk_zone_15min.RDS"))
saveRDS(no_use, file.path("data", "output", "no_use_city_property.RDS"))
saveRDS(land_walk_zone_15min, file.path("data", "output", "land_walk_zone_15min.RDS"))

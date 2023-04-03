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
  st_centroid()

mb_access_token(mapbox_token, install = T, overwrite = T)

get_isochrone = function(data, t = 15) {
  temp = mb_isochrone(location = as.vector(st_coordinates(data$geometry)),
                      profile = "walking",
                      time = t)
  Sys.sleep(0.25) # to force us not to go over the API limit
  return(temp)
}
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

# takes ~15 minutes since we are rate limited to 300 requests a minute
land_walk_zone_15min = apply(no_use, 1, FUN = get_isochrone)
land_walk_zone_15min = bind_rows(land_walk_zone_15min)


# ------------------------------------------------------------------------------
# get population from census and blank out any population within a pool isochrone

census_api_key(census_token, install = T)
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
#   a pool already (assuming uniform population dist over block)
pop = pop %>%
  merge(block_intersection, by = "GEOID", all.x = T) %>%
  mutate(total_area = as.numeric(st_area(.)), 
         area_within_pool_zone = replace_na(area_within_pool_zone, 0), 
         perc_area = (total_area - area_within_pool_zone)/total_area,
         
         # get rid of tiny measurement errors like -0.000000000001
         perc_area = ifelse(perc_area < 0, 0, perc_area), 
         
         # scale pop by % not in zone
         pop = value * perc_area) %>%
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

saveRDS(pools, file.path("data", "output", "pools.RDS"))
saveRDS(pools_walk_zone_15min, file.path("data", "output", "pools_walk_zone_15min.RDS"))
saveRDS(no_use, file.path("data", "output", "no_use_city_property.RDS"))

################################################################################
# make plots
################################################################################

# load in other data for plotting 
ej_areas = st_read("https://data.cityofnewyork.us/api/geospatial/ykru-djh7?method=export&format=GeoJSON") %>%
  filter(ejdesignat %in% c("EJ Area", "Potential EJ Area")) %>%
  st_union()


# ------------------------------------------------------------------------------
# make plot showing areas of interest

interest_area = ej_areas %>%
  st_difference(pools_walk_zone_15min)

no_use = no_use %>%
  mutate(label = paste0("<strong>Agency</strong>: ", agency, "<br>",
                       "<strong>Parcel Name:</strong> ", parcelname, "<br>", 
                       "<strong>Use:</strong> ", usetype, "<br>",
                       "<strong># of people within <15 min, who don't have pool access:</strong> ", 
                       format(round(new_users/100)*100, big.mark = ","))) %>%
  st_filter(interest_area)

# dcas = Department of Citywide Administrative Services
# parks = parks
# hpd = Housing Preservation & Development
# edc = Economic Development Corporation?
# dsbs = Department of Small Business Services
# dep = Department of Environmental Protection 
#nycta 
pal <- colorFactor(
  palette = carto_pal(5, "SunsetDark"),
  domain = no_use$usetype
)

# make plot of interest zones
map = leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 16)) %>%
  addProviderTiles('CartoDB.Positron', 
                   options = providerTileOptions(minZoom = 10, maxZoom = 16)) %>%
  addPolygons(data = interest_area, weight = 0, col = 'grey') %>%
  addCircles(data = pools, weight = 3, radius = 50, col = '#3498DB', 
             opacity = 1, fillOpacity = 1) %>%
  addPolygons(data = council_districts, weight = 1, col = '#686868', fillOpacity = 0) %>%
  addCircles(data = no_use, weight = 2, 
             radius = ~range01(new_users)*200, #75, #~sqrt(new_users)-5, 
             opacity = ~range01(new_users), fillOpacity = ~range01(new_users), 
             color = ~pal(usetype), popup = ~label)
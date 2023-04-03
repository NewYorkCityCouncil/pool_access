source("code/00_load_dependencies.R")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/3/2023
#
# This file creates the summary statistics and tables for the briefing paper
################################################################################

################################################################################
# read in data 
################################################################################

pools = readRDS(file.path("data", "output", "pools.RDS"))

pools_walk_zone_15min = readRDS(file.path("data", "output", "pools_walk_zone_15min.RDS"))

land_walk_zone_15min = readRDS(file.path("data", "output", "land_walk_zone_15min.RDS"))

interest_area = st_read("https://data.cityofnewyork.us/api/geospatial/ykru-djh7?method=export&format=GeoJSON") %>%
  filter(ejdesignat %in% c("EJ Area", "Potential EJ Area")) %>%
  st_union() %>%
  st_difference(pools_walk_zone_15min)

council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326))


################################################################################
# static maps
################################################################################

# ------------------------------------------------------------------------------
# council districts colored by number of existing pools
# ------------------------------------------------------------------------------

pools_by_district = st_intersects(council_districts, pools)
pools_by_district = sapply(pools_by_district, length)

council_districts$num_pools = pools_by_district

pal = colorFactor(
  palette = c("grey", carto_pal(5, "Teal")),
  domain = council_districts$num_pools
)

leaflet(options = leafletOptions(zoomControl = FALSE, 
                                 minZoom = 10, 
                                 maxZoom = 16)) %>% 
  addPolygons(data = council_districts, weight = 0, color = ~pal(num_pools), fillOpacity = 0.8) %>% 
  addProviderTiles('CartoDB.Positron', 
                   options = providerTileOptions(minZoom = 11, maxZoom = 16)) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend(position = "topleft", pal = pal, 
            values = sort(unique(council_districts$num_pools)), opacity = 1)


# ------------------------------------------------------------------------------
# percent of population within a 15 minute walk of a pool
# ------------------------------------------------------------------------------

# load api key
census_api_key(census_token, install = T)
readRenviron("~/.Renviron")

# pull population info at block level for all relevant counties 
pop = get_decennial(geography = "block", 
                    state = "NY",
                    county = c("061", "081", "085", "005", "047"),
                    variables = "P1_001N", 
                    year = 2020, 
                    geometry = T) %>%
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


get_overlap_pop = function(x, population, col="pop") {
  # where x is list of ids from pop
  overlap_pop = population[x[[1]], ]
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

pal = colorNumeric(
  palette = colorRampPalette(carto_pal(7, "Teal")),
  domain = c(-0.0001, 1.0001),
  na.color = "transparent"
)

leaflet(options = leafletOptions(zoomControl = FALSE, 
                                 minZoom = 10, 
                                 maxZoom = 16)) %>% 
  addPolygons(data = council_districts, weight = 0, color = ~pal(perc_near_pool), fillOpacity = 0.8) %>% 
  addProviderTiles('CartoDB.Positron', 
                   options = providerTileOptions(minZoom = 11, maxZoom = 16)) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend(position = "topleft", pal = pal, 
            values = sort(unique(council_districts$num_pools)), opacity = 1)


# ------------------------------------------------------------------------------
# council districts showing number of "no use" city owned land parcels
# ------------------------------------------------------------------------------




################################################################################
# create council district level tables
################################################################################

# ------------------------------------------------------------------------------
# get number of existing pools by district


# ------------------------------------------------------------------------------
# get number of "no use" locations within each council district


# ------------------------------------------------------------------------------
# figure out the location within that district with the max number ppl who woud 
#   newly gain access to a pool 


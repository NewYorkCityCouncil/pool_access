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

ej_areas = st_read("https://data.cityofnewyork.us/api/geospatial/ykru-djh7?method=export&format=GeoJSON") %>%
  filter(ejdesignat %in% c("EJ Area", "Potential EJ Area")) %>%
  st_union()

council_districts = st_read("https://data.cityofnewyork.us/api/geospatial/yusd-j4xi?method=export&format=GeoJSON") %>%
  st_transform(st_crs(4326))


################################################################################
# static maps
################################################################################

# ------------------------------------------------------------------------------
# council districts colored by number of existing pools

council_districts

leaflet() %>% 
  addProviderTiles('CartoDB.Positron', 
                   options = providerTileOptions(minZoom = 11, maxZoom = 16)) %>%
  addCouncilStyle(add_dists = TRUE)

# ------------------------------------------------------------------------------
# council districts showing number of "no use" city owned land parcels




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


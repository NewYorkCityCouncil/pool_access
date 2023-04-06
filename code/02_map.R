source("code/00_load_dependencies.R")

########################################################################################
# Created by: Anne Driscoll
# Last edited on: 3/30/2023
#
# This file creates the map of existing pools and "no use" land locations for 
#   potential future pools
########################################################################################


################################################################################
# read in data 
################################################################################

pools = readRDS(file.path("data", "output", "pools.RDS"))

pools_walk_zone_15min = readRDS(file.path("data", "output", "pools_walk_zone_15min.RDS"))

ej_areas = st_read("https://data.cityofnewyork.us/api/geospatial/ykru-djh7?method=export&format=GeoJSON") %>%
  filter(ejdesignat %in% c("EJ Area", "Potential EJ Area")) %>%
  st_union()

council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326))

################################################################################
# finish assembling data
################################################################################

interest_area = ej_areas %>%
  st_difference(pools_walk_zone_15min)

no_use = no_use %>%
  mutate(label = paste0("<strong>Agency</strong>: ", agency, "<br>",
                        "<strong>Parcel Name:</strong> ", parcelname, "<br>", 
                        "<strong>Use:</strong> ", usetype, "<br>",
                        "<strong># of people within <15 min, who don't have pool access:</strong> ", 
                        format(round(new_users/100)*100, big.mark = ","))) %>%
  st_filter(interest_area)


################################################################################
# plot
################################################################################

pal = colorFactor(
  palette = nycc_pal("mixed")(5),
  domain = no_use$usetype
)

map = leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 16)) %>%
  addProviderTiles('CartoDB.Positron', 
                   options = providerTileOptions(minZoom = 11, maxZoom = 16)) %>%
  addPolygons(data = interest_area, weight = 0, col = 'grey') %>%
  addCircles(data = pools, weight = 3, radius = 50, col = '#3498DB', 
             opacity = 1, fillOpacity = 1) %>%
  addPolygons(data = council_districts, weight = 1, col = '#686868', fillOpacity = 0) %>%
  addCircles(data = no_use, weight = 2, 
             radius = ~range01(new_users)*200, #75, #~sqrt(new_users)-5, 
             opacity = ~range01(new_users), fillOpacity = ~range01(new_users), 
             color = ~pal(usetype), popup = ~label) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend(position = "topleft", pal = pal, 
                       title = "Land Use Type",
                       values = no_use$usetype, opacity = 1, 
                       labFormat = labelFormat(transform = str_to_title))

saveWidget(map, file=file.path('visuals', 
                               "potential_pool_locations.html"))
mapview::mapshot(map, 
        file = file.path("visuals", "potential_pool_locations.png"),
        remove_controls = c("homeButton", "layersControl"), vwidth = 1000, vheight = 850)



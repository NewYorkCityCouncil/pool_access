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

council_districts = councildown:::dists %>%
  st_transform(st_crs(4326))

no_use = read_rds(file.path("data", "output", "no_use_city_property.RDS"))

################################################################################
# finish assembling data
################################################################################

interest_area = ej_areas %>%
  st_difference(pools_walk_zone_15min)

no_use = no_use %>%
  mutate(label = paste0("<strong>Agency</strong>: ", agency, "<br>",
                        "<strong>Parcel Name:</strong> ", parcelname, "<br>", 
                        "<strong>Use:</strong> ", usetype, "<br>",
                        "<strong># of people within 15 min walk, <br> who currently don't have pool access:</strong> ", 
                        format(round(new_users/100)*100, big.mark = ","))) %>%
  st_filter(interest_area)

pools_by_district = st_intersects(council_districts, pools)
pools_by_district = sapply(pools_by_district, length)
council_districts$num_pools = pools_by_district


################################################################################
# plot
################################################################################

#dot_color = unname(councildown:::warm[3])
pal = colorFactor(
  palette = c("lightgrey", "#2F56A6", "#3498DB"),
  domain = c("existing pool", 
             "council districts that currently have no pool", 
             "area that is both >15 minute walk from a pool <br>&emsp;&emsp;and an Environmental Justice area")
) 

breaks = classInt::classIntervals(no_use$new_users, n = 5, style = 'jenks')$brks
breaks[length(breaks)] = breaks[length(breaks)] + 5000
reds = c("#ff0000", "#ff8080", "#ffbfbf", "#ffe6e6")

pal2 = colorBin(
  #palette = sequential_hcl(5, h = c(0, 0), c = c(100), l = c(5, 95), 
  #                         rev = TRUE, power = 1),
  palette = rev(colorRampPalette(reds)(5)),
  bins = round(breaks/5000) * 5000,
  domain = no_use$new_users
) 

map = leaflet() %>%
  addPolygons(data = interest_area, weight = 0, col = 'grey', 
              fillOpacity = 0.15) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addCircles(data = pools, weight = 3, radius = 50, col = '#3498DB', 
             opacity = 1, fillOpacity = 1, popup = ~name) %>%
  addCircles(data = no_use, radius = 110, 
            fillOpacity = 1, fillColor = ~pal2(new_users), 
            opacity = 1, color = "#660000", weight = 0.5,
            popup = ~label) %>%
  addPolygons(data = councildown:::dists[councildown:::dists$coun_dist %in% 
                                           council_districts$coun_dist[council_districts$num_pools == 0], ], 
              col = unname(councildown:::nycc_colors[1]), weight = 1.5, 
              fillOpacity = 0, opacity = 1) %>%
  addLegend_decreasing(position="topleft", pal, 
                       values = c("existing pool", 
                                  "area that is both >15 minute walk from a pool <br>&emsp;&emsp;and an Environmental Justice area", 
                                  "council districts that currently have no pool"), 
                       opacity = 1) %>%
  addLegend_decreasing(position="topleft", pal2, values = no_use$new_users, 
                       opacity = 1, decreasing = T, 
                       title = paste0("People who don't have pool access<br>", 
                                      "that would gain access if a pool <br>", 
                                      "were constructed at this location"))

saveWidget(map, file=file.path('visuals', 
                               "potential_pool_locations.html"))
mapview::mapshot(map, 
        file = file.path("visuals", "potential_pool_locations.png"),
        remove_controls = c("homeButton", "layersControl", "zoomControl"), vwidth = 1000, vheight = 850)



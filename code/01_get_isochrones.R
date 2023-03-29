source("code/00_load_dependencies.R")
source("../tokens.R")

########################################################################################
# Created by: Anne Driscoll
# Last edited on: 3/29/2023
#
# This file pulls in the pools location data
########################################################################################



pools = st_read("https://data.cityofnewyork.us/api/geospatial/y5rm-wagw?method=export&format=GeoJSON") %>%
  st_transform(st_crs(4326)) %>%
  st_centroid()

mb_access_token(mapbox_token, install = T, overwrite = T)

get_isochrone = function(data, t = 15) {
  mb_isochrone(location = as.vector(st_coordinates(data$geometry)),
               profile = "walking",
               time = t)
}
walk_zone_15min = apply(pools, 1, FUN = get_isochrone)
walk_zone_15min = bind_rows(walk_zone_15min)
walk_zone_15min = st_union(walk_zone_15min)




ej_areas = st_read("https://data.cityofnewyork.us/api/geospatial/ykru-djh7?method=export&format=GeoJSON") %>%
  filter(ejdesignat == "EJ Area") %>%
  st_union()

pool_shapes = st_read("https://data.cityofnewyork.us/api/geospatial/y5rm-wagw?method=export&format=GeoJSON") %>%
  st_transform(st_crs(4326))

map = leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 16)) %>%
  addProviderTiles('CartoDB.Positron', 
                   options = providerTileOptions(minZoom = 10, maxZoom = 16)) %>%
  addPolygons(data = ej_areas, weight = 0, col = 'grey') %>%
  addPolygons(data = walk_zone_15min, weight = 2, col = 'lightblue') %>%
  addPolygons(data = pool_shapes, weight = 3, col = 'blue')


nyc = st_read("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON") %>%
  st_transform(st_crs(4326))
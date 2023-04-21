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

no_use = readRDS(file.path("data", "output", "no_use_city_property.RDS"))

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
  palette = c("#CACACA", rev(nycc_pal("cool")(4))),
  domain = council_districts$num_pools
)

map = leaflet() %>% 
  councildown::addPolygons(data = council_districts, color = ~pal(num_pools), opacity = 1,) %>% 
  addCouncilStyle(add_dists = TRUE, 
                  highlight_dists = council_districts$CounDist[council_districts$num_pools >= 3]) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
            title = "Number of Pools",
            values = sort(unique(council_districts$num_pools)), opacity = 1, 
            decreasing = T) %>%
  addSourceText("Source: NYC Parks. Only public pools under the jurisdiction of NYC Parks are included.")

mapview::mapshot(map, 
        file = file.path("visuals", "pool_count_by_council_district.pdf"),
        remove_controls = c("homeButton", "layersControl", "zoomControl"), 
        vwidth = 1000, vheight = 850)


# ------------------------------------------------------------------------------
# percent of population within a 15 minute walk of a pool
# ------------------------------------------------------------------------------

# load api key
#census_api_key(census_token, install = T)
options(tigris_use_cache = TRUE)
#readRenviron("~/.Renviron")

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

# for each block group get the population <15 min from a pool + the total pop
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


# formula to apply for each council district
get_overlap_pop = function(x, population, col="pop") {
  # where x is list of ids from pop
  overlap_pop = population[x, ]
  overlap_pop = sum(overlap_pop %>% pull(col))
  return(overlap_pop)
}

# get the list of blocks that each council district overlaps then apply the formula
#   to get the pop <15 min from pool + total council district pop
council_block_intersection = st_intersects(council_districts, pop)
council_districts$pop_near_pool = sapply(council_block_intersection,
                                         get_overlap_pop, 
                                         population=pop, col="pop_near_pool")
council_districts$pop = sapply(council_block_intersection, get_overlap_pop, 
                               population=pop, col="pop")
council_districts$perc_near_pool = council_districts$pop_near_pool/council_districts$pop


# prep for plotting
pal = colorBin(
  palette = colorRamp(rev(nycc_pal("cool")(12))),
  domain = c(0, 1),
  bins = c(0, .2, .4, .6, .8, 1),
  na.color = "transparent"
)

map = leaflet() %>% 
  councildown::addPolygons(data = council_districts, weight = 0, color = ~pal(perc_near_pool), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = TRUE, 
                  highlight_dists = council_districts$CounDist[council_districts$perc_near_pool >= .4]) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
            title = paste0("% of population that has access to <br>", 
                           "a pool within a 15 minute walk"),  
            values = c(0, 1), opacity = 1, decreasing = T, 
            labFormat = labelFormat(transform = function(x){x*100}, 
                                    suffix = "%"))%>%
  addSourceText("Sources: NYC Parks, Census, Mapbox. Only public pools under the jurisdiction of NYC Parks are included.")

mapview::mapshot(map, 
        file = file.path("visuals", "perc_pool_access_by_council_district.pdf"),
        remove_controls = c("homeButton", "layersControl", "zoomControl"), 
        vwidth = 1000, vheight = 850)


# ------------------------------------------------------------------------------
# council districts showing number of "no use" city owned land parcels
# ------------------------------------------------------------------------------

nouse_by_district = st_intersects(council_districts, no_use)
nouse_by_district = sapply(nouse_by_district, length)

council_districts$num_nouse = nouse_by_district 

# prep for plotting
pal = colorBin(
  palette = colorRamp(rev(nycc_pal("cool")(12))),
  domain = council_districts$num_nouse,
  bins = c(0, 10, 20, 50, 100, 450),
  na.color = "transparent"
)

map = leaflet() %>% 
  councildown::addPolygons(data = council_districts, weight = 0, 
                           color = ~pal(num_nouse)) %>% 
  addCouncilStyle(add_dists = TRUE, 
                  highlight_dists = council_districts$CounDist[council_districts$num_nouse >= 50]) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
            values = council_districts$capped_num_nouse,
            title = paste0("Number of 'no use' city owned <br>", 
                           "properties in district that are <br>", 
                           "in an Environmental Justice Area and <br>",
                           "a >15 minute walk from an existing pool"), 
            opacity = 1, decreasing = T) %>%
  addSourceText("Sources: NYC Parks, Mayor's Office of Climate and Sustainability, Department of City Planning")

mapview::mapshot(map, 
        file = file.path("visuals", "number_no_use_by_council_district.pdf"),
        remove_controls = c("homeButton", "layersControl", "zoomControl"), 
        vwidth = 1000, vheight = 850)


################################################################################
# create council district level tables
################################################################################

# ------------------------------------------------------------------------------
# figure out the location within that district with the max number ppl who woud 
#   newly gain access to a pool 

get_max_pop_impact = function(x, no_use) {
  # where x is list of ids from pop
  overlap_locs = no_use[x, ]
  max_impact = max(overlap_locs$new_users)
  return(max_impact)
}

council_nouse_intersection = st_intersects(council_districts, no_use)
council_districts$max_pop_impact = sapply(council_nouse_intersection,
                                         get_max_pop_impact, 
                                         no_use = no_use)
council_districts$max_pop_impact = ifelse(is.infinite(council_districts$max_pop_impact), 
                                          NA, council_districts$max_pop_impact)


council_districts %>%
  st_drop_geometry() %>%
  mutate(perc_near_pool = round(perc_near_pool*100, 0), 
         max_pop_impact = format(round(max_pop_impact, 0), big.mark=",")) %>%
  select(CounDist, num_pools, num_nouse, perc_near_pool, max_pop_impact) %>%
  arrange(CounDist)  %>%
  mutate(perc_near_pool = paste0(perc_near_pool, "%"), 
         max_pop_impact = gsub("NA", "", max_pop_impact)) %>%  
  rename(`Council District` = CounDist, 
         `# of Pools` = num_pools, 
         `# "No Use" City Owned Parcels` = num_nouse, 
         `% of Population <15 minute walk from a pool` = perc_near_pool, 
         `Population <15 minute walk from best located "No Use" parcel` = max_pop_impact) %>%
  gt() %>%
  tab_header(title = "Pool Access by Council District") %>%
  gt_theme_nytimes() %>%
  gtsave("visuals/info_table.pdf")
 
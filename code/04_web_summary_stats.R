source("code/00_load_dependencies.R")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/13/2023
#
# This file creates the summary statistics and tables for the webpage
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
# pools by borough bar chart
################################################################################

borough_count = pools %>% 
  st_drop_geometry() %>%
  group_by(borough) %>%
  summarise(count = n()) %>%
  mutate(borough = case_when(borough == "B" ~ "Brooklyn", 
                             borough == "M" ~ "Manhattan", 
                             borough == "Q" ~ "Queens", 
                             borough == "X" ~ "Bronx", 
                             borough == "R" ~ "Staten Island"))

col_chart = ggplot(borough_count) + 
  geom_col_interactive(aes(borough, count, tooltip = count), 
                       color = "#2F56A6", fill = "#2F56A6") + 
  theme_fivethirtyeight() + 
  theme(rect = element_rect(fill = "white", linetype = 0, colour = NA), 
        axis.title.y = element_text()) + 
  ylab("Number of Pools")

tooltip_css = "background-color:#CACACA;"

plot_interactive = girafe(ggobj = col_chart,   
                           width_svg = 9,
                           height_svg = 5, 
                           options = list(opts_tooltip(css = tooltip_css)))
save_html(plot_interactive, file.path("visuals", "borough_pool_count.html"))

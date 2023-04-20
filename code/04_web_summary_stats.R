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
# simple pool count
################################################################################

raw_pools_data = st_read("https://data.cityofnewyork.us/api/geospatial/y5rm-wagw?method=export&format=GeoJSON")
nrow(raw_pools_data)

################################################################################
# pop w/o access to pools
################################################################################

pop = readRDS(file.path("data", "output", "block_population.RDS")) %>%
  st_drop_geometry() 
perc_access = pop %>%
  summarise(perc_pop_no_pool = sum(pop)/sum(tot_pop))

perc_access_borough = pop %>%
  mutate(borough = str_extract(NAME, "[A-Za-z ]* County") %>% trimws(), 
         borough = gsub(" County", "", borough), 
         borough = gsub("Richmond", "Staten Island", borough)) %>%
  group_by(borough) %>%
  summarise(perc_pop_no_pool = sum(pop)/sum(tot_pop))

col_chart = ggplot(perc_access_borough) + 
  geom_col_interactive(aes(borough, perc_pop_no_pool*100, 
                           tooltip = paste0(perc_pop_no_pool*100, "%")), 
                       color = "#2F56A6", fill = "#2F56A6") + 
  theme_fivethirtyeight() + 
  theme(rect = element_rect(fill = "white", linetype = 0, colour = NA), 
        axis.title.y = element_text()) + 
  ylab("% of Borough without a pool <15 minute walk away")

tooltip_css = "background-color:#CACACA;"

plot_interactive = girafe(ggobj = col_chart,   
                          width_svg = 8,
                          height_svg = 5, 
                          options = list(opts_tooltip(css = tooltip_css)))
save_html(plot_interactive, file.path("visuals", "borough_pool_access.html"))


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
                           width_svg = 8,
                           height_svg = 5, 
                           options = list(opts_tooltip(css = tooltip_css)))
save_html(plot_interactive, file.path("visuals", "borough_pool_count.html"))

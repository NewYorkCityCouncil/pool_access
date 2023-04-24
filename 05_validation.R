source("code/00_load_dependencies.R")
source("../tokens.R")

###### Double check population estimates near [not near] a 15 min walk to a pool ----

pools_walk_zone_15min = readRDS(file.path("data", "output", "pools_walk_zone_15min.RDS"))

####### Results show that block & bbl population results are the same -------
# Using Block 2020 census values : 68%
# Using Tract 2021 ACS 5YR estimates : 70%
# Using derived BBL population estimates: 68%

##### set up - use your census api key
#load api key
#census_api_key(census_token, install = T)
#readRenviron("~/.Renviron")

##### TRACT ----
tract_pop <- get_acs(geography = "tract", 
                     state = 'NY',
                     county = c('Kings County', 'Queens County', 
                                'Richmond County', 'Bronx County',
                                'New York County'), 
                     variables = "B01003_001",
                     year = 2021,
                     key = Sys.getenv('census'),
                     survey = 'acs5',
                     geometry = T) %>%
  st_transform(st_crs(4326)) %>% 
  janitor::clean_names()

# figure out what blocks are within the pool isochrone
tract_intersection = st_intersection(tract_pop, 
                                     pools_walk_zone_15min) %>% 
  mutate(area_within_pool_zone = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  select(geoid, area_within_pool_zone)

# adjust the population to only include people not within a 15 minute walk of 
#   a pool already (assuming uniform population dist over each block)
pop = tract_pop %>%
  merge(tract_intersection, by = "geoid", all.x = T) %>%
  mutate(total_area = as.numeric(st_area(.)), 
         area_within_pool_zone = replace_na(area_within_pool_zone, 0), 
         perc_area = (total_area - area_within_pool_zone)/total_area,
         
         # get rid of tiny measurement errors like -0.000000001
         perc_area = ifelse(perc_area < 0, 0, perc_area), 
         
         # scale pop by (assumed) % not in zone
         pop = estimate * perc_area, 
         tot_pop = estimate) %>%
  select(-total_area, -area_within_pool_zone, -estimate)

# result
sum(pop$pop, na.rm = T)/sum(pop$tot_pop, na.rm = T)
##### BBL ----
# figure out what bbls are within the pool isochrones

# read in bbl file
bbl_pop <- read_csv(file.path("data", "input", 
                              "bbl_population_estimates.csv")) %>%
  janitor::clean_names() %>% 
  # convert to spatial points
  st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))

# leading zeros are needed to make bbl key column
bbl_pop <- bbl_pop %>% 
  mutate(boro_code = case_when(borough == "MN" ~ 1 ,
                               borough == "BX" ~ 2,
                               borough == "BK" ~ 3,
                               borough == "QN" ~ 4,
                               TRUE ~ 5),
    bbl = as.numeric(paste0(boro_code,
                 str_pad(block,5,pad="0"),
                 str_pad(lot, 4, pad = "0")))
  )


# filter down which bbl points fall into walk zone
bbl_intersects <- bbl_pop %>% 
  st_join(pools_walk_zone_15min %>% st_as_sf(), 
          st_intersects, left = F) 

# result
1 - sum(bbl_intersects$pop_estimate_pluto, na.rm = T)/sum(bbl_pop$pop_estimate_pluto, na.rm = T)




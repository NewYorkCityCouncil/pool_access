census_api_key(census_token, install = T)
readRenviron("~/.Renviron")


# pull population info at tract level for all relevant counties 
income = get_acs(geography = "tract", 
                 state = "NY",
                 county = c("061", "081", "085", "005", "047"),
                 
                 # total universe, # households in poverty, # total universe, # NH white alone
                 variables = c("B17001_001", "B17001_002", "B03002_001", "B03002_003"), 
                 year = 2018, 
                 geometry = T) %>%
  pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), 
              names_from = "variable", values_from = c("estimate", "moe")) %>%
  
  # error propogation: 
  #   https://chem.libretexts.org/Bookshelves/Analytical_Chemistry/Supplemental_Modules_(Analytical_Chemistry)/Quantifying_Nature/Significant_Digits/Propagation_of_Error
  #   https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf
  mutate(sd_B03002_001 = moe_B03002_001/1.645,
         sd_B03002_003 = moe_B03002_003/1.645,
         sd_B17001_002 = moe_B17001_002/1.645, 
         sd_B17001_001 = moe_B17001_001/1.645, 
         
         perc_below_poverty = estimate_B17001_002/estimate_B17001_001, 
         perc_minority = (estimate_B03002_001 - estimate_B03002_003)/estimate_B03002_001, 
         
         sd_below_poverty = sqrt((sd_B17001_002/estimate_B17001_002)^2 + (sd_B17001_001/estimate_B17001_001)^2) * perc_below_poverty, 
         sd_minority = sqrt((sd_B03002_003/estimate_B03002_003)^2 + (sd_B03002_001/estimate_B03002_001)^2) * perc_minority) %>%
  
  # set EJ areas
  mutate(ej_area = case_when(perc_below_poverty > 0.2359 ~ "EJ", 
                             perc_minority > 0.511 ~ "EJ", 
                             
                             perc_below_poverty + 1.645*sd_below_poverty > 0.2359 ~ "maybe EJ", 
                             perc_minority + 1.645*sd_minority > 0.511 ~ "maybe EJ", 
                             
                             T ~ "not EJ"))
 
# crop it to NYC boundaries just for easy visual parsing
nyc = st_read("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON") %>%
  st_transform(st_crs(income))

y = st_intersection(income, nyc)

ggplot() + geom_sf(data = y, aes(fill = ej_area)) +
  scale_fill_viridis(discrete = TRUE)


# plot the NYC reported version 

interest_area = st_read("https://data.cityofnewyork.us/api/geospatial/ykru-djh7?method=export&format=GeoJSON") %>%
  mutate(ej_area = factor(ejdesignat, levels = c("EJ Area", "Potential EJ Area", "Not EJ Area")))

ggplot() + geom_sf(data = interest_area, aes(fill = ej_area)) +
  scale_fill_viridis(discrete = TRUE)

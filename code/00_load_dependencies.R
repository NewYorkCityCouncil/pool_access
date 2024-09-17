## Load Libraries --------------------------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "mapboxapi", "tidycensus", "parallel", 
                      "leaflet", "leaflet.extras", "sf", "councildown", 
                      "rcartocolor", "RColorBrewer", "gtExtras", "gt", "webshot2", 
                      "leaflegend", "htmlwidgets", "ggiraph", "htmltools", 
                      "ggthemes", "openxlsx")

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)

# remove created variables for packages
rm(list.of.packages,new.packages)


## Functions -------------------------------------------------------------------

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

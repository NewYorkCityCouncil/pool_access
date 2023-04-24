## 	Public swimming pool locations and swimming lessons offered by the department of parks and recreation.

An associated webpage for this analysis can be found [on the council website](https://council.nyc.gov/data/pools/).


### Summary & Intention

This bill would require the Department of Parks and Recreation (DPR) to conduct a survey of sites owned by the City to identify suitable locations where additional public swimming pools could be built, with a focus on Environmental Justice (EJ) Communities and determine whether sites can accommodate more than one pool or other athletic equipment. It would also require that DPR consult with the Department of Education (DOE) on creating a plan to open pools under DOE jurisdiction for use by the public. Finally, it would require that DPR offer free swimming lessons at swimming pools under its jurisdiction.

Given we have data on the location of current pools, the 2018 definitions of EJ Communities, and the locations of all City Owned and Leased Property, we can find where gaps in pool coverage are and where might be relevant areas to focus on in this 


### Main Takeaways

* Pool access varies heavily by borough. The percentage of population within a 15 minute walk of a parks pool is 65% in Manhattan, 34% in Bronx, 31% in Brooklyn, 19% in Staten Island and 12% in Queens.
* 35% of council districts don't have a public Parks pool.
* Though this analysis doesn't consider if each of these locations are appropriate for a pool (ie what is already built there, the size of the plot, etc) there are many city owned properties that are currently classified as "no use" that are within a 15 minute walk of >100k residents not already served by a pool. 


### Scripts + Replication 

* **00_load_dependencies.R** - reads in the necessary dependencies and defines some data to be used throughout the code 
* **01_get_isochrones.R** - For every existing parks pool, generates the 15 minute walking isochrone (area that is within a 15 minute walk of the location). Combines this info with EJ Communities to find our areas of interest. Generates the 15 minute walking isochrones for all the "no use" properties of interest and overlaps with population data to find and save how many people are reached by each location. Most analysis is in this file, and all data is saved out to be mapped in further files. 
* **02_map.R** - Creates an interactive map featured on the pools website showing where existing pools are located, the EJ communities the bill is focused on, and the "no use" city owned land that falls within them. 
* **03_summary_stat_maps.R** - creates static maps for committee report including: number of pools by council district, % of population within a 15 minute walk of a pool by council district, and number of "no use" city owned land parcels by council district
* **04_web_summary_stats.R** - creates the numbers called out specifically on the web page including: number of pools, number of pools after grouping locations, % of the city without access to a pool within a 15 minute walk, and a pools by borough table. 

# This script prepares all input data for later use in accessibility 
# calculations.

## Grab and create spatial extents ----------------------------------------------

harris_blocks <- blocks(state = "TX", county = "Harris County", year = 2010) 
#mutate(geoid10 = substr(GEO_ID, 10, 20))

tx_counties <- counties(state = "TX", year = 2010)
harris_county <- tx_counties %>%
  st_transform("+init=epsg:3673") %>%
  filter(NAME10 == "Harris")

# Create hexagonal grid
harris_hex <- harris_county %>%
  st_transform("+init=epsg:3673") %>%
  st_make_grid(square = FALSE, n = c(160, 160)) %>%
  st_intersection(harris_county) %>%
  st_sf()

harris_hex$hexid <- as.numeric(row.names(harris_hex))

## Calculate origin and destination centroids ----------------------------------

# Identify hexagons that meet similar inclusion rules to FTA's 
# Simplified-Trips-on-Project Software (STOPS)

# Read GTFS feeds representing pre- and post-reimagining networks in Houston
metro_post <- 
  read_gtfs(here("data", "20150818_htx.zip"), local = TRUE, geometry = TRUE)

metro_pre <- 
  read_gtfs(here("data", "20150517_htx.zip"), local = TRUE, geometry = TRUE)

all_stops <- rbind(metro_post$.$stops_sf, metro_pre$.$stops_sf)
after_routes <- filter(metro_post$.$routes_sf, route_id %in% c(28363, 28364, 28365))
# before_routes <- filter(metro_pre$.$routes_sf, route_id %in% c(28363, 28364, 28365)) # Nothing

metro_buff <- all_stops %>%
  st_transform("+init=epsg:3673") %>% #http://spatialreference.org/ref/epsg/3673/
  st_buffer(1609.34) %>% # 1 mile in meters
  group_by() %>% # dissolve buffers
  summarize()

# Select Harris County hexagons that lie within a mile of METRO routes either
# before or after the system reimagining as origin and destination centroids
centroids <- harris_hex %>%
  st_centroid()

# Subset centroids within 1 mile buffer
centroids <- st_transform(
  centroids[st_within(centroids, metro_buff) %>% lengths > 0, ],
  "+init=epsg:4326")

centroids_latlong <- 
  data.frame(GEOID = centroids$hexid, st_coordinates(centroids))
# write.csv(centroids_latlong,
#  here("output", "harris_hex.csv"), row.names = FALSE)

# Project centroids for QA/QC
# centroids <- st_transform(centroids, "+init=epsg:4326") # WGS84
# ggplot(centroids) + geom_sf()

## Grab census data and associate it with the hexagonal cells ------------------

# v17 <- load_variables(2017, "acs5", cache = TRUE)

hlstatus <- c("B03002_003", # white alone
              "B03002_004", # black alone
              "B03002_006", # Asian
              "B03002_012") # Hispanic or Latino

hlrace <- st_transform(get_acs(geography = "block group", variables = hlstatus,
                summary_var = "B03002_001", state = "TX", 
                county = "Harris County", geometry = TRUE),
                "+init=epsg:3673")
hlrace$orig_area <- st_area(hlrace)

hlrace_hex <- st_intersection(hlrace, harris_hex)
hlrace_hex$area <- st_area(hlrace_hex)
hlrace_hex$prop <- hlrace_hex$area / hlrace_hex$orig_area

hex_demogs <- hlrace_hex %>%
  group_by(hexid, variable) %>%
  summarize(est = sum(estimate * prop),
            totpop = sum(summary_est * prop),
            share = est / totpop)

# Strip out the units on all counts
hex_demogs$est <- as.vector(hex_demogs$est)
hex_demogs$totpop <- as.vector(hex_demogs$totpop)
hex_demogs$share <- as.vector(hex_demogs$share)
hex_demogs$hexid <- as.numeric(hex_demogs$hexid) # Needed for later joins

# QA/QC
# Sum of both the census block group and hex population estimates 
# are about 4.4 million
sum(hlrace$summary_est)
sum(hex_demogs$totpop)

# Transit dependent population
trandep_vars <- c("B25046_001", # Aggregate vehicles available
                 "B26001_001", # Group quarters population  
                 "B26101_166", # Non-institutionalized group quarters pop
                 "B23025_001", # Population aged >= 16 in workforce or not
                 "B09001_008", # In households, 12-14 years old
                 "B12001_001") # Total pop. > 15 years old 

# Block group is too fine for these variables - many are missing at the scale
trandep <- st_transform(get_acs(geography = "tract", 
                variables = trandep_vars, state = "TX", 
                county = "Harris County", geometry = TRUE),
                "+init=epsg:3673")
trandep$orig_area <- st_area(trandep)

# Which variables are missing?
table(st_drop_geometry(trandep)[is.na(trandep$estimate), "variable"])
# Mostly B26101 - the non-institutionalized group quarters population, which 
# is super important for the calculation. Also there are 17 tracts with
# missing data on aggregate vehicles available. 
# for simplicity, we'll omit the GQ population. 

trandep_hex <- st_intersection(trandep, harris_hex)
trandep_hex$area <- st_area(trandep_hex)
trandep_hex$prop <- trandep_hex$area / trandep_hex$orig_area

hex_trandep <- trandep_hex %>%
  group_by(hexid, variable) %>%
  summarize(est = sum(estimate * prop))

# Strip out the units on all counts
hex_demogs$est <- as.vector(hex_demogs$est)
hex_demogs$totpop <- as.vector(hex_demogs$totpop)
hex_demogs$share <- as.vector(hex_demogs$share)
hex_demogs$hexid <- as.numeric(hex_demogs$hexid) # Needed for later joins

# Grab LEHD data and associate with hexagonal cells ----------------------------
tx_lodes <- grab_lodes("TX", 2015, "wac", "JT00", "S000", "block", "main")

# Add spatial information
harris_lodes <- 
  left_join(harris_blocks, tx_lodes, by = c("GEOID10" = "w_geocode")) %>%
  st_transform("+init=epsg:3673") %>%
  mutate(orig_area = st_area(.))

# Code blocks with no employment as zero for all categories we need
harris_lodes$C000[is.na(harris_lodes$C000)] <- 0

harris_lodes_hex <- st_intersection(harris_lodes, harris_hex)
harris_lodes_hex$area <- st_area(harris_lodes_hex)
harris_lodes_hex$prop <- harris_lodes_hex$area / harris_lodes_hex$orig_area
harris_lodes_hex$hexid <- as.numeric(harris_lodes_hex$hexid)

hex_lodes <- harris_lodes_hex %>%
  group_by(hexid) %>%
  summarize(totjobs = sum(C000 * prop))

# TODO: Add other job types

# Strip out the units on all counts
hex_lodes$totjobs <- as.vector(hex_lodes$totjobs)

# Ideas - look at a different opportunity type
# look at a different time period - add a column with the times 

## Travel time and access basic calculations------------------------------------
wmata_shapes <- st_read("data/Metro_Lines_Regional.geojson")

data_path <- "data/feeds_202211"

# Indicate the path where OSM and GTFS data are stored
r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

departure_datetime = as.POSIXct("11-09-2022 9:00:00", format = "%m-%d-%Y %H:%M:%S")
departure_datetime = as.POSIXct("11-09-2022 23:00:00", format = "%m-%d-%Y %H:%M:%S")

dc_bgs <- 
  block_groups(state = "DC", year = 2019)

dc_bgs_pt <- 
  block_groups(state = "DC", year = 2019) %>%
  st_centroid() %>%
  st_transform("+init=epsg:4326") %>%
  transmute(id = GEOID, lat = as.numeric(INTPTLAT), lon = as.numeric(INTPTLON), geometry = geometry)

ttm <- travel_time_matrix(
  r5r_core,
  time_window = 120,
  mode = mode,
  origins = dc_bgs_pt,
  destinations = dc_bgs_pt,
  departure_datetime = departure_datetime,
  max_walk_time = max_walk_time,
  max_trip_duration = max_trip_duration
)

dc_lodes <- 
  grab_lodes(
    state = c("dc"), year = 2019, lodes_type = "wac", 
    job_type = "JT00", segment = "S000", state_part = "main", 
    agg_geo = "bg")

dc_jobs_bg <- 
  left_join(dc_bgs_pt, dc_lodes, by = c("id" = "w_bg"))

dc_access <- 
  left_join(
    ttm, dc_lodes, by = c("to_id" = "w_bg")) %>%
  group_by(from_id) %>%
  filter(travel_time_p50 < 45) %>%
  summarize(acc45 = sum(C000))

dc_access <- 
  left_join(dc_bgs, dc_access, by = c("GEOID" = "from_id"))

ggplot() +
  geom_sf(data = dc_access, aes(col = acc45, fill = acc45)) + 
  geom_sf(data = wmata_shapes, col = "black") + 
  coord_sf(xlim = c(-77.12, -76.9), ylim = c(38.79, 39.0), expand = FALSE) +
  scale_color_viridis_c() + 
  scale_fill_viridis_c() + 
  ggthemes::theme_map() +
  guides(fill = guide_legend(title = "transit access to jobs\n(45 min.)"),
         color = guide_legend(title = "transit access to jobs\n(45 min.)"))

ttm_am$period <- "AM"
ttm_ev$period <- "EV"

ttm <- rbind(ttm_am, ttm_ev)

# Clean SNAP data 

# https://github.com/jshannon75/snap_retailers
snap <- read.csv("data/hist_snap_retailer_final2021.csv")
snap_dc <- 
  filter(snap, state == "DC") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

save(snap_dc, file = "data/SNAP_dc.Rdata")

ggplot() + 
  geom_sf(data = dc_bgs) + 
  geom_sf(data = snap_dc, col = "red") + 
  ggthemes::theme_map()


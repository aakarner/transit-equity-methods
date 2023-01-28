# Troubleshooting

any_acc <- 
  filter(before, accessibility > 9000 & percentile == 50) %>%
  mutate(id = as.numeric(id))

any_acc <- inner_join(dc_hex, any_acc, by = c("hexid" = "id"))

ggplot(any_acc, aes(color = accessibility, fill = accessibility,
                    label = hexid)) + 
  geom_sf() + 
  geom_sf_label(fill = "white",  # override the fill from aes()
                fun.geometry = sf::st_centroid) 
  

points <- hex_coords[ c(sample(1:nrow(hex_coords), 100, replace=FALSE)), ]

# Set parameters
mode <- c("WALK", "TRANSIT")
max_walk_time <- 60
max_trip_duration <- 180

departure_datetime = as.POSIXct("11-09-2022 9:27:00", format = "%m-%d-%Y %H:%M:%S")

dit <- detailed_itineraries(r5r_core = r5r_core,
                            origins = hex_coords[3404, ],
                            destinations = hex_coords[3536, ],
                            # destinations = hex_coords[3457, ],
                            mode = mode,
                            max_walk_time = max_walk_time,
                            max_trip_duration = max_trip_duration,
                            departure_datetime = departure_datetime,
                            shortest_path = FALSE,
                            verbose = TRUE)

head(dit)


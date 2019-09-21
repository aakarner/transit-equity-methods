

# Read travel time matrixes into memory

  # List all .csv outputs of OTP
    tt_before <- list.files(here("data"), full.names = TRUE)[grep("tt_before*", 
                                                 list.files(here("data")))]
    
    tt_after <- list.files(here("data"), full.names = TRUE)[grep("tt_after*", 
                                             list.files(here("data")))]

  # Read travel time matrixes
    skims_before <- lapply(X=tt_before, FUN=fread) %>% rbindlist()
    skims_after <- lapply(X=tt_after, FUN=fread) %>% rbindlist()

  # pile travel time matrices up
    skims_after$when <- 'after'
    skims_before$when <- 'before'
    ttmatrix <- rbindlist(list(skims_after, skims_before))
    head(ttmatrix)
    
    # clean memory
      rm(skims_after, skims_before)
      gc(reset = T)

      
# Read jobs data and drop geometry column
  hex_lodes <- readRDS('./data/hex_lodes.rds') %>% setDT()
  hex_lodes$geometry <- NULL 
  head(hex_lodes)
  
  
# Read census data and drop geometry column
  hex_trandep <- readRDS('./data/hex_trandep.rds') %>% setDT()
  hex_trandep$geometry <- NULL 
  head(hex_trandep)
  
  # Reshape data from long to wide format   
  hex_trandep_wide <- data.table::dcast(hex_trandep, hexid ~ variable, value.var = c("est"))
  head(hex_trandep_wide)
  
  

# Merge job count with OD Matrix, allocating job counts to Destination  
  
  # origin
    ttmatrix <- ttmatrix[hex_trandep_wide, on=c('origin'='hexid'), nomatch=0]
  # destination
    ttmatrix <- ttmatrix[hex_lodes, on=c('origin'='hexid'), nomatch=0]
  
  head(ttmatrix)
  gc(reset = T)
  
  
  # ttmatrix <- copy(skims_before)
    
  
  # Calculate accessibility for each OD pair at each time slice and when (before/after)
    access_intraday <- setDT(ttmatrix)[travel_time < 3600,
                                            .( acc30 = sum( totjobs[which( travel_time <= 30)], na.rm=T)
                                             , acc60 = sum( totjobs[which( travel_time <= 60)], na.rm=T)
                                             , acc90 = sum( totjobs[which( travel_time <= 90)], na.rm=T)),
                                         by=.(origin, depart_time, when) ]
    
  # Calculate Median accessibility before/after so that infrequent service doesn't appear attractive
    access <- access_intraday[, .( acc30 = median( acc30, na.rm=T)
                                 , acc60 = median( acc60, na.rm=T)
                                 , acc90 = median( acc90, na.rm=T)), 
                              by=.(origin, when) ]
    

    
# Combine before and after into a single dataframe for plotting [LONG Format]
  # add origin land use info
    output_long <- left_join(access, hex_trandep_wide, by=c('origin'='hexid'))
    # add geometry
    harris_hex <- readRDS("./data/harris_hex.rds") # Load hex grid
    output_long <- left_join(output_long, harris_hex, by=c('origin'='hexid')) %>% st_as_sf()
    head(output_long)
    

# Combine before and after [WIDE Format]
    # Reshape data from wide to long format   
    output_wide <- data.table::dcast(access, origin ~ when, value.var = c("acc30", "acc60", "acc90"))
    # add geometry
    output_wide <- left_join(output_wide, harris_hex, by=c('origin'='hexid')) %>% st_as_sf()
    head(output_wide)

    
# Save accessibility outputs    
    saveRDS(output_wide, './data/output_wide.rds', compress=T)
    saveRDS(output_long, './data/output_long.rds', compress=T)
    
    
    
    
    
    
    
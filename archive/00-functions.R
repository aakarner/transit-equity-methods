full_tt_matrix <- function(origin_list, destination_list, departures) {
  do.call(
    rbind,
    lapply(departure_datetimes, 
           function(x) {
             ttm_now <- 
               travel_time_matrix(
                 r5r_core,
                 origins = origin_list,
                 destinations = destination_list,
                 mode = mode,
                 departure_datetime = x,
                 max_walk_dist = max_walk_dist,
                 max_trip_duration = max_trip_duration,
                 verbose = FALSE)
             ttm_now$slice <- x
             
             return(ttm_now)
           })
  )
}


summarize_demogs <- function(bgs) {
  bgs %>%
  group_by(county) %>%
    summarize(total_voters = sum(cvap_est_total),
              total_nhl = sum(cvap_est_total_nhl),
              total_w = sum(cvap_est_White),
              total_b = sum(cvap_est_Black),
              total_a = sum(cvap_est_Asian),
              total_l = sum(cvap_est_Latinx))
}

cut_times <- function(times_to_cut) {
  the_cuts <- cut(times_to_cut, breaks = c(0, 10, 20, 30, 40, 50, 60, 120, 180))
}

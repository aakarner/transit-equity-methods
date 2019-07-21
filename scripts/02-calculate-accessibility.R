# Create a local in-memory Monet database to store CTPP data 
dbdir <- here("data", "accessdb")
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

tt_before <- 
  list.files(here("data"), full.names = TRUE)[grep("tt_before*", 
                                             list.files(here("data")))]

tt_after <- 
  list.files(here("data"), full.names = TRUE)[grep("tt_after*", 
                                             list.files(here("data")))]

# Read all tt matrices into a database table 
monet.read.csv(con, tt_before, "skims_before")
monet.read.csv(con, tt_after, "skims_after")

# TODO: Calculate accessibility for each OD pair at each time slice and 
# place it in a new table 

# Write jobs to database
dbWriteTable(con, "jobs", st_drop_geometry(hex_lodes))

# Create view that joins the jobs and skims for the two time periods
dbExecute(con,
  "CREATE VIEW time_jobs_before AS
  SELECT origin, destination, avg(totjobs) AS totjobs,
  avg(travel_time) AS avgtime
  FROM skims_before INNER JOIN jobs
  ON skims_before.destination = jobs.hexid
  GROUP BY origin, destination")

dbExecute(con,
  "CREATE VIEW time_jobs_after AS
  SELECT origin, destination, avg(totjobs) AS totjobs,
  avg(travel_time) AS avgtime
  FROM skims_after INNER JOIN jobs
  ON skims_after.destination = jobs.hexid
  GROUP BY origin, destination")

# Once this is ready, then just grab each of the threshold accessibilites 
acc60_before <- dbGetQuery(con, 
  "SELECT origin AS hexid, sum(totjobs) AS acc60
  FROM time_jobs_before
  WHERE avgtime < 3600
  GROUP BY origin")          

acc60_before_hex <- left_join(harris_hex, acc60_before)

acc60_after <- dbGetQuery(con, 
  "SELECT origin AS hexid, sum(totjobs) AS acc60 
  FROM time_jobs_after
  WHERE avgtime < 3600
  GROUP BY origin")          

acc60_after_hex <- left_join(harris_hex, acc60_after)

# Combine before and after into a single dataframe for plotting
acc60 <- rbind(mutate(acc60_before_hex, when = "before"),
               mutate(acc60_after_hex, when = "after"))

# Combine before and after into a single dataframe for comparisons
acc60_comp <- inner_join(rename(acc60_before, acc60_b = acc60),
                         rename(acc60_after, acc60_a = acc60))

dbDisconnect(con, shutdown = TRUE)

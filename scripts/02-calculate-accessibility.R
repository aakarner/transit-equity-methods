# Create a local in-memory Monet database to store CTPP data 
dbdir <- here("data", "accessdb")
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

tt_matrices <- list.files()[grep("traveltime_matrix*", list.files())]

# Read all tt matrices into a database table 
monet.read.csv(con, tt_matrices, "skims")

# Count number of records
dbGetQuery(con, "SELECT count(*) FROM skims")

# Calculate accessibility for each OD pair at each time slice and 
# place it in a new table 

# Query summary table of average travel times
mean_times <- dbGetQuery(
  con, "
  SELECT origin, destination, 
  avg(travel_time) AS average, median(travel_time) AS med
  FROM skims
  GROUP BY origin, destination")

ggplot(filter(mean_times, origin == 1), aes(x = destination, y = average)) + 
  geom_point()

# Write jobs to database
# dbWriteTable(con, "jobs", st_drop_geometry(hex_lodes))

# Create view that joins the jobs and skims and calculates accessibility
dbExecute(con,
  "CREATE VIEW accessibility45 AS
  SELECT origin, sum(totjobs) AS acc45
  FROM skims INNER JOIN jobs
  ON skims.destination = jobs.hexid
  WHERE travel_time < 3600
  GROuP BY origin")

dbExecute(con, "DROP VIEW accessibility45")

# QA/QC
dbGetQuery(con, "SELECT count(*) FROM accessibility45")
acc45 <- dbGetQuery(con, "SELECT * FROM accessibility45")
mean(acc45$acc45)

acc_demogs <- inner_join(filter(hex_demogs, variable == "B03002_003"),
                         acc45, by = c("hexid" = "origin"))
                                                                                                                                                                                                                                                                                                        
weighted.mean(acc_demogs$acc45, acc_demogs$totpop)

ggplot(filter(acc_demogs, variable == "B03002_003"), aes(fill = acc45, col = acc45)) + 
  geom_sf()

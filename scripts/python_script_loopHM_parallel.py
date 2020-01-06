# INPUT ###################################################################################################
###################################################################################################

# max number of threads to use in parallel
max_threads = 20

# Trips
fromm = 6             # set first departure time
until = 8             # set last time
every = 20             # set frequency (every 30 minutes)
time_threshold = 10800  # set a limit to maximum travel time (seconds)

# 1h = 3600 seconds , 1.5h = 5400 s, 2h = 7200 seconds

# set date of trips
year = 2015
month = 5
day = 12
mydate = 20150512

##year = 2015
##month = 8
##day = 27
##mydate = 20150827
##

# define router and time period
router_v = 'before'
period = 'am' 

###################################################################################################

import gc
gc.collect()

import random

# Start timing the code
import time
start_time = time.time()

# THREADED VERSION OF OTP SCRIPT
import threading
from time import sleep


#!/usr/bin/jython
from org.opentripplanner.scripting.api import OtpsEntryPoint


# Instantiate an OtpsEntryPoint
otp = OtpsEntryPoint.fromArgs(['--graphs', '.',
                               '--router', router_v])

# Get the default router
router = otp.getRouter(router_v)

# Read Points of Destination - The file points.csv contains the columns GEOID, X and Y.
# points = otp.loadCSVPopulation('harris_hex.csv', 'Y', 'X')
# dests = otp.loadCSVPopulation('harris_hex.csv', 'Y', 'X')

### make a list of jobs to do
# times should be randomly selected to avoid periodicity effects
jobs = []
random.seed(534)

for h in range(fromm, until):
  for m in range(0 ,60, every):
    jobs.append((h, int(round(m + random.uniform(0, every)))))
    
# define a function describing a complete job
# I just copy-pasted what you had in the loop into here
def skim_transit_trips(h,m):

  # Read Points of Destination - The file points.csv contains the columns GEOID, X and Y [inside]
  points = otp.loadCSVPopulation('harris_hex.csv', 'Y', 'X')
  dests = otp.loadCSVPopulation('harris_hex.csv', 'Y', 'X')

  # Create a default request for a given time
  req = otp.createRequest()
  req.setDateTime(year, month, day, h, m, 00)
  req.setMaxTimeSec(time_threshold) 
  req.setModes('WALK,TRANSIT,BUS,RAIL,SUBWAY,TRAM') # define transport mode : ("WALK,CAR, TRANSIT, TRAM,RAIL,SUBWAY,FUNICULAR,GONDOLA,CABLE_CAR,BUS")
  req.setClampInitialWait(0)
  # for more routing options, check: http://dev.opentripplanner.org/javadoc/0.19.0/org/opentripplanner/scripting/api/OtpsRoutingRequest.html

  # Create a CSV output
  matrixCsv = otp.createCSVOutput()
  matrixCsv.setHeader(['depart_time', 'origin', 'destination', 'walk_distance', 'travel_time', 'boardings'])
  
  # Start Loop
  for origin in points:
    print "Processing origin: ", str(h)+"-"+str(m)," ", origin.getStringData('GEOID'), 'on ', threading.current_thread()
    req.setOrigin(origin)
    spt = router.plan(req)
    if spt is None: continue

    # Evaluate the SPT for all points
    result = spt.eval(dests)

    # Add a new row of result in the CSV output
    for r in result:
      matrixCsv.addRow([ str(h) + ":" + str(m), origin.getStringData('GEOID'), r.getIndividual().getStringData('GEOID'), r.getWalkDistance(), r.getTime(), r.getBoardings()])

  # Save the result
  matrixCsv.save('tt_'+ str(router_v) + '_' + str(period) + '_' + str(h) + "-" + str(m) + '.csv')

#
# ^ that ^ function has to be defined before it's called
# the threading bit is down here vvv
#

# how many threads do you want?
#max_threads = int(raw_input('max threads (int) ? --> '))
# start looping over jobs
while len(jobs) > 0:
  if threading.active_count() < max_threads + 1:
    h,m = jobs.pop()
    thread = threading.Thread(target = skim_transit_trips, args = (h,m))
    # thread.daemon = True
    thread.start()
  else:
    sleep(0.1)
# now wait for all daemon threads to end before letting
# the main thread die. Otherwise stuff will get cut off
# before it's finished
while threading.active_count() > 1:
  sleep(0.1)
print 'ALL JOBS COMPLETED!'

print("Elapsed time was %g seconds" % (time.time() - start_time))



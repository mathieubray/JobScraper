library(taskscheduleR)
library(lubridate)
library(here)

# COMPUTER MUST BE PLUGGED IN TO RUN!!!

taskscheduler_create(taskname = "scrape_academic_jobs", 
                     rscript = here::here("Academic-Jobs-Update.R"),
                     schedule = "DAILY", 
                     startdate = format(today(), "%m/%d/%Y"),
                     starttime = format(now() + 62, "%H:%M"))

taskscheduler_delete(taskname = "scrape_academic_jobs")

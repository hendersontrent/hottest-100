#-------------------------------------------
# This script sets out to prep the data
# for analysis
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 17 September 2020
#-------------------------------------------

# Read in data

the_raw_data <- read_excel("data/full_data_set.xlsx") %>%
  mutate(artist = gsub("Â", "", artist)) %>%
  mutate(australian = as.factor(australian))

#------------------ CLEANING & PREP --------------------------------

# Calculate days between song release and vote opening

votes_open <- ymd("2019-12-16")
oldest_date <- ymd("2019-01-01")
timespan <- as.vector(votes_open - oldest_date)
timespan_bucket <- timespan/4

the_data <- the_raw_data %>%
  mutate(song_released = ymd(song_released)) %>%
  mutate(days_released = votes_open - song_released) %>%
  mutate(days_released = as.numeric(days_released)) %>%
  mutate(time_bucket = case_when(
    days_released <= timespan_bucket              ~ "Recency",
    days_released >= (timespan - timespan_bucket) ~ "Primacy",
    TRUE                                          ~ "Other")) %>%
  mutate(time_bucket = as.factor(time_bucket))

#------------------ CLEAN ENVIRONMENT ------------------------------

rm(the_raw_data)
rm(f)
rm(oldest_date)
rm(r_files)
rm(timespan)
rm(timespan_bucket)
rm(votes_open)

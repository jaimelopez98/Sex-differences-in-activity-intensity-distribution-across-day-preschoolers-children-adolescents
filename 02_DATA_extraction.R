# Script name: 02_DATA_extraction.R

# Author: J.Lopez, Inserm

# Doing: Using time-series of GGIR files:
#   * Restricted time window between 06:00 to 22:00, and removed time not considered "school period" in BFCS study.
#   * Removed time considered non-wear and as sleep, after used the combination of the two algorithms developt in GGIR software.
#   * Calculated the wear time of each participants and kept only those who wore the device at least 8h/day and 3 weekdays and 1 week-end day.
#   * Identify the season (only consider first day) when the device was worn.
#   * Create a time-series including only valid participants.
#   * Calculated the time spent in SB and each PA intensity using the cut-points approach (Romanzini's cutpoints).
#   * Create a data frame including time in SB and each PA intensity on weekdays and week-end days.

# PACKAGES ----

library(tidyr)
library(plyr)
library(dplyr)
library(haven)
library(stringr)
library(purrr)
library(vroom)
library(lubridate)
library(hms)

# 1) LOAD ALL THE FILES OF TIME-SERIES IN THE SAME OBJECT  -----

# Path of 15sec epoch files
path.files <- "/path/"

# Load raw data from participants with complete data 
acc_data <- data.frame(files = list.files(path = path.files,
                                                pattern = ".csv$",
                                                recursive = TRUE)) %>% 
  mutate(ID = as.numeric(str_extract(files, "[0-9]+"))) %>% 
  split(.$ID, drop = T) %>%
  map_dfr(~ {
    
    i_dat <- vroom(file = paste0(path.files, unique(.x$files)), 
                   delim = ",")
    
  }, .id = "ID") %>% 
  mutate(ID = as.integer(.$ID),
         timenum = as.POSIXct(timenum, origin="1970-1-1", tz="Europe/Paris"),
         timeserie = as.POSIXct(format(timenum, format = "%H:%M:%S"), format = "%H:%M:%S"), #create a new column with the same day for all (TIME-SERIES)
         weekday = wday(timenum), # extract the day ("Sunday" = 1, "Monday" = 2, ...) #extract the day of the week
         daytype = ifelse(wday(timenum) %in% 2:6, 0, 1), # extract the type of day (WD = 0  vs WE = 1) 
         calendar_date = as.Date(format(timenum, format = "%Y-%m-%d"),format = "%Y-%m-%d")) %>% 
  mutate(log_ACC = log(1+ACC))

acc_data <- acc_data %>% filter(timeserie >= "2024-07-12 06:00:00 CET" &  timeserie <= "2024-07-12 22:00:00 CET") # restricting the time window
  
acc_data2 <- acc_data %>%   filter(calendar_date < "2014-07-01" |  calendar_date > "2014-09-01") # removing summer holidays 

acc_data3 <- acc_data %>%   filter(calendar_date < "2014-07-01" |  calendar_date > "2014-09-01") %>% # removing all holidays
  filter(calendar_date < "2014-10-27" |  calendar_date > "2014-10-31") %>% # Toussaint
  filter(calendar_date != "2014-11-11") %>% # Armistice
  filter(calendar_date < "2014-12-22" |  calendar_date > "2015-01-02") %>% # Noel
  filter(calendar_date < "2015-02-16" |  calendar_date > "2015-02-20") %>% # Carnaval
  filter(calendar_date < "2015-04-06" |  calendar_date > "2014-04-17") %>% # Paques
  filter(calendar_date != "2015-05-01") %>% # Fete du Travail
  filter(calendar_date != "2015-05-14") %>% # Ascension
  filter(calendar_date != "2015-05-25") # Pentecote

save(acc_data, file = "/path/acc_data.rda")

# 2) REMOVE NON WEAR TIME AND SLEEP PERIOD TIME ----
acc_data_valid <- acc_data3 %>% # use acc_data, acc_data2 or acc_data3 depending the sample used it
  filter(SleepPeriodTime == 0,
         invalidepoch == 0) %>%  
  mutate(duration = 0.25) #it represents the time of each epoch by a number (15s = 0.25 min)

# 3) CALCULATE THE WAKING TIME OF EACH DAY BY PARTICIPANT ----

acc_ad_wak <- acc_data_valid %>%
  group_by(ID, calendar_date, daytype) %>%
  summarise(dur_day_min = sum(duration)) %>%
  ungroup()

# 4) CREATE TWO VARIABLES TO CONSIDER A DAY VALID OR NOT  ON WEEKDAYS  OR WEEK-END DAYS ----

acc_day <- acc_ad_wak %>%
  mutate(
    crit.wd = ifelse(daytype == 0 & dur_day_min >= 480, 1,0), # create a criteria for weekdays with more than 8h 
    crit.we = ifelse(daytype == 1 & dur_day_min >= 480, 1,0) # create a criteria for week-end day with more than 8h
) %>%
  filter(dur_day_min >= 480)

# 5) SUM THE NUMBER OF VALID DAY OF EACH PARTICIPANT AND SELECT ONLY THOSE MEETING THE STUDY PROTOCOL ----

acc_valid <- acc_day %>% 
  mutate(Nvalid = crit.wd + crit.we) %>%
  select(ID, Nvalid, crit.wd, crit.we) %>% 
  aggregate(. ~ ID, data = ., FUN = sum)%>%
  filter(crit.wd > 2 & crit.we > 0) # this criteria can change (we used at least 3WD and 1WE)

save(acc_valid, file = "/path/acc_valid.rda")

# 6) CREATE A DATAFRAME INCLUDING ONLY PARTICPANTS CONSIDERED VALIDS -----

acc_day_valid <- acc_day %>% filter(ID %in% acc_valid$ID & dur_day_min >= 480)

save(acc_day_valid, file = "/path/acc_day_valid.rda")

# 7) KEPT IN THE TIME-SERIES ONLY PARTICPANTS WITH VALID DAYS ----
acc_data_valid2 <- acc_data_valid %>% 
  merge(., acc_day_valid, by = c("ID", "calendar_date", "daytype"))

save(acc_data_valid2, file = "/path/acc_data_valid.rda")

# 8) CREATE A VARIABLE FOR SEASON (CONSIDERING ONLY THE FIRST DAY) ----
season <- acc_day_valid %>% select(ID, calendar_date) %>%
  aggregate(calendar_date ~ ID, data = ., FUN = function(x) x[1]) %>%
  mutate(season =  ifelse((month(calendar_date) == 3 & day(calendar_date) >= 20) | month(calendar_date) %in% c(4, 5) | (month(calendar_date) == 6 & day(calendar_date) < 21), 1,
                          ifelse((month(calendar_date) == 6 & day(calendar_date) >= 21) | month(calendar_date) %in% c(7, 8) | (month(calendar_date) == 9 & day(calendar_date) < 23), 2,
                                 ifelse((month(calendar_date) == 9 & day(calendar_date) >= 23) | month(calendar_date) %in% c(10, 11) | (month(calendar_date) == 12 & day(calendar_date) < 21), 3, 0)))) %>%
  select(-calendar_date)


# 9) CALCULATE THE TIME SPENT IN SB AND EACH PA INTENSITY BY DAY ----

# WAKING TIME 
acc_wak <- acc_data_valid2 %>% group_by(ID, calendar_date, daytype) %>%
  summarise(dur_day_min = sum(duration))

# SEDENTARY TIME (<181 counts/15s)
acc_SB <- acc_data_valid2 %>% 
  filter (ACC < 181) %>%
  group_by(ID, calendar_date, daytype) %>%
  summarise(dur_day_SB_min = sum(duration))

# LIGHT PA TIME (181-756counts/15s)
acc_LIG <- acc_data_valid2 %>% 
  filter (ACC >= 181 & ACC < 757) %>%
  group_by(ID, calendar_date, daytype) %>%
  summarise(dur_day_LIG_min = sum(duration))

# MODERATE PA TIME (757-1111 counts/15s)
acc_MOD <- acc_data_valid2 %>% 
  filter (ACC >= 757 & ACC < 1112) %>%
  group_by(ID, calendar_date, daytype) %>%
  summarise(dur_day_MOD_min = sum(duration))

# VIGOROUS PA TIME (1112 counts/15s)
acc_VIG <- acc_data_valid2 %>% 
  filter (ACC >= 1112) %>%
  group_by(ID, calendar_date, daytype) %>%
  summarise(dur_day_VIG_min = sum(duration))

# MERGE ALL THE TIMES IN A SAME OBJECT (part5_daysummary)
acc_day_sum <- merge (acc_wak, acc_SB, by = c("ID","calendar_date", "daytype")) %>%
  merge (., acc_LIG, by = c("ID","calendar_date", "daytype")) %>%
  merge (., acc_MOD, by = c("ID","calendar_date", "daytype")) %>%
  merge (., acc_VIG, by = c("ID","calendar_date", "daytype"))

write.csv(acc_day_sum, file = "/path/part5_daysummary.csv", row.names = FALSE)

# 10) CALCULATE THE MEAN OF TIME IN SB AND EACH PA INTENSITY BY PARTICIPANT -----

# ALL DAYS
acc_sum_ad <- acc_day_sum %>%
  group_by(ID) %>%
  summarise(
    dur_min_pla = mean (dur_day_min),
    dur_SB_min_pla = mean (dur_day_SB_min), 
    dur_LIG_min_pla = mean (dur_day_LIG_min),
    dur_MOD_min_pla = mean (dur_day_MOD_min),
    dur_VIG_min_pla = mean(dur_day_VIG_min))
    
# WEEKDAYS
acc_day_wd_sum <- acc_day_sum %>% filter(daytype == 0)

acc_sum_Wd <- acc_day_wd_sum %>%
  group_by(ID) %>%
  summarise(
    dur_min_WD = mean (dur_day_min),
    dur_SB_min_WD = mean (dur_day_SB_min), 
    dur_LIG_min_WD = mean (dur_day_LIG_min),
    dur_MOD_min_WD = mean (dur_day_MOD_min),
    dur_VIG_min_WD = mean(dur_day_VIG_min))

#WEEKENDS
acc_day_we_sum <- acc_day_sum %>% filter(daytype == 1)

acc_sum_We <- acc_day_we_sum %>%
  group_by(ID) %>%
  summarise(
    dur_min_WE = mean (dur_day_min),
    dur_SB_min_WE = mean (dur_day_SB_min), 
    dur_LIG_min_WE = mean (dur_day_LIG_min),
    dur_MOD_min_WE = mean (dur_day_MOD_min),
    dur_VIG_min_WE = mean(dur_day_VIG_min))

# 11) CREATE A DATAFRAME INCLUDING TIME SPENT IN SB AND EACH PA INTENSITY AND SEASON FOR ALL DAYS? WEEKDAYS AND WEEK-END DAYS ----
acc_def <- merge(acc_sum_Wd,acc_sum_We, by = "ID") %>% 
  merge(acc_sum_ad,., by = "ID") %>%
  merge(season, ., by = "ID")

write.csv(acc_def, file = "/path/part5_personsummary.csv",row.names = FALSE)

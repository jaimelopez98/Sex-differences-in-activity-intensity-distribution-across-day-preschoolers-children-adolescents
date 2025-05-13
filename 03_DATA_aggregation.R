# Script name: "03_DATA_aggregation.R"

# Author: J.López, Inserm

# Doing: 

# Aggregation of time-series in 15s epoch to one-minute epoch

# PACKAGES --------------------------------------------------------------------

library(tidyr)
library(plyr)
library(dplyr)
library(haven)
library(stringr)
library(purrr)
library(vroom)
library(ks)

# 0) LOAD DEMOGRAPHIC AND ACCELEROMETER DATA --------------------------------------------------------------------------------

labda_data <- read.csv("/path/labda_data.csv") %>% select(ID)

load("/path/acc_data.rda")
load("/path/acc_wak_ACT.rda")

# 1) REMOVE NON WEAR TIME ---- 

acc_data_valid <- acc_data %>%
  filter(calendar_date %in% acc_wak$calendar_date) # include only valid days

# 2) CALCULATE TIME SERIES BY MINUTE ----

interval <- seq.POSIXt(min(acc_data_valid$timeserie), max(acc_data_valid$timeserie), by = "1 min") # aggregate by minute
interval <- interval [-961]

timeday <- paste0("y_", rep(seq(6, 21), each = 60), "_", rep(seq(0,59), each = 1))

ts_data_valid <- acc_data_valid %>% 
  select(ID, ACC, timenum,timeserie,daytype,weekday) %>%
  mutate(interval = cut(timeserie, breaks = interval))

# WEEKDAYS
ts_wd <- ts_data_valid %>% filter(daytype == 0) %>% select(ID,interval,ACC) %>% group_by(ID,interval) %>%
  summarise(counts = mean(ACC))

ts_wd2 <- spread(ts_wd, key = interval, value = counts)
ts_wd2[is.na(ts_wd2)] <- 0 
names(ts_wd2) <- c("ID",timeday)

ts_wd2 <- semi_join(ts_wd2,labda_data)

save(ts_wd2, file = "/path/WD_ts.rda")

# WEEKE-END DAYS
ts_we <- ts_data_valid %>% filter(daytype == 1) %>% select(ID,interval,ACC) %>% group_by(ID,interval) %>%
  summarise(counts = mean(ACC))

ts_we2 <- spread(ts_we, key = interval, value = counts)
ts_we2[is.na(ts_we2)] <- 0 
names(ts_we2) <- c("ID",timeday)

ts_we2 <- semi_join(ts_we2,labda_data)

save(ts_we2, file = "/path/WE_ts.rda")


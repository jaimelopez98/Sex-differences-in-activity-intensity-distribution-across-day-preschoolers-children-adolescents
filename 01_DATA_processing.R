# Script name: 01_DATA_processing.R

# Author: J.Lopez, Inserm

# Doing: Processing accelerometer files using GGIR package:
#   * Using 15s epoch and 20min of consecutives zero to considered as non-wearRestricted time window between 06:00 to 22:00, and removed time not considered "school period" in BFCS study.
#   * Combining two algorithm to de detect sleep windowRemoved time considered non-wear and as sleep, after used the combination of the two algorithms developt in GGIR software.
#   * Not including imputation data

# PACKAGES ----

library(remotes)
library(GGIR)
library(actilifecounts)

remotes::install_github("wadpac/GGIR")

# 1) GGIR CODE ----

    GGIR(datadir = "C:/Users/jlopezgarcia/Desktop/paper2/analysis/studies/BFCS/data/",
     outputdir = "C:/Users/jlopezgarcia/Desktop/paper2/analysis/studies/BFCS/ggir/",
     mode = 1:5,
     windowsizes =  c(15, 300, 1200), # 15s epoch and 20min non-wear
     dataFormat = "actigraph_csv",
     extEpochData_timeformat = "%m/%d/%Y %H:%M:%S",
     acc.metric = "NeishabouriCount_y",
     overwrite = TRUE,
     idloc = 2, # extract ID
    
     # sleep window detection (2 algorithm)
     HASPT.algo = "NotWorn", 
     HASIB.algo = "Sadeh1994", # use this algorithm to remove the night in this devices wearing all the day
     
     do.imp = FALSE, # not include imputation
     visualreport = FALSE,
     outliers.only = FALSE,
     save_ms5rawlevels = TRUE,
     ignorenonwear = FALSE,
     HASPT.ignore.invalid = FALSE,
     save_ms5raw_without_invalid = FALSE,
     do.report = c(2,4,5))


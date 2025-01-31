# Script name: 11_ACT_kernel.R

# Author: J.López, Inserm

# Doing: 
#   *Estimating the probabiblity denstity function for each participant, on weekdays and week-end days

# PACKAGES ----
library(tidyr)
library(plyr)
library(dplyr)
library(haven)
library(stringr)
library(purrr)
library(vroom)
library(ks)
library(pscl)
library(lubridate)
library(hms)
                            
# 0) LOAD TIME-SERIES AND DEMOGRAPHIC CHARACTERISTICS ----

load("//path/acc_data_valid.rda")
labda_data <- read.csv("/path/labda_data.csv")

# HISTOGRAM ----

hist(acc_data_valid2$ACC, breaks = 200, xlim = c(0,3000))

# TYPE OF DAY ----                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             # SPLIT DATA BY TYPE OF DAY AND CALCULATE EXTREME VALUES ----
acc_data_valid_wd <- acc_data_valid2 %>%
  filter(daytype == 0)
acc_data_valid_we <- acc_data_valid2 %>%
  filter(daytype == 1)

# IDENTIFY CRITICAL VALUES OF THE DISTRIBUTION ----

c_value_wd <- quantile(acc_data_valid_wd$ACC, 0.995) # c_value is a critical value to remove extreme values of the distribution
c_value_we <- quantile(acc_data_valid_we$ACC, 0.995) # c_value is a critical value to remove extreme values of the distribution

save(c_value_wd, file ="/path/WD_c_value.rda")
save(c_value_we, file ="/path/WE_c_value.rda") 

# 1) WEEKDAYS ----

# Coordinates where densities will be estimated (Min and max)

min.pts_wd <- 0
max.pts_wd <- c_value_wd
pts_wd <- seq(min.pts_wd, max.pts_wd, length.out = 200) #equally distributed with 200 points 

plot(x = pts_wd, y = rep(0, 200), type = "p", pch= "l")

# Estimate median bandwidth
h.med_wd <- acc_data_valid_wd %>% 
  split(.$ID) %>%
  # Extract data from the large dataset
  map_dbl(~ {
    
    acc <- .x$ACC
    
    # Density function estimation for the participant
    fit <- kde(acc,
               eval.points = pts_wd, # at given points
               density = TRUE) # only positive data
    
    # Get bandwidth value
    fit$h
    
  }) %>%
  # Median bandwidth
  median(.)

# Estimate individual density function with a commun bandwidth
dst_wd <- acc_data_valid_wd %>% 
  # Keep only acceleration during waking time and valid days (needs to be implemented)
  
  split(.$ID) %>% 
  map_dfr(~ {
    
    acc <- .x$ACC
    
    # Density function estimation for the participant
    fit <- kde(acc, 
               eval.points = pts_wd, # at given coordinates
               h = h.med_wd, # with a given bandwidth 
               density = TRUE) # only positive data
    
    # > Continuous density function for positive values
    data.frame(x = fit$eval.points, # coordinates at which the function was estimated
               f_i = fit$estimate) # estimated function
    
  }, .id = "ID") %>%
  mutate(ID = as.numeric(.$ID))

any(dst_wd$f_i < 0) # Check if there is no negative values

save(dst_wd, file = "/path/WD_dst.rda") # Save it for next code (12_ACT_standard)

# WEEK-END DAYS --------------------------------------------------------------------

# Coordinates where densities will be estimated (Min and max)
min.pts_we <- 0
max.pts_we <- c_value_we
pts_we <- seq(min.pts_we, max.pts_we, length.out = 200) #equally distributed with 200 points 

plot(x = pts_we, y = rep(0, 200), type = "p", pch= "l")

# Estimate median bandwidth
h.med_we <- acc_data_valid_we %>% 
  split(.$ID) %>%
  # > Extract data from the large dataset
  map_dbl(~ {
    
    acc <- .x$ACC
    
    # > Density function estimation for the participant
    fit <- kde(acc,
               eval.points = pts_we, # at given points
               density = TRUE) # only positive data
    
    # Get bandwidth value
    fit$h
    
  }) %>%
  # Median bandwidth
  median(.)

# Estimate individual density function with a commun bandwidth
dst_we <- acc_data_valid_we %>% 
  
  split(.$ID) %>% 
  map_dfr(~ {
    
    acc <- .x$ACC
    
    # Density function estimation for the  participant
    fit <- kde(acc, 
               eval.points = pts_we, # at given coordinates
               h = h.med_we, # with a given bandwidth 
               density = TRUE) # only positive data
    
    # Continuous density function for positive values
    data.frame(x = fit$eval.points, # coordinates at which the function was estimated
               f_i = fit$estimate) # estimated function
    
  }, .id = "ID") %>%
  mutate(ID = as.numeric(.$ID))

any (dst_we$f_i < 0) # Check if there is no negative values

save(dst_we, file = "/path/WE_dst.rda") # Save it for next code (12_ACT_standard)

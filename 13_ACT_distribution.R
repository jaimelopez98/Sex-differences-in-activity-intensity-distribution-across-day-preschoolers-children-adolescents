# Script name: 13_ACT_distribution.R

# Author: J.López, Inserm

# Doing: 
#     *Computing activity distribution from mean daily waking time and density functions on weekdays and week-end days

# PACKAGES ----
library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(haven)
library(ggplot2)

# 0) LOAD  PA AND DEMOGRAPHIC DATA  ----
load("/path/WD_dst_trap.rda")
load("/path/WE_dst_trap.rda")

labda_data <- read.csv("/path/labda_data.csv")

# 1) WEEKDAYS ----
dst_2_wd <- dst_2_wd %>%
  select(-f_i, -surf) 

act_wd <- dst_2_wd %>% 
  mutate(ID = as.numeric(as.character(ID))) %>%
  merge(labda_data, by = "ID") %>% 
  mutate(dur_MVPA_min_WD = dur_MOD_min_WD + dur_VIG_min_WD, 
         A_i = f_i_2*dur_min_WD)%>%
  select(-f_i_2)
  
save(act_wd, file = "/path/WD_act.rda")

# 2) WEEK-END DAYS ----
dst_2_we <- dst_2_we %>%
  select(-f_i, -surf) 

act_we <- dst_2_we %>% 
  mutate(ID = as.numeric(as.character(ID))) %>%
  merge(labda_data, by = "ID") %>% 
  mutate(dur_MVPA_min_WE = dur_MOD_min_WE + dur_VIG_min_WE, 
         A_i = f_i_2*dur_min_WE)%>%
  select(-f_i_2)

save(act_we, file = "/path/WE_act.rda")
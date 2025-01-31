# Script name: 12_ACT_standard.R

# Author: J.López, Inserm

# Doing: 
#     *Standardize the probabiblity denstifunction of physical activity to reduce error of PDF estimation (dividing PDF
#       by total surface under the PDF curve estimated by trapeze methods) on weekdays and week-end days

# PACKAGES ---------------------------------------------------------------------

# Tools
library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(pracma)
library(ggplot2)

# 0) LOAD THE DATA (obtained in 11_ACT_kernel) ----

load("/path/WD_dst.rda")
load("/path/WE_dst.rda")

# WEEKDAYS ----
#Compute total area under the curve 
area_dst_wd <- dst_wd %>% 
  group_by(ID) %>% 
  summarise(surf = trapz(x, f_i))

summary(area_dst_wd$surf) # > Some are not equal to 1

# Standardize density function by total area under the curve
dst_2_wd <- dst_wd %>% 
  left_join(area_dst_wd, by = "ID") %>% 
  mutate(f_i_2 = f_i/surf)

# Recompute total area 
area_dst_2_wd <- dst_2_wd %>% 
  group_by(ID) %>% 
  summarise(surf = trapz(x, f_i_2))

# Check if new total area = 1
summary(area_dst_2_wd$surf)

# Save
save(dst_2_wd, file = "/path/WD_dst_trap.rda") # Save it for next code (13_ACT_distribution)

# WEEK-END DAYS  ----

#Compute total area under the curve 
area_dst_we <- dst_we %>% 
  group_by(ID) %>% 
  summarise(surf = trapz(x, f_i))

summary(area_dst_we$surf) # > Some are not equal to 1

# Standardize density function by total area under the curve
dst_2_we <- dst_we %>% 
  left_join(area_dst_we, by = "ID") %>% 
  mutate(f_i_2 = f_i/surf)

# Recompute total area 
area_dst_2_we <- dst_2_we %>% 
  group_by(ID) %>% 
  summarise(surf = trapz(x, f_i_2))

# Check if new total area = 1
summary(area_dst_2_we$surf)

# Save
save(dst_2_we, file = "/path/WE_dst_trap.rda")  # Save it for next code (13_ACT_distribution)


# Script name: 16_ACT_predictions.R
# 
# Author: J.López, Inserm
#
# Doing: Predictions from models obtained using function-on scalar regression over the full range of the activity intensity distribution on weekdays and week-end days and for:
#     *Pre-schoolers (n=1057)
#     *Children (n=1153)
#     *Adolescents (n=297)

#PACKAGES ----

library(dplyr)
library(tidyr)
library(tidyfun)
library(ggplot2)
library(purrr)
library(refund)

# MODELS ----
#WEEKDAYS
load("/path/fm_wd_pre.rda") # PRESCHOOLERS
load("/path/fm_wd_chi.rda") # CHILDREN
load("/path/fm_wd_ado.rda") # ADOLESCENTS

# WEEK-END DAYS
load("/path/fm_we_pre.rda") # PRESCHOOLERS
load("/path/fm_we_chi.rda") # CHILDREN
load("/path/fm_we_ado.rda") # ADOLESCENTS

load("/path/WD_c_value.rda") # WEEKDAYS
xi_wd <- seq(0,c_value_wd,length.out = 200)

load("/path/WE_c_value.rda") # WEEKENDS
xi_we <- seq(0,c_value_we,length.out = 200)

# 1) PRE-SCHOOLERS (n=1057) ----
# WEEKDAYS
pred_obj_wd_pre =
  tibble(xi = xi_wd,
         ID = 1,
         O_waking_time_WD = 1,
         AGE=1,
         SEX=1,
         edu_cat1 = 1, edu_cat2 = 1,
         study1 = 1,
         season1 = 1, season2 = 1, season3 = 1) %>%
  predict(fm_wd_pre, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_wd_pre, file = "/path/pred_obj_wd_pre.rda")

# WEEK-END DAYS
pred_obj_we_pre =
  tibble(xi = xi_we,
         ID = 1,
         O_waking_time_WE = 1,
         AGE=1,
         SEX=1,
         edu_cat1 = 1, edu_cat2 = 1,
         study1 = 1,
         season1 = 1, season2 = 1, season3 = 1) %>%
  predict(fm_we_pre, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_we_pre, file = "/path/pred_obj_we_pre.rda")

# 2) CHILDREN (n=1153) ----
# WEEKDAYS
pred_obj_wd_chi =
  tibble(xi = xi_wd,
         ID = 1,
         O_waking_time_WD = 1,
         AGE=1,
         SEX=1,
         edu_cat1 = 1, edu_cat2 = 1,
         study1 = 1,
         season1 = 1, season2 = 1, season3 = 1) %>%
  predict(fm_wd_chi, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_wd_chi, file = "/path/pred_obj_wd_chi.rda")

# WEEK-END DAYS
pred_obj_we_chi =
  tibble(xi = xi_we,
         ID = 1,
         O_waking_time_WE = 1,
         AGE=1,
         SEX=1,
         edu_cat1 = 1, edu_cat2 = 1,
         study1 = 1,
         season1 = 1, season2 = 1, season3 = 1) %>%
  predict(fm_we_chi, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_we_chi, file = "/path/pred_obj_we_chi.rda")

# 3) ADOLESCENTS (n=297) ----
# WEEKDAYS
pred_obj_wd_ado =
  tibble(xi = xi_wd,
         ID = 1,
         O_waking_time_WD = 1,
         AGE=1,
         SEX=1,
         edu_cat1 = 1, edu_cat2 = 1,
         study2 = 1,
         season1 = 1, season2 = 1, season3 = 1) %>%
  predict(fm_wd_ado, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_wd_ado, file = "/path/pred_obj_wd_ado.rda")

# WEEK-END DAYS
pred_obj_we_ado =
  tibble(xi = xi_we,
         ID = 1,
         O_waking_time_WE = 1,
         AGE=1,
         SEX=1,
         edu_cat1 = 1, edu_cat2 = 1,
         study2 = 1,
         season1 = 1, season2 = 1, season3 = 1) %>%
  predict(fm_we_ado, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_we_ado, file = "/path/pred_obj_we_ado.rda")


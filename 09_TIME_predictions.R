# Script name: 09_TIME_predictions.R
# 
# Author: J.López, Inserm
#
# Doing: Predictions from models obtained using function-on scalar regression over the time of the day on weekdays and week-end days and for:
#     *Pre-schoolers (n=1057)
#     *Children (n=1153)
#     *Adolescents (n=297)

# PACKAGES --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidyfun)
library(ggplot2)
library(purrr)
library(refund)

# 0) LOAD MODELS ----
#WEEKDAYS
load("/path/fm_wd_pre.rda") # PRESCHOOLERS
load("/path/fm_wd_chi.rda") # CHILDREN
load("/path/fm_wd_ado.rda") # ADOLESCENTS

# WEEK-END DAYS
load("/path/fm_we_pre.rda") # PRESCHOOLERS
load("/path/fm_we_chi.rda") # CHILDREN
load("/path/fm_we_ado.rda") # ADOLESCENTS

xi <- seq(6,22, length=960)

# 1) PRE-SCHOOLERS (n=1057) ----
# WEEKDAYS
pred_obj_wd_pre =
  tibble(xi = xi,
         ID = 1,
         O_waking_time_WD = 1,
         SEX=1,
         AGE = 1,
         edu_cat1 = 1, edu_cat2 = 1,
         season1 = 1, season2 = 1, season3 = 1,
         study1 = 1) %>%
  predict(fm_wd_pre, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_wd_pre, file = "/path/pred_obj_wd_pre.rda")

# WEEK-END DAYS
pred_obj_we_pre =
  tibble(xi = xi,
         ID = 1,
         O_waking_time_WE = 1,
         SEX=1,
         AGE = 1,
         edu_cat1 = 1, edu_cat2 = 1,
         season1 = 1, season2 = 1, season3 = 1,
         study1 = 1) %>%
  predict(fm_we_pre, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_we_pre, file = "/path/pred_obj_we_pre.rda")

# 2) CHILDREN (n=1153) ----
# WEEKDAYS
pred_obj_wd_chi =
  tibble(xi = xi,
         ID = 1,
         O_waking_time_WD = 1,
         SEX=1,
         AGE = 1,
         edu_cat1 = 1, edu_cat2 = 1,
         season1 = 1, season2 = 1, season3 = 1,
         study1 = 1) %>%
  predict(fm_wd_chi, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_wd_chi, file = "/path/pred_obj_wd_chi.rda")

# WEEK-END DAYS
pred_obj_we_chi =
  tibble(xi = xi,
         ID = 1,
         O_waking_time_WE = 1,
         SEX= 1,
         AGE = 1,
         edu_cat1 = 1, edu_cat2 = 1,
         season1 = 1, season2 = 1, season3 = 1,
         study1 = 1) %>%
  predict(fm_we_chi, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_we_chi, file = "/path/pred_obj_we_chi.rda")

# 3) ADOLESCENTS (n=297) ----
# WEEKDAYS
pred_obj_wd_ado =
  tibble(xi = xi,
         ID = 1,
         O_waking_time_WD = 1,
         SEX=1,
         AGE = 1,
         edu_cat1 = 1, edu_cat2 = 1,
         season1 = 1, season2 = 1, season3 = 1,
         study2 = 1) %>%
  predict(fm_wd_ado, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_wd_ado, file = "/path/pred_obj_wd_ado.rda")

# WEEK-END DAYS
pred_obj_we_ado =
  tibble(xi = xi,
         ID = 1,
         O_waking_time_WE = 1,
         SEX=1,
         AGE = 1,
         edu_cat1 = 1, edu_cat2 = 1,
         season1 = 1, season2 = 1, season3 = 1,
         study2 = 1) %>%
  predict(fm_we_ado, newdata=.,type= "terms", se.fit= TRUE)

save(pred_obj_we_ado, file = "/path/pred_obj_we_ado.rda")

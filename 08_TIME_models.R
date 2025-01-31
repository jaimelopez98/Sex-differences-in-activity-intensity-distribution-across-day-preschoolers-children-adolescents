# Script name: 08_TIME_models.R

# Author: J.López, Inserm

# Doing:  
#   *Modelling using the distribution of counts per minutes over time of the day during waking hours as dependent term in form of function
#   and sex and other covariates as independent variables (function-on-scalar regression) on weekdays and week-end days and for:
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

# 0) LOAD DATA ----

# WEEKDAYS
load("/path/WD_ts_pre.rda")
load("/path/WD_ts_chi.rda")
load("/path/WD_ts_ado.rda")

# WEEK-END DAYS

load("/path/WE_ts_pre.rda")
load("/path/WE_ts_chi.rda")
load("/path/WE_ts_ado.rda")

xi <- seq(6,22, length=960)

# 1) PRE-SCHOOLERS (n=1057)
fm_wd_pre <- pffr(Y_Activity ~ O_waking_time_WD + AGE + SEX + edu_cat1 + edu_cat2 + season1 + season2 + season3 + study1 + s(ID, bs= "re"), 
                yind = xi,
                bs.yindex = list(bs = "ps", k=-1), 
                bs.int = list(bs = "ps", k = 50),
                data = data0_wd_pre)

save(fm_wd_pre, file ="/path/fm_wd_pre.rda")

fm_we_pre <- pffr(Y_Activity ~ O_waking_time_WE  + AGE + SEX + edu_cat1 + edu_cat2 + season1 + season2 + season3 + study1 + s(ID, bs= "re"), 
                  yind = xi,
                  bs.yindex = list(bs = "ps", k=-1), 
                  bs.int = list(bs = "ps", k = 50),
                  data = data0_we_pre)
save(fm_we_pre, file ="/path/fm_we_pre.rda")

# 2) CHILDREN (n=1153)
fm_wd_chi <- pffr(Y_Activity ~ O_waking_time_WD  + AGE + SEX + edu_cat1 + edu_cat2 + season1 + season2 + season3 + study1 + s(ID, bs= "re"), 
                yind = xi,
              bs.yindex = list(bs = "ps", k=-1), 
              bs.int = list(bs = "ps", k = 50),
              data = data0_wd_chi)
save(fm_wd_chi, file ="/path/fm_wd_chi.rda")

fm_we_chi <- pffr(Y_Activity ~ O_waking_time_WE  + AGE + SEX + edu_cat1 + edu_cat2 + season1 + season2 + season3 + study1 + s(ID, bs= "re"), 
                  yind = xi,
                  bs.yindex = list(bs = "ps", k=-1), 
                  bs.int = list(bs = "ps", k = 50),
                  data = data0_we_chi)
save(fm_we_chi, file ="/fm_we_chi.rda")

# 3) ADOLESCENTS (n=297)
fm_wd_ado <- pffr(Y_Activity ~ O_waking_time_WD  + AGE + SEX + edu_cat1 + edu_cat2  + season1 + season2 + season3 + study2 + s(ID, bs= "re"), 
                yind = xi,
              bs.yindex = list(bs = "ps", k=-1), 
              bs.int = list(bs = "ps", k = 50),
              data = data0_wd_ado)
save(fm_wd_ado, file ="/fm_wd_ado.rda")

fm_we_ado <- pffr(Y_Activity ~ O_waking_time_WE  + AGE + SEX + edu_cat1 + edu_cat2 + season1 + season2 + season3 + study2 + s(ID, bs= "re"), 
                   yind = xi,
                   bs.yindex = list(bs = "ps", k=-1), 
                   bs.int = list(bs = "ps", k = 50),
                   data = data0_we_ado)
save(fm_we_ado, file ="/path/fm_we_ado.rda")

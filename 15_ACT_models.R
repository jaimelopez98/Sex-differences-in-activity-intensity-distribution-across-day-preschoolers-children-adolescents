# Script name: 15_ACT_models.R
# 
# Author: J.López, Inserm
#
# Doing:  
#   *Modelling using the time spent in each specific range of the full distriburion of PA intensity  dependent term in form of function
#   and sex and other covariates as independent variables (function-on-scalar regression) on weekdays and week-end days and for:
#     *Pre-schoolers (n=1057)
#     *Children (n=1153)
#     *Adolescents (n=297)

#PACKAGES --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidyfun)
library(ggplot2)
library(purrr)
library(refund)

# 0) LOAD DATA ----
# WEEKDAYS
load("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/WD_act_pre.rda") # PRESCHOOLERS
load("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/WD_act_chi.rda") # CHILDREN
load("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/WD_act_ado.rda") # ADOLESCENTS

# WEEKENDS
load("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/WE_act_pre.rda") # PRESCHOOLERS
load("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/WE_act_chi.rda") # CHILDREN
load("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/WE_act_ado.rda") # ADOLESCENTS

load("/path/WD_c_value.rda") # WEEKDAYS
xi_wd <- seq(0,c_value_wd,length.out = 200)

load("path/WE_c_value.rda") # WEEKENDS
xi_we <- seq(0,c_value_we,length.out = 200)

# 1) PRE-SCHOOLERS (n=1057) ----
# WEEKDAYS
fm_wd_pre <- pffr(Y_Activity ~ O_waking_time_WD + AGE + edu_cat1 + edu_cat2 + SEX + season1 + season2 + season3 + study1 + s(ID, bs= "re"), 
                yind = xi_wd,
                bs.yindex = list(bs = "ps", k=-1), 
                bs.int = list(bs = "ps", k = 50),
                data = data0_wd_pre)
save(fm_wd_pre, file ="C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/MODELS/fm_wd_pre.rda")

# WEEKENDS
fm_we_pre <- pffr(Y_Activity ~ O_waking_time_WE + AGE + edu_cat1 + edu_cat2 + SEX + season1 + season2 + season3 + study1 + s(ID, bs= "re"), 
                  yind = xi_we,
                  bs.yindex = list(bs = "ps", k=-1), 
                  bs.int = list(bs = "ps", k = 50),
                  data = data0_we_pre)
save(fm_we_pre, file ="C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/MODELS/fm_we_pre.rda")

# 2) CHILDREN (n= 1153) ----
# WEEKDAYS
fm_wd_chi <- pffr(Y_Activity ~ O_waking_time_WD + AGE + edu_cat1 + edu_cat2 + SEX + season1 + season2 + season3 + study1 + s(ID, bs= "re"), 
                yind = xi_wd,
              bs.yindex = list(bs = "ps", k=-1), 
              bs.int = list(bs = "ps", k = 50),
              data = data0_wd_chi)
save(fm_wd_chi, file ="C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/MODELS/fm_wd_chi.rda")

# WEEKENDS
fm_we_chi <- pffr(Y_Activity ~ O_waking_time_WE + AGE + edu_cat1 + edu_cat2 + SEX + season1 + season2 + season3 + study1 + s(ID, bs= "re"), 
                  yind = xi_we,
                  bs.yindex = list(bs = "ps", k=-1), 
                  bs.int = list(bs = "ps", k = 50),
                  data = data0_we_chi)
save(fm_we_chi, file ="C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/MODELS/fm_we_chi.rda")

# 3) ADOLESCENTS (n=297) ----
# WEEKDAYS
fm_wd_ado <- pffr(Y_Activity ~ O_waking_time_WD + AGE + edu_cat1 + edu_cat2 + SEX + season1 + season2 + season3 + study2 + s(ID, bs= "re"), 
              yind = xi_wd,
              bs.yindex = list(bs = "ps", k=-1), 
              bs.int = list(bs = "ps", k = 50),
              data = data0_wd_ado)
save(fm_wd_ado, file ="C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/MODELS/fm_wd_ado.rda")

# WEEKENDS
fm_we_ado <- pffr(Y_Activity ~ O_waking_time_WE + AGE + edu_cat1 + edu_cat2 + SEX + season1 + season2 + season3 + study2 + s(ID, bs= "re"), 
                   yind = xi_we,
                   bs.yindex = list(bs = "ps", k=-1), 
                   bs.int = list(bs = "ps", k = 50),
                   data = data0_we_ado)
save(fm_we_ado, file ="C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/ACT/MODELS/fm_we_ado.rda")

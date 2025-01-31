# Script name: 07_TIME_data.R

# Author: J.López , Inserm

# Doing: 
#   * Preparing data for function-on-scalar regression in physical activity over the course the day for each age group
#     * DATA on weekdays
#     * DATA on week-end days

# PACKAGES----------------------------------------------------------------------

library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)

# LOAD THE DATA --------------------------------------------------------------------

load("/path/WD_ts.rda")
load("/path/WE_ts.rda")

labda_data <- read.csv("/path/labda_data.csv") 

xi <- seq(6, 22, length = 960) # time of the day (waking hours)

# 1) WEEKDAYS ----

# PRE-SCHOOLERS (n = 1057)
labda_data_pre <- labda_data %>% filter(AGE >=3 & AGE < 6) 

pre <- labda_data_pre %>% select(ID, AGE) 

ts_wd_pre <- merge(ts_wd2, pre, by ="ID") %>%
  select(-AGE)

tab_pa_pre <- ts_wd_pre[,-1]
id_pre <- as.vector(pre$ID)

dimnames(tab_pa_pre) <- list(id_pre, dimnames(tab_pa_pre)[[2]])

Y_Activity <- I(as.matrix(tab_pa_pre))  

tab_cov_pre <- tab_cov%>%
  filter(age1==0 & age2==0)

data0_wd_pre <- tab_cov_pre %>%
  cbind(Y_Activity) %>%
  mutate(ID = as.factor(.$ID)) 

save(data0_wd_pre,  file = "/path/WD_ts_pre.rda")

# CHILDREN  (n = 1153)
labda_data_chi <- labda_data %>% filter(age1 == 1) 

chi <- labda_data_chi %>% select(ID, AGE) 

ts_wd_chi <- merge(ts_wd2, chi, by ="ID") %>%
  select(-AGE)

tab_pa_chi <- ts_wd_chi[,-1]
id_chi <- as.vector(chi$ID)

dimnames(tab_pa_chi) <- list(id_chi, dimnames(tab_pa_chi)[[2]])

Y_Activity <- I(as.matrix(tab_pa_chi))

tab_cov_chi <- tab_cov%>%
  filter(age1 ==1)

data0_wd_chi <- tab_cov_chi %>%
  cbind(Y_Activity) %>%
  mutate(ID = as.factor(.$ID)) 

save(data0_wd_chi,  file = "/path/WD_ts_chi.rda")

# ADOLESCENTS  (n = 297)
labda_data_ado <- labda_data %>% filter(age2 == 1) 

ado <- labda_data_ado %>% select(ID, AGE) 

ts_wd_ado <- merge(ts_wd2, ado, by ="ID") %>%
  select(-AGE)

tab_pa_ado <- ts_wd_ado[,-1]
id_ado <- as.vector(ado$ID)

dimnames(tab_pa_ado) <- list(id_ado, dimnames(tab_pa_ado)[[2]])

Y_Activity <- I(as.matrix(tab_pa_ado))
                
tab_cov_ado <- tab_cov%>%
  filter(age2 ==1)

data0_wd_ado <- tab_cov_ado %>%
  cbind(Y_Activity) %>%
  mutate(ID = as.factor(.$ID)) 

save(data0_wd_ado,  file = "/path/WD_ts_ado.rda")

# 2) WEEK-END DAYS ----

# PRE-SCHOOLERS (n = 1057)
labda_data_pre <- labda_data %>% filter(AGE >=3 & AGE < 6) 

pre <- labda_data_pre %>% select(ID, AGE) 

ts_we_pre <- merge(ts_we2, pre, by ="ID") %>%
  select(-AGE)

tab_pa_pre <- ts_we_pre[,-1]
id_pre <- as.vector(pre$ID)

dimnames(tab_pa_pre) <- list(id_pre, dimnames(tab_pa_pre)[[2]])

Y_Activity <- I(as.matrix(tab_pa_pre))  

tab_cov_pre <- tab_cov%>%
  filter(age1==0 & age2==0)

data0_we_pre <- tab_cov_pre %>%
  cbind(Y_Activity) %>%
  mutate(ID = as.factor(.$ID))  

save(data0_we_pre,  file = "/path/WE_ts_pre.rda")

# CHILDREN (n = 1153)
labda_data_chi <- labda_data %>% filter(age1==1) 

chi <- labda_data_chi %>% select(ID, AGE) 

ts_we_chi <- merge(ts_we2, chi, by ="ID") %>%
  select(-AGE)

tab_pa_chi <- ts_we_chi[,-1]
id_chi <- as.vector(chi$ID)

dimnames(tab_pa_chi) <- list(id_chi, dimnames(tab_pa_chi)[[2]])

Y_Activity <- I(as.matrix(tab_pa_chi))

tab_cov_chi <- tab_cov%>%
  filter(age1 ==1)

data0_we_chi <- tab_cov_chi %>%
  cbind(Y_Activity) %>%
  mutate(ID = as.factor(.$ID))  

save(data0_we_chi,  file = "/path/WE_ts_chi.rda")

# ADOLESCENTS (n = 297)
labda_data_ado <- labda_data %>% filter(age2==1) 

ado <- labda_data_ado %>% select(ID, AGE) 

ts_we_ado <- merge(ts_we2, ado, by ="ID") %>%
  select(-AGE)

tab_pa_ado <- ts_we_ado[,-1]
id_ado <- as.vector(ado$ID)

dimnames(tab_pa_ado) <- list(id_ado, dimnames(tab_pa_ado)[[2]])

Y_Activity <- I(as.matrix(tab_pa_ado))

tab_cov_ado <- tab_cov%>%
  filter(age2 ==1)

data0_we_ado <- tab_cov_ado %>%
  cbind(Y_Activity) %>%
  mutate(ID = as.factor(.$ID)) 

save(data0_we_ado,  file = "/path/WE_ts_ado.rda")
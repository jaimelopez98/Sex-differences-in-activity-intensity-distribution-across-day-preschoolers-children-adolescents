# Script name: 14_ACT_data_WD.R

# Author: J.López, Inserm

# Doing: 
#   * Preparing data for function-on-scalar regression in physical activity over the full range of the activity intensity distribution for each age group
#     * DATA on weekdays
#     * DATA on week-end days

# PACKAGES ---------------------------------------------------------------------

library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(testthat)

# DATA -------------------------------------------------------------------------

load("/path/WD_act.rda")
load("/path/WE_act.rda")

labda_data <- read.csv("/path/labda_data.csv") 

# 1) WEEKDAYS ----

# PRE-SCHOOLERS (n=1057)
labda_data_pre <- labda_data %>% filter(AGE >=3 & AGE < 6) 
act_wd_pre <- act_wd %>% filter(AGE >=3 & AGE < 6)

tab_pa_pre <- act_wd_pre %>%
  filter(ID %in% unique(labda_data_pre$ID)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(ID) %>% 
  select(-starts_with("dur_"), -ID, -AGE, -SEX, -EDUCATION_P, -STUDY, -season, -O_waking_time_WE, -EDUCATION_P_fact,
         -AGE_fact, -age1, -age2, -edu_cat1, -edu_cat2, -season1, -season2, -season3, -study1, -study2, -O_waking_time_WD)

id <- as.vector(labda_data_pre$ID)
dimnames(tab_pa_pre) <- list(id, dimnames(tab_pa_pre)[[2]])
Y_Activity <- I(as.matrix(tab_pa_pre)) 

tab_cov_pre <- tab_cov %>%  filter(age1==0 & age2==0)

data0_wd_pre <- tab_cov_pre %>%
  cbind(Y_Activity) %>%
  mutate(ID = as.factor(.$ID)) 

save(data0_wd_pre,  file = "/path/WD_act_pre.rda")

# CHILDREN (n=1153)
labda_data_chi <- labda_data %>% filter(age1 == 1) 
act_wd_chi <- act_wd %>% filter(age1 == 1)

tab_pa_chi <- act_wd_chi %>%
  filter(ID %in% unique(labda_data_chi$ID)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(ID) %>% 
  select(-starts_with("dur_"), -ID, -AGE, -SEX, -EDUCATION_P, -STUDY, -season, -O_waking_time_WE, -EDUCATION_P_fact,
         -AGE_fact, -age1, -age2, -edu_cat1, -edu_cat2, -season1, -season2, -season3, -study1, -study2, -O_waking_time_WD)

id <- as.vector(labda_data_chi$ID)
dimnames(tab_pa_chi) <- list(id, dimnames(tab_pa_chi)[[2]])
Y_Activity <- I(as.matrix(tab_pa_chi)) 

tab_cov_chi <- tab_cov %>%  filter(age1==1)

data0_wd_chi <- tab_cov_chi %>%
  cbind(Y_Activity) %>%
  mutate(ID = as.factor(.$ID)) 

save(data0_wd_chi,  file = "/path/WD_act_chi.rda")

# ADOLESCENTS (n=297 )
labda_data_ado <- labda_data %>% filter(age2 == 1) 
act_wd_ado <- act_wd %>% filter(age2 == 1)

tab_pa_ado <- act_wd_ado %>%
  filter(ID %in% unique(labda_data_ado$ID)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(ID) %>% 
  select(-starts_with("dur_"), -ID, -AGE, -SEX, -EDUCATION_P, -STUDY, -season, -O_waking_time_WE, -EDUCATION_P_fact,
         -AGE_fact, -age1, -age2, -edu_cat1, -edu_cat2, -season1, -season2, -season3, -study1, -study2, -O_waking_time_WD)

id <- as.vector(labda_data_ado$ID)
dimnames(tab_pa_ado) <- list(id, dimnames(tab_pa_ado)[[2]])
Y_Activity <- I(as.matrix(tab_pa_ado)) 

tab_cov_ado <- tab_cov %>%  filter(age2==1)

data0_wd_ado <- tab_cov_ado %>%
  cbind(Y_Activity) %>%
  mutate(ID = as.factor(.$ID)) 

save(data0_wd_ado,  file = "/path/WD_act_ado.rda")

# 2) WEEK-END DAYS ----

# PRE-SCHOOLERS (n=1057)
labda_data_pre <- labda_data %>% filter(AGE >=3 & AGE < 6) 
act_we_pre <- act_we %>% filter(AGE >=3 & AGE < 6)

tab_pa_pre <- act_we_pre %>%
  filter(ID %in% unique(labda_data_pre$ID)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(ID) %>% 
  select(-starts_with("dur_"), -ID, -AGE, -SEX, -EDUCATION_P, -STUDY, -season, -O_waking_time_WE, -EDUCATION_P_fact,
         -AGE_fact, -age1, -age2, -edu_cat1, -edu_cat2, -season1, -season2, -season3, -study1, -study2, -O_waking_time_WD)

id <- as.vector(labda_data_pre$ID)
dimnames(tab_pa_pre) <- list(id, dimnames(tab_pa_pre)[[2]])
Y_Activity <- I(as.matrix(tab_pa_pre)) 

tab_cov_pre <- tab_cov %>%  filter(age1==0 & age2==0)

data0_we_pre <- tab_cov_pre %>%
  cbind(Y_Activity) %>%   
  mutate(ID = as.factor(.$ID)) 

save(data0_we_pre,  file = "/path/WE_act_pre.rda")

# CHILDREN (n=1153)
labda_data_chi <- labda_data %>% filter(age1 == 1) 
act_we_chi <- act_we %>% filter(age1 == 1)

tab_pa_chi <- act_we_chi %>%
  filter(ID %in% unique(labda_data_chi$ID)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(ID) %>% 
  select(-starts_with("dur_"), -ID, -AGE, -SEX, -EDUCATION_P, -STUDY, -season, -O_waking_time_WE, -EDUCATION_P_fact,
         -AGE_fact, -age1, -age2, -edu_cat1, -edu_cat2, -season1, -season2, -season3, -study1, -study2, -O_waking_time_WD)

id <- as.vector(labda_data_chi$ID)
dimnames(tab_pa_chi) <- list(id, dimnames(tab_pa_chi)[[2]])
Y_Activity <- I(as.matrix(tab_pa_chi)) 

tab_cov_chi <- tab_cov %>%  filter(age1==1)

data0_we_chi <- tab_cov_chi %>%
  cbind(Y_Activity) %>%   
  mutate(ID = as.factor(.$ID)) 

save(data0_we_chi,  file = "/path/WE_act_chi.rda")

# ADOLESCENTS (n=297)
labda_data_ado <- labda_data %>% filter(age2 == 1) 
act_we_ado <- act_we %>% filter(age2 == 1)

tab_pa_ado <- act_we_ado %>%
  filter(ID %in% unique(labda_data_ado$ID)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(ID) %>% 
  select(-starts_with("dur_"), -ID, -AGE, -SEX, -EDUCATION_P, -STUDY, -season, -O_waking_time_WE, -EDUCATION_P_fact,
         -AGE_fact, -age1, -age2, -edu_cat1, -edu_cat2, -season1, -season2, -season3, -study1, -study2, -O_waking_time_WD)

id <- as.vector(labda_data_ado$ID)
dimnames(tab_pa_ado) <- list(id, dimnames(tab_pa_ado)[[2]])
Y_Activity <- I(as.matrix(tab_pa_ado)) 

tab_cov_ado <- tab_cov %>%  filter(age2==1)

data0_we_ado <- tab_cov_ado %>%
  cbind(Y_Activity) %>%   
  mutate(ID = as.factor(.$ID)) 

save(data0_we_ado,  file = "/path/WE_act_ado.rda")
# Script name: 05_INTERACTIONS.R

# Author: J.López, Inserm

# Doing: 

#   *Check interactions between age, sex and type of day (triple interaction: age*sex*daytype)
#   *Check interactions between age and type of day in boys and girls (double interaction: age*daytype)
#   *Check interactions between sex and type of day in each age group (double interaction: sex*daytype)

#PACKAGES --------------------------------------------------------------------

library(tidyr)
library(plyr)
library(dplyr)
library(haven)
library(purrr)
library(lubridate)
library(openxlsx)
library(lme4)
library(lmtest)
library(lme4)

# 0) LOAD PHYSICAL ACTIVITY DATA AND DEMOGRAPHIC DATA ----

act5_day <- read.csv("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/part5_daysummary.csv")
act5_sum <- read.csv("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/part5_personsummary.csv")
act5_sum <- act5_sum %>% select(ID,season)

labda_demo <- read.csv("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/labda_demo.csv")
labda_demo <- merge(labda_demo, act5_sum)

# CREATE A VARIABLE FOR MVPA intensity
act5_day <- act5_day %>%
  mutate(dur_day_MVPA_min = dur_day_MOD_min + dur_day_VIG_min)

# 1) CALCULATE THE MEAN OF TIME SPENT IN SB AND PA INTENSITIES PER DAY FOR EACH PARTICIPANT ON WEEKDAYS AND WEEK-END DAYS
act_wd <- act5_day %>%
  filter(daytype == 0) %>%
  group_by(ID) %>%
  summarise(daytype = mean(daytype),
            dur_wak_min = mean(dur_day_min),
            dur_SB_min = mean(dur_day_SB_min),
            dur_LIG_min = mean(dur_day_LIG_min),
            dur_MOD_min = mean(dur_day_MOD_min),
            dur_VIG_min = mean(dur_day_VIG_min),
            dur_MVPA_min = mean(dur_day_MVPA_min))

act_we <- act5_day %>%
  filter(daytype == 1) %>%
  group_by(ID) %>%
  summarise(daytype = mean(daytype),
            dur_wak_min = mean(dur_day_min),
            dur_SB_min = mean(dur_day_SB_min),
            dur_LIG_min = mean(dur_day_LIG_min),
            dur_MOD_min = mean(dur_day_MOD_min),
            dur_VIG_min = mean(dur_day_VIG_min),
            dur_MVPA_min = mean(dur_day_MVPA_min))

act_full<- rbind(act_wd,act_we) %>% .[order(.$ID),]
act_full <- act_full %>%
  mutate(O_waking_time = dur_wak_min - 16*60)


# 2) CREATE OBJECTS WITH DEMOGRAPHIC DATA AND ACCELERATION VARIABLES ----

cut_data0 <- merge(labda_demo,act_full, by = "ID") %>%
  filter(complete.cases(.))

# all participants
cut_data <- cut_data0 %>%
  select(ID,SEX, AGE, age1, age2, edu_cat1, edu_cat2, season1,season2,season3, study1, study2, daytype, O_waking_time, dur_SB_min, dur_LIG_min, dur_MOD_min, dur_VIG_min, dur_MVPA_min)

# by sex
cut_data_b <- cut_data %>% filter(SEX==0)
cut_data_g <- cut_data %>% filter(SEX==1)

# by age
cut_data_pre <- cut_data %>% filter(AGE >=3 & AGE <6)
cut_data_chi <- cut_data %>% filter(age1==0) 
cut_data_ado <- cut_data %>% filter(age2==1)

# 3) THREE-WAY INTERACTION (age*sex*daytype) ----

# interaction variables
cut_data$age1_x_sex <- cut_data$age1 * cut_data$SEX
cut_data$age2_x_sex <- cut_data$age2 * cut_data$SEX

cut_data$age1_x_tod <- cut_data$age1 * cut_data$daytype
cut_data$age2_x_tod <- cut_data$age2 * cut_data$daytype

cut_data$sex_x_tod <- cut_data$SEX * cut_data$daytype

cut_data$age1_x_sex_x_tod <- cut_data$age1 * cut_data$SEX * cut_data$daytype
cut_data$age2_x_sex_x_tod <- cut_data$age2 * cut_data$SEX * cut_data$daytype

#models without interaction
lm_SB_no <-  lmer(dur_SB_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                    age1_x_sex + age2_x_sex + sex_x_tod +
                    (1 | ID) , data = cut_data)

lm_LIG_no <-  lmer(dur_LIG_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                     age1_x_sex + age2_x_sex + sex_x_tod +
                     (1 | ID) , data = cut_data)

lm_MOD_no <-  lmer(dur_MOD_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                     age1_x_sex + age2_x_sex + sex_x_tod +
                     (1 | ID) , data = cut_data)

lm_VIG_no <-  lmer(dur_VIG_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                     age1_x_sex + age2_x_sex + sex_x_tod +
                     (1 | ID) , data = cut_data)

lm_MVPA_no <-  lmer(dur_MVPA_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                      age1_x_sex + age2_x_sex + sex_x_tod +
                      (1 | ID) , data = cut_data)

# models with interactions
lm_SB_int <- lmer(dur_SB_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + 
                    age1_x_sex + age2_x_sex + 
                    age1_x_sex_x_tod + age2_x_sex_x_tod +
                    sex_x_tod + 
                    study1 + study2 + season1 + season2 + season3 +
                    (1 | ID), data = cut_data)

lm_LIG_int <- lmer(dur_LIG_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + 
                     age1_x_sex + age2_x_sex + 
                     age1_x_sex_x_tod + age2_x_sex_x_tod +
                     sex_x_tod + 
                     study1 + study2 + season1 + season2 + season3 +
                     (1 | ID), data = cut_data)

lm_MOD_int <- lmer(dur_MOD_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + 
                     age1_x_sex + age2_x_sex + 
                     age1_x_sex_x_tod + age2_x_sex_x_tod +
                     sex_x_tod + 
                     study1 + study2 + season1 + season2 + season3 +
                     (1 | ID), data = cut_data)

lm_VIG_int <- lmer(dur_VIG_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + 
                     age1_x_sex + age2_x_sex + 
                     age1_x_sex_x_tod + age2_x_sex_x_tod +
                     sex_x_tod + 
                     study1 + study2 + season1 + season2 + season3 +
                     (1 | ID), data = cut_data)

lm_MVPA_int <- lmer(dur_MVPA_min ~ O_waking_time + AGE + age1 + age2 + SEX + edu_cat1 + edu_cat2 + daytype + 
                      age1_x_sex + age2_x_sex + 
                      age1_x_sex_x_tod + age2_x_sex_x_tod +
                      sex_x_tod + 
                      study1 + study2 + season1 + season2 + season3 +
                      (1 | ID), data = cut_data)

# checking the best model (LOG LIKELIHOOD RATIO test)
models <- c("SB", "LIG", "MOD","VIG","MVPA")

ll_no <-  c( logLik(lm_SB_no), logLik(lm_LIG_no),logLik(lm_MOD_no),logLik(lm_VIG_no), logLik(lm_MVPA_no))
ll_int <- c( logLik(lm_SB_int), logLik(lm_LIG_int),logLik(lm_MOD_int),logLik(lm_VIG_int), logLik(lm_MVPA_int))

ll_in <- data.frame(rbind(ll_no, ll_int), row.names = c("WITHOUT INTERACTIONS", "WITH INTERACTIONS"))
colnames(ll_in) <- models

write.xlsx(ll_in,"E:/DEFINITIVO/RESULTS/interactions.xlsx",rowNames = TRUE)

# 4) DOUBLE-WAY INTERACTION (age*daytype) ----

# BOYS

# interaction variables
cut_data_b$age1_x_tod <- cut_data_b$age1 * cut_data_b$daytype
cut_data_b$age2_x_tod <- cut_data_b$age2 * cut_data_b$daytype

# models without interactions
lm_SB_no_b <-  lmer(dur_SB_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                    (1 | ID) , data = cut_data_b)

lm_LIG_no_b <-  lmer(dur_LIG_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                     (1 | ID) , data = cut_data_b)

lm_MOD_no_b <-  lmer(dur_MOD_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                     (1 | ID) , data = cut_data_b)

lm_VIG_no_b <-  lmer(dur_VIG_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                     (1 | ID) , data = cut_data_b)

lm_MVPA_no_b <-  lmer(dur_MVPA_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                      (1 | ID) , data = cut_data_b)

# models with interactions
lm_SB_int_b <- lmer(dur_SB_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                    age1_x_tod + age2_x_tod +
                    (1 | ID) , data = cut_data_b)
  
lm_LIG_int_b <- lmer(dur_LIG_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       age1_x_tod + age2_x_tod +
                       (1 | ID) , data = cut_data_b)

lm_MOD_int_b <- lmer(dur_MOD_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       age1_x_tod + age2_x_tod +
                       (1 | ID) , data = cut_data_b)

lm_VIG_int_b <- lmer(dur_VIG_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       age1_x_tod + age2_x_tod +
                       (1 | ID) , data = cut_data_b)

lm_MVPA_int_b <- lmer(dur_MVPA_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        age1_x_tod + age2_x_tod +
                        (1 | ID) , data = cut_data_b)

# checking the best model (LOG LIKELIHOOD RATIO test)
models <- c("SB", "LIG", "MOD","VIG","MVPA")

ll_no_b <-  c( logLik(lm_SB_no_b), logLik(lm_LIG_no_b),logLik(lm_MOD_no_b),logLik(lm_VIG_no_b), logLik(lm_MVPA_no_b))
ll_int_b <- c( logLik(lm_SB_int_b), logLik(lm_LIG_int_b),logLik(lm_MOD_int_b),logLik(lm_VIG_int_b), logLik(lm_MVPA_int_b))

ll_in_b <- data.frame(rbind(ll_no_b, ll_int_b), row.names = c("WITHOUT INTERACTIONS", "WITH INTERACTIONS"))
colnames(ll_in_b) <- models

write.xlsx(ll_in_b,"E:/DEFINITIVO/RESULTS/interactions_boys.xlsx",rowNames = TRUE)

# GIRLS

# interaction variables
cut_data_g$age1_x_tod <- cut_data_g$age1 * cut_data_g$daytype
cut_data_g$age2_x_tod <- cut_data_g$age2 * cut_data_g$daytype

#models without interactions
lm_SB_no_g <-  lmer(dur_SB_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                      (1 | ID) , data = cut_data_g)

lm_LIG_no_g <-  lmer(dur_LIG_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_g)

lm_MOD_no_g <-  lmer(dur_MOD_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_g)

lm_VIG_no_g <-  lmer(dur_VIG_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_g)

lm_MVPA_no_g <-  lmer(dur_MVPA_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        (1 | ID) , data = cut_data_g)

# models with interactions
lm_SB_int_g <- lmer(dur_SB_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                      age1_x_tod + age2_x_tod +
                      (1 | ID) , data = cut_data_g)

lm_LIG_int_g <- lmer(dur_LIG_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       age1_x_tod + age2_x_tod +
                       (1 | ID) , data = cut_data_g)

lm_MOD_int_g <- lmer(dur_MOD_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       age1_x_tod + age2_x_tod +
                       (1 | ID) , data = cut_data_g)

lm_VIG_int_g <- lmer(dur_VIG_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       age1_x_tod + age2_x_tod +
                       (1 | ID) , data = cut_data_g)

lm_MVPA_int_g <- lmer(dur_MVPA_min ~ O_waking_time + AGE + age1 + age2 + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        age1_x_tod + age2_x_tod +
                        (1 | ID) , data = cut_data_g)

# checking the best model (LOG LIKELIHOOD RATIO test)
models <- c("SB", "LIG", "MOD","VIG","MVPA")

ll_no_g <-  c( logLik(lm_SB_no_g), logLik(lm_LIG_no_g),logLik(lm_MOD_no_g),logLik(lm_VIG_no_g), logLik(lm_MVPA_no_g))
ll_int_g <- c( logLik(lm_SB_int_g), logLik(lm_LIG_int_g),logLik(lm_MOD_int_g),logLik(lm_VIG_int_g), logLik(lm_MVPA_int_g))

ll_in_g <- data.frame(rbind(ll_no_g, ll_int_g), row.names = c("WITHOUT INTERACTIONS", "WITH INTERACTIONS"))
colnames(ll_in_g) <- models

write.xlsx(ll_in_g,"E:/DEFINITIVO/RESULTS/interactions_girls.xlsx",rowNames = TRUE)

# 5) DOUBLE-WAY INTERACTION (sex*daytype) ----

# PRE-SCHOOLERS (3-5y)

# interaction variables
cut_data_pre$SEX_x_tod <- cut_data_pre$SEX * cut_data_pre$daytype
cut_data_pre$SEX_x_tod <- cut_data_pre$SEX * cut_data_pre$daytype

#models without interactions
lm_SB_no_pre <-  lmer(dur_SB_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        (1 | ID) , data = cut_data_pre)

lm_LIG_no_pre <-  lmer(dur_LIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_pre)

lm_MOD_no_pre <-  lmer(dur_MOD_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_pre)

lm_VIG_no_pre <-  lmer(dur_VIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_pre)

lm_MVPA_no_pre <-  lmer(dur_MVPA_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        (1 | ID) , data = cut_data_pre)

# models with interactions
lm_SB_int_pre <- lmer(dur_SB_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                      SEX_x_tod + SEX_x_tod +
                      (1 | ID) , data = cut_data_pre)

lm_LIG_int_pre <- lmer(dur_LIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       SEX_x_tod + SEX_x_tod +
                       (1 | ID) , data = cut_data_pre)

lm_MOD_int_pre <- lmer(dur_MOD_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       SEX_x_tod + SEX_x_tod +
                       (1 | ID) , data = cut_data_pre)

lm_VIG_int_pre <- lmer(dur_VIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       SEX_x_tod + SEX_x_tod +
                       (1 | ID) , data = cut_data_pre)

lm_MVPA_int_pre <- lmer(dur_MVPA_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        SEX_x_tod + SEX_x_tod +
                        (1 | ID) , data = cut_data_pre)

# checking the best model (LOG LIKELIHOOD RATIO test)
models <- c("SB", "LIG", "MOD","VIG","MVPA")

ll_no_pre <-  c( logLik(lm_SB_no_pre), logLik(lm_LIG_no_pre),logLik(lm_MOD_no_pre),logLik(lm_VIG_no_pre), logLik(lm_MVPA_no_pre))
ll_int_pre <- c( logLik(lm_SB_int_pre), logLik(lm_LIG_int_pre),logLik(lm_MOD_int_pre),logLik(lm_VIG_int_pre), logLik(lm_MVPA_int_pre))

ll_in_pre <- data.frame(rbind(ll_no_pre, ll_int_pre), row.names = c("WITHOUT INTERACTIONS", "WITH INTERACTIONS"))
colnames(ll_in_pre) <- models

write.xlsx(ll_in_pre,"E:/DEFINITIVO/RESULTS/interactions_pre.xlsx",rowNames = TRUE)

# CHILDREN (6-10y)

# interaction variables
cut_data_chi$SEX_x_tod <- cut_data_chi$SEX * cut_data_chi$daytype
cut_data_chi$SEX_x_tod <- cut_data_chi$SEX * cut_data_chi$daytype

# models without interactions
lm_SB_no_chi <-  lmer(dur_SB_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                      (1 | ID) , data = cut_data_chi)

lm_LIG_no_chi <-  lmer(dur_LIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_chi)

lm_MOD_no_chi <-  lmer(dur_MOD_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_chi)

lm_VIG_no_chi <-  lmer(dur_VIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_chi)

lm_MVPA_no_chi <-  lmer(dur_MVPA_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        (1 | ID) , data = cut_data_chi)

# models with interactions
lm_SB_int_chi <- lmer(dur_SB_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                      SEX_x_tod + SEX_x_tod +
                      (1 | ID) , data = cut_data_chi)

lm_LIG_int_chi <- lmer(dur_LIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       SEX_x_tod + SEX_x_tod +
                       (1 | ID) , data = cut_data_chi)

lm_MOD_int_chi <- lmer(dur_MOD_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       SEX_x_tod + SEX_x_tod +
                       (1 | ID) , data = cut_data_chi)

lm_VIG_int_chi <- lmer(dur_VIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       SEX_x_tod + SEX_x_tod +
                       (1 | ID) , data = cut_data_chi)

lm_MVPA_int_chi <- lmer(dur_MVPA_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        SEX_x_tod + SEX_x_tod +
                        (1 | ID) , data = cut_data_chi)

# checking the best model (LOG LIKELIHOOD RATIO test)
models <- c("SB", "LIG", "MOD","VIG","MVPA")

ll_no_chi <-  c( logLik(lm_SB_no_chi), logLik(lm_LIG_no_chi),logLik(lm_MOD_no_chi),logLik(lm_VIG_no_chi), logLik(lm_MVPA_no_chi))
ll_int_chi <- c( logLik(lm_SB_int_chi), logLik(lm_LIG_int_chi),logLik(lm_MOD_int_chi),logLik(lm_VIG_int_chi), logLik(lm_MVPA_int_chi))

ll_in_chi <- data.frame(rbind(ll_no_chi, ll_int_chi), row.names = c("WITHOUT INTERACTIONS", "WITH INTERACTIONS"))
colnames(ll_in_chi) <- models

write.xlsx(ll_in_chi,"E:/DEFINITIVO/RESULTS/interactions_chi.xlsx",rowNames = TRUE)

# ADOLESCENTS (11-17y)

# interaction variables
cut_data_ado$SEX_x_tod <- cut_data_ado$SEX * cut_data_ado$daytype
cut_data_ado$SEX_x_tod <- cut_data_ado$SEX * cut_data_ado$daytype

#models without interactions
lm_SB_no_ado <-  lmer(dur_SB_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                      (1 | ID) , data = cut_data_ado)

lm_LIG_no_ado <-  lmer(dur_LIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_ado)

lm_MOD_no_ado <-  lmer(dur_MOD_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_ado)

lm_VIG_no_ado <-  lmer(dur_VIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       (1 | ID) , data = cut_data_ado)

lm_MVPA_no_ado <-  lmer(dur_MVPA_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        (1 | ID) , data = cut_data_ado)

# models with interaction
lm_SB_int_ado <- lmer(dur_SB_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                      SEX_x_tod + SEX_x_tod +
                      (1 | ID) , data = cut_data_ado)

lm_LIG_int_ado <- lmer(dur_LIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       SEX_x_tod + SEX_x_tod +
                       (1 | ID) , data = cut_data_ado)

lm_MOD_int_ado <- lmer(dur_MOD_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       SEX_x_tod + SEX_x_tod +
                       (1 | ID) , data = cut_data_ado)

lm_VIG_int_ado <- lmer(dur_VIG_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                       SEX_x_tod + SEX_x_tod +
                       (1 | ID) , data = cut_data_ado)

lm_MVPA_int_ado <- lmer(dur_MVPA_min ~ O_waking_time + AGE + SEX + edu_cat1 + edu_cat2 + daytype + study1 + study2 + season1 + season2 + season3 + 
                        SEX_x_tod + SEX_x_tod +
                        (1 | ID) , data = cut_data_ado)

# checking the best model (LOG LIKELIHOOD RATIO test)
models <- c("SB", "LIG", "MOD","VIG","MVPA")

ll_no_ado <-  c( logLik(lm_SB_no_ado), logLik(lm_LIG_no_ado),logLik(lm_MOD_no_ado),logLik(lm_VIG_no_ado), logLik(lm_MVPA_no_ado))
ll_int_ado <- c( logLik(lm_SB_int_ado), logLik(lm_LIG_int_ado),logLik(lm_MOD_int_ado),logLik(lm_VIG_int_ado), logLik(lm_MVPA_int_ado))

ll_in_ado <- data.frame(rbind(ll_no_ado, ll_int_ado), row.names = c("WITHOUT INTERACTIONS", "WITH INTERACTIONS"))
colnames(ll_in_ado) <- models

write.xlsx(ll_in_ado,"E:/DEFINITIVO/RESULTS/interactions_ado.xlsx",rowNames = TRUE)


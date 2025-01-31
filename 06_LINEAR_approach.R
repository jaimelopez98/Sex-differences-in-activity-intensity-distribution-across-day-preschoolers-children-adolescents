# Script name: 06_LINEAR_approach.R

# Author: J.López, Inserm

# Doing:  
#   *Sex differences in each age group depending on the daytype:
#     *Pre-schoolers (n=1057), separately
#     *Children (n=1153), separately
#     *Adolescents (n=297), separately

#PACKAGES --------------------------------------------------------------------

library(dplyr)
library(lmtest)
library(refund)
library(rms)
library(ggplot2)
library(scales)
library(purrr)
library(openxlsx)

# 1) DATA PREPARATION ----

data <- read.csv("C:/Users/j_lopez/Downloads/paper1/analysis/DATA/labda_data.csv")

data_pre <- data %>% filter(AGE >=3 & AGE < 6)
data_chi <- data %>% filter(age1 == 1)
data_ado <- data %>% filter(age2 == 1)

# 2) SEX DIFFERENCES IN EACH AGE GROUP DEPENDING ON DAYTYPE (TABLE 3) ----

# weekdays
# PRE-SCHOOLERS
lm_SB_pre <-lm( dur_SB_min_WD ~ O_waking_time_WD + SEX + AGE + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_pre)
lm_LIG_pre <-lm( dur_LIG_min_WD ~ O_waking_time_WD + SEX + AGE + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_pre)
lm_MOD_pre <-lm( dur_MOD_min_WD ~ O_waking_time_WD + SEX + AGE + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_pre)
lm_VIG_pre <-lm( dur_VIG_min_WD ~ O_waking_time_WD + SEX+ AGE + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_pre)

# CHILDREN
lm_SB_chi <-lm( dur_SB_min_WD ~ O_waking_time_WD + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_chi)
lm_LIG_chi <-lm( dur_LIG_min_WD ~ O_waking_time_WD + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_chi)
lm_MOD_chi <-lm( dur_MOD_min_WD ~ O_waking_time_WD + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_chi)
lm_VIG_chi <-lm( dur_VIG_min_WD ~ O_waking_time_WD + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_chi)

# ADOLESCENTS
lm_SB_ado <-lm( dur_SB_min_WD ~ O_waking_time_WD + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_ado)
lm_LIG_ado <-lm( dur_LIG_min_WD ~ O_waking_time_WD + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_ado)
lm_MOD_ado <-lm( dur_MOD_min_WD ~ O_waking_time_WD + SEX + AGE + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_ado)
lm_VIG_ado <-lm( dur_VIG_min_WD ~ O_waking_time_WD + SEX + AGE + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_ado)

pre_sex <- round(c(coef(lm_SB_pre)[3],confint(lm_SB_pre)[3],confint(lm_SB_pre)[14],
                   coef(lm_LIG_pre)[3],confint(lm_LIG_pre)[3],confint(lm_LIG_pre)[14],
                   coef(lm_MOD_pre)[3],confint(lm_MOD_pre)[3],confint(lm_MOD_pre)[14],
                   coef(lm_VIG_pre)[3],confint(lm_VIG_pre)[3],confint(lm_VIG_pre)[14]),1)

chi_sex <- round(c(coef(lm_SB_chi)[3],confint(lm_SB_chi)[3],confint(lm_SB_chi)[14],
                   coef(lm_LIG_chi)[3],confint(lm_LIG_chi)[3],confint(lm_LIG_chi)[14],
                   coef(lm_MOD_chi)[3],confint(lm_MOD_chi)[3],confint(lm_MOD_chi)[14],
                   coef(lm_VIG_chi)[3],confint(lm_VIG_chi)[3],confint(lm_VIG_chi)[14]),1)

ado_sex <- round(c(coef(lm_SB_ado)[3],confint(lm_SB_ado)[3],confint(lm_SB_ado)[14],
                   coef(lm_LIG_ado)[3],confint(lm_LIG_ado)[3],confint(lm_LIG_ado)[14],
                   coef(lm_MOD_ado)[3],confint(lm_MOD_ado)[3],confint(lm_MOD_ado)[14],
                   coef(lm_VIG_ado)[3],confint(lm_VIG_ado)[3],confint(lm_VIG_ado)[14]),1)

sex_age <- data.frame(rbind(pre_sex, chi_sex, ado_sex), row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS"))
colnames(sex_age) <- c("SB","2.5%", "97.5%", "LIG","2.5%.1", "97.5%.1", "MOD","2.5%.2", "97.5%.2", "VIG","2.5%.3", "97.5%.3")

group1 <- sprintf("%s (%s ; %s)", sex_age$SB, sex_age$`2.5%`, sex_age$`97.5%`)
group2 <- sprintf("%s (%s ; %s)", sex_age$LIG, sex_age$`2.5%.1`, sex_age$`97.5%.1`)
group3 <- sprintf("%s (%s ; %s)", sex_age$MOD, sex_age$`2.5%.2`, sex_age$`97.5%.2`)
group4 <- sprintf("%s (%s ; %s)", sex_age$VIG, sex_age$`2.5%.3`, sex_age$`97.5%.3`)

sex_age2 <- data.frame(group1, group2, group3, group4, row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS"))
colnames(sex_age2) <- c("SB", "LIG","MOD","VIG")

write.xlsx(sex_age2,"C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/WD_sex_age_new.xlsx",rowNames = TRUE)

# week-end days
# PRE-SCHOOLERS
lm_SB_pre <-lm( dur_SB_min_WE ~ O_waking_time_WE + SEX + AGE + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_pre)
lm_LIG_pre <-lm( dur_LIG_min_WE ~ O_waking_time_WE + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_pre)
lm_MOD_pre <-lm( dur_MOD_min_WE ~ O_waking_time_WE + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_pre)
lm_VIG_pre <-lm( dur_VIG_min_WE ~ O_waking_time_WE + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_pre)

# CHILDREN
lm_SB_chi <-lm( dur_SB_min_WE ~ O_waking_time_WE + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_chi)
lm_LIG_chi <-lm( dur_LIG_min_WE ~ O_waking_time_WE + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_chi)
lm_MOD_chi <-lm( dur_MOD_min_WE ~ O_waking_time_WE + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_chi)
lm_VIG_chi <-lm( dur_VIG_min_WE ~ O_waking_time_WE + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_chi)

# PRE-ADOLESCENT
lm_SB_ado <-lm( dur_SB_min_WE ~ O_waking_time_WE + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_ado)
lm_LIG_ado <-lm( dur_LIG_min_WE ~ O_waking_time_WE + SEX + AGE  + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_ado)
lm_MOD_ado <-lm( dur_MOD_min_WE ~ O_waking_time_WE + SEX + AGE + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_ado)
lm_VIG_ado <-lm( dur_VIG_min_WE ~ O_waking_time_WE + SEX + AGE + season1 + season2 + season3 + edu_cat1 + edu_cat2 + study1 + study2, data = data_ado)

pre_sex <- round(c(coef(lm_SB_pre)[3],confint(lm_SB_pre)[3],confint(lm_SB_pre)[14],
                   coef(lm_LIG_pre)[3],confint(lm_LIG_pre)[3],confint(lm_LIG_pre)[14],
                   coef(lm_MOD_pre)[3],confint(lm_MOD_pre)[3],confint(lm_MOD_pre)[14],
                   coef(lm_VIG_pre)[3],confint(lm_VIG_pre)[3],confint(lm_VIG_pre)[14]),1)

chi_sex <- round(c(coef(lm_SB_chi)[3],confint(lm_SB_chi)[3],confint(lm_SB_chi)[14],
                   coef(lm_LIG_chi)[3],confint(lm_LIG_chi)[3],confint(lm_LIG_chi)[14],
                   coef(lm_MOD_chi)[3],confint(lm_MOD_chi)[3],confint(lm_MOD_chi)[14],
                   coef(lm_VIG_chi)[3],confint(lm_VIG_chi)[3],confint(lm_VIG_chi)[14]),1)

ado_sex <- round(c(coef(lm_SB_ado)[3],confint(lm_SB_ado)[3],confint(lm_SB_ado)[14],
                    coef(lm_LIG_ado)[3],confint(lm_LIG_ado)[3],confint(lm_LIG_ado)[14],
                    coef(lm_MOD_ado)[3],confint(lm_MOD_ado)[3],confint(lm_MOD_ado)[14],
                    coef(lm_VIG_ado)[3],confint(lm_VIG_ado)[3],confint(lm_VIG_ado)[14]),1)

sex_age <- data.frame(rbind(pre_sex, chi_sex, ado_sex), row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS"))
colnames(sex_age) <- c("SB","2.5%", "97.5%", "LIG","2.5%.1", "97.5%.1", "MOD","2.5%.2", "97.5%.2", "VIG","2.5%.3", "97.5%.3")

group1 <- sprintf("%s (%s ; %s)", sex_age$SB, sex_age$`2.5%`, sex_age$`97.5%`)
group2 <- sprintf("%s (%s ; %s)", sex_age$LIG, sex_age$`2.5%.1`, sex_age$`97.5%.1`)
group3 <- sprintf("%s (%s ; %s)", sex_age$MOD, sex_age$`2.5%.2`, sex_age$`97.5%.2`)
group4 <- sprintf("%s (%s ; %s)", sex_age$VIG, sex_age$`2.5%.3`, sex_age$`97.5%.3`)

sex_age2 <- data.frame(group1, group2, group3, group4, row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS"))
colnames(sex_age2) <- c("SB", "LIG","MOD","VIG")

write.xlsx(sex_age2,"C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/WE_sex_age_new.xlsx",rowNames = TRUE)
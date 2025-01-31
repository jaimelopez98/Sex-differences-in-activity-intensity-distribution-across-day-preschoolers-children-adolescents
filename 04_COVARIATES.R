# Script name: 04_COVARIATES.R
# 
# Author: J.López , Inserm
#
# Doing: 
#   *Preparation of covariates.
#   *creation of a table with characteristics of study population (TABLE 1). 
#   *Creation of accelerometer data differences by age and sex (TABLE 2).

#PACKAGES ----

library(tidyr)
library(plyr)
library(dplyr)
library(haven)
library(purrr)
library(lubridate)
library(openxlsx)

# FUNCTIONS ----

categorical = function(x){
  # Author: Ian Danilevicz 
  # Goal: this function transform a vector in a dummy matrix
  # Purpose: this function is useful to transform categorical age in a handle matrix 
  p = length(table(x))
  n = length(x)
  B = matrix(0, ncol = (p-1),nrow = n)
  for(j in 1:(p-1)){
    B[,j] = ifelse(x==j, 1, 0)
  }
  x = as.factor(x)
  colnames(B) = levels(x)[-p]
  return(B)
}

# 1) LOAD PHYSICAL ACTIVITY DATA AND MERGE WITH SOCIO-DEMOGRAPHIC DATA ----

act5_sum <- read.csv("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/part5_personsummary.csv")

labda_demo <- read.csv("C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/demo_data.csv") %>%
  merge(.,act5_sum, by ="ID")

# 2) PREPARATION OF COVARIATES ----

labda_demo <- labda_demo %>% mutate (AGE_fact = as.factor(ifelse(AGE >=3 & AGE < 6, 0, 
                                                                 ifelse(AGE >= 6 & AGE < 11, 1,2))),
                                     EDUCATION_P_fact = as.factor(ifelse(EDUCATION_P == 1,0,
                                                                         ifelse(EDUCATION_P == 2,1,2))),
                                     STUDY = as.factor(ifelse(STUDY == 1,1,
                                                                         ifelse(STUDY == 2,2,0))),
                                     season = as.factor(season))
# AGE (Preschoolers, Children, and Adolescents)
AGE_cat <- categorical(labda_demo$AGE_fact)

labda_demo$age1 <- AGE_cat[,1]
labda_demo$age2 <- AGE_cat[,2]

# PARENTS' EDUCATION LEVEL (Upper secondary education, University and Higher university)
EDU_cat <- categorical(labda_demo$EDUCATION_P_fact)

labda_demo$edu_cat1 <- EDU_cat[,1]
labda_demo$edu_cat2 <- EDU_cat[,2]

# SEASON (Winter, Spring, Summer, Autumn)
season_cat <- categorical(labda_demo$season)
labda_demo$season1 <- season_cat[,1]
labda_demo$season2 <- season_cat[,2]
labda_demo$season3 <- season_cat[,3]

# STUDY (BFCS, ACTNOW, ASK)
STUDY_cat <- categorical(labda_demo$STUDY)

labda_demo$study1 <- STUDY_cat[,1]
labda_demo$study2 <- STUDY_cat[,2]

write.csv(labda_demo,"C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/labda_demo.csv", row.names = FALSE)

labda_data <- labda_demo %>% select(,-ends_with("_pla")) %>% # only include weekdays and week-end days
  mutate(O_waking_time_WD = dur_min_WD - 16*60, # calculate wear time centered by the total of waking time (16h)
         O_waking_time_WE = dur_min_WE - 16*60) # calculate wear time centered by the total of waking time (16h)

write.csv(labda_data,"C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/labda_data.csv", row.names = FALSE)

# 2) CHARACTERISTICS OF STUDY POPULATION (TABLE 1) ----------------------------------------

# AGE
ta_labda <- labda_data %>% 
  select(ID, SEX,AGE) %>% 
  gather(key="activity_behaviour", value = "duration", AGE) %>%
  mutate(SEX = recode(SEX, "0" = "Boys", "1" = "Girls"),
         activity_behaviour = recode(activity_behaviour,
                                     "AGE" = "Age, mean (SD)"),
         activity_behaviour = factor(activity_behaviour, levels = c("Age, mean (SD)"))) %>%
  group_by(SEX, activity_behaviour) %>%
  summarise(mean_duration = mean(duration),
            sd_duration = sd(duration)) %>% 
  mutate(lab = paste0(format(round(mean_duration, 2), nsmall = 1), " (", format(round(sd_duration, 2), nsmall = 1), ")")) %>%
  select(-mean_duration, -sd_duration) %>%
  spread(key = "SEX", value = lab) %>% 
  right_join(data.frame(activity_behaviour = c("Age, mean (SD)"),
                        p.value = c(t.test(labda_data[which(labda_data$SEX == 0),"AGE"], 
                                           labda_data[which(labda_data$SEX == 1),"AGE"])$p.value)), by = "activity_behaviour") %>%
  mutate(p.value = as.numeric(.$p.value),
         p.value = round(p.value, digits = 3),
         p.value = ifelse(.$p.value < 0.001,"< 0.001", p.value)) #number of digits wanted

# PARENTS' EDUCATION LEVEL
tcov_labda <- labda_data %>% 
  select(ID, SEX, EDUCATION_P) %>%
  mutate(SEX = if_else(SEX == 0, "Boys", "Girls"),
         EDUCATION_P = if_else(EDUCATION_P == 1, "Low Education, n (%)", ifelse(EDUCATION_P == 2,"Mid Education, n (%)", "High Education, n (%)"))) %>%
  rename("Education Parents"= "EDUCATION_P") %>% 
  gather(key = "Variable", value = "value", -ID, -SEX) %>%
  group_by(SEX, Variable, value) %>% 
  count() %>%
  group_by(SEX, Variable) %>%
  mutate(freq = (n/sum(n))*100) %>% 
  mutate(lab = paste0(n, " (", round(freq, digits = 2), ")")) %>%
  ungroup() %>% 
  select(-n, -freq) %>%
  spread(key = SEX, value = "lab") %>% 
  rename("Value" = "value") %>% 
  mutate(Value = factor(Value, levels = c("Low Education, n (%)", "Mid Education, n (%)","High Education, n (%)"))) %>% 
  arrange(Value) %>%
  mutate(Value = as.character(Value)) %>%
  left_join(., 
            labda_data %>% 
              select(ID, SEX, EDUCATION_P) %>%
              mutate( Value = EDUCATION_P,
                      p.value = ifelse(EDUCATION_P == 1, 
                                       round(chisq.test(SEX, Value == 1)$p.value, digits =3),
                                       ifelse(Value == 2, 
                                              round(chisq.test(SEX, Value == 2)$p.value, digits =3),
                                              round(chisq.test(SEX, Value == 3)$p.value, digits =3)))) %>%
              group_by(Value) %>%
              summarise(p.value = mean(p.value, na.rm = TRUE)) %>%
              mutate(Value = case_when(
                Value == 1 ~ "Low Education, n (%)",
                Value == 2 ~ "Mid Education, n (%)",
                Value == 3 ~ "High Education, n (%)",
                TRUE ~ NA_character_
              )) ,
            by = "Value") %>%
  filter(Value %in% c("Low Education, n (%)", "Mid Education, n (%)","High Education, n (%)")) %>%
  select(-Variable)%>%
  rename(activity_behaviour = Value)

# STUDY
tcov_labda2 <- labda_data %>% 
  select(ID, SEX, STUDY) %>%
  mutate(SEX = if_else(SEX == 0, "Boys", "Girls"),
         STUDY = if_else(STUDY == 1, "ACTNOW, n (%)", ifelse(STUDY == 2,"ASK, n (%)", "BELGIUM SURVEY, n (%)"))) %>%
  rename("STUDY"= "STUDY") %>% 
  gather(key = "Variable", value = "value", -ID, -SEX) %>%
  group_by(SEX, Variable, value) %>% 
  count() %>%
  group_by(SEX, Variable) %>%
  mutate(freq = (n/sum(n))*100) %>% 
  mutate(lab = paste0(n, " (", round(freq, digits = 2), ")")) %>%
  ungroup() %>% 
  select(-n, -freq) %>%
  spread(key = SEX, value = "lab") %>% 
  rename("Value" = "value") %>% 
  mutate(Value = factor(Value, levels = c("ACTNOW, n (%)", "ASK, n (%)","BELGIUM SURVEY, n (%)"))) %>% 
  arrange(Value) %>%
  mutate(Value = as.character(Value)) %>%
  left_join(., 
            labda_data %>% 
              select(ID, SEX, STUDY) %>%
              mutate( Value = STUDY,
                      p.value = ifelse(STUDY == 1, 
                                       round(chisq.test(SEX, Value == 1)$p.value, digits =3),
                                       ifelse(Value == 2, 
                                              round(chisq.test(SEX, Value == 2)$p.value, digits =3),
                                              round(chisq.test(SEX, Value == 0)$p.value, digits =3)))) %>%
              group_by(Value) %>%
              summarise(p.value = mean(p.value, na.rm = TRUE)) %>%
              mutate(Value = case_when(
                Value == 1 ~ "ACTNOW, n (%)",
                Value == 2 ~ "ASK, n (%)",
                Value == 0 ~ "BELGIUM SURVEY, n (%)",
                TRUE ~ NA_character_
              )) ,
            by = "Value") %>%
  filter(Value %in% c("ACTNOW, n (%)", "ASK, n (%)","BELGIUM SURVEY, n (%)")) %>%
  select(-Variable)%>%
  rename(activity_behaviour = Value)

explora_demo <- rbind(ta_labda,tcov_labda,tcov_labda2)

write.csv(explora_demo, "C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/exploratory_demo.csv", row.names = FALSE)


# 3) MEAN AND SD OF ACCELEOROMETER DATA IN EACH AGE GROUP STRATIFIED BY SEX (TABLE 2) ----

ta_labda <- labda_data %>% 
  select(ID, SEX,AGE, starts_with("dur")) %>% 
  gather(key="activity_behaviour", value = "duration", AGE,starts_with("dur")) %>%
  mutate(SEX = recode(SEX, "0" = "Boys", "1" = "Girls"),
         activity_behaviour = recode(activity_behaviour,
                                     "AGE" = "Age, mean (SD)",
                                     "dur_min_WD" = "WD Waking time, mean (SD)",
                                     "dur_SB_min_WD" = "WD SB time, mean (SD)",
                                     "dur_LIG_min_WD" = "WD LIPA time, mean (SD)",
                                     "dur_MOD_min_WD" = "WD MOD time, mean (SD)",
                                     "dur_VIG_min_WD" = "WD VIG time, mean (SD)",
                                     "dur_min_WE" = "WE Waking time, mean (SD)",
                                     "dur_SB_min_WE" = "WE SB time, mean (SD)",
                                     "dur_LIG_min_WE" = "WE LIPA time, mean (SD)",
                                     "dur_MOD_min_WE" = "WE MOD time, mean (SD)",
                                     "dur_VIG_min_WE" = "WE VIG time, mean (SD)"),
         activity_behaviour = factor(activity_behaviour, levels = c("Age, mean (SD)",
                                                                    "WD Waking time, mean (SD)","WD SB time, mean (SD)","WD LIPA time, mean (SD)","WD MOD time, mean (SD)","WD VIG time, mean (SD)",
                                                                    "WE Waking time, mean (SD)","WE SB time, mean (SD)","WE LIPA time, mean (SD)","WE MOD time, mean (SD)","WE VIG time, mean (SD)"))) %>%
  group_by(SEX, activity_behaviour) %>%
  summarise(mean_duration = mean(duration),
            sd_duration = sd(duration)) %>% 
  mutate(lab = paste0(format(round(mean_duration, 2), nsmall = 1), " (", format(round(sd_duration, 2), nsmall = 1), ")")) %>%
  select(-mean_duration, -sd_duration) %>%
  spread(key = "SEX", value = lab) %>% 
  right_join(data.frame(activity_behaviour = c("Age, mean (SD)",
                                               "WD Waking time, mean (SD)","WD SB time, mean (SD)","WD LIPA time, mean (SD)","WD MOD time, mean (SD)","WD VIG time, mean (SD)",
                                               "WE Waking time, mean (SD)","WE SB time, mean (SD)","WE LIPA time, mean (SD)","WE MOD time, mean (SD)","WE VIG time, mean (SD)"),
                        p.value = c(t.test(labda_data[which(labda_data$SEX == 0),"AGE"], 
                                           labda_data[which(labda_data$SEX == 1),"AGE"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_min_WD"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_min_WD"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_SB_min_WD"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_SB_min_WD"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_LIG_min_WD"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_LIG_min_WD"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_MOD_min_WD"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_MOD_min_WD"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_VIG_min_WD"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_VIG_min_WD"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_min_WE"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_min_WE"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_SB_min_WE"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_SB_min_WE"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_LIG_min_WE"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_LIG_min_WE"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_MOD_min_WE"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_MOD_min_WE"])$p.value,
                                    t.test(labda_data[which(labda_data$SEX == 0),"dur_VIG_min_WE"], 
                                           labda_data[which(labda_data$SEX == 1),"dur_VIG_min_WE"])$p.value)), by = "activity_behaviour") %>%
  mutate(p.value = as.numeric(.$p.value),
         p.value = round(p.value, digits = 3),
         p.value = ifelse(.$p.value < 0.001,"< 0.001", p.value)) #number of digits wanted

explora_demo <- rbind(tcov_labda,tcov_labda2, tcov_labda3, ta_labda)

write.csv(explora_demo, "C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/exploratory_demo.csv", row.names = FALSE)

# PRE-SCHOOLERS (3-5y)
pre_labda <- labda_data %>%
  filter(AGE_fact==0)
pre_labda <- pre_labda %>%
  select(ID, SEX, starts_with("dur")) %>% 
  gather(key="activity_behaviour", value = "duration",starts_with("dur")) %>%
  mutate(SEX = recode(SEX, "0" = "Boys", "1" = "Girls"),
         activity_behaviour = recode(activity_behaviour,
                                     "dur_min_WD" = "WD Waking time, mean (SD)",
                                     "dur_SB_min_WD" = "WD SB time, mean (SD)",
                                     "dur_LIG_min_WD" = "WD LIPA time, mean (SD)",
                                     "dur_MOD_min_WD" = "WD MOD time, mean (SD)",
                                     "dur_VIG_min_WD" = "WD VIG time, mean (SD)",
                                     "dur_min_WE" = "WE Waking time, mean (SD)",
                                     "dur_SB_min_WE" = "WE SB time, mean (SD)",
                                     "dur_LIG_min_WE" = "WE LIPA time, mean (SD)",
                                     "dur_MOD_min_WE" = "WE MOD time, mean (SD)",
                                     "dur_VIG_min_WE" = "WE VIG time, mean (SD)"),
         activity_behaviour = factor(activity_behaviour, levels = c("WD Waking time, mean (SD)","WD SB time, mean (SD)","WD LIPA time, mean (SD)","WD MOD time, mean (SD)","WD VIG time, mean (SD)",
                                                                    "WE Waking time, mean (SD)","WE SB time, mean (SD)","WE LIPA time, mean (SD)","WE MOD time, mean (SD)","WE VIG time, mean (SD)"))) %>%
  group_by(SEX, activity_behaviour) %>%
  summarise(mean_duration = mean(duration),
            sd_duration = sd(duration)) %>% 
  mutate(lab = paste0(format(round(mean_duration, 2), nsmall = 1), " (", format(round(sd_duration, 2), nsmall = 1), ")")) %>%
  select(-mean_duration, -sd_duration) %>%
  spread(key = "SEX", value = lab) %>% 
  right_join(data.frame(activity_behaviour = c("WD Waking time, mean (SD)","WD SB time, mean (SD)","WD LIPA time, mean (SD)","WD MOD time, mean (SD)","WD VIG time, mean (SD)",
                                               "WE Waking time, mean (SD)","WE SB time, mean (SD)","WE LIPA time, mean (SD)","WE MOD time, mean (SD)","WE VIG time, mean (SD)"),
                        p.value = c(t.test(pre_labda[which(pre_labda$SEX == 0),"dur_min_WD"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_min_WD"])$p.value,
                                    t.test(pre_labda[which(pre_labda$SEX == 0),"dur_SB_min_WD"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_SB_min_WD"])$p.value,
                                    t.test(pre_labda[which(pre_labda$SEX == 0),"dur_LIG_min_WD"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_LIG_min_WD"])$p.value,
                                    t.test(pre_labda[which(pre_labda$SEX == 0),"dur_MOD_min_WD"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_MOD_min_WD"])$p.value,
                                    t.test(pre_labda[which(pre_labda$SEX == 0),"dur_VIG_min_WD"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_VIG_min_WD"])$p.value,
                                    t.test(pre_labda[which(pre_labda$SEX == 0),"dur_min_WE"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_min_WE"])$p.value,
                                    t.test(pre_labda[which(pre_labda$SEX == 0),"dur_SB_min_WE"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_SB_min_WE"])$p.value,
                                    t.test(pre_labda[which(pre_labda$SEX == 0),"dur_LIG_min_WE"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_LIG_min_WE"])$p.value,
                                    t.test(pre_labda[which(pre_labda$SEX == 0),"dur_MOD_min_WE"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_MOD_min_WE"])$p.value,
                                    t.test(pre_labda[which(pre_labda$SEX == 0),"dur_VIG_min_WE"], 
                                           pre_labda[which(pre_labda$SEX == 1),"dur_VIG_min_WE"])$p.value)), by = "activity_behaviour") %>%
  mutate(p.value = as.numeric(.$p.value),
         p.value = round(p.value, digits = 3),
         p.value = ifelse(.$p.value < 0.001,"< 0.001", p.value)) #number of digits wanted

# CHILDREN (6-10y)
chi_labda <- labda_data %>%
  filter(AGE_fact==1) 

chi_labda <- chi_labda %>%
  select(ID, SEX, starts_with("dur")) %>% 
  gather(key="activity_behaviour", value = "duration",starts_with("dur")) %>%
  mutate(SEX = recode(SEX, "0" = "Boys", "1" = "Girls"),
         activity_behaviour = recode(activity_behaviour,
                                     "dur_min_WD" = "WD Waking time, mean (SD)",
                                     "dur_SB_min_WD" = "WD SB time, mean (SD)",
                                     "dur_LIG_min_WD" = "WD LIPA time, mean (SD)",
                                     "dur_MOD_min_WD" = "WD MOD time, mean (SD)",
                                     "dur_VIG_min_WD" = "WD VIG time, mean (SD)",
                                     "dur_min_WE" = "WE Waking time, mean (SD)",
                                     "dur_SB_min_WE" = "WE SB time, mean (SD)",
                                     "dur_LIG_min_WE" = "WE LIPA time, mean (SD)",
                                     "dur_MOD_min_WE" = "WE MOD time, mean (SD)",
                                     "dur_VIG_min_WE" = "WE VIG time, mean (SD)"),
         activity_behaviour = factor(activity_behaviour, levels = c("WD Waking time, mean (SD)","WD SB time, mean (SD)","WD LIPA time, mean (SD)","WD MOD time, mean (SD)","WD VIG time, mean (SD)",
                                                                    "WE Waking time, mean (SD)","WE SB time, mean (SD)","WE LIPA time, mean (SD)","WE MOD time, mean (SD)","WE VIG time, mean (SD)"))) %>%
  group_by(SEX, activity_behaviour) %>%
  summarise(mean_duration = mean(duration),
            sd_duration = sd(duration)) %>% 
  mutate(lab = paste0(format(round(mean_duration, 2), nsmall = 1), " (", format(round(sd_duration, 2), nsmall = 1), ")")) %>%
  select(-mean_duration, -sd_duration) %>%
  spread(key = "SEX", value = lab) %>% 
  right_join(data.frame(activity_behaviour = c("WD Waking time, mean (SD)","WD SB time, mean (SD)","WD LIPA time, mean (SD)","WD MOD time, mean (SD)","WD VIG time, mean (SD)",
                                               "WE Waking time, mean (SD)","WE SB time, mean (SD)","WE LIPA time, mean (SD)","WE MOD time, mean (SD)","WE VIG time, mean (SD)"),
                        p.value = c(t.test(chi_labda[which(chi_labda$SEX == 0),"dur_min_WD"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_min_WD"])$p.value,
                                    t.test(chi_labda[which(chi_labda$SEX == 0),"dur_SB_min_WD"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_SB_min_WD"])$p.value,
                                    t.test(chi_labda[which(chi_labda$SEX == 0),"dur_LIG_min_WD"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_LIG_min_WD"])$p.value,
                                    t.test(chi_labda[which(chi_labda$SEX == 0),"dur_MOD_min_WD"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_MOD_min_WD"])$p.value,
                                    t.test(chi_labda[which(chi_labda$SEX == 0),"dur_VIG_min_WD"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_VIG_min_WD"])$p.value,
                                    t.test(chi_labda[which(chi_labda$SEX == 0),"dur_min_WE"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_min_WE"])$p.value,
                                    t.test(chi_labda[which(chi_labda$SEX == 0),"dur_SB_min_WE"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_SB_min_WE"])$p.value,
                                    t.test(chi_labda[which(chi_labda$SEX == 0),"dur_LIG_min_WE"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_LIG_min_WE"])$p.value,
                                    t.test(chi_labda[which(chi_labda$SEX == 0),"dur_MOD_min_WE"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_MOD_min_WE"])$p.value,
                                    t.test(chi_labda[which(chi_labda$SEX == 0),"dur_VIG_min_WE"], 
                                           chi_labda[which(chi_labda$SEX == 1),"dur_VIG_min_WE"])$p.value)), by = "activity_behaviour") %>%
  mutate(p.value = as.numeric(.$p.value),
         p.value = round(p.value, digits = 3),
         p.value = ifelse(.$p.value < 0.001,"< 0.001", p.value)) #number of digits wanted

# ADOLESCENTS (11-17y)
ado_labda <- labda_data %>%
  filter(AGE_fact==2) 

ado_labda <- ado_labda %>%
  select(ID, SEX, starts_with("dur")) %>% 
  gather(key="activity_behaviour", value = "duration",starts_with("dur")) %>%
  mutate(SEX = recode(SEX, "0" = "Boys", "1" = "Girls"),
         activity_behaviour = recode(activity_behaviour,
                                     "dur_min_WD" = "WD Waking time, mean (SD)",
                                     "dur_SB_min_WD" = "WD SB time, mean (SD)",
                                     "dur_LIG_min_WD" = "WD LIPA time, mean (SD)",
                                     "dur_MOD_min_WD" = "WD MOD time, mean (SD)",
                                     "dur_VIG_min_WD" = "WD VIG time, mean (SD)",
                                     "dur_min_WE" = "WE Waking time, mean (SD)",
                                     "dur_SB_min_WE" = "WE SB time, mean (SD)",
                                     "dur_LIG_min_WE" = "WE LIPA time, mean (SD)",
                                     "dur_MOD_min_WE" = "WE MOD time, mean (SD)",
                                     "dur_VIG_min_WE" = "WE VIG time, mean (SD)"),
         activity_behaviour = factor(activity_behaviour, levels = c("WD Waking time, mean (SD)","WD SB time, mean (SD)","WD LIPA time, mean (SD)","WD MOD time, mean (SD)","WD VIG time, mean (SD)",
                                                                    "WE Waking time, mean (SD)","WE SB time, mean (SD)","WE LIPA time, mean (SD)","WE MOD time, mean (SD)","WE VIG time, mean (SD)"))) %>%
  group_by(SEX, activity_behaviour) %>%
  summarise(mean_duration = mean(duration),
            sd_duration = sd(duration)) %>% 
  mutate(lab = paste0(format(round(mean_duration, 2), nsmall = 1), " (", format(round(sd_duration, 2), nsmall = 1), ")")) %>%
  select(-mean_duration, -sd_duration) %>%
  spread(key = "SEX", value = lab) %>% 
  right_join(data.frame(activity_behaviour = c("WD Waking time, mean (SD)","WD SB time, mean (SD)","WD LIPA time, mean (SD)","WD MOD time, mean (SD)","WD VIG time, mean (SD)",
                                               "WE Waking time, mean (SD)","WE SB time, mean (SD)","WE LIPA time, mean (SD)","WE MOD time, mean (SD)","WE VIG time, mean (SD)"),
                        p.value = c(t.test(ado_labda[which(ado_labda$SEX == 0),"dur_min_WD"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_min_WD"])$p.value,
                                    t.test(ado_labda[which(ado_labda$SEX == 0),"dur_SB_min_WD"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_SB_min_WD"])$p.value,
                                    t.test(ado_labda[which(ado_labda$SEX == 0),"dur_LIG_min_WD"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_LIG_min_WD"])$p.value,
                                    t.test(ado_labda[which(ado_labda$SEX == 0),"dur_MOD_min_WD"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_MOD_min_WD"])$p.value,
                                    t.test(ado_labda[which(ado_labda$SEX == 0),"dur_VIG_min_WD"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_VIG_min_WD"])$p.value,
                                    t.test(ado_labda[which(ado_labda$SEX == 0),"dur_min_WE"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_min_WE"])$p.value,
                                    t.test(ado_labda[which(ado_labda$SEX == 0),"dur_SB_min_WE"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_SB_min_WE"])$p.value,
                                    t.test(ado_labda[which(ado_labda$SEX == 0),"dur_LIG_min_WE"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_LIG_min_WE"])$p.value,
                                    t.test(ado_labda[which(ado_labda$SEX == 0),"dur_MOD_min_WE"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_MOD_min_WE"])$p.value,
                                    t.test(ado_labda[which(ado_labda$SEX == 0),"dur_VIG_min_WE"], 
                                           ado_labda[which(ado_labda$SEX == 1),"dur_VIG_min_WE"])$p.value)), by = "activity_behaviour") %>%
  mutate(p.value = as.numeric(.$p.value),
         p.value = round(p.value, digits = 3),
         p.value = ifelse(.$p.value < 0.001,"< 0.001", p.value)) #number of digits wanted

explora_pa <- rbind(pre_labda,chi_labda, ado_labda)

write.csv(explora_pa, "C:/Users/jlopezgarcia/Desktop/paper1/analysis/DATA/exploratory_pa.csv", row.names = FALSE)
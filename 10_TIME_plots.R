# Script name: 10_TIME_plots.R
# 
# Author: J.López, Inserm
#
# Doing: 
#   *Plots from models obtained using function-on scalar regression over the time of the day on weekdays and week-end days and for:
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

# 0) LOAD PREDICTIONS FROM FUNCTIONAL MODELS ----

# PRESCHOOLERS (n=1057)
load("C:/Users/j_lopez/Desktop/paper1/analysis/DATA/TIME/PREDICTIONS/pred_obj_wd_pre.rda") # WEEKDAYS
load("C:/Users/j_lopez/Desktop/paper1/analysis/DATA/TIME/PREDICTIONS/pred_obj_we_pre.rda") # WEEK-END DAYS

# CHILDREN (n=1153)
load("C:/Users/j_lopez/Desktop/paper1/analysis/DATA/TIME/PREDICTIONS/pred_obj_wd_chi.rda") # WEEKDAYS
load("C:/Users/j_lopez/Desktop/paper1/analysis/DATA/TIME/PREDICTIONS/pred_obj_we_chi.rda") # WEEK-END DAYS

# ADOLESCENTS (n=297)
load("C:/Users/j_lopez/Desktop/paper1/analysis/DATA/TIME/PREDICTIONS/pred_obj_wd_ado.rda") # WEEKDAYS
load("C:/Users/j_lopez/Desktop/paper1/analysis/DATA/TIME/PREDICTIONS/pred_obj_we_ado.rda") # WEEK-END DAYS

xi <- seq(6,22, length = 960)

# 1) EXTRACTION COEFFICIENTS (fitted values and standard error)  ----

# FITTED values
fit_sex_wd_pre <- pred_obj_wd_pre$fit$`s(xi.vec):SEX`[1,]
fit_sex_we_pre <- pred_obj_we_pre$fit$`s(xi.vec):SEX`[1,]

fit_sex_wd_chi <- pred_obj_wd_chi$fit$`s(xi.vec):SEX`[1,]
fit_sex_we_chi <- pred_obj_we_chi$fit$`s(xi.vec):SEX`[1,]

fit_sex_wd_ado <- pred_obj_wd_ado$fit$`s(xi.vec):SEX`[1,]
fit_sex_we_ado <- pred_obj_we_ado$fit$`s(xi.vec):SEX`[1,]

fit_all <-
  tibble(
    pre_wd = fit_sex_wd_pre,
    pre_we = fit_sex_we_pre,
    chi_wd = fit_sex_wd_chi,
    chi_we = fit_sex_we_chi,
    prea_wd = fit_sex_wd_ado,
    prea_we = fit_sex_we_ado)

# SE values
se_sex_wd_pre <- pred_obj_wd_pre$se$`s(xi.vec):SEX`[1,]
se_sex_we_pre <- pred_obj_we_pre$se$`s(xi.vec):SEX`[1,]

se_sex_wd_chi <- pred_obj_wd_chi$se$`s(xi.vec):SEX`[1,]
se_sex_we_chi <- pred_obj_we_chi$se$`s(xi.vec):SEX`[1,]

se_sex_wd_ado <- pred_obj_wd_ado$se$`s(xi.vec):SEX`[1,]
se_sex_we_ado <- pred_obj_we_ado$se$`s(xi.vec):SEX`[1,]

se_all <-
  tibble(
    pre_wd = se_sex_wd_pre,
    pre_we = se_sex_we_pre,
    chi_wd = se_sex_wd_chi,
    chi_we = se_sex_we_chi,
    ado_wd = se_sex_wd_ado,
    ado_we = se_sex_we_ado)

# 2) CREATE A DATAFRAME WITH ALL COEFFICIENTS ----

coef_all =
  tibble(
    type = c("weekday","weekend","weekday","weekend","weekday","weekend"),
    Day = c(
    "0.Pre-schoolers (n=1057)","3.Pre-schoolers (n=1057)",
    "1.Children (n=1153)","4.Children (n=1153)", 
    "2.Adolescents (n=297)", "5.Adolescents (n=297)"),
    diff = tfd(t(fit_all), arg=xi),
    se = tfd(t(se_all), arg=xi)) %>%
  mutate(
    ub =diff + 1.96 * se,
    lb =diff - 1.96 * se)

# 3) PLOT ----

coef_all %>% 
  ggplot(aes(y=diff))+
  geom_spaghetti()+
  geom_errorband(aes(ymax = ub,ymin = lb, fill = type))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 125, col= "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_x_continuous(breaks = seq(6, 22, by = 1)) +
  scale_fill_manual(values = c("orange", "green","orange", "green","orange", "green"))+
  facet_wrap(~Day, scales = "free_x",nrow = 2) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                     size = 20,
                     face = "bold"),
        axis.title = element_text(size = 15), 
        strip.text = element_text(size = 20,
                                  face = "bold"),
        panel.spacing = unit(3, "lines"))


# Script name: 17_ACT_plots.R
# 
# Author: J.López, Inserm
#
# Doing: 
#   *Plots from models obtained using function-on scalar regression over the full range of the activity intensity distribution on weekdays and week-end days and for:
#     *Pre-schoolers (n=1057)
#     *Children (n=1153)
#     *Adolescents (n=297)

# PACKAGES ----

library(dplyr)
library(tidyr)
library(tidyfun)
library(ggplot2)
library(purrr)
library(refund)

# PREDICTIONS FROM FUNCTIONAL MODELS ----

# PRESCHOOLERS (n=1057)
load("/path/pred_obj_wd_pre.rda") # WEEKDAYS
load("/path/pred_obj_we_pre.rda") # WEEK-END DAYS

# CHILDREN (n=1153)
load("/path/pred_obj_wd_chi.rda") # WEEKDAYS
load("/path/pred_obj_we_chi.rda") # WEEK-END DAYS

# ADOLESCENTS (n=297)
load("/path/pred_obj_wd_ado.rda") # WEEKDAYS
load("/path/pred_obj_we_ado.rda") # WEEK-END DAYS

load("/path/WD_c_value.rda") # WEEKDAYS
xi <- seq(0,3000,length.out = 200)

load("/path/WE_c_value.rda") # WEEKENDS
xi_we <- seq(0,3000,length.out = 200)

# 1) EXTRACTION COEFFICIENTS (fitted values and standard error)  ----

# FITTED values

fit_sex_wd_pre <- pred_obj_wd_pre$fit$`s(xi_wd.vec):SEX`[1,]
fit_sex_we_pre <- pred_obj_we_pre$fit$`s(xi_we.vec):SEX`[1,]

fit_sex_wd_chi <- pred_obj_wd_chi$fit$`s(xi_wd.vec):SEX`[1,]
fit_sex_we_chi <- pred_obj_we_chi$fit$`s(xi_we.vec):SEX`[1,]

fit_sex_wd_ado <- pred_obj_wd_ado$fit$`s(xi_wd.vec):SEX`[1,]
fit_sex_we_ado <- pred_obj_we_ado$fit$`s(xi_we.vec):SEX`[1,]

fit_wd <-
  tibble(
    xi_wd = xi_wd,
    pre_wd = fit_sex_wd_pre,
    chi_wd = fit_sex_wd_chi,
    ado_wd = fit_sex_wd_ado)

fit_we <-
  tibble(
    xi_we = xi_we,
    pre_we = fit_sex_we_pre,
    chi_we = fit_sex_we_chi,
    ado_we = fit_sex_we_ado)

# SE values

se_sex_wd_pre <- pred_obj_wd_pre$se.fit$`s(xi_wd.vec):SEX`[1,]
se_sex_we_pre <- pred_obj_we_pre$se.fit$`s(xi_we.vec):SEX`[1,]

se_sex_wd_chi <- pred_obj_wd_chi$se.fit$`s(xi_wd.vec):SEX`[1,]
se_sex_we_chi <- pred_obj_we_chi$se.fit$`s(xi_we.vec):SEX`[1,]

se_sex_wd_ado <- pred_obj_wd_ado$se.fit$`s(xi_wd.vec):SEX`[1,]
se_sex_we_ado <- pred_obj_we_ado$se.fit$`s(xi_we.vec):SEX`[1,]

se_wd <-
  tibble(
    xi_wd = xi_wd,
    pre_wd = se_sex_wd_pre,
    chi_wd = se_sex_wd_chi,
    ado_wd = se_sex_wd_ado)

se_we <-
  tibble(
    xi_we = xi_we,
    pre_we = se_sex_we_pre,
    chi_we = se_sex_we_chi,
    ado_we = se_sex_we_ado)

# 2) CREATE A DATAFRAME WITH ALL COEFFICIENTS ----

fit_all <-
  tibble(
    pre_wd = fit_sex_wd_pre,
    pre_we = fit_sex_we_pre,
    chi_wd = fit_sex_wd_chi,
    chi_we = fit_sex_we_chi,
    ado_wd = fit_sex_wd_ado,
    ado_we = fit_sex_we_ado) %>%
  mutate(ado_wd = ifelse(fit_sex_wd_ado > 0.35,NA, fit_sex_wd_ado))


se_all <-
  tibble(
    pre_wd = se_sex_wd_pre,
    pre_we = se_sex_we_pre,
    chi_wd = se_sex_wd_chi,
    chi_we = se_sex_we_chi,
    ado_wd = se_sex_wd_ado,
    ado_we = se_sex_we_ado)

coef_all =
  tibble(
    Day = c(
      "0.Pre-schoolers (n=1057)","5.Pre-schoolers (n=1057)",
      "1.Children (n=1153)","6.Children (n=1153)", 
      "2.Adolescents (n=297)", "7.Adolescents (n=297)"),
    diff = tfd(t(fit_all), arg=xi),
    se = tfd(t(se_all), arg=xi)) %>%
  mutate(
    ub =diff + 1.96 * se,
    lb =diff - 1.96 * se)

# 3) PLOT ----

ggplot(aes(y = diff)) +
  geom_spaghetti() +
  geom_errorband(aes(ymax = ub, ymin = lb, fill = Day)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = c(180, 756, 1111), linetype = "dashed", color = "red") +
  geom_hline(yintercept = -0.4, color = "gray") +
  geom_hline(yintercept = 0.4, color = "gray") +
  scale_x_continuous(breaks = seq(0, 2000, by = 250), limits = c(0, 2000)) +
  scale_fill_manual(values = c("orange", "orange", "orange", "green", "green", "green")) +
  facet_wrap(~Day, scales = "free_x", nrow = 2) +
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


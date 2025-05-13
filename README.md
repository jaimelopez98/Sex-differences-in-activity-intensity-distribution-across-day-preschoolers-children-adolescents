
# Sex differences in the activity intensity distribution throughout the day in preschoolers, children and adolescents: A multi-study analysis.

Code to examine how the physical activity (PA) differed by sex from age 3 to 17 depending on the day of the the week and across the time, using three different approaches:
  1) Linear regression to calculate sex differences considering time spent in sedentary behaviour, light PA, moderate PA and vigorous PA.
  2) Functional scalar regression, to calculate sex differences in PA intensity across the time (waking hours)
  3) Functional scalar regression to calculate sex differences in time spent in a specific activity intensity range.

Data were analysed using R 3.6.1 (http://www.r-project.org), analyses required downloading of the following packages:
- GGIR for accelerometer data processing (version 3.0.9, https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html)
- ks for kernel smoothing (version 1.14.2, https://cran.r-project.org/web/packages/ks/ks.pdf)
- REFUND for all function-on-scalar regressions (version 0.1.36, https://cran.r-project.org/web/packages/refund/refund.pdf)
- pracma for trapezoidal integration of functional coefficients (version 2.4.4, https://cran.r-project.org/web/packages/pracma/pracma.pdf)
- tidyfun for data preparation (version 0.0.98, https://tidyfun.github.io/tidyfun/index.html)
- ggplot2 for illustratibng plots of functional coefficients (version 3.5.1, https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf)

Here is a schema of the workflow: 

![readme](https://github.com/user-attachments/assets/a9959ccb-954d-4f75-82ef-67612fb9ee4c)

More details on each steps are provided in the following sections:

## Step 0 - Data preparation

- Data processing, a modified code to process accelerometer data using GGIR packages, including the combination of two algoriythm to detect the sleep window in hip-count accelerometer devices. (01_DATA_processing)
- Data extraction, this code was used to identify the waking window, identify valid participants and calculate the time spent in SB and each PA intensity using the cutpoint approach. (02_DATA_extraction)
- Data aggregation, for functional analysis over the day, time-series were aggregate in one-minute epoch using this code. (03_DATA_aggregation)

## Step 1 - Exploratory analysis 

 - Covariates preparation and description of sample (Table 1 and Table 2 from the manuscript). (04_COVARIATES)
 - Interaction tests, including one for triple interaction (age*sex*daytype) and two for double interaction(age*daytype, and sex*daytype). (05_INTERACTIONS)

## Step 2 - Linear regression approach 

- Preparation of data for each age group and type of day. (06_LINEAR_approach)
- Modelling for SB and each PA intensity on weekdays and week-end days, separately.
- Extracting of coefficients from linear regression models (Table 3 from the manuscript).

## Step 3 -  Functional approach across the time of the day 

- Preparation of data for modelling in each age group and type of day. (07_TIME_data)
- Modelling fit conducted in pre-schoolers, children and adolescents on weekdays and week-end days, separately.(08_TIME_models)
- Predictiong the coefficients of sex differences for each age group and daytype. (09_TIME_predictions)
- Ploting the sex differences across the day (Figure 2 from the manuscript). (10_TIME_plots)

## Step 4 - Functional approach over the full range of the activity intensity distribution

- Characterising the probability density function of each individual using the kernel smoothing method. (11_ACT_kernel)
- Standardizing kernel densities. (12_ACT_standard)
- Estimating activity distribution (13_ACT_distribution)
- Preparation of data for modelling in each age group and type of day. (14_ACT_data)
- Modelling fit conducted in pre-schoolers, children and adolescents on weekdays and week-end days, separately. (15_ACT_models)
- Predictiong the coefficients of sex differences for each age group and daytype. (16_ACT_predictions)
- Ploting the sex differences across the day (Figure 3  from manustript). (17_ACT_plots)

More information on the computation of the activity distribution function in the following document: create.pdf

(link to paper doi:)

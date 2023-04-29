#------------------------------------------------------------------------------------------------------------------------
#                                                     LIBRARIES
#------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(plm)
library(systemfit)
library(estimatr)
library(modelsummary)
library(fixest)
library(stargazer)
library(haven)
library(lmtest)
library(AER)
library(sandwich)
library(car)
library(tibble)
library(data.table)
library(recipes)
library(multiwayvcov)
library(fastDummies)
library(fixest)

#-------------------------------------------------------------------------------------------------------------------
#                                                       START
#-------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------
# DATA
#-----------------------------------------------

# Complete sample census data
census <- readRDS("census.rds")

# Fixed effects to be used in the regressions
state_fe <- c(colnames(census)[46:93])
yob_fe_40to49 <- c(colnames(census)[99:127])
yob_fe <- c(colnames(census)[94:142])
instruments <- c(colnames(census)[37:39])
interactions_complete <- c(colnames(census)[148:294])
interactions_south_40to49 <- c(colnames(census)[153:181])
interactions_west_40to49 <- c(colnames(census)[202:230])
interactions_midwest_40to49 <- c(colnames(census)[251:279])
interactions_40to49 <- c(interactions_midwest_40to49,interactions_south_40to49,interactions_west_40to49)
additional_controls <- c("age","age2","age3","age4","census70","census80")
gender <- c("male")

# For columns I and II
census_40to49 <- readRDS("census_40to49.rds")
# For columns III and IV
census_whites_male <- readRDS("census_whites_male.rds")
# For columns V and VI
census_whites <- readRDS("census_whites.rds")
# For columns VII and VIII
census_whites_non_south <- readRDS("census_whites_non_south.rds")
census_whites_south <- readRDS("census_whites_south.rds")


#-----------------------------------------------
# Creating FIGURE I
#-----------------------------------------------

# Need to create de adjusted weight measure in the data before doing this
## To be added latter

# Plotting the density
ggplot(census_whites) +
  geom_bar(aes(x = numlawyears,weight = adjwt),fill = "red",just = 2,width = 0.2)+
  geom_bar(aes(x = cl,weight = adjwt),fill = "blue",just = 1,width = 0.2)+
  geom_bar(aes(x = ca,weight = adjwt),fill = "green",just = 0,width = 0.2)+
  geom_bar(aes(x = cca,weight = adjwt),fill = "yellow",just = -1,width = 0.2)

#-----------------------------------------------
# TABLE I
#-----------------------------------------------


#-------------
# COMLUMN I
#-------------

### OLS Estimation for the first column
# The only way to get the exact estimator found in stata is not using the slwt variable, don't know why :/
col1_ols <- feols(lnwkwage ~ gradcap | bplg + yob,
                  data = census_40to49,
                  weights = ~slwt,
                  cluster = ~yob_bplg)
summary(col1_ols)


### IV Regression for the first column
col1_1st_stage <- feols(gradcap ~ ny7 + ny8 + ny9 | bplg + yob,
                        data = census_40to49,
                        weights = ~slwt,
                        cluster = ~yob_bplg)
summary(col1_1st_stage)
grad_hat <- col1_1st_stage$fitted.values # Fitted values 


### Second stage of the first column
# Again the estimates are a bit different from the ones in stata
# Still need to create the correct F statistic and the correct CLR bounds
col1_iv <- feols(lnwkwage ~ 1 | yob + bplg | gradcap ~ ny7 + ny8 + ny9,
                 data = census_40to49,
                 weights = ~slwt,
                 cluster = ~yob_bplg)
summary(col1_iv)

#-------------
# COMLUMN II
#-------------

### OLS regression for column 2 
col2_ols <- feols(lnwkwage ~ gradcap | bplg + yob + yob^region,
                  data = census_40to49,
                  weights = ~slwt,
                  cluster = ~yob_bplg)
summary(col2_ols)

### IV regression for column 2
#### First stage
col2_1st_stage <- feols(gradcap ~ ny7 + ny8 + ny9 | yob + bplg + yob^region,
                        data = census_40to49,
                        weights = ~slwt,
                        cluster = ~yob_bplg)
summary(col2_1st_stage)

### IV estimation for column 2
col2_iv <- feols(lnwkwage ~ 1 | yob + bplg + yob^region | gradcap ~ ny7 + ny8 + ny9,
                 data = census_40to49,
                 weights = ~slwt,
                 cluster = ~yob_bplg)
summary(col2_iv)


#-------------
# COMLUMN III
#-------------

### OLS
col3_ols <- feols(lnwkwage ~ gradcap + age + age2 + age3 + age4 + census70 + census80 | yob + bplg,
                  data = census_whites_male,
                  weights = ~slwt,
                  cluster = ~yob_bplg)
summary(col3_ols)

### 1st Stage 
col3_1st_stage <- feols(gradcap ~ ny7+ny8+ny9+age+age2+age3+age4+census70+census80 | yob + bplg,
                        data = census_whites_male,
                        weights = ~slwt,
                        cluster = ~yob_bplg)
summary(col3_1st_stage)

### IV estimation column 3
col3_iv <- feols(lnwkwage ~ age+age2+age3+age4+census70+census80 | yob + bplg | gradcap ~ ny7 + ny8 + ny9,
                 data = census_whites_male,
                 weights = ~slwt,
                 cluster = ~yob_bplg)
summary(col3_iv)


#-------------
# COMLUMN IV
#-------------


### OLS
col4_ols <- feols(lnwkwage ~ gradcap + age + age2 + age3 + age4 + census70 + census80 | yob + bplg + yob^region,
                  data = census_whites_male,
                  weights = ~slwt,
                  cluster = ~yob_bplg)
summary(col4_ols)

### First stage
col4_1st_stage <- feols(gradcap ~ ny7+ny8+ny9+age+age2+age3+age4+census70+census80 | yob + bplg + yob^region,
                        data = census_whites_male,
                        weights = ~slwt,
                        cluster = ~yob_bplg)
summary(col4_1st_stage)

### 2SLS
col4_iv <- feols(lnwkwage ~ age+age2+age3+age4+census70+census80 | yob + bplg + yob^region | gradcap ~ ny7+ny8+ny9,
                 data = census_whites_male,
                 weights = ~slwt,
                 cluster = ~yob_bplg)
summary(col4_iv)

#-------------
# COMLUMN V
#-------------


# OLS 
col5_ols <- feols(lnwkwage ~ gradcap + age+age2+age3+age4+census70+census80+male | yob+bplg,
                  data = census_whites,
                  weights = ~slwt,
                  cluster = ~yob_bplg)
summary(col5_ols)


# First stage of the IV
col5_1st_stage <- feols(gradcap ~ ny7+ny8+ny9+age+age2+age3+age4+census70+census80+male | yob+bplg,
                        data = census_whites,
                        weights = ~slwt,
                        cluster = ~yob_bplg)
summary(col5_1st_stage)

# 2SLS
col4_iv <- feols(lnwkwage ~ age+age2+age3+age4+census70+census80+male |yob+bplg| gradcap ~ ny7+ny8+ny9,
                 data = census_whites,
                 weights = ~slwt,
                 cluster = ~yob_bplg)
summary(col4_iv)

#-------------
# COMLUMN VI
#-------------


# OLS 
col6_ols <- feols(lnwkwage ~ gradcap + age+age2+age3+age4+census70+census80+male |yob+bplg+yob^region,
                  data = census_whites,
                  weights = ~slwt,
                  cluster = ~yob_bplg)
summary(col6_ols)


# First stage of the IV
col6_1st_stage <- feols(gradcap ~ ny7+ny8+ny9+age+age2+age3+age4+census70+census80+male | yob+bplg+yob^region,
                        data = census_whites,
                        weights = ~slwt,
                        cluster = ~yob_bplg)
summary(col6_1st_stage)

# 2SLS
col6_iv <- feols(lnwkwage ~ age+age2+age3+age4+census70+census80+male |yob+bplg+yob^region| gradcap ~ ny7+ny8+ny9,
                 data = census_whites,
                 weights = ~slwt,
                 cluster = ~yob_bplg)
summary(col6_iv)

#-------------
# COMLUMN VII
#-------------

# OLS 
col7_ols <- feols(lnwkwage ~ gradcap + age+age2+age3+age4+census70+census80+male | yob+bplg,
                  data = census_whites_non_south,
                  weights = ~slwt,
                  cluster = ~yob_bplg)
summary(col7_ols)


# First stage of the IV
col7_1st_stage <- feols(gradcap ~ ny7+ny8+ny9 + age+age2+age3+age4+census70+census80+male | yob+bplg,
                        data = census_whites_non_south,
                        weights = ~slwt,
                        cluster = ~yob_bplg)
summary(col7_1st_stage)

# 2SLS
col7_iv <- feols(lnwkwage ~ age+age2+age3+age4+census70+census80+male | yob+bplg | gradcap ~ ny7+ny8+ny9,
                 data = census_whites_non_south,
                 weights = ~slwt,
                 cluster = ~yob_bplg)
summary(col7_iv)

#-------------
# COMLUMN VIII
#-------------


# OLS 
col8_ols <- feols(lnwkwage ~ gradcap + age+age2+age3+age4+census70+census80+male | yob+bplg,
                  data = census_whites_south,
                  weights = ~slwt,
                  cluster = ~yob_bplg)
summary(col8_ols)


# First stage of the IV
col8_1st_stage <- feols(gradcap ~ ny7+ny8+ny9 + age+age2+age3+age4+census70+census80+male | yob+bplg,
                        data = census_whites_south,
                        weights = ~slwt,
                        cluster = ~yob_bplg)
summary(col8_1st_stage)

# 2SLS
col8_iv <- feols(lnwkwage ~ age+age2+age3+age4+census70+census80+male | yob+bplg | gradcap ~ ny7+ny8+ny9,
                 data = census_whites_south,
                 weights = ~slwt,
                 cluster = ~yob_bplg)
summary(col8_iv)


#-----------------------------------------------
# TABLE II
#-----------------------------------------------

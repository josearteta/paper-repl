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
col1_ols <- lm(reformulate(c("gradcap",state_fe,yob_fe_40to49),
                           response = "lnwkwage"),
               weights = slwt,
               data = census_40to49)
col1_ols <- feols(lnwkwage ~ gradcap | bplg + yob, data = census_40to49)
summary(col1_ols)
coeftest(col1_ols,vcovCL,cluster = census_40to49$yob_bplg)


### IV Regression for the first column
col1_1st_stage <- lm(reformulate(c(instruments,state_fe,yob_fe_40to49),
                                 response = "gradcap"),
                          weights = slwt,
                          data = census_40to49)
coeftest(col1_1st_stage,vcovCL,cluster = census_40to49$yob_bplg)
grad_hat <- col1_1st_stage$fitted.values # Fitted values 


### Second stage of the first column

### Using FWL for the IV estimation
out_adj <- lm(reformulate(c(state_fe,yob_fe_40to49),
                          response = "lnwkwage"),
              weights = slwt,
              data = census_40to49)$residuals

grad_adj <- lm(reformulate(c(state_fe,yob_fe_40to49),
                          response = "gradcap"),
              weights = slwt,
              data = census_40to49)$residuals

ny7_adj <- lm(reformulate(c(state_fe,yob_fe_40to49),
                          response = "ny7"),
              weights = slwt,
              data = census_40to49)$residuals
ny8_adj <- lm(reformulate(c(state_fe,yob_fe_40to49),
                          response = "ny8"),
              weights = slwt,
              data = census_40to49)$residuals
ny9_adj <- lm(reformulate(c(state_fe,yob_fe_40to49),
                          response = "ny9"),
              weights = slwt,
              data = census_40to49)$residuals
col1_iv <- ivreg(out_adj ~ grad_adj | ny7_adj + ny8_adj + ny9_adj,
                 weights = slwt,
                 data = census_40to49)
summary(col1_iv)

#-------------
# COMLUMN II
#-------------

### OLS regression for column 2 
col2_ols <- lm(reformulate(c("gradcap",state_fe,yob_fe_40to49,interactions_40to49),
                           response = "lnwkwage"),
               weights = slwt,
               data = census_40to49)
coeftest(col2_ols,vcovCL,cluster = census_40to49$yob_bplg)

### IV regression for column 2
#### First stage
col2_1st_stage <- lm(reformulate(c(instruments,state_fe,yob_fe_40to49,interactions_40to49),
                                 response = "gradcap"),
                     weights = slwt,
                     data = census_40to49)
summary(col2_1st_stage)


#### Using FWL to calculate the IV estimation
out_adj <- lm(reformulate(c(state_fe,yob_fe_40to49,interactions_40to49),
                          response = "lnwkwage"),
              weights = slwt,
              data = census_40to49)$residuals

grad_adj <- lm(reformulate(c(state_fe,yob_fe_40to49,interactions_40to49),
                           response = "gradcap"),
               weights = slwt,
               data = census_40to49)$residuals

ny7_adj <- lm(reformulate(c(state_fe,yob_fe_40to49,interactions_40to49),
                          response = "ny7"),
              weights = slwt,
              data = census_40to49)$residuals
ny8_adj <- lm(reformulate(c(state_fe,yob_fe_40to49,interactions_40to49),
                          response = "ny8"),
              weights = slwt,
              data = census_40to49)$residuals
ny9_adj <- lm(reformulate(c(state_fe,yob_fe_40to49,interactions_40to49),
                          response = "ny9"),
              weights = slwt,
              data = census_40to49)$residuals
col2_iv <- ivreg(out_adj ~ grad_adj | ny7_adj + ny8_adj + ny9_adj,
                 weights = slwt,
                 data = census_40to49)
summary(col2_iv)


#-------------
# COMLUMN III
#-------------

### OLS
col3_ols <- lm(reformulate(c("gradcap",additional_controls,state_fe,yob_fe),
                           response = "lnwkwage"),
               weights = slwt,
               data = census_whites_male)
summary(col3_ols)
### 1st Stage 
col3_1st_stage <- lm(reformulate(c(instruments,additional_controls,state_fe,yob_fe),
                                 response = "gradcap"),
                     weights = slwt,
                     data = census_whites_male)
summary(col3_1st_stage)

### 2SLS Using FWL
out_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe),
                          response = "lnwkwage"),
              weights = slwt,
              data = census_whites_male)$residuals

grad_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe),
                          response = "gradcap"),
              weights = slwt,
              data = census_whites_male)$residuals

ny7_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe),
                          response = "ny7"),
              weights = slwt,
              data = census_whites_male)$residuals

ny8_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe),
                          response = "ny8"),
              weights = slwt,
              data = census_whites_male)$residuals

ny9_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe),
                          response = "ny9"),
              weights = slwt,
              data = census_whites_male)$residuals

col3_iv <- ivreg(out_adj ~ grad_adj | ny7_adj + ny8_adj + ny9_adj,
                 weights = slwt,
                 data = census_whites_male)
summary(col3_iv)


#-------------
# COMLUMN IV
#-------------


### OLS
col4_ols <- lm(reformulate(c("gradcap",additional_controls,state_fe,yob_fe,interactions_complete),
                                       response = "lnwkwage"),
                           weights = slwt,
                           data = census_whites_male)
summary(col4_ols)

### First stage

col4_1st_stage <- lm(reformulate(c(instruments, additional_controls,state_fe,yob_fe,interactions_complete),
                                 response = "gradcap"),
                     weights = slwt,
                     data = census_whites_male)
summary(col4_1st_stage)

### 2SLS
out_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,interactions_complete),
                          response = "lnwkwage"),
              weights = slwt,
              data = census_whites_male)$residuals

grad_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,interactions_complete),
                           response = "gradcap"),
               weights = slwt,
               data = census_whites_male)$residuals

ny7_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,interactions_complete),
                          response = "ny7"),
              weights = slwt,
              data = census_whites_male)$residuals

ny8_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,interactions_complete),
                          response = "ny8"),
              weights = slwt,
              data = census_whites_male)$residuals

ny9_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,interactions_complete),
                          response = "ny9"),
              weights = slwt,
              data = census_whites_male)$residuals

col4_iv <- ivreg(out_adj ~ grad_adj | ny7_adj + ny8_adj + ny9_adj,
                 weights = slwt,
                 data = census_whites_male)
summary(col4_iv)

#-------------
# COMLUMN V
#-------------


# OLS 
col5_ols <- lm(reformulate(c("gradcap",additional_controls,state_fe,yob_fe,gender),
                           response = "lnwkwage"),
               weights = slwt,
               data = census_whites)
summary(col5_ols)


# First stage of the IV
col5_1st_stage <- lm(reformulate(c(instruments,additional_controls,state_fe,yob_fe,gender),
                           response = "gradcap"),
               weights = slwt,
               data = census_whites)
summary(col5_1st_stage)

# FWL
out_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "lnwkwage"),
              weights = slwt,
              data = census_whites)$residuals

grad_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                           response = "gradcap"),
               weights = slwt,
               data = census_whites)$residuals

ny7_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "ny7"),
              weights = slwt,
              data = census_whites)$residuals

ny8_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "ny8"),
              weights = slwt,
              data = census_whites)$residuals

ny9_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "ny9"),
              weights = slwt,
              data = census_whites)$residuals

col4_iv <- ivreg(out_adj ~ grad_adj | ny7_adj + ny8_adj + ny9_adj,
                 weights = slwt,
                 data = census_whites)
summary(col4_iv)

#-------------
# COMLUMN VI
#-------------


# OLS 
col6_ols <- lm(reformulate(c("gradcap",additional_controls,state_fe,yob_fe,gender,interactions_complete),
                           response = "lnwkwage"),
               weights = slwt,
               data = census_whites)
summary(col6_ols)


# First stage of the IV
col6_1st_stage <- lm(reformulate(c(instruments,additional_controls,state_fe,yob_fe,gender,interactions_complete),
                                 response = "gradcap"),
                     weights = slwt,
                     data = census_whites)
summary(col6_1st_stage)

# FWL
out_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender,interactions_complete),
                          response = "lnwkwage"),
              weights = slwt,
              data = census_whites)$residuals

grad_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender,interactions_complete),
                           response = "gradcap"),
               weights = slwt,
               data = census_whites)$residuals

ny7_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender,interactions_complete),
                          response = "ny7"),
              weights = slwt,
              data = census_whites)$residuals

ny8_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender,interactions_complete),
                          response = "ny8"),
              weights = slwt,
              data = census_whites)$residuals

ny9_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender,interactions_complete),
                          response = "ny9"),
              weights = slwt,
              data = census_whites)$residuals

col6_iv <- ivreg(out_adj ~ grad_adj | ny7_adj + ny8_adj + ny9_adj,
                 weights = slwt,
                 data = census_whites)
summary(col6_iv)

#-------------
# COMLUMN VII
#-------------


# OLS 
col7_ols <- lm(reformulate(c("gradcap",additional_controls,state_fe,yob_fe,gender),
                           response = "lnwkwage"),
               weights = slwt,
               data = census_whites_non_south)
summary(col7_ols)


# First stage of the IV
col7_1st_stage <- lm(reformulate(c(instruments,additional_controls,state_fe,yob_fe,gender),
                                 response = "gradcap"),
                     weights = slwt,
                     data = census_whites_non_south)
summary(col7_1st_stage)

# FWL
out_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "lnwkwage"),
              weights = slwt,
              data = census_whites_non_south)$residuals

grad_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                           response = "gradcap"),
               weights = slwt,
               data = census_whites_non_south)$residuals

ny7_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "ny7"),
              weights = slwt,
              data = census_whites_non_south)$residuals

ny8_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "ny8"),
              weights = slwt,
              data = census_whites_non_south)$residuals

ny9_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "ny9"),
              weights = slwt,
              data = census_whites_non_south)$residuals

col7_iv <- ivreg(out_adj ~ grad_adj | ny7_adj + ny8_adj + ny9_adj,
                 weights = slwt,
                 data = census_whites_non_south)
summary(col7_iv)

#-------------
# COMLUMN VIII
#-------------


# OLS 
col8_ols <- lm(reformulate(c("gradcap",additional_controls,state_fe,yob_fe,gender),
                           response = "lnwkwage"),
               weights = slwt,
               data = census_whites_south)
summary(col8_ols)


# First stage of the IV
col8_1st_stage <- lm(reformulate(c(instruments,additional_controls,state_fe,yob_fe,gender),
                                 response = "gradcap"),
                     weights = slwt,
                     data = census_whites_south)
summary(col8_1st_stage)

# FWL
out_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "lnwkwage"),
              weights = slwt,
              data = census_whites_south)$residuals

grad_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                           response = "gradcap"),
               weights = slwt,
               data = census_whites_south)$residuals

ny7_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "ny7"),
              weights = slwt,
              data = census_whites_south)$residuals

ny8_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "ny8"),
              weights = slwt,
              data = census_whites_south)$residuals

ny9_adj <- lm(reformulate(c(additional_controls,state_fe,yob_fe,gender),
                          response = "ny9"),
              weights = slwt,
              data = census_whites_south)$residuals

col8_iv <- ivreg(out_adj ~ grad_adj | ny7_adj + ny8_adj + ny9_adj,
                 weights = slwt,
                 data = census_whites_south)
summary(col8_iv)
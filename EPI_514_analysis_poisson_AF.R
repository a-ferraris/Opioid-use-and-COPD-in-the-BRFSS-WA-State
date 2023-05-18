### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Author: Augusto Ferraris
# Contact information: aferra@uw.edu
# Date: april 2023
#
# Purpose: to write an script for importing, cleaning, and reshaping  variables in the BRFSS dataset. 
#
# History: this code  was written as part of the evaluation of the course EPI 514, spring 2023 at University of Washington.
#
# Program outline:
# 1.MH pooled analysis
# 2. rural vs urban stratified estimates.
# 3. dose-response analysis
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#
# Code starts
# 1. loading data and libraries ----

rm(list=ls())
setwd("C:/Users/Augusto/OneDrive/r_projects/EPI 514/datasets")

library(epiR) # MH analyses
library(tidyverse) # subsetting filtering. 
library(rigr)
library(survey)

copd<-read_rds("copd_clean.rds") # loading dataset

copd<-copd[,c("age", "psu", "ststr_year", "llcpwt", "male", "income_cat", "phys_14", "ment_14", "coronary_mi", 
                  "stroke", "cancer",  "arthritis", "ckd", "diabetes", "smoker",  "drinking_any",
                  "urban", "county", "op_any", "depressive")] # keeping variables for analysis 

copd$age[copd$age==7|copd$age==9]<-NA # removing NAs from age. 

# weighted poisson regression ----
copd$op_numeric[copd$op_any=="No"]<-0
copd$op_numeric[copd$op_any=="Yes"]<-1

copd$depressive_numeric[copd$depressive=="No"]<-0
copd$depressive_numeric[copd$depressive=="Yes"]<-1

options(survey.lonely.psu = 'adjust') # survey design features. 

svy_design<-svydesign(data=copd, 
                      id= ~psu, strata= ~ststr_year, weights = ~llcpwt, 
                      nest=TRUE) # setting survey design

poisson <- svyglm(op_numeric~depressive_numeric+factor(male)+
                            factor(stroke)+factor(phys_14)+factor(coronary_mi)+
                            factor(cancer)+factor(arthritis)+factor(ckd)+factor(diabetes)+
                            factor(smoker)+factor(drinking_any),
                             family="poisson", design = svy_design)


exp(cbind(coef(poisson), confint(poisson)))

summary(logweight_model)


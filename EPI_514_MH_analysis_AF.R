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
# 3. 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#
# Code starts
# 1. loading data and libraries ----

rm(list=ls())
setwd("C:/Users/Augusto/OneDrive/r_projects/EPI 514/datasets")

library(epiR) # MH analyses
library(tidyverse) # subsetting filtering. 
library(rigr)

copd<-read_rds("copd_clean.rds")

# 1. Preparing data for MH stratified analysis. ----
# removing observations with NAs in depressive or opioid variables

analysis<-copd[,c("over_65", "age", "male", "income_cat", "phys_14", "ment_14", "coronary_mi", 
                  "stroke", "cancer",  "arthritis", "ckd", "diabetes", "smoker",  "drinking_any",
                  "op_any", "depressive", "any_chronic", "urban", "op_any01", "depressive_01", "county")]

urban<-analysis[analysis$urban=="Urban",]
rural<-analysis[analysis$urban=="Rural",]

# 5. MH analysis ----
(mantel_haen_table<-xtabs(~depressive+op_any+over_65+male+any_chronic,
                          data=analysis))

array<-array(mantel_haen_table,
             dim=c(2,2,8), 
             list(depressive=c("yes", "No"), 
                  op_any=c("Yes", "No"), 
                  confounders= 1:8
             )) # this function rearranges the set of 2by2byk tables created using xtabs to present 
# only 3 dimensions, and be included in the epi.2by2() function. 

(epi.2by2(array, method='cohort.count')) # running MH analyses. 

# Stratified analyses
(mantel_urban<-xtabs(~depressive+op_any+over_65+male+mental_01+any_chronic,
                          data=urban))

array_urban<-array(mantel_urban,
             dim=c(2,2,16), 
             list(depressive=c("yes", "No"), 
                  op_any=c("Yes", "No"), 
                  confounders= 1:16
             )) # this function rearranges the set of 2by2byk tables created using xtabs to present 

epi.2by2(array_urban, method = 'cohort.count')

# rural now
(mantel_rural<-xtabs(~depressive+op_any+over_65+male+mental_01+any_chronic,
                     data=rural))

array_rural<-array(mantel_rural,
                   dim=c(2,2,16), 
                   list(depressive=c("yes", "No"), 
                        op_any=c("Yes", "No"), 
                        confounders= 1:16
                   )) # this function rearranges the set of 2by2byk tables created using xtabs to present 

epi.2by2(array_rural, method = 'cohort.count')

# 
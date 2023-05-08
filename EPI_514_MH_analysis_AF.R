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

copd<-read_rds("copd_clean.rds")

# 1. Preparing data for MH stratified analysis. ----
# removing observations with NAs in depressive or opioid variables

analysis<-copd[,c("over_65", "age", "male", "income_cat", "phys_14", "ment_14", "coronary_mi", 
                  "stroke", "cancer",  "arthritis", "ckd", "diabetes", "smoker",  "drinking_any",
                  "op_any", "depressive", "any_chronic", "urban", "op_any01", "depressive_01", "county", "pneuvac4")]

analysis$phys_bin[analysis$phys_14=="Zero days"|analysis$phys_14=="1 to 13 days"]<-0
analysis$phys_bin[analysis$phys_14=="14 or more days"]<-1

urban<-analysis[analysis$urban=="Urban",]
rural<-analysis[analysis$urban=="Rural",]

mh_analysis<-analysis[, c("over_65", "depressive", "op_any", "phys_bin", "male", "any_chronic")]
mh_analysis<-mh_analysis%>%na.omit

# 5. MH analysis ----
(mh_1<-xtabs(~depressive+op_any+over_65+male+any_chronic+phys_bin,
                          data=mh_analysis))

array_mh_1<-array(mh_1,
             dim=c(2,2,16), 
             list(depressive=c("yes", "No"), 
                  op_any=c("Yes", "No"), 
                  confounders= 1:16
             )) # this function rearranges the set of 2by2byk tables created using xtabs to present 
# only 3 dimensions, and be included in the epi.2by2() function. 
(epi.2by2(array_mh_1, method='cohort.count')) # running MH analyses. 




# secondary analyses
# Stratified analyses
(urban<-urban[,c("over_65", "depressive", "op_any", "phys_bin", "male", "any_chronic")])
urban<-urban%>%na.omit

(mh_urban<-xtabs(~depressive+op_any+over_65+male+any_chronic+phys_bin,
                 data=urban))

array_urban<-array(mh_urban,
             dim=c(2,2,16), 
             list(depressive=c("yes", "No"), 
                  op_any=c("Yes", "No"), 
                  confounders= 1:16
             )) # this function rearranges the set of 2by2byk tables created using xtabs to present 
epi.2by2(array_urban, method = 'cohort.count')

# rural now
(rural<-rural[,c("over_65", "depressive", "op_any", "phys_bin", "male", "any_chronic")])
rural<-rural%>%na.omit

(mh_rural<-xtabs(~depressive+op_any+over_65+male+any_chronic+phys_bin,
                 data=rural))

array_rural<-array(mh_rural,
                   dim=c(2,2,16), 
                   list(depressive=c("yes", "No"), 
                        op_any=c("Yes", "No"), 
                        confounders= 1:16
                   )) # this function rearranges the set of 2by2byk tables created using xtabs to present 

epi.2by2(array_rural, method = 'cohort.count')

# Now, dose-response analysis 

zero<-analysis[analysis$ment_14=="Zero days",]
zero<-zero[,c("over_65", "depressive", "op_any", "phys_bin", "male", "any_chronic")]
zero<-zero%>%na.omit

(mh_zero<-xtabs(~depressive+op_any+over_65+male+any_chronic+phys_bin,
                 data=zero))

array_zero<-array(mh_zero,
                   dim=c(2,2,16), 
                   list(depressive=c("yes", "No"), 
                        op_any=c("Yes", "No"), 
                        confounders= 1:16
                   )) # this function rearranges the set of 2by2byk tables created using xtabs to present 

epi.2by2(array_zero, method = 'cohort.count')

# 1 to 13 days
mental_13<-analysis[analysis$ment_14=="1 to 13 days"|analysis$ment_14=="14 or more days",]
mental_13<-mental_13[,c("over_65", "depressive", "op_any", "phys_bin", "male", "any_chronic")]
mental_13<-mental_13%>%na.omit

(mh_13<-xtabs(~depressive+op_any+over_65+male+any_chronic+phys_bin,
                data=mental_13))

array_13<-array(mh_13,
                  dim=c(2,2,16), 
                  list(depressive=c("yes", "No"), 
                       op_any=c("Yes", "No"), 
                       confounders= 1:16
                  )) 

epi.2by2(array_13, method = 'cohort.count')

# positive control 
positive<-analysis[,c("over_65", "depressive", "op_any", "phys_bin", "male", "any_chronic","pneuvac4")]

(mh_positive<-xtabs(~depressive+pneuvac4+over_65+male+any_chronic+phys_bin,
              data=positive))

array_13<-array(mh_13,
                dim=c(2,2,16), 
                list(depressive=c("yes", "No"), 
                     pneuvac4=c("Yes", "No"), 
                     confounders= 1:16
                )) 

epi.2by2(array_13, method = 'cohort.count')

# sensitivity analyses
rm(list=ls())
op_one<-read.csv("copd_na_equals_one.csv")

analysis<-op_one[,c("over_65", "age", "male", "income_cat", "phys_14", "ment_14", "coronary_mi", 
                  "stroke", "cancer",  "arthritis", "ckd", "diabetes", "smoker",  "drinking_any",
                  "op_any", "depressive", "any_chronic", "urban", "op_any01", "depressive_01")]

analysis$phys_bin[analysis$phys_14=="Zero days"|analysis$phys_14=="1 to 13 days"]<-0
analysis$phys_bin[analysis$phys_14=="14 or more days"]<-1

op_one<-analysis[,c("over_65", "depressive", "op_any", "phys_bin", "male", "any_chronic")]

#  MH analysis ----
(mh_op_one<-xtabs(~depressive+op_any+over_65+male+any_chronic+phys_bin,
             data=op_one))

array_mh_op_one<-array(mh_op_one,
                  dim=c(2,2,16), 
                  list(depressive=c("yes", "No"), 
                       op_any=c("Yes", "No"), 
                       confounders= 1:16
                  )) # this function rearranges the set of 2by2byk tables created using xtabs to present 
# only 3 dimensions, and be included in the epi.2by2() function. 

(epi.2by2(array_mh_op_one, method='cohort.count')) # running MH analyses. 


# now second sensitivity analysis

rm(list=ls())
op_two<-read.csv("copd_na_equals_two.csv")

analysis<-op_two[,c("over_65", "age", "male", "income_cat", "phys_14", "ment_14", "coronary_mi", 
                    "stroke", "cancer",  "arthritis", "ckd", "diabetes", "smoker",  "drinking_any",
                    "op_any", "depressive", "any_chronic", "urban", "op_any01", "depressive_01")]

analysis$phys_bin[analysis$phys_14=="Zero days"|analysis$phys_14=="1 to 13 days"]<-0
analysis$phys_bin[analysis$phys_14=="14 or more days"]<-1

op_two<-analysis[,c("over_65", "depressive", "op_any", "phys_bin", "male", "any_chronic")]

#  MH analysis ----
(mh_op_two<-xtabs(~depressive+op_any+over_65+male+any_chronic+phys_bin,
                  data=op_two))

array_mh_op_two<-array(mh_op_two,
                       dim=c(2,2,16), 
                       list(depressive=c("yes", "No"), 
                            op_any=c("Yes", "No"), 
                            confounders= 1:16
                       )) # this function rearranges the set of 2by2byk tables created using xtabs to present 
# only 3 dimensions, and be included in the epi.2by2() function. 

(epi.2by2(array_mh_op_two, method='cohort.count')) # running MH analyses. 



# end of R script
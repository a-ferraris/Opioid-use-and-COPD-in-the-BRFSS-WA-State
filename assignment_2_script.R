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
# 1. Effect modification assessment
# 2. Adjusting for confounders
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#
# Code starts
# loading data and libraries ----

rm(list=ls())
setwd("C:/Users/Augusto/OneDrive/r_projects/EPI 514/assignment_1")

library(survey)
library(epiR)

brfss<-readRDS("brfss_clean.rds") # data from assignment 1 is loaded
results<-list() # creating list to store results

# Analysis---- 
# Sex as effect modifier
sex_strat<-xtabs(~bmi_category+diabetes_bin+male, data=brfss) # creating array of tables for analysis (stratified)

sex_strat_male<-matrix(c(128, 3359,
                         1837, 44836), nrow = 2) # table for male==1, underweight/diabetes

sex_strat_female<-matrix(c(211, 4514,
                         4725, 75936), nrow = 2) # table for male==0, underweight/diabetes

results$sex_strat$male__under_analysis<-epi.2by2(sex_strat_male, method='cohort.count') # saving results in list
results$sex_strat$female__under_analysis<-epi.2by2(sex_strat_female, method='cohort.count') # saving results in list

sex_strat_male<-matrix(c(9264, 3359,
                         70664, 44836), nrow=2) # table for male==1, overweight/diabetes

sex_strat_female<-matrix(c(8413,4514,
                           60552,75936), nrow=2) # table for male==0, overweight/diabetes

results$sex_strat$male_over_analysis<-epi.2by2(sex_strat_male, method='cohort.count') # saving results in list
results$sex_strat$female_over_analysis<-epi.2by2(sex_strat_female, method='cohort.count') # saving results in list

sex_strat_male<-matrix(c(13704, 3359, 
                         45651, 44836), nrow = 2) # table for male==1, obese/diabetes

sex_strat_female<-matrix(c(16346,4514, 
                           52664, 75946), nrow=2) # table for male==0, obese/diabetes

results$sex_strat$male_obese_analysis<-epi.2by2(sex_strat_male, method='cohort.count')
results$sex_strat$female_obese_analysis<-epi.2by2(sex_strat_female, method='cohort.count')

# stratifying by race
race_strat<-xtabs(~bmi_category+diabetes_bin+race_imput, data=brfss) # stratifying diabetes/bmi by race

# first, estimating results for white, non-hispanic category

white_strat<-matrix(c(219,5459, 
                      5084, 96744), nrow=2) # table for white, diabetes/under

results$race_strat$white_under<-epi.2by2(white_strat, method = 'cohort.count') 

white_strat<-matrix(c(12676, 5459, 
                      103493, 96744), nrow = 2) # table for white, diabetes/over

results$race_strat$white_over<-epi.2by2(white_strat, method='cohort.count')

white_strat<-matrix(c(21784, 73917,
                      5459, 96744), nrow=2) # table for white, diabetes/obese

results$race_strat$white_obese<-epi.2by2(white_strat, method='cohort.count')

# Black race analysis. 

black_strata<-matrix(c(36, 851, 
                     399, 6752), nrow=2) # table for black, diabetes under

results$race_strat$black_under<-epi.2by2(black_strata, method='cohort.count')

black_strata<-matrix(c(1977, 851, 
                       8928, 6752), nrow=2) # table for black, diabetes/ over

results$race_strat$black_over<-epi.2by2(black_strata, method='cohort.count')

black_strata<-matrix(c(3833, 851, 
                       10001, 6752), nrow=2) # table for black, obese

results$race_strat$black_obese<-epi.2by2(black_strata, method='cohort.count')

# Asian population analysis 

asian_strata<-matrix(c(9, 358, 
                       345, 4202), nrow=2) # table for asian, diabetes/under

results$race_strat$asian_under<-epi.2by2(asian_strata, method='cohort.count')

asian_strata<-matrix(c(413, 358, 
                       2537, 4202), nrow=2) # table for asian, diabetes/over

results$race_strat$asian_over<-epi.2by2(asian_strata, method='cohort.count')

asian_strat<-matrix(c(207, 358, 
                      831, 4202), nrow=2) # table for asian, diabetes/obese

results$race_strat$asian_obese<-epi.2by2(asian_strat, method='cohort.count')

# American Indian/Alaskan Native, non-Hispanic

native_strata<-matrix(c(10,221, 
                        107, 1646), nrow=2) # table for native, diabetes/under

results$race_strat$native_under<-epi.2by2(native_strata, method='cohort.count')

native_strata<-matrix(c(526, 221, 
                        2161, 1646), nrow=2) # table for native, diabetes/ over

results$race_strat$native_over<-epi.2by2(native_strata, method='cohort.count')

native_strata<-matrix(c(942, 221,
                        2189, 1646), nrow=2) # table for native, diabetes/ obese

results$race_strat$native_obese<-epi.2by2(native_strata, method='cohort.count')

# Hispanic strata

hispanic_strata<-matrix(c(49,734, 
                          413, 8116), nrow=2) # tab for hispanic, under/diabets

results$race_strat$hispanic_under<-epi.2by2(hispanic_strata, method='cohort.count')

hispanic_strata<-matrix(c(1616, 734, 
                          10455, 8116), nrow=2) # tab for hispanic, over/diabetes

results$race_strat$hispanic_over<-epi.2by2(hispanic_strata, method='cohort.count')

hispanic_strata<-matrix(c(2277, 734, 
                          8223, 8116), nrow=2) # table for hispanic, obese/diabetes

results$race_strat$hispanic_obese<-epi.2by2(hispanic_strata, method='cohort.count')

# other race

other_strata<-matrix(c(16, 255, 
                       220, 3372), nrow=2) # table for other, under/diabetes

results$race_strat$other_under<-epi.2by2(other_strata, method='cohort.count')

other_strata<-matrix(c(476, 255, 
                       3705, 3372), nrow=2) # table for other, over/diabetes

results$race_strat$other_over<-epi.2by2(other_strata, method='cohort.count')

other_strata<-matrix(c(1025, 255, 
                       3198, 3372), nrow=2) # table for other, obese/diabetes

results$race_strat$other_obese<-epi.2by2(other_strata, method='cohort.count')

# some tests before starting to conduct analysis. 

heterogeneity<-xtabs(~under_12+diabetes_bin+male, data=brfss)
results$sex_strat$heteregoneity_under<-epi.2by2(heterogeneity, method='cohort.count')
 
heterogeneity<-xtabs(~over_12+diabetes_bin+male, data=brfss)
results$sex_strat$heteregoneity_over<-epi.2by2(heterogeneity, method='cohort.count')

heterogeneity<-xtabs(~obese_12+diabetes_bin+male, data=brfss)
results$sex_strat$heteregoneity_obese<-epi.2by2(heterogeneity, method='cohort.count')

# there is evidence of heterogeneity for categories overweight and obese. 

# Analysis stratifying. 
# 1. creating array

females<-brfss[brfss$male=="Female",] # subsetting data to capture information only for females

table_f<-array(xtabs(~under_12+diabetes_bin+age_group+income_cal, data=females), 
               dim=c(2,2,30), 
               list(under_12=c("Underweight", "Normal"), 
                    diabetes_12=c("Yes", "No"), 
                    confounders= 1:30
                   )
                ) # this command creates an array of 2 by2 (outcome and expposure ) and 30 tables (6x5 categories in the adjusting variables)
               
results$sex_strat$female_adj_under<-epi.2by2(table_f, method='cohort.count') # saving results of the pooled analysis in list. 

# analysis for overweight among females. 

table_f<-array(xtabs(~over_12+diabetes_bin+age_group+income_cal, data=females), 
                  dim=c(2,2,30), 
                  list(under_12=c("Overweight", "Normal"), 
                  diabetes_12=c("Yes", "No"), 
                  confounders= 1:30
                    )
                )

results$sex_strat$female_adj_over<-epi.2by2(table_f, method='cohort.count')

# analysis for obese among females. 

table_f<-array(xtabs(~obese_12+diabetes_bin+age_group+income_cal, data=females), 
               dim=c(2,2,30), 
               list(under_12=c("Obese", "Normal"), 
                    diabetes_12=c("Yes", "No"), 
                    confounders= 1:30
               )
          )

results$sex_strat$female_adj_obese<-epi.2by2(table_f, method='cohort.count')


# analysis for males. 

males<-brfss[brfss$male=="Male",]

table_m<-array(xtabs(~under_12+diabetes_bin+age_group+income_cal, data=males), 
               dim=c(2,2,30),
               list(under_12=c("Under", "Normal"), 
                    diabetes_12=c("Yes", "No"),
                    confounders=1:30
               )
          )

results$sex_strat$male_adj_under<-epi.2by2(table_m, method='cohort.count')


table_m<-array(xtabs(~over_12+diabetes_bin+age_group+income_cal, data=males), 
               dim=c(2,2,30),
               list(over_12=c("Over", "Normal"), 
                    diabetes_12=c("Yes", "No"),
                    confounders=1:30
               )
)

results$sex_strat$male_adj_over<-epi.2by2(table_m, method='cohort.count')


table_m<-array(xtabs(~obese_12+diabetes_bin+age_group+income_cal, data=males), 
               dim=c(2,2,30),
               list(obese_12=c("Obese", "Normal"), 
                    diabetes_12=c("Yes", "No"),
                    confounders=1:30
               )
)

results$sex_strat$male_adj_obese<-epi.2by2(table_m, method='cohort.count')

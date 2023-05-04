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
# 1. Data import. libraries. Data merging
# 2. Data cleaning, labeling, factorizing variables
# 3. 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#
# Code starts
# 1. loading data and libraries ----

rm(list=ls())
setwd("C:/Users/Augusto/OneDrive/r_projects/EPI 514/datasets")

library(haven) # importing data from SAS to R
library(epiR) # MH analyses
library(tidyverse) # subsetting filtering. 
library(readxl)  # required for importing zip_codes 
library(table1) # required for exporting tables in appropriate formatting 
library(flextable) # required for exporting tables in appropriate formatting 
library(magrittr) # required for exporting tables in appropriate formatting 
library(survey) # weighting proportions for table 1.

# a. loading year 2019
# data is loaded twice to deal with the underscore that some variables present.
# by doing so we avoid the use of variables with quotes
# year 2019 has 12993 observations | year 2020 12902 observations | and year 2021 13142 observations
# total 39037
brfss<-read_dta("WA_BRFSS_2019.dta")
write.csv(brfss, "wa_brfss_2019.csv", row.names = FALSE)
brfss<-read.csv("wa_brfss_2019.csv", stringsAsFactors = FALSE)

brfss$X_llcpwt<-brfss$X_llcpwt*12993/39037 # adjusting the weight to the number of observations/total
brfss$ststr_year<-brfss$X_ststr+19000000 # we create a variable that adds the year to the stratum of the survey

columns_interest<-c("X_age65yr", "age", "X_race", "sex", "zipcode1", "X_incomg", "X_phys14d", "X_ment14d", "X_michd", 
                    "cvdstrk3", "chcocncr",  "X_drdxar2", "chckdny2", "diabete4", "X_smoker3",  "drnkany5",
                     "pneuvac4", "op_any", "ststr_year", "X_llcpwt", 
                    "X_psu", "X_bmi5cat", "chccopd2", "addepev3", "X_age_g") # we create a vector of column names to simplify the merging of the data.

merged<-brfss[,columns_interest] # data is saved in a merged dataset that will be later filtered

# b. loading year 2020

brfss<-read_dta("WA_BRFSS_2020.dta")
write.csv(brfss, "wa_brfss_2020.csv", row.names = FALSE)
brfss<-read.csv("wa_brfss_2020.csv", stringsAsFactors = FALSE)

brfss$X_llcpwt<-brfss$X_llcpwt*12902/39037 # adjusting weight to the number of individuals in final sample
brfss$ststr_year<-brfss$X_ststr+20000000 # we create a variable that adds the year to the stratum of the survey

merged<-rbind(merged, brfss[,columns_interest]) # data is saved in a merged dataset that will be later filtered

# c. loading year 2021

brfss<-read_dta("WA_BRFSS_2021.dta")
write.csv(brfss, "wa_brfss_2021.csv", row.names = FALSE)
brfss<-read.csv("wa_brfss_2021.csv", stringsAsFactors = FALSE)

brfss$X_llcpwt<-brfss$X_llcpwt*13142/39037 # adjusting variable weight to the total final n of individuals. 
brfss$ststr_year<-brfss$X_ststr+21000000 # we create a variable that adds the year to the stratum of the survey

# some column names have changed for year 2021. I create new variables with the prior names to simplify the task:
# the columns are x_incomg1, X_drdxar3, and chccopd3

brfss$X_incomg<-brfss$X_incomg1
brfss$X_drdxar2<-brfss$X_drdxar3
brfss$chccopd2<-brfss$chccopd3

merged<-rbind(merged, brfss[,columns_interest])# data is saved in a merged dataset that will be later filtered

# 2. data cleaning: renaming variables, creating factor variables. ----
# 39037 individuals. 

copd<-merged[merged$chccopd2==1,] # filtering patients, keeping only those with a diagnosis of COPD
rm(brfss, columns_interest, merged)

# 2648 individuals with COPD 
# renaming variables
 
colnames(copd)<-c("over_65", "age", "race", "male", "zip", "income_cat", "phys_14", "ment_14", "coronary_mi", 
                  "stroke", "cancer",  "arthritis", "ckd", "diabetes", "smoker",  "drinking_any",
                  "pneuvac4", "op_any", "ststr_year", "llcpwt", 
                  "psu", "bmi_cat", "copd", "depressive", "age_group")

# creating urban category using zipcodes
zip<-read_xlsx("urban_zip_codes.xlsx") # loading original list of urban codes. 
# Other codes are considered rural. 

# merging
copd<-merge(copd, zip, by = "zip", all.x = TRUE) # left-join of the datasets conditional on matching the zip. Thus, the zip that are present in both datasets are coded as 1
copd$urban[is.na(copd$urban)==TRUE]<-0 # this code subsitutes NA in urban as zero 
copd$urban[copd$zip==99999|copd$zip==77777]<-NA # replacing NAs. 

rm(zip)

# creating counties
counties<-read.csv("counties.csv")
copd<-merge(copd, counties, by="zip", all.x = TRUE)

rm(counties)

# cleaning one by one and recategorizing as factors

copd$over_65[copd$over_65==3]<-NA # re-coding 3 (refused) as NA
copd$over_65[copd$over_65==2]<-0 # re-coding 2 as 0 (no)
copd$over_65<-factor(copd$over_65, 
                     levels=0:1, 
                     labels = c("No", "Yes")) # re-coding as factor variables 0=no, 1=yes


copd$race[copd$race==9]<-NA # re-coding missing as NA
copd$race<-factor(copd$race, 
                  levels= 1:8, 
                  labels= c("White", "Black", "Native American", "Asian", "Pacific Islander", 
                            "Other", "Multiracial", "Hispanic")) # re-coding as factor variable using codebook


copd$male[copd$male==2]<-0 # re-coding 2 as 0 (females)
copd$male<-factor(copd$male, 
                  levels=0:1, 
                  labels=c("Female", "Male")) # re-coding as factor variable Female vs Male

copd$income_cat[copd$income_cat==9]<-NA # re-coding missing as NA
copd$income_cat[copd$income_cat==6|copd$income_cat==7]<-5 # variables 6 and 7 were introduced in 2021, corresponding to >100 and 200000 us dollars
                                                          # since they are not available in previous years, they are re-coded to agree the format. 
copd$income_cat<-factor(copd$income_cat, 
                        level=1:5, 
                        labels=c("<15k", "15 to =25k","25+ to =35k", "35+ to =50", "50+k")) # re-coding as factor variable using codebook


copd$phys_14[copd$phys_14==9]<-NA # re-coding missing as NA
copd$phys_14<-copd$phys_14-1 # re-coding the variable as factor with 3 levels, 0:2
copd$phys_14<-factor(copd$phys_14, 
                     levels=0:2, 
                     labels=c("Zero days", "1 to 13 days", "14 or more days")) # re-coding following codebooks


copd$ment_14[copd$ment_14==9]<-NA # re-coding missing as NA
copd$ment_14<-copd$ment_14-1 # re-coding as 0,1,2 levels
copd$ment_14<-factor(copd$ment_14, 
                     levels=0:2, 
                     labels=c("Zero days", "1 to 13 days", "14 or more days")) # re-coding following codebook 


copd$coronary_mi[copd$coronary_mi==2]<-0 # re-coding to 0/1 format
copd$coronary_mi<-factor(copd$coronary_mi, 
                         levels=0:1, 
                         labels=c("No", "Yes")) # re-coding following codebook 


copd$stroke[copd$stroke==7]<-NA # re-coding NAs
copd$stroke[copd$stroke==2]<-0 # re-coding to 0/1 format
copd$stroke<-factor(copd$stroke, 
                    levels=0:1, 
                    labels=c("No", "Yes")) # re-coding according to codebook


copd$cancer[copd$cancer==7]<-NA # re-coding missing 
copd$cancer[copd$cancer==2]<-0 # re-coding to 0/1 format
copd$cancer<-factor(copd$cancer, 
                    levels=0:1, 
                    labels=c("No", "Yes")) # re-coding according to codebook

                                        
copd$arthritis[copd$arthritis==2]<-0 # re-coding to 0/1 format
copd$arthritis<-factor(copd$arthritis, 
                       levels=0:1,
                       labels=c("No", "Yes")) # re-coding according to codebook


copd$ckd[copd$ckd==7|copd$ckd==9]<-NA # re-coding refused or unsure as NAs
copd$ckd[copd$ckd==2]<-0
copd$ckd<-factor(copd$ckd, 
                 levels=0:1, 
                 labels=c("No", "Yes")) #re-coding according to codebook


copd$diabetes[copd$diabetes==2|copd$diabetes==3|copd$diabetes==4]<-0 # re-coding gestational, borderline and no as 0/1 variable
copd$diabetes[copd$diabetes==7]<-NA # re-coding NA
copd$diabetes<-factor(copd$diabetes, 
                      levels=0:1, 
                      labels=c("No", "Yes")) # re-coding according to codebook


copd$smoker[copd$smoker==2|copd$smoker==3]<-1 # ever smokers re-categorized as 1
copd$smoker[copd$smoker==4]<-0 # never smokers categorized as 0,m for 0/1 format variable
copd$smoker[copd$smoker==9]<-NA #re-coding NAs. 
copd$smoker<-factor(copd$smoker, 
                     levels=0:1, 
                     labels=c("No", "Yes")) # re-coding following codebook


copd$drinking_any[copd$drinking_any==7|copd$drinking_any==9]<-NA # re-coding refused and not sure as NA
copd$drinking_any[copd$drinking_any==2]<-0 # re-coding as 0/1 format
copd$drinking_any<-factor(copd$drinking_any, 
                          levels=0:1, 
                          labels=c("No", "Yes")) # re-coding as codebook


copd$pneuvac4[copd$pneuvac4==7|copd$pneuvac4==9]<-NA # re-coding refused and unsure as NA
copd$pneuvac4[copd$pneuvac4==2]<-0 # re-coding as binary 0/1 variable
copd$pneuvac4<-factor(copd$pneuvac4, 
                      levels = 0:1, 
                      labels=c("No", "Yes")) # re-coding as codebook


copd$op_any[copd$op_any==7|copd$op_any==9]<-NA # re-coding missing data
copd$op_any01<-copd$op_any
copd$op_any01[copd$op_any==2]<-0
copd$op_any<-factor(copd$op_any, 
                    levels=1:2, 
                    labels = c("Yes", "No")) # this variable stays in 1/2 format to fit in epi.2by2() function

copd$op_any01<-factor(copd$op_any01, 
                    levels=0:1, 
                    labels = c("No", "Yes")) # this variable stays in 1/2 format to fit in epi.2by2() function

copd$bmi_cat<-factor(copd$bmi_cat, 
                     levels=1:4, 
                     labels=c("Underweight", "Normal", "Overweight", "Obese")) # No Na, just recoding variable for tables


copd$depressive[copd$depressive==7|copd$depressive==9]<-NA # re-coding refused or unsure as NA
copd$depressive_01<-copd$depressive
copd$depressive_01[copd$depressive_01==2]<-0 # binary version 0/1

copd$depressive<-factor(copd$depressive, 
                        levels = 1:2, 
                        labels= c("Yes", "No")) # this variable stays in 1/2 format to fit in epi.2by2() function

copd$depressive_01<-factor(copd$depressive_01, 
                        levels = 0:1, 
                        labels= c("No", "Yes")) # this variable is coded in 0/1 format

copd$urban<-factor(copd$urban, 
                   levels=0:1, 
                   labels= c("Rural", "Urban"))

copd$age_group<-factor(copd$age_group, 
                       levels=1:6, 
                       labels = c("18 to 24", "25 to 34", "35 to 44", 
                                  "45 to 54", "55 to 64", "65+"))

copd$any_chronic[copd$diabetes=="No"& copd$cancer=="No" & copd$arthritis=="No" & copd$ckd=="No"&
                   copd$coronary_mi=="No" & copd$stroke=="No"]<-0 # if all are negative, the any_chronic is 0

copd$any_chronic[copd$diabetes=="Yes" | copd$cancer=="Yes" | copd$arthritis=="Yes" | copd$ckd=="Yes" |
                   copd$coronary_mi=="Yes" | copd$stroke=="Yes"]<-1 # if any of the variables is positive, the any_comorb is positive

copd$any_chronic<-factor(copd$any_chronic, 
                         levels=0:1, 
                         labels=c("No", "Yes"))

copd$county<-as.factor(copd$county)

table(copd$depressive, copd$op_any, useNA = 'always', deparse.level = 2)

# 3. creating table1 and weighting proportions.---- 

copd<-copd[!is.na(copd$depressive_01)==T & !is.na(copd$op_any01)==T,] # keeping observations without missing data for main exposure and outcome. 

table_one<-table1(~age+over_65+race+male+income_cat+urban+coronary_mi+stroke+cancer+arthritis+
                    diabetes+ckd+smoker+drinking_any+phys_14+ment_14+age_group+any_chronic|depressive, data=copd)
table_one
t1flex(table_one) %>%save_as_docx(path="table_1.docx") # exporting table 1.

# income_cat has >5% of missing data. To account for this, we create a new category with NA as ==9

copd$income_na<-as.numeric(copd$income_cat) # creating new var as numeric
copd$income_na[is.na(copd$income_na)==TRUE]<-9 # replacing missing as 9 category to be weighted later

## 3.b. Weighting prevalences and estimating proportions ----

options(survey.lonely.psu = 'adjust') # survey design features. 

svy_design<-svydesign(data=copd, 
                      id= ~psu, strata= ~ststr_year, weights = ~llcpwt, 
                      nest=TRUE) # setting survey design

for (i in names(copd[,c(2:17,26:29)])){ # selecting columns of interest to run the loop
    print(i) # points the name of the variable that is being addressed
    formula_str<-paste("~depressive+", i) # First, we create a string object to merge with the name of the column
    formula_obj<-as.formula(formula_str) # to svytable() to work, we need to convert the strings in formulas/objects
    print(
    prop.table(
    svytable(formula_obj, design = svy_design),  # running function of interest
                margin=1) 
  )
  rm(formula_str, formula_obj) # removing objects created.
} # this for loop function runs the function svytable across the columns of interest. 

# separate procedure for income categories including NAs. 
prop.table(svytable(~depressive+income_na,design=svy_design), margin=1) # Weighted % to be used in the table 1. 

write_rds(copd, "copd_clean.rds") # saving clean version

# end of R script. 
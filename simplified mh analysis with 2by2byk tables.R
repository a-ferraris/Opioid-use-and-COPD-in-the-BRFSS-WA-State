
# Mantel Haenszel analysis with more than 1 adjusting variable. 

# to use the epi.2by2() variable, the array of tables introduced must have 3 dimensions 
# (2by2 xK tables)
# To create such tables, since the order does not matter in MH, we need to re-array tables
# created using the array function. 

# to create table 2by2 for epi 2by2:

# stratifying var 1==1, stratifying var 2==0
#                   Outcome yes (1) | outcome no (2)
# Exposure yes(1)|     XX                XX
# Exposure No (2)|     XX                XX


# stratifying var 1==1, stratifying var 2==1
#                   Outcome yes (1) | outcome no (2)
# Exposure yes(1)|     XX                XX
# Exposure No (2)|     XX                XX


# (and so on...)

# exposure and outcomes MUST be coded 1=yes, 2=no. just binaries

stratified<-xtabs(~exposure+outcome+adjvar1+adjvar2+...+adjvarK, data=data)

# this will create an array with 2+ k dimensions
# we need to convert it to 3 dimensions. To do so, the array() function first decomposes the 
# tables into numbers, and then you can re-arrange the data in 2by2xk tables (but with just 3 dimensions!)
# we must specify the number of tables that will be created. 
# example
# adjvar1=binary, adjvar2=binary, adjvar3=binary -->2x2x2=8 tables. Then we would replace ntables as 8
# Example 2
# adjvar1=binary, adjvar2=5levels, adjvar3=6 levels->2x5x6=60 tables. We would replace ntables as 60. 
# always double check the number of tables and that they are in the right order (as in xtabs)

mh_array<-array(stratified, 
                dim=c(2,2,ntables), # this creates a 3 dimension array with n tables. 
                list(exposure=c("exposure yes", "exposure no"), 
                     outcomes=c("outcome yes", "outcome no"), 
                     confounders= 1:ntables))

(epi.2by2(mh_array, method='cohort.count'))

# take into account that if you have tables without observations, MH wont run. 
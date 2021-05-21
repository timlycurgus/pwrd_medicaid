##' ---
##' title: "Load AHRF subsetted data, format properly, then join with subgroup1318.csv"
##' output: github_document
##' ---
##'
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
cleanupunderway  <- TRUE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)
stopifnot(getRversion() >="3.5.0") # cf below use of `factor(...,levels=,labels=)`
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

#################### Read in AHRF File and Combine with Subgroup File ###################

ahrf_1117 = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/ahrf_1117.csv")
ahrf_18 = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/ahrf_18.csv")
subgroup_1318 = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/subgroup_1318.csv")

ahrf_1118 = left_join(ahrf_1117,ahrf_18)

ahrf_1118 %>% 
  select(-c('pop_2011','medIncome_2011','pctPovty_2011','unplmtRate_2011')) %>%
  gather(pop_ahrf,pop_count_ahrf,c(pop_2017:pop_2012,pop_2018)) %>% 
  mutate(YEAR = case_when(pop_ahrf == 'pop_2017' ~ 2017,
                          pop_ahrf == 'pop_2016' ~ 2016,
                          pop_ahrf == 'pop_2015' ~ 2015,
                          pop_ahrf == 'pop_2014' ~ 2014,
                          pop_ahrf == 'pop_2013' ~ 2013,
                          pop_ahrf == 'pop_2012' ~ 2012,
                          pop_ahrf == 'pop_2018' ~ 2018),
         FIPS = case_when(FIPS < 10000 ~ paste("0",FIPS,sep = ""),
                          FIPS > 9999 ~ paste(FIPS,sep=""))) %>%
  select(-c(pop_ahrf)) -> temp

ahrf_subgroup = temp %>% 
  select(1,41,42)

ahrf_1118 %>% 
  gather(inc_year,med_income,c(medIncome_2017:medIncome_2012,medIncome_2018)) %>% 
  select(c('med_income')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,med_income = temp$med_income)

ahrf_1118 %>% 
  gather(pvt_year,pov,c(pctPovty_2017:pctPovty_2012,pctPovty_2018)) %>% 
  select(c('pov')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,pov = temp$pov)

ahrf_1118 %>% 
  gather(unemp_year,unemp,c(unplmtRate_2017:unplmtRate_2012,unplmtRate_2018)) %>% 
  select(c('unemp')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,unemp = temp$unemp)

ahrf_1118 %>% 
  gather(medicaid_year,med_elig,c(popu65u138_2017:popu65u138_2012,popu65u138_2018)) %>% 
  select(c('med_elig')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,med_elig = temp$med_elig)

ahrf_1118 %>% 
  gather(unins_year,unins,c(noins_u65u138_2017:noins_u65u138_2012,noins_u65u138_2018)) %>% 
  select(c('unins')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,unins = temp$unins)

subgroup_1318 = left_join(subgroup_1318,ahrf_subgroup)

write.csv(subgroup_1318,file='subgroup_pop1318.csv',row.names=FALSE)

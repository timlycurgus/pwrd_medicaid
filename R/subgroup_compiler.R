##' ---
##' title: "Assemble/extract subgroups for outcome analysis"
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
##'
##' Read in the county level population data for 2013-2017
##' keeping variables related to sex/race/age.
##' 

subgroup_alldata <- read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/cc-est2019-alldata.csv")
#subgroup_1317 = subset(subgroup_alldata,YEAR %in% c(6,7,8,9,10))
#remove(subgroup_alldata)
#write.csv(subgroup_1317,file='subgroup_1317.csv',row.names=FALSE)
#subgroup_1317 = read.csv("~/mnt/compute1-tlycurgu/subgroup_1317.csv")
#subgroup_alldata <- read.csv("~/cc-est2019-alldata.csv")

subgroup_alldata %>% 
  filter(AGEGRP %in% c(5:13)) %>%
  mutate(agegrp = case_when(AGEGRP==0~'0_85plus',
                            AGEGRP%in%c(5:7)~'20_34',
                            AGEGRP%in%c(8:9)~'35_44',
                            AGEGRP%in%c(10:11)~'45_54',
                            AGEGRP%in%c(12:13)~'55_64'),
         latino = (H_MALE+H_FEMALE)/TOT_POP,
         NHAA_MALE = NHAA_MALE + NHNA_MALE,
         NHAA_FEMALE = NHAA_FEMALE + NHNA_FEMALE,
         O_MALE = TOT_MALE - NHWA_MALE - NHBA_MALE - NHIA_MALE - NHAA_MALE - H_MALE,
         O_FEMALE = TOT_FEMALE - NHWA_FEMALE - NHBA_FEMALE - NHIA_FEMALE - NHAA_FEMALE - H_FEMALE,
         stateFIPS = case_when(STATE < 10 ~ paste("0",STATE,sep=""),
                               STATE > 9 ~ paste(STATE,sep="")),
         cntyFIPS = case_when(COUNTY < 10 ~ paste("00",COUNTY,sep=""),
                              COUNTY > 9 & COUNTY < 100 ~ paste("0",COUNTY,sep=""),
                              COUNTY > 99 ~ paste(COUNTY,sep="")),
        FIPS = paste(stateFIPS,cntyFIPS,sep="")
         ) -> subgroup_1317


subgroup_1317$YEAR = subgroup_1317$YEAR + 2007

subgroup_1317 %>% 
  filter(YEAR > 2012 & YEAR < 2019) -> subgroup_1318

subgroup_1318 %>% 
  select(2:10,35:42,57,58,81,82,83,84,85,86,87) %>%
  gather(race_sex,pop_count,c(NHWA_MALE:NHAA_FEMALE,H_MALE:H_FEMALE,O_MALE:O_FEMALE))-> subgroup_1318

subgroup_1318$FIPS[which(subgroup_1318$FIPS == "02158")] <- "02270" # Wade Hampton AK
subgroup_1318$FIPS[which(subgroup_1318$FIPS == "46102")] <- "46113" # Shannon SD

subgroup_1318 %>%
  select(-c(6:9,11)) -> subgroup_1318

subgroup_1318 %>%
  group_by_at(setdiff(names(subgroup_1318),'pop_count')) %>%
  summarise(pop_count=sum(pop_count)) -> subgroup_1318


write.csv(subgroup_1318,file='subgroup_1318.csv',row.names=FALSE)

#################### Read in AHRF File and Combine with Subgroup File ###################

ahrf_1117 = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/ahrf_1117.csv")
ahrf_18 = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/ahrf_18.csv")

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

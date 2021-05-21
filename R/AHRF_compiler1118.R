###################################################################################################
#Script: AHRF_compiler.R
#Inputs: data-raw/AHRF2019.asc
#Outputs: data/base_ahrf_cnty.csv
#         data/temp/cnty_fips.csv
#Author: TL
#Date: 6/17/2020
###################################################################################################

library(dplyr)
library(tidyr)
library(readr)

###################################################################################################

#ahrf_raw  <- paste0("../data-raw/", "AHRF2019.asc") %>%
ahrf_raw <- paste0("~/DATA/","AHRF2020.asc") %>%
  read_fwf(fwf_cols(stateFIPS = c(122,123), # 'state FIPS'
                    cntyFIPS = c(124,126), # 'county FIPS'
                    stateName = c(46,64),
                    cntyName = c(67,91),
                    pop_2018 = c(16868,16875),
                    medIncome_2018 = c(24300,24305),
                    pctPovty_2018 = c(25196,25199),
                    unplmtRate_2018 = c(30940,30942),
                    popu65u138_2018 = c(26556,26562),
                    noins_u65u138_2018 = c(26612,26618)#,
                    #pm2014 = c(31742,31746),
                    #pm2013 = c(31747,31751),
                    #pm2012 = c(31752,31756),
                    #pm2011 = c(31757,31761)
  ),
  progress=FALSE
  ) 

ahrf <- ahrf_raw %>%
  mutate(FIPS=paste0(sprintf('%s', stateFIPS), sprintf('%s', cntyFIPS))) %>%
  mutate_at(vars(pop_2018:noins_u65u138_2018), function(x){as.numeric(x)}) %>%
  #mutate_at(vars(pop_2018:pm2011), function(x){as.numeric(x)}) %>%
  mutate_at(vars(matches("pct"), matches("unplmt"), matches("popDens")), function(x){x/10}) %>%
  filter(cntyName != "State of Alaska") %>%
  dplyr::select(FIPS, everything()) #reorder so FIPS is first variable

#check whether counties included align between AHRF and mortality data
#base_cnty <- read.csv("../data/base_cnty.csv", colClasses = c("FIPS"="character") )
base_cnty <- read.csv("base_cnty.csv", colClasses = c("FIPS"="character") )
ahrf.fips <- ahrf$FIPS
mort.fips <- base_cnty$FIPS

#fips codes in the mortality data not in AHRF
mort.fips[which(!(mort.fips %in% ahrf.fips))]

# There are two counties in the mortality data that don't match with the AHRF data
# 02270	Wade Hampton	AK
# 46113	Shannon	SD

# FROM THE AHRF DOCUMENTATION:
# 1)	Effective July 1, 2015, Wade Hampton Census Area, Alaska (02270) was changed to 
# Kusilvak Census Area, Alaska (02158). 
# This change was made with the 2015-2016 release of the AHRF.
# 
# 2)	Effective May 1, 2015, Shannon County, South Dakota (46113) was changed to 
# Oglala Lakota County, South Dakota (46102). 
# This change was made with the 2015-2016 release of the AHRF.

# In the documentation we can see that these counties are just indicated with different
# FIPS county codes in the AHRF data, so we change the AHRF data FIPS to match the mortality data

ahrf$FIPS[which(ahrf$FIPS == "02158")] <- "02270" # Wade Hampton AK
ahrf$FIPS[which(ahrf$FIPS == "46102")] <- "46113" # Shannon SD

ahrf.fips <- ahrf$FIPS

#ahrf_1117 = ahrf[,c(1:12,14,18,25:39,45:65,72:74)]

#ahrf_18 = ahrf[,c(1:12,25:38,45:51,54:65)]

#write.csv(ahrf_1117,file='ahrf_1117prop.csv',row.names = FALSE)
write.csv(ahrf,file='ahrf_18.csv',row.names = FALSE)


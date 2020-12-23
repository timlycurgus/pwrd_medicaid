library(dplyr)
library(tidyr)
library(readr)

###################################################################################################
######## NOTE: This pulls data from the AHRF 2019 file ################
## Since we use 2014 data only for this calculation, we do not need to 
## update this file with the new data. File path may need to be changed.
#ahrf_raw  <- paste0("../data-raw/", "AHRF2019.asc") %>%
newelig_raw <- paste0("~/DATA/","AHRF2019.asc") %>%
  read_fwf(fwf_cols(stateFIPS = c(122,123), # 'state FIPS'
                    cntyFIPS = c(124,126), # 'county FIPS'
                    stateName = c(46,64),
                    cntyName = c(67,91),
                    pop_2014 = c(16449,16456),
                    pop1865_2014 = c(26121,26127),
                    pop1865_u138_2014 = c(26721,26727),
                    pop_pov = c(24040,24046),
                    pop_pov017 = c(24128,24133),
                    pop_pov65plus = c(24560,24565)
  ),
  progress=FALSE
  ) 

newelig <- newelig_raw %>%
  mutate(FIPS=paste0(sprintf('%s', stateFIPS), sprintf('%s', cntyFIPS))) %>%
  mutate_at(vars(pop_2014:pop_pov65plus), function(x){as.numeric(x)}) %>%
  filter(cntyName != "State of Alaska") %>%
  dplyr::select(FIPS, everything()) #reorder so FIPS is first variable

#check whether counties included align between AHRF and mortality data
#base_cnty <- read.csv("../data/base_cnty.csv", colClasses = c("FIPS"="character") )
base_cnty <- read.csv("base_cnty.csv", colClasses = c("FIPS"="character") )
newelig.fips <- newelig$FIPS
mort.fips <- base_cnty$FIPS

#fips codes in the mortality data not in AHRF
mort.fips[which(!(mort.fips %in% newelig.fips))]

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

newelig$FIPS[which(newelig$FIPS == "02158")] <- "02270" # Wade Hampton AK
newelig$FIPS[which(newelig$FIPS == "46102")] <- "46113" # Shannon SD

newelig.fips <- newelig$FIPS

write.csv(newelig,file='newelig.csv',row.names = FALSE)

#################### Finish Calculating Tiers for new PWRD Weights #########################

newelig = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/newelig.csv")

newelig$FIPS = as.character(newelig$FIPS)
newelig %>%
  mutate(FIPS = case_when(nchar(FIPS) < 5 ~ paste("0",FIPS,sep=""),
                          nchar(FIPS) > 4 ~ FIPS)) %>%
  filter(stateName !='Hawaii',stateName !='Alaska',stateFIPS < 60,
         cntyName !='Bedford City',cntyName != 'Clifton Forge City') -> newelig

newelig %>%
  mutate(newly_eligible = pmax(pop1865_u138_2014 - (pop_pov - pop_pov017 - pop_pov65plus),0),
         pct_ne = newly_eligible/pop1865_2014) -> newelig

newelig = newelig[order(newelig$pct_ne),]
#strat = c(rep(1,444),rep(2,444),rep(3,444),rep(4,444),rep(5,444),rep(6,444),rep(7,444))

bucket = c(rep(1,518),rep(2,518),rep(3,518),rep(4,518),rep(5,518),rep(6,518))

newelig$bucket = as.factor(bucket)

newelig_df = newelig[,c(1,13,14)]

save(newelig_df,file='newelig_df.Rdata')
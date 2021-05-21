library(dplyr)
library(tidyr)
library(readr)

###################################################################################################
######## NOTE: This pulls data from the AHRF 2019 file ################
## Since we use 2014 data only for this calculation, we do not need to 
## update this file with the new data. File path may need to be changed.
#ahrf_raw  <- paste0("../data-raw/", "AHRF2019.asc") %>%
newelig_raw <- paste0("~/old_ahrf/DATA/","AHRF2019.asc") %>%
  read_fwf(fwf_cols(stateFIPS = c(122,123), # 'state FIPS'
                    cntyFIPS = c(124,126), # 'county FIPS'
                    stateName = c(46,64),
                    cntyName = c(67,91),
                    pop_2014 = c(16449,16456),
                    pop1865_2014 = c(26121,26127),
                    pop1865_2010 = c(26149,26155),
                    pop1865_u138_2014 = c(26721,26727),
                    pop_pov_2014 = c(24040,24046),
                    pop_pov_2010 = c(24070,24077),
                    pop_pov017_2014 = c(24128,24133),
                    pop_pov017_2010 = c(24156,24163),
                    pop_pov65plus = c(24560,24565),
                    medelig21_64 = c(20726,20733),
                    pop_2010 = c(16481,16488)
  ),
  progress=FALSE
  ) 

newelig <- newelig_raw %>%
  mutate(FIPS=paste0(sprintf('%s', stateFIPS), sprintf('%s', cntyFIPS))) %>%
  mutate_at(vars(pop_2014:pop_2010), function(x){as.numeric(x)}) %>%
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

sahie_2010 = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/sahie_2010.csv")

newelig = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/newelig.csv")

sahie_2010%>%
  select(statefips,countyfips,agecat,iprcat,racecat,sexcat,NIPR)%>%
  filter(agecat==1, iprcat == 3,countyfips != 0,sexcat == 0, racecat==0) %>%
  select(-c(agecat,iprcat,racecat,sexcat)) %>%
  rename(pop1865_u138_2010 = NIPR,
         stateFIPS = statefips,
         cntyFIPS = countyfips) %>% right_join(newelig) -> newelig 

newelig$pop1865_u138_2010 = as.numeric(as.character(newelig$pop1865_u138_2010))


newelig$medelig21_64[(is.na(newelig$medelig21_64))] <- 0

newelig$FIPS = as.character(newelig$FIPS)
newelig %>%
  mutate(FIPS = case_when(nchar(FIPS) < 5 ~ paste("0",FIPS,sep=""),
                          nchar(FIPS) > 4 ~ FIPS)) %>%
  filter(stateName !='Hawaii',stateName !='Alaska',stateFIPS < 60,
         cntyName !='Bedford City',cntyName != 'Clifton Forge City') -> newelig

newelig$pop1865_u138_2010[is.na(newelig$pop1865_u138_2010)] = 4214

newelig %>%
  mutate(#newly_eligible = pmax(pop1865_u138_2014 - (pop_pov_2014 - pop_pov017_2014 - pop_pov65plus),0),
         newly_eligible = pmax(pop1865_u138_2010 - (pop_pov_2010 - pop_pov017_2010 - pop_pov65plus),0),
         pct_ne_old = newly_eligible/pop1865_2014) -> newelig

newelig %>%
  mutate(pct_ne = 
           case_when(stateName=='Wisconsin' ~ (pop_pov_2010 - pop_pov017_2010 - pop_pov65plus)/pop1865_2010 - 
                       medelig21_64/pop1865_2010,
                     stateName!='Wisconsin' ~ pop1865_u138_2010/pop1865_2010 - 
                       medelig21_64/pop1865_2010),
         diff = pct_ne_old - pct_ne) -> newelig


#newelig %>%
#  mutate(pct_ne2 = case_when(stateName == 'Massachusetts' ~ 0.01,
#                                    stateName != 'Massachusetts' ~ pct_ne)) -> newelig

newelig = newelig[order(newelig$pct_ne),]

bucket = c(rep(1,518),rep(2,518),rep(3,518),rep(4,518),rep(5,518),rep(6,518))

newelig$bucket = as.factor(bucket)

newelig = newelig[order(newelig$pct_ne_old),]
newelig$bucket2 = as.factor(bucket)

newelig$pct_ne[(newelig$pct_ne < 0)] = 0

newelig_df = newelig[,c(4,19,20,22,23)]


save(newelig_df,file='newelig_df_updated.Rdata')
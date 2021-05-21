##' ---
##' title: "Calculate Propensity Scores by County"
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

subgroup_alldata <- read.csv("~/pwrd_medicaid/cc-est2019-alldata.csv")
subgroup_alldata <- read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/cc-est2019-alldata.csv")
#subgroup_1317 = subset(subgroup_alldata,YEAR %in% c(6,7,8,9,10))
#remove(subgroup_alldata)
#write.csv(subgroup_1317,file='subgroup_1317.csv',row.names=FALSE)
#subgroup_1317 = read.csv("~/mnt/compute1-tlycurgu/subgroup_1317.csv")
#subgroup_alldata <- read.csv("~/cc-est2019-alldata.csv")

subgroup_alldata %>% 
  filter(AGEGRP %in% c(0,5:13)) %>%
  mutate(agegrp = case_when(AGEGRP==0~'0_85plus',
                            AGEGRP%in%c(5:7)~'20_34',
                            AGEGRP%in%c(8:9)~'35_44',
                            AGEGRP%in%c(10:11)~'45_54',
                            AGEGRP%in%c(12:13)~'55_64'),
         latino = (H_MALE+H_FEMALE)/TOT_POP,
         O_MALE = TOT_MALE - NHWA_MALE - NHBA_MALE - NHIA_MALE - NHAA_MALE - H_MALE,
         O_FEMALE = TOT_FEMALE - NHWA_FEMALE - NHBA_FEMALE - NHIA_FEMALE - NHAA_FEMALE - H_FEMALE,
         o_pct = (O_MALE + O_FEMALE)/TOT_POP,
         white_pct = (NHWA_MALE + NHWA_FEMALE)/TOT_POP,
         black_pct = (NHBA_MALE + NHBA_FEMALE)/TOT_POP,
         asian_pct = (NHAA_MALE + NHAA_FEMALE)/TOT_POP,
         indig_pct = (NHIA_MALE + NHIA_FEMALE)/TOT_POP,
         stateFIPS = case_when(STATE < 10 ~ paste("0",STATE,sep=""),
                               STATE > 9 ~ paste(STATE,sep="")),
         cntyFIPS = case_when(COUNTY < 10 ~ paste("00",COUNTY,sep=""),
                              COUNTY > 9 & COUNTY < 100 ~ paste("0",COUNTY,sep=""),
                              COUNTY > 99 ~ paste(COUNTY,sep="")),
         FIPS = paste(stateFIPS,cntyFIPS,sep="")
  ) -> subgroup_1317

subgroup_1317 %>%
  select(6:8,92) %>%
  filter(YEAR == 7) %>%
  spread(AGEGRP,TOT_POP) -> age_temp

age_temp %>%
  rename(p5 = '5',p6 = '6', p7 = '7', p8 = '8',p9 = '9',p10 = '10',
         p11 = '11', p12 = '12', p13 = '13', p0 = '0') %>%
  mutate(pct_2034 = (p5 + p6 + p7)/(p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13),
         pct_3544 = (p8 + p9)/(p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13),
         pct_4554 = (p10 + p11)/(p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13),
         pct_5564 = (p12 + p13)/(p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13)) %>%
  select(-c(YEAR,p5,p6,p7,p8,p9,p10,p11,p12,p13,p0)) -> age_temp
  
subgroup_1317$YEAR = subgroup_1317$YEAR + 2007

subgroup_1317 %>% 
  filter(YEAR == 2014,AGEGRP==0) -> subgroup_1317

subgroup_1317 %>% 
  select(4,5,8,82,85:92) -> subgroup_1317

subgroup_1317 %>% 
  left_join(age_temp) -> subgroup_1317

subgroup_1317$FIPS[which(subgroup_1317$FIPS == "02158")] <- "02270" # Wade Hampton AK
subgroup_1317$FIPS[which(subgroup_1317$FIPS == "46102")] <- "46113" # Shannon SD

ahrf_1117 = read.csv("~/pwrd_medicaid/ahrf_1117prop.csv")
#ahrf_1117 = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/ahrf_prop.csv")

ahrf_1117 %>% 
  select(-c('pop_2011','medIncome_2011','pctPovty_2011','unplmtRate_2011')) %>%
  gather(pop_ahrf,pop_count_ahrf,c(pop_2017:pop_2012)) %>% 
  mutate(YEAR = case_when(pop_ahrf == 'pop_2017' ~ 2017,
                          pop_ahrf == 'pop_2016' ~ 2016,
                          pop_ahrf == 'pop_2015' ~ 2015,
                          pop_ahrf == 'pop_2014' ~ 2014,
                          pop_ahrf == 'pop_2013' ~ 2013,
                          pop_ahrf == 'pop_2012' ~ 2012),
         FIPS = case_when(FIPS < 10000 ~ paste("0",FIPS,sep = ""),
                          FIPS > 9999 ~ paste(FIPS,sep=""))) %>%
  select(-c(pop_ahrf)) -> temp

Deaths = (as.numeric(as.character(temp$deaths_2011))+ as.numeric(as.character(temp$deaths_2012)) + 
            as.numeric(as.character(temp$deaths_2013)))/3

death_df = data.frame(YEAR = temp$YEAR, FIPS = temp$FIPS, Deaths = Deaths)
death_df %>% filter(YEAR == 2014) %>% select(-c(YEAR)) -> death_df

ahrf_subgroup = temp %>% 
  select(1,6,7,20,27,28,44,45)

ahrf_1117 %>% 
  gather(inc_year,med_income,c(medIncome_2017:medIncome_2012)) %>% 
  select(c('med_income')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,med_income = temp$med_income)

ahrf_1117 %>% 
  gather(pvt_year,pov,c(pctPovty_2017:pctPovty_2012)) %>% 
  select(c('pov')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,pov = temp$pov)

ahrf_1117 %>% 
  gather(unemp_year,unemp,c(unplmtRate_2017:unplmtRate_2012)) %>% 
  select(c('unemp')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,unemp = temp$unemp)

ahrf_1117 %>% 
  gather(medicaid_year,med_elig,c(popu65u138_2017:popu65u138_2012)) %>% 
  select(c('med_elig')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,med_elig = temp$med_elig)

ahrf_1117 %>% 
  gather(unins_year,unins,c(noins_u65u138_2017:noins_u65u138_2012)) %>% 
  select(c('unins')) -> temp

ahrf_subgroup = cbind(ahrf_subgroup,unins = temp$unins)

unins_df = data.frame(YEAR = ahrf_subgroup$YEAR, FIPS = ahrf_subgroup$FIPS, 
                      unins = ahrf_subgroup$unins)

unins_df %>% filter(YEAR == 2013) %>% select(-c(YEAR)) %>% 
  left_join(death_df)-> unins_df

ahrf_subgroup %>%
  filter(YEAR == 2014) %>%
  select(-c(unins)) -> ahrf_subgroup

ahrf_subgroup %>% left_join(unins_df) -> ahrf_subgroup

prop_df = left_join(subgroup_1317,ahrf_subgroup)


state.df = data.frame(STNAME = c(state.name,'District of Columbia'),
                      expansion = c(9999,2015,2014,2014,2014,2014,2014,2014,9999,9999,
                                    2014,2020,2014,2015,2014,9999,2014,2016,2019,2014,
                                    2014,2014,2014,9999,9999,2016,2020,2014,2014,2014,
                                    2014,2014,9999,2014,2014,9999,2014,2015,2014,9999,
                                    9999,9999,9999,2020,2014,2019,2014,2014,2014,9999,2014))

prop_df %>%
  left_join(state.df) %>%
  mutate(Tx = case_when(expansion == 2014 ~ 1,
                        expansion != 2014 ~ 0)) -> prop_df

############# Above this, data managing. Below this, prop calculations ##########
library(arm)
library(dplyr)
library(RItools)
library(ggplot2)
prop_df = read.csv("~/mnt/compute1-tlycurgu/pwrd_medicaid/propcalc_df.csv")

prop_df %>%
  rename(hisp_pct = latino,
         pctUrban = pctUrban_2010, popDens = popDens_2010,
         vetPop = vetPop_2014, snap = snap_2013,
         avgPM25 = avgPM25_2011
         ) -> prop_df

prop_df %>% 
  filter(STNAME !='Hawaii',STNAME !='Alaska') %>%
  mutate(unins_pct = unins/TOT_POP,
         medelig_pct = med_elig/TOT_POP) -> prop_df


## SLB: AGE, Race, Poverty, Med. Income, Unemp., Unins. (2013), BASELINE MORTALITY
prop_mod = bayesglm(Tx ~ asian_pct + hisp_pct + black_pct + indig_pct + 
                      pct_2034 + pct_3544 + pct_4554 + pct_5564 + Deaths + 
             med_income + pov + unemp + unins_pct + log(TOT_POP),
             data = prop_df,family='binomial')

propensities = predict(prop_mod,type='response')
prop_df$propensities = propensities

df_histograms = data.frame(Propensities = prop_df$propensities, Expansion = prop_df$Tx,
                           Wts = log(prop_df$TOT_POP))
df_histograms$Expansion = as.factor(df_histograms$Expansion)

ggplot(df_histograms, aes(x = Propensities,color=Expansion,weight=Wts)) + 
  geom_histogram(alpha=0.5,position="identity",fill="white",bins=20)


prop_df = prop_df[order(prop_df$propensities),]
#strat = c(rep(1,444),rep(2,444),rep(3,444),rep(4,444),rep(5,444),rep(6,444),rep(7,444))

strat = c(rep(1,259),rep(2,259),rep(3,259),rep(4,259),rep(5,259),rep(6,259),rep(7,259),
          rep(8,259),rep(9,259),rep(10,259),rep(11,259),rep(12,259))

prop_df$strat = as.factor(strat)

xBalance(Tx ~ asian_pct + hisp_pct + black_pct + indig_pct + 
           pct_2034 + pct_3544 + pct_4554 + pct_5564 + Deaths + 
           med_income + pov + unemp + unins_pct + log(TOT_POP), strata = as.factor(strat),
         data = prop_df, report=c("adj.mean.diffs", "chisquare.test"))

save(prop_df,file='strat_data.Rdata')




########### With Charlotte's PScores ##############
load("~/mnt/compute1-tlycurgu/pwrd_medicaid/pscores.Rdata")

state.df = data.frame(stateName = c(state.name,'District of Columbia'),
                      expansion = c(9999,2015,2014,2014,2014,2014,2014,2014,9999,9999,
                                    2014,2020,2014,2015,2014,9999,2014,2016,2019,2014,
                                    2014,2014,2014,9999,9999,2016,2020,2014,2014,2014,
                                    2014,2014,9999,2014,2014,9999,2014,2015,2014,9999,
                                    9999,9999,9999,2020,2014,2019,2014,2014,2014,9999,2014))

mod.dat %>%
  left_join(state.df) %>%
  mutate(Tx = case_when(expansion == 2014 ~ 1,
                        expansion != 2014 ~ 0)) -> mod.dat

mod.dat = mod.dat[order(mod.dat$pscore2),]

mod.dat %>% filter(!(is.na(pscore2))) -> mod.dat



strat = c(rep(1,157),rep(2,153),rep(3,153),rep(4,152),rep(5,152),rep(6,152),
          rep(7,152),rep(8,152),rep(9,152),rep(10,152),rep(11,152),rep(12,152),
          rep(13,152),rep(14,152),rep(15,152),rep(16,152),rep(17,152),rep(18,153),
          rep(19,153),rep(20,157))

strat = c(rep(1,148),rep(2,148),rep(3,148),rep(4,148),rep(5,148),rep(6,148),rep(7,148),
          rep(8,148),rep(9,148),rep(10,148),rep(11,148),rep(12,148),rep(13,148),rep(14,148),
          rep(15,148),rep(16,148),rep(17,148),rep(18,148),rep(19,148),rep(20,148),rep(21,148))




strat = c()
no_strat = 500
for(i in 1:no_strat){
  size = floor(3054/no_strat)
  rem = 3054%%no_strat
  if(i == 1){
    strat = rep(1,size+rem/2)
  }
  if(i !=1 && i !=no_strat){
    strat = c(strat,rep(i,size))
  }
  if(i == no_strat){
    strat = c(strat,rep(i,size + rem/2))
  }
}

mod.dat$strat = as.factor(strat)

xBalance(Tx ~ log_adult_w_a_cnt +
           white_race+black_race+latino + male+
           a20_34+a35_44+a45_54+a55_64+ mortAC_20_64+
           mortAC_20_34 + mortAC_35_44 + mortAC_45_54 + mortAC_55_64 +
           mortACWhite_M+mortACWhite_F+mortACBlack_M+mortACBlack_F+
           mortACother_M+mortACother_F+ 
           mortHC_amenable_not_flu + mortOpioid + mortFlu+
           popDens_2010+pctUrban_2010+vetPop_2013+medIncome_2013+
           pctPovty_2013 + snap_2013 + pctNoIns_2013+unplmtRate_2013+
           avgPM25_2011+smk_tot_2012+alc_2012+
           diabetes_2012 + hyper_male_2009 +
           hyper_female_2009 + obsty_male_2011 +
           obsty_female_2011 + phys_act_male_2011 + 
           phys_act_female_2011 + pctRep_2012 + calc_multi_house,
         strata = as.factor(strat),data=mod.dat,report=c("adj.mean.diffs", "chisquare.test"))

xBalance(Tx ~ white_race + black_race + latino + male + 
           a20_34 + a35_44 + a45_54 + a55_64 + mortHC_amenable_not_flu + 
           medIncome_2013 + pctPovty_2013 + unplmtRate_2013 + 
           pctNoIns_2013, #+ log_10_adult_w_a_cnt,
         strata = as.factor(strat),
         data = mod.dat, report=c("adj.mean.diffs", "chisquare.test"))



test_df = data.frame(Tx = c(rep(0,40),rep(1,10),rep(0,10),rep(1,40)),
                     st = c(rep(1,50),rep(2,50)),
                     X = c(rnorm(40,1,0.05),rnorm(10,1.04,0.05),
                           rnorm(10,2,0.05),rnorm(40,2.04,0.05)))

st = test_df$st
xBalance(Tx ~ X,strata = as.factor(test_df$st),data = test_df,
         report=c("adj.mean.diffs", "chisquare.test"))

,strata = st)
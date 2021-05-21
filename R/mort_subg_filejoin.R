######## Processed through here ##############

## Read in all det_mort files into det_mort_comb
#load('~/mnt/compute1-tlycurgu/pwrd_medicaid/det_mort_2013.Rdata')
load('det_mort_2013.Rdata')
det_mort_comb = mort_det_sum
#load('~/mnt/compute1-tlycurgu/pwrd_medicaid/det_mort_2014.Rdata')
load('det_mort_2014.Rdata')
det_mort_comb = rbind(det_mort_comb, mort_det_sum)
#load('~/mnt/compute1-tlycurgu/pwrd_medicaid/det_mort_2015.Rdata')
load('det_mort_2015.Rdata')
det_mort_comb = rbind(det_mort_comb, mort_det_sum)
#load('~/mnt/compute1-tlycurgu/pwrd_medicaid/det_mort_2016.Rdata')
load('det_mort_2016.Rdata')
det_mort_comb = rbind(det_mort_comb, mort_det_sum)
#load('~/mnt/compute1-tlycurgu/pwrd_medicaid/det_mort_2017.Rdata')
load('det_mort_2017.Rdata')
det_mort_comb = rbind(det_mort_comb, mort_det_sum)
#load('~/mnt/compute1-tlycurgu/pwrd_medicaid/det_mort_2018.Rdata')
load('det_mort_2018.Rdata')
det_mort_comb = rbind(det_mort_comb, mort_det_sum)


det_mort_comb$race_bridged[det_mort_comb$race_bridged == "Hispanic"]  <- "H"

det_mort_comb %>%
  mutate(race_sex = case_when(sex=='M' ~ paste(race,'_MALE',sep=""),
                              sex=='F' ~ paste(race,'_FEMALE',sep=""))) -> det_mort_comb

det_mort_comb %>% 
  #filter(c_o_d_cat == "HCA") -> det_mort_comb
  filter(c_o_d_cat == "HCUA") -> det_mort_comb

colnames(det_mort_comb) = c('staterFIPS','cntyFIPS','YEAR','race','race_bridged','agegrp','sex',
                            'c_o_d','HCUA','race_sex')

save(det_mort_comb, file = 'det_mort_combHCUA.Rdata')

###### For All Cause Mortality ######
det_mort_comb %>%
  filter(c_o_d_cat == "HCUA") -> mort_HCUA

det_mort_comb %>%
  filter(c_o_d_cat == "HCA") -> mort_HCA

mort_HCUA = mort_HCUA[,c(1:7,9,10)]
mort_HCA = mort_HCA[,c(1:7,9,10)]

colnames(mort_HCA) = c('staterFIPS','cntyFIPS','YEAR','race','race_bridged','agegrp','sex',
                            'HCA','race_sex')
colnames(mort_HCUA) = c('staterFIPS','cntyFIPS','YEAR','race','race_bridged','agegrp','sex',
                            'HCUA','race_sex')

mort_allcause = full_join(mort_HCA, mort_HCUA)
mort_allcause$HCA[(is.na(mort_allcause$HCA))] <- 0
mort_allcause$HCUA[(is.na(mort_allcause$HCUA))] <- 0
mort_allcause$All.Cause = mort_allcause$HCA + mort_allcause$HCUA
mort_allcause$c_o_d = 'All'
mort_allcause = mort_allcause[,c(1:7,9,11,12)]

save(mort_allcause, file = 'det_mort_combALL.Rdata')

########## New Processing ############
require(dplyr)
require(readr)
## Need to read in subgroup_1318.csv
subgroup_1318 = read_csv('subgroup_pop1318.csv')
load('det_mort_comb1318.Rdata')

det_mort_comb1318 %>% 
  ungroup %>%
  select(-c('race_bridged')) -> det_mort_comb1318

det_mort_comb1318 %>%
  #ungroup %>%
  #select(-c('race_bridged')) %>%
  group_by_at(setdiff(names(det_mort_comb1318),'HCUA')) %>%
  summarize(HCUA = sum(HCUA)) -> det_mort_comb1318

state.df = data.frame(STNAME = c(state.name,'District of Columbia'),staterFIPS = c(state.abb,'DC'))

subgroup_1318 = left_join(subgroup_1318,state.df)

subgroup_1318 = left_join(subgroup_1318,det_mort_comb1318)

medicaid_comp1318ALL = subgroup_1318
save(medicaid_comp1318ALL,file='medicaid_comp1318ALL.Rdata')

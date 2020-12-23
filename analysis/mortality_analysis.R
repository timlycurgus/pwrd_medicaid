##'
##' title: "Mortality outcome analyses in style of SLB 2014"
##' output: github_document
##' 
##+ setup-packages, echo=FALSE, warning=FALSE, message=FALSE
library(MASS)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)
library(clubSandwich)
library(survey)
stopifnot(getRversion() >="3.5.0") # cf below use of `factor(...,levels=,labels=)`
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

##' ## Data
##'
##' Read in data with mortality counts, matches,
##' stratifications, and newly eligible buckets
##' 
#load('~/mnt/compute1-tlycurgu/pwrd_medicaid/medicaid_comp.Rdata')
load('medicaid_comp1318.Rdata')
load('mod.dat.Rdata')
load('strat_data.Rdata')
load('newelig_df.Rdata')
state.df = read.csv('state_df.csv')
medicaid_comp = medicaid_comp1318

mod.dat %>%
  select(c(1,51)) -> match_data

prop_df$FIPS = as.character(prop_df$FIPS)

prop_df %>%
  select(c(12,35)) %>%
  mutate(FIPS = case_when(nchar(FIPS) < 5 ~ paste("0",FIPS,sep=""),
                          nchar(FIPS) > 4 ~ FIPS)) -> strat_data

medicaid_comp = left_join(medicaid_comp,match_data)
medicaid_comp = left_join(medicaid_comp,strat_data)
medicaid_comp = left_join(medicaid_comp,newelig_df)


medicaid_comp %>% 
  filter(pop_count > 0) -> medicaid_comp

medicaid_comp$HCA[is.na(medicaid_comp$HCA)] <- 0

medicaid_comp %>%
  left_join(state.df) %>%
  mutate(yrs_expansion = YEAR - expansion,
         Tx = case_when(yrs_expansion >-1 ~ 1,
                        yrs_expansion < 0 ~ 0)) -> medicaid_comp


######## Weights by Bucket calculated on Control Group ############

medicaid_comp %>%
  filter(YEAR > 2013) %>%
         #,Tx==0) %>%
  #filter(YEAR < 2018) %>% 
  ungroup() %>%
  select(YEAR,FIPS,yrs_expansion,med_elig,Tx,pct_ne,bucket) %>%
  unique()-> wts_agg

bucket_pct = aggregate(wts_agg$pct_ne,by=list(wts_agg$bucket),mean,na.rm=TRUE)
wts_bct = data.frame(bucket = as.factor(c(1:6)),weights_bct = bucket_pct$x/sum(bucket_pct$x))

medicaid_comp %>% left_join(wts_bct) -> medicaid_comp

########################## With Tx:Bucket ##################################

nb0  <- nb <- list()

system.time(
  nb0$strat <- glm.nb(HCA~ 
                        bucket + race_sex*agegrp +
                        pov + med_income + unemp +
                        YEAR + stateFIPS + strat + 
                        offset(log(pop_count)),
                      data=filter(medicaid_comp,
                                  !is.na(strat),
                                  pop_count>0,
                                  YEAR > 2013),
                      maxit=100#, trace=2
  )
)

system.time(
  nb$strat  <- glm.nb(update(formula(nb0$strat), .~.+Tx:bucket),
                      start=c(coef(nb0$strat), "bucket1:Tx"=0,"bucket2:Tx"=0,"bucket3:Tx"=0,
                              "bucket4:Tx"=0,"bucket5:Tx"=0,"bucket6:Tx"=0),
                      init.theta=nb0$strat$theta,
                      data=filter(medicaid_comp,
                                  !is.na(strat),
                                  pop_count>0,
                                  YEAR > 2013),
                      trace=0#2
  )
)

ests = coef(nb$strat)[c("bucket1:Tx","bucket2:Tx","bucket3:Tx","bucket4:Tx",
                        "bucket5:Tx","bucket6:Tx")]
wb = wts_bct$weights_bct

nb$strat$vcovHC1 <- sandwich::vcovCL(nb$strat,cluster=~stateFIPS,type='HC1')
var_terms = wb[1]^2*nb$strat$vcovHC1["bucket1:Tx","bucket1:Tx"] + 
  wb[2]^2*nb$strat$vcovHC1["bucket2:Tx","bucket2:Tx"] + 
  wb[3]^2*nb$strat$vcovHC1["bucket3:Tx","bucket3:Tx"] + 
  wb[4]^2*nb$strat$vcovHC1["bucket4:Tx","bucket4:Tx"] + 
  wb[5]^2*nb$strat$vcovHC1["bucket5:Tx","bucket5:Tx"] + 
  wb[6]^2*nb$strat$vcovHC1["bucket6:Tx","bucket6:Tx"]
cov_terms = 2*(wb[1]*wb[2]*nb$strat$vcovHC1["bucket1:Tx","bucket2:Tx"] +
  wb[1]*wb[3]*nb$strat$vcovHC1["bucket1:Tx","bucket3:Tx"] +
  wb[1]*wb[4]*nb$strat$vcovHC1["bucket1:Tx","bucket4:Tx"] +
  wb[1]*wb[5]*nb$strat$vcovHC1["bucket1:Tx","bucket5:Tx"] +
  wb[1]*wb[6]*nb$strat$vcovHC1["bucket1:Tx","bucket6:Tx"] +
  wb[2]*wb[3]*nb$strat$vcovHC1["bucket2:Tx","bucket3:Tx"] +
  wb[2]*wb[4]*nb$strat$vcovHC1["bucket2:Tx","bucket4:Tx"] +
  wb[2]*wb[5]*nb$strat$vcovHC1["bucket2:Tx","bucket5:Tx"] +
  wb[2]*wb[6]*nb$strat$vcovHC1["bucket2:Tx","bucket6:Tx"] +
  wb[3]*wb[4]*nb$strat$vcovHC1["bucket3:Tx","bucket4:Tx"] +
  wb[3]*wb[5]*nb$strat$vcovHC1["bucket3:Tx","bucket5:Tx"] +
  wb[3]*wb[6]*nb$strat$vcovHC1["bucket3:Tx","bucket6:Tx"] +
  wb[4]*wb[5]*nb$strat$vcovHC1["bucket4:Tx","bucket5:Tx"] +
  wb[4]*wb[6]*nb$strat$vcovHC1["bucket4:Tx","bucket6:Tx"] +
  wb[5]*wb[6]*nb$strat$vcovHC1["bucket5:Tx","bucket6:Tx"])
  
ests%*%wb/sqrt(var_terms + cov_terms)

## Unweighted version
system.time(
  nb$stratuw  <- glm.nb(update(formula(nb0$strat), .~.+Tx),
                      start=c(coef(nb0$strat), "Tx"=0),
                      init.theta=nb0$strat$theta,
                      data=filter(medicaid_comp,
                                  !is.na(strat),
                                  pop_count>0,
                                  YEAR > 2013),
                      trace=0#2
  )
)

nb$stratuw$vcovHC1 <- sandwich::vcovCL(nb$stratuw,cluster=~stateFIPS,type='HC1')
coef(nb$stratuw)["Tx"]/
  sqrt(nb$stratuw$vcovHC1["Tx","Tx"])



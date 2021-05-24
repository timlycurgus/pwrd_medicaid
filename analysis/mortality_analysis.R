##'
##' title: "Mortality outcome analyses for 2015-2018"
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
library(lme4)
stopifnot(getRversion() >="3.5.0") # cf below use of `factor(...,levels=,labels=)`
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

##' ## Data
##'
##' Read in data with mortality counts, matches,
##' stratifications, and newly eligible buckets
##' 

## CM - add path to compute1 directory
path <- file.path("..","..","..", "compute1","data", "pwrd_medicaid")

# Load Healthcare Amenable Data
#load('~/mnt/compute1-tlycurgu/pwrd_medicaid/medicaid_comp1318.Rdata')
load(file.path(path, 'medicaid_comp1318.Rdata'))
medicaid_comp1318 %>%
  rename(death_count = HCA) -> medicaid_comp

# Load All Cause Mortality Data
#load('~/mnt/compute1-tlycurgu/pwrd_medicaid/medicaid_comp1318.Rdata')
load(file.path(path,'medicaid_comp1318ALL.Rdata'))
medicaid_comp1318ALL %>%
  rename(death_count = All.Cause) -> medicaid_comp

## CM: This overwrites the medicaid_comp data, so it is only all cause

load('mod.dat_final.Rdata')
load('newelig_df_updated.Rdata')
state.df = read.csv('state_df.csv')

mod.dat %>%
  select(c(1,52)) -> match_data

medicaid_comp = left_join(medicaid_comp,match_data)
medicaid_comp = left_join(medicaid_comp,newelig_df)

medicaid_comp %>% 
  filter(pop_count > 0) -> medicaid_comp

medicaid_comp$death_count[is.na(medicaid_comp$death_count)] <- 0

medicaid_comp %>%
  left_join(state.df) %>%
  mutate(yrs_expansion = YEAR - expansion,
         Tx = case_when(yrs_expansion >-1 ~ 1,
                        yrs_expansion < 0 ~ 0)) -> medicaid_comp

######## Weights by Bucket calculated on Control Group ############

medicaid_comp %>%
  filter(YEAR>2014,!is.na(matches.final),
         pop_count > 0, Tx == 0) -> control

bracket_pct = aggregate(control$pct_ne,by=list(control$bucket),mean,na.rm=TRUE)
wts_bct = data.frame(bracket = as.factor(1:6), weights_bct = bracket_pct$x/sum(bracket_pct$x))

## CM: My understanding from this is that the weights are proportional to the percent newly eligible in each bucket

full_formula <- death_count ~ Tx + offset(log(pop_count))
PB_formula <- update(full_formula,.~. - Tx)

mod_control  <- glm.nb(PB_formula, 
                     data=control)

## CM: these predictions are on the scale of hte linear predictor
fits_cont = predict(mod_control)
dummies_buck = model.matrix(~factor(control$bucket)-1)
b_names = c('B1','B2','B3','B4','B5','B6')
colnames(dummies_buck) = b_names

## CM: predicting death count using the buckets only and the coefficients from the control model
mod_c = glm.nb(death_count ~ dummies_buck - 1 + offset(fits_cont), data=control)

## CM: sandwhich estimate of covariance matrix w original method, clustered by county
sigma_exp = vcovCR(mod_c,cluster=control$FIPS,type='CR0')

##CM: Inverse variance weighting?
weights_b = t(wts_bct$weights_bct%*%solve(sigma_exp))

##CM: This step gives more weight to the higher buckets and lessens the weight to the lower buckets
wts_bct = data.frame(bucket = as.factor(c(1:6)),weights_bct = weights_b/sum(weights_b))
medicaid_comp %>% left_join(wts_bct) -> medicaid_comp

########################## With Tx:Bucket ##################################

nb0  <- nb <- list()

system.time(
  nb0$fixef <- glm.nb(death_count ~
                         race_sex*agegrp +
                        scale(pov) + scale(med_income) + scale(unemp) +
                        as.character(YEAR) + stateFIPS + 
                        offset(log(pop_count)) - 1,
                      data=filter(medicaid_comp,
                                  !is.na(matches.final),
                                  pop_count>0,
                                  YEAR > 2014),
                      maxit=100#, trace=2
  )
)

system.time(
  nb$fixef  <- glm.nb(update(formula(nb0$fixef), .~.+Tx:bucket+Tx:as.character(YEAR)),
                      start=c(coef(nb0$fixef), "Tx:bucket1"=0,"Tx:bucket2"=0,"Tx:bucket3"=0,
                              "Tx:bucket4"=0,"Tx:bucket5"=0,"Tx:bucket6"=0,
                              "Tx:as.character(YEAR)2016"=0,"Tx:as.character(YEAR)2017"=0,
                              "Tx:as.character(YEAR)2018"=0),
                      init.theta=nb0$fixef$theta,
                      data=filter(medicaid_comp,
                                  !is.na(matches.final),
                                  pop_count>0,
                                  YEAR > 2014),
                      trace=0#2
  )
)

ests = coef(nb$fixef)[c("Tx:bucket1","Tx:bucket2","Tx:bucket3","Tx:bucket4",
                        "Tx:bucket5","Tx:bucket6")]
wb = wts_bct$weights_bct

nb$fixef$vcovHC1 <- sandwich::vcovCL(nb$fixef,cluster=~stateFIPS,type='HC1')

#CM - checking manual calculate variance and covariance terms 
bucket_names <- c("Tx:bucket1","Tx:bucket2","Tx:bucket3","Tx:bucket4",
             "Tx:bucket5","Tx:bucket6")

cov.mat <- nb$fixef$vcovHC1[bucket_names, bucket_names]
wb.mat <- matrix(rep(wb, 6), nrow = 6, byrow = T)
var.cov.mat <- wb.mat * t(wb.mat) * cov.mat
var.terms = sum(diag(var.cov.mat))
cov.terms = 2*sum(var.cov.mat*upper.tri(var.cov.mat))

var_terms = wb[1]^2*nb$fixef$vcovHC1["Tx:bucket1","Tx:bucket1"] + 
  wb[2]^2*nb$fixef$vcovHC1["Tx:bucket2","Tx:bucket2"] + 
  wb[3]^2*nb$fixef$vcovHC1["Tx:bucket3","Tx:bucket3"] + 
  wb[4]^2*nb$fixef$vcovHC1["Tx:bucket4","Tx:bucket4"] + 
  wb[5]^2*nb$fixef$vcovHC1["Tx:bucket5","Tx:bucket5"] + 
  wb[6]^2*nb$fixef$vcovHC1["Tx:bucket6","Tx:bucket6"]
  
cov_terms = 2*(wb[1]*wb[2]*nb$fixef$vcovHC1["Tx:bucket1","Tx:bucket2"] +
                 wb[1]*wb[3]*nb$fixef$vcovHC1["Tx:bucket1","Tx:bucket3"] +
                 wb[1]*wb[4]*nb$fixef$vcovHC1["Tx:bucket1","Tx:bucket4"] +
                 wb[1]*wb[5]*nb$fixef$vcovHC1["Tx:bucket1","Tx:bucket5"] +
                 wb[1]*wb[6]*nb$fixef$vcovHC1["Tx:bucket1","Tx:bucket6"] +
                 wb[2]*wb[3]*nb$fixef$vcovHC1["Tx:bucket2","Tx:bucket3"] +
                 wb[2]*wb[4]*nb$fixef$vcovHC1["Tx:bucket2","Tx:bucket4"] +
                 wb[2]*wb[5]*nb$fixef$vcovHC1["Tx:bucket2","Tx:bucket5"] +
                 wb[2]*wb[6]*nb$fixef$vcovHC1["Tx:bucket2","Tx:bucket6"] +
                 wb[3]*wb[4]*nb$fixef$vcovHC1["Tx:bucket3","Tx:bucket4"] +
                 wb[3]*wb[5]*nb$fixef$vcovHC1["Tx:bucket3","Tx:bucket5"] +
                 wb[3]*wb[6]*nb$fixef$vcovHC1["Tx:bucket3","Tx:bucket6"] +
                 wb[4]*wb[5]*nb$fixef$vcovHC1["Tx:bucket4","Tx:bucket5"] +
                 wb[4]*wb[6]*nb$fixef$vcovHC1["Tx:bucket4","Tx:bucket6"] +
                 wb[5]*wb[6]*nb$fixef$vcovHC1["Tx:bucket5","Tx:bucket6"])

## CM: looks good!
cov_terms == cov.terms
var_terms - var.terms < .00000001

## CM: What is this value?
ests%*%wb/sqrt(var_terms + cov_terms)

##CM: Why do you not include the treatment year interactions in the unweighted model?

### Unweighted version ###
system.time(
  nb$fixefuw  <- glm.nb(update(formula(nb0$fixef), .~.+Tx),
                      start=c(coef(nb0$fixef), "Tx"=0),
                      init.theta=nb0$fixef$theta,
                      data=filter(medicaid_comp,
                                  !is.na(matches.final),
                                  pop_count>0,
                                  YEAR > 2014),
                      trace=0#2
  )
)

nb$fixefuw$vcovHC1 <- sandwich::vcovCL(nb$fixefuw,cluster=~stateFIPS,type='HC1')
coef(nb$fixefuw)["Tx"]/
  sqrt(nb$fixefuw$vcovHC1["Tx","Tx"])

########### Random Effects Model ####################

system.time(
  nb$rew <- glmer(death_count ~  Tx:bucket + as.character(YEAR) + 
                        race_sex*agegrp +
                        scale(pov) + scale(med_income) + scale(unemp) +
                        Tx:(as.character(YEAR)) + stateFIPS + 
                        offset(log(pop_count)) + 
                          (1|matches.final),
                     verbose = FALSE,
                     family = MASS::negative.binomial(theta= nb$fixef$theta),
                      data=filter(medicaid_comp,
                                  !is.na(matches.final),
                                  pop_count>0,
                                  YEAR > 2014),
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000))
  )
)

## CM: load output to check, since I don't run the random effects model
load(file.path(path,'nb_reHCA.Rdata'))
load(file.path(path,'nb_reALL.Rdata'))

ests = fixef(nb$rew)[c("Tx:bucket1","Tx:bucket2","Tx:bucket3","Tx:bucket4",
                        "Tx:bucket5","Tx:bucket6")]
wb = wts_bct$weights_bct
ests%*%wb

##CM: This isn't working for me
require(merDeriv)
system.time(
  rew_vcovCR0 <- vcov(nb$rew, full = TRUE, ranpar = 'var')
  )

## CM: I would change these to the names of the columns, just to be confident if the columns change
rel_mat = rew_vcovCR0[70:75, 70:75]

var_terms = sum(diag(rel_mat)%*%c(wb^2))
wts_mat = c(wb)%*%t(c(wb))
wts_vec = wts_mat[lower.tri(wts_mat,diag = FALSE)]

vcov_vec = rel_mat[lower.tri(rel_mat, diag = FALSE)]
cov_terms = 2*(wts_vec%*%vcov_vec)


var_terms = wb[1]^2*rew_vcovCR0["Tx:bucket1","Tx:bucket1"] + 
  wb[2]^2*rew_vcovCR0["Tx:bucket2","Tx:bucket2"] + 
  wb[3]^2*rew_vcovCR0["Tx:bucket3","Tx:bucket3"] + 
  wb[4]^2*rew_vcovCR0["Tx:bucket4","Tx:bucket4"] + 
  wb[5]^2*rew_vcovCR0["Tx:bucket5","Tx:bucket5"] + 
  wb[6]^2*rew_vcovCR0["Tx:bucket6","Tx:bucket6"]

cov_terms = 2*(wb[1]*wb[2]*rew_vcovCR0["Tx:bucket1","Tx:bucket2"] +
                 wb[1]*wb[3]*rew_vcovCR0["Tx:bucket1","Tx:bucket3"] +
                 wb[1]*wb[4]*rew_vcovCR0["Tx:bucket1","Tx:bucket4"] +
                 wb[1]*wb[5]*rew_vcovCR0["Tx:bucket1","Tx:bucket5"] +
                 wb[1]*wb[6]*rew_vcovCR0["Tx:bucket1","Tx:bucket6"] +
                 wb[2]*wb[3]*rew_vcovCR0["Tx:bucket2","Tx:bucket3"] +
                 wb[2]*wb[4]*rew_vcovCR0["Tx:bucket2","Tx:bucket4"] +
                 wb[2]*wb[5]*rew_vcovCR0["Tx:bucket2","Tx:bucket5"] +
                 wb[2]*wb[6]*rew_vcovCR0["Tx:bucket2","Tx:bucket6"] +
                 wb[3]*wb[4]*rew_vcovCR0["Tx:bucket3","Tx:bucket4"] +
                 wb[3]*wb[5]*rew_vcovCR0["Tx:bucket3","Tx:bucket5"] +
                 wb[3]*wb[6]*rew_vcovCR0["Tx:bucket3","Tx:bucket6"] +
                 wb[4]*wb[5]*rew_vcovCR0["Tx:bucket4","Tx:bucket5"] +
                 wb[4]*wb[6]*rew_vcovCR0["Tx:bucket4","Tx:bucket6"] +
                 wb[5]*wb[6]*rew_vcovCR0["Tx:bucket5","Tx:bucket6"])

ests%*%wb/sqrt(var_terms + cov_terms)

nb_re  <- list()

nb_re$model = nb$rew
nb_re$vcov = rew_vcovCR0
nb_re$var_terms = var_terms
nb_re$cov_terms = cov_terms
nb_re$wts = wb
nb_re$eff_est = ests
nb_re$yr_effs = fixef(nb$rew)[c("Tx:as.character(YEAR)2016", "Tx:as.character(YEAR)2017",
                                "Tx:as.character(YEAR)2018")]

## CM: did you just run all of this twice to get either the HCA or all cause results?
save(nb_re,file='nb_reHCA.Rdata')

#temp_data = filter(medicaid_comp,!is.na(matches.final),pop_count > 0, YEAR > 2014)
#system.time(
#  nb$rew$vcovCR0 <- vcovCR(nb$rew,cluster=temp_data$stateFIPS,type='CR0'))


#system.time(
#  nb$stratuw  <- glm.nb(update(formula(nb0$strat), .~.+Tx),
#                        start=c(coef(nb0$strat), "Tx"=0),
#                        init.theta=nb0$strat$theta,
#                        data=filter(medicaid_comp,
#                                    !is.na(matches1),
#                                    pop_count>0,
#                                    YEAR > 2014),
#                        trace=0#2
#  )
#)

############## Test for GLM clubSandwich ##################

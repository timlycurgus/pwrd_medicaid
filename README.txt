README: pwrd_medicaid

AHRF_compiler1118.R: This file reads in the 2020 AHRF, extracts necessary covariates, 
and returns ahrf_18.csv

mort_subgroup_compiler.R: This file reads in yearly mortality, sorts based on ICD strings
for healthcare amenable vs. all cause mortality, and outputs a file with mortality counts
for race, sex, and age for each county: det_mort_YEAR

mortality_analysis.R: This file reads in matches, the combined mortality file (medicaid_comp1318.Rdata), the newly eligible data, and state data, and conducts outcome analysis. 

mort_subg_filejoin.R: This file needs to read in all theh det_mort_YEAR files into one file, det_mort_comb. This file then cleans, and merges with subgroup_1318.csv. It outputs the final file, medicaid_comp1318.Rdata.

newelig_calc.R: The file calculates the newly eligible by county. This occurs in two steps. First, it reads in the 2019 AHRF and extracts necessary covariates and saves this file. Then, it reads in SAHIE data, calculates newly eligible by county, and outputs newelig_df_updated.Rdata

prop_calculator.R: This file calculates the propensity score used by Tim (and since discarded) and stratifies on that propensity score.

subgroup_compiler.R: This file merges the AHRF data (ahrf_1118.csv) with the ccest data (subgroup_1318.csv) for data by subgroup. The final data file is 'subgroup_pop1318.csv' 

################### To recreate #################

1. Run AHRF_compiler1118.R
2. Run subgroup_compiler.R
3. Run mort_subgroup_compiler.R
4. Combine det_mort_YEAR into det_mort_comb
5. Run mort_subg_filejoin.R
6. Run newelig_calc.R
7. Run mortality analysis.R

################### Files ########################

#### .Rdata files 

det_mort_YEAR: detailed mortality for YEAR
det_mort_comb1318: detailed mortality for 2013-2018
icd10_strings: ICD strings
medicaid_comp: final data for 2013-2017 
medicaid_comp1318: final data for 2013-2018
mod.dat: contains old matches
mod.dat_final: contains final matches
newelig_df_update: middle newly eligible
newelig_df_updated: final newly eligible results and brackets
pooled.sd1v21: Updated pooled sds, as of 2/17
pscores: Propensiy scores from Tim, no longer in use
strat_data: Stratifications from Tim, no longer in use
subgroup_1317: subgroup data for 2013-2017
subgroup_pop: Merged data from step 2 above for 2013-2017. 
subgroup_pop1318: Merged data from step 2 above for 2013-2017

#### .csv files

ahrf_18: ahrf covariates for 2018
ahrf_1117: ahrf covariates for 2011-2017
subgroup_1318.csv: This is an intermediate step between ccest and subgroup_pop1318. 





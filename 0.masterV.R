
source("0.loadPackages.R")

source("1.data3.R")
#This is where the filename for the new preterm estimates are
source("1.addingPretermpostCC.R")
source("1.addingNMR2.R")
source("1.studiesMeta2WPP.R")

#------move to the model fitting in 3. fitModels.R
#That sets up the model data in the correct form for JAGs, 
#sets up the covariates to be added into the models,
#then sets and runs the parameters for the model. 
#It calls a function which outputs the model data and the parameters from the model, including trace plots for validation if defined. 
source("3. fitModelsV_vn_timeNormalCregions2.R")

#Run this to output the prevalence rates and numbers by region, 
#the neonatal deaths and PAR tables for the paper.
source("4.countryRegionalAndGlobalEstimates.R")

#Need to open and run the file:
"5. Plot countries C (1 model) vn_timeNormalCregions2.R"
#This will produce the estimates and plot them. 





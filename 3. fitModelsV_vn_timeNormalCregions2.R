library(tidyverse)
#devtools::install_github("MJAlexander/distortr")
library(distortr)
library(rjags)
library(R2jags)
library(fields)
library(splines)
library(boot)
library(RColorBrewer)
library(writexl)

source("functions/runMCMC2.R")
source("functions/runMCMCGlobal2V2.R")
source("functions/createArrayForStan.R")
source("0.loadPackages.R")
source("functions/processDataV.R")

regionCodes<-readRDS("output/regionCodes.RDS")
vnFinalMeta<-readRDS("output/vnDataFinal_meta2.rds")
d<-vnFinalMeta %>% filter(Year>=2010 & !is.na(level)) %>% 
  mutate(sourceIndex=ifelse(level=="National", 1, 
                            ifelse(level=="Study", 2, NA)),
         region2=ifelse(regionIndex==1, 1, 2))%>% 
  mutate(PT_SGA=ifelse(level=="Study", PT_SGA_meta ,PT_SGA),
         T_SGA=ifelse(level=="Study", T_SGA_meta ,T_SGA),
         Preterm=ifelse(level=="Study", Preterm_meta ,Preterm),
         Term=ifelse(level=="Study", Term_meta ,Term)) %>%
  mutate(PT_SGA_P1=ifelse(level=="Study", PT_SGA/Preterm ,PT_SGA_P1),
         T_SGA_P1=ifelse(level=="Study", T_SGA/Term ,T_SGA_P1)) %>%
  mutate(PT_SGA_Logit=qlogis(PT_SGA_P1), 
         T_SGA_Logit=qlogis(T_SGA_P1),
         PT_SGA_LogitSe=sqrt(1/(Preterm*PT_SGA_P1*(1-PT_SGA_P1))),
         T_SGA_LogitSe=sqrt(1/(Term*T_SGA_P1*(1-T_SGA_P1))))


N<-dim(d)[1]

endyear <- 2020
startyear<-2010
estyears <- seq(startyear,endyear)

input.data <- processDataV(d = d, 
                           iso.column = "ISO",
                           data.column = "PT_SGA_Logit",
                           se.column = "T_SGA_Logit",
                           obsyear.column = "Year",
                           region.column = "region2",
                           source.column="sourceIndex",
                           end.year = endyear,
                           start.year = startyear)
input.data4 <- processDataV(d = d, 
                            iso.column = "ISO",
                            data.column = "PT_SGA_LogitSe",
                            se.column = "T_SGA_LogitSe",
                            obsyear.column = "Year",
                            region.column = "region2",
                            source.column="level",
                            end.year = endyear,
                            start.year = startyear)
dim(input.data$y.ci)
data_array<-array(NA, dim = c(dim(input.data$y.ci)[1],dim(input.data$y.ci)[2],2))
data_array[,,1]<-input.data$y.ci
data_array[,,2]<-input.data$se.ci

input.data$y.cip<-data_array
input.data$n1<-input.data4$y.ci
input.data$n2<-input.data4$se.ci
input.data$shift.ci<-input.data4$gett.ci


for(i in 1:length(input.data)){
  assign(names(input.data)[i], input.data[[i]])
}
isos <- unique(d[["ISO"]])

#------
#Coefficients
coeffs<-readRDS("output/vnDataFinal.rds") %>% filter(Year>=2010 & 
                                                                ISO %in% d$ISO) %>% 
  mutate(pretermSe=(pretermRate-pretermLower)/1.96) %>% 
  dplyr::select(ISO, year=Year, pretermRate,pretermSe) %>% 
  distinct()

covar_array <- create_covar_array(interest_cov = 
                                    c("pretermRate", "pretermSe"),
                                  estyears = estyears, 
                                  dataset = coeffs)
input.data$covar_array<-covar_array
input.data$ncov<-3

#------
#Model
cs.smoothing <- TRUE
nserror.estimated <- TRUE
input.data$sigma.y <- NA

## JAGS parameters (can be changed if not converging - set to be relatively fast)
#a. - this is what is already run
# nchains = 4
# nburnin = 1750
# niter = 2000
# nthin = 1
#b. - try with this first
# nchains = 4
# nburnin = 100000
# niter = 120000
# nthin = 10
#c. - This is the final one we ran last time
nchains = 4
nburnin = 500000
niter = 1750000
nthin = 500
#d.
# nchains = 4
# nburnin = 500000
# niter =   1.3e+07 #5000 x 2500 - because only keeping every 2500 of them + 500,000 burnin
# nthin = 5000

((niter-nburnin)/nthin)*nchains

# 3c. Splines order 1 -----------------------------------------------------
# P-splines with first order penalization has intercept but no time trend
# need additional input data

#No random time slopes
source("functions/getSplines_Old.R")
splines.data <- getSplinesData(11, order = 1, degree=3, I=155)
#splines.data <- getSplinesData(26, order = 1, degree=3, I=10)


input.data.splines <- c(input.data, splines.data)
rm("input.data", "input.data2"
   #, "stanData"
)

load.module("glm")
file<-"models/vnModelPostCC_TEST.txt"
fileName<-substr(file, 8, as.numeric(gregexpr(".txt", file))-1)
starttime<-Sys.time()
mod<-runMCMC2(method = "splines",
                         order = 1,
                         input.data = input.data.splines,
                         cs.smoothing = cs.smoothing,
                         nserror.estimated = nserror.estimated,
                         nchains = nchains,
                         nburnin = nburnin,
                         niter = niter,
                         nthin = nthin,
                         model.file.path = file)
Sys.time()-starttime

pdf_name <- paste0("output/" ,fileName,"_", 
                   ((niter-nburnin)/nthin)*nchains,"_tracePlots", ".pdf")
pdf(pdf_name, width = 10, height = 5)
R2jags::traceplot(mod, varname=c("beta.d", "mu.beta",
                                 "tau.beta", "mu.beta.global",
                                 "tau.beta.global", "sigma.delta", 
                                 "tau.beta", "tau.delta",
                                 "alpha", "Z.tk", "delta.hc"), mfrow=c(3,3),
                  ask=FALSE)

dev.off()

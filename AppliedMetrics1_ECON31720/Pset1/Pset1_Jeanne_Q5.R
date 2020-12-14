## ---------------------------
##
## Script name: Applied Econometrics pset 1 - Q5
##
## Purpose of script: Replicate part of Voigtlander Voth, QJE 2012
##
## Author: Jeanne Sorin
##
## Date Created: 2020-10-14
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC
rm(list=ls())
setwd("~/Documents/PhD_Git/PhD_Season2/Econometrics1/Pset1/")    
## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need

require(tidyverse)
require(data.table)
require(readstata13)
require(pracma)
require(geodist)
require(tidyselect)
## ---------------------------

## load up our functions into memory
source("Pset1_Functions.R") 



###############################################################################
################################# Question 5 ##################################
###############################################################################
## ---------------------------
## Import data
data = read.dta13("Persecution_Perpetuated_QJE_Replicate/Dataset_QJE_Replicate_with_Cities.dta")

## ------ 
## Clean Data / Create relevant dummies & variables
data <- data %>%
  mutate(exist1349 = ifelse(judaica==1 | comm1349 ==1, 1, 0),
         logpop25c = log(c25pop),
         percentjews = 100*c25juden/c25pop,
         percentprot = 100*c25prot/c25pop)

data = data %>% 
  filter(exist1349 == 1, !is.na(pog1349)) %>%
  mutate(constant = 1)


## ----
## Panel A
panelA_cluster = lm_cluster_own(data = data, dep_var="pog20s", indep_var = c("constant", "pog1349", "logpop25c", "percentjews", "percentprot"), "kreis_nr")
panelA_cluster

## ----
## Panel B Matching
dep_var = "pog20s"
indep_var = c("logpop25c", "percentjews", "percentprot")
treat_var = "pog1349"
clustervar = "kreis_nr"
k=2
est="ATT"
B=100
S=150
k=4
pscore=FALSE
coef_panelB = estimate_matching(data, dep_var, treat_var, indep_var, clustervar, k, est, pscore)
se_panelB = se_bootstrap(B, S, data, dep_var, treat_var, indep_var, clustervar, k, est, pscore)

## ----
## Panel C Matching on longitude & latitude
dep_var = "pog20s"
indep_var = c("Longitude", "Latitude")
treat_var = "pog1349"
clustervar = "kreis_nr"
k=2
B=100
S=150
k=4
pscore=FALSE
est="ATT"
coef_panelC = estimate_matching(data, dep_var, treat_var, indep_var, clustervar, k, est, pscore)
se_panelC = se_bootstrap(B, S, data, dep_var, treat_var, indep_var, clustervar, k, est, pscore)


## -----
## Matching on propensity score
dep_var = "pog20s"
indep_var = c("logpop25c", "percentjews", "percentprot")
treat_var = "pog1349"
clustervar = "kreis_nr"
k=2
B=100
S=150
k=4
pscore=TRUE
est="ATT"
coef_pscoreATT = estimate_matching(data, dep_var, treat_var, indep_var, clustervar, k, est, pscore)
se_pscoreATT = se_bootstrap(B, S, data, dep_var, treat_var, indep_var, clustervar, k, est, pscore)
est="ATE"
coef_pscoreATE = estimate_matching(data, dep_var, treat_var, indep_var, clustervar, k, est, pscore)
se_pscoreATE = se_bootstrap(B, S, data, dep_var, treat_var, indep_var, clustervar, k, est, pscore)



#### Make tables
# names = c("constant", "pog1349", "logpop25c", "percentjews", "percentprot")
# title = "Regression Panel A"
# make_tables(panelA_cluster$betahat, panelA_cluster$se, N = 320, 
#             names=names, dep_var="pog20s",
#             title=title, r=4)
# 
# names = c("pog1349")
# title = "Regression Panel B"
# make_tables(coef_panelB, se_panelB, N = 320, 
#             names=names, dep_var="pog20s",
#             title=title, r=4)
# 
# names = c("pog1349")
# title = "Regression Panel C"
# make_tables(coef_panelC, se_panelC, N = 320, 
#             names=names, dep_var="pog20s",
#             title=title, r=4)
# 
# names = c("pog1349")
# title = "Pscore Matching ATT"
# make_tables(coef_pscoreATT, se_pscoreATT, N = 320, 
#             names=names, dep_var="pog20s",
#             title=title, r=4)
# 
# names = c("pog1349")
# title = "Pscore Matching ATE"
# make_tables(coef_pscoreATE, se_pscoreATE, N = 320, 
#             names=names, dep_var="pog20s",
#             title=title, r=4)


## ---------------------------
##
## Script name: Pset 2 - Problem 6
##
## Purpose of script: 
##
## Author: Jeanne Sorin
##
## Date Created: 2020-11-01
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC
rm(list=ls())
setwd("~/Documents/PhD_Git/PhD_Season2/Econometrics1/")    

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(data.table)
require(mvtnorm)
require(stargazer)
## ---------------------------

## load up our functions into memory

source("Pset_Functions2.R") 

## ---------------------------




## ---------------------------
## Problem 6

### DGP
Ef <-  function(sigma, n){ rmvnorm(n=n, mean=c(0.0, 0.0), 
                                   sigma=sigma, method="chol")}
Zf <- function(n, meanX=0, sdX=1){
  Z <-  zeros(n, 20)
  for (i in 1:20){
    Z[,i] <- rnorm(n, meanX, sdX)
  }
  return(Z)}
Xf <- function(Z1, V){0.3*Z1 + V}
Yf <- function(X, U){X + U}


monte_carlo_f<- function(N, M, method="ols"){
  ## This function performs the MC simulations for problem 6
  # M = number of loops in MC
  # N = number of observations simulated each time
  
  ## ----
  ## Check that the inputs are correct
  valid_methods = c("ols", "tsls_z1", "tsls_allz", 
                    "jackknife_z1", "jackknife_allz")
  if ((method %in% valid_methods)==FALSE){
    print("Invalid method")
    break
  }
  
  ## ----
  ## Initialize MC
  beta_hat = matrix(, nrow = 1, ncol = 1)
  within95 = matrix(, nrow = 1, ncol = 1)
  
  ## As many time as specified by the number of MC simulations M
  for (m in 1:M){
    print(m)
    sigma = matrix(c(0.25, 0.20, 0.20, 0.25), ncol=2)
    E <- Ef(sigma, N)
    U = E[,1]
    V = E[,2]
    Z <- Zf(N)
    X <- Xf(Z[,1], V)
    Y <- Yf(X, U)
    
    if (method=="ols"){
      X_withone <- cbind(ones(N, 1), X)
      output <- lm_own(data="NA", Y, X_withone)
      beta_hat <- rbind(beta_hat, output$beta_hat[2])
      CI = c(output$beta_hat[2] - 1.96*output$se[2], 
             output$beta_hat[2] + 1.96*output$se[2])
      within95 = rbind(within95, 1*(CI[1] <= 1.0 & CI[2] >= 1.0))
    }
    
    if (method=="tsls_z1"){
      output <- tsls_own(data="NA", dep_var = Y, indep_var = NA, 
                         treat_var = X, instru_var = Z[,1])
      beta_hat <- rbind(beta_hat, output$beta_hat[2])
      within95 = rbind(within95, 1*(output$low95 <= 1.0 & output$high95 >= 1))
    }
    
    if (method=="tsls_allz"){
      output <- tsls_own(data="NA", dep_var = Y, indep_var = NA, 
                         treat_var = X, instru_var = Z)
      beta_hat <- rbind(beta_hat, output$beta_hat)
      within95 = rbind(within95, 1*(output$low95 <= 1.0 & output$high95 >= 1))
    }
    
    if (method=="jackknife_z1"){
      Z1 = Z[,1]
      if (is.null(dim(X))){
        X = matrix(X, ncol=1)
      }
      if (is.null(dim(Z1))){
        Z1 = matrix(Z1, ncol=1)
      }
      if (is.null(dim(Y))){
        Y = matrix(Y, ncol=1)
      }      
      output_jack = jackknife_iv_own(dep_var = Y, indep_var = NA, 
                                     treat_var = X, instru_var = Z1)
      beta_hat <- rbind(beta_hat, output_jack$beta_hat[2])
      CI = c(output_jack$beta_hat[2] - 1.96*output_jack$se[2], 
             output_jack$beta_hat[2] + 1.96*output_jack$se[2])
      within95 = rbind(within95, 1*(CI[1] <= 1.0 & CI[2] >= 1.0))
    }
    
    if (method=="jackknife_allz"){
      if (is.null(dim(X))){
        X = matrix(X, ncol=1)
      }
      if (is.null(dim(Y))){
        Y = matrix(Y, ncol=1)
      }     
      output_jack = jackknife_iv_own(dep_var = Y, indep_var = NA, 
                                     treat_var = X, instru_var = Z)
      beta_hat <- rbind(beta_hat, output_jack$beta_hat[2])
      CI = c(output_jack$beta_hat[2] - 1.96*output_jack$se[2], 
             output_jack$beta_hat[2] + 1.96*output_jack$se[2])
      within95 = rbind(within95, 1*(CI[1] <= 1.0 & CI[2] >= 1.0))
    }
    
  }
  
  beta_hat = beta_hat[2:nrow(beta_hat),]
  within95 = within95[2:nrow(within95),]
  bias = mean(beta_hat) - 1.0
  median = median(beta_hat)
  CI95 = mean(within95)
  std_dev = sqrt(M/(M-1)*(mean(beta_hat^2) - mean(beta_hat)^2))
  # CI95 = quantile(beta_hat, c(0.05, 0.95))
  return(list(bias, median, std_dev, CI95))
}

all <- data.frame(Estimator = "",
                  N = "",
                  Bias = "",
                  Median = "",
                  Std = "", 
                  CI_cov = "")
all[] <- lapply(all, as.character)
size = c(100, 200, 400, 800)

OLS_100 <- monte_carlo_f(100, 100, method="ols")
OLS_200 <- monte_carlo_f(200, 100, method="ols")
OLS_400 <- monte_carlo_f(400, 100, method="ols")
OLS_800 <- monte_carlo_f(800, 100, method="ols")
OLS = list(OLS_100, OLS_200, OLS_400, OLS_800)
for (i in 1:4){
  print(i)
  obs = c("OLS", size[i], OLS[[i]][[1]], OLS[[i]][[2]], 
          OLS[[i]][[3]], OLS[[i]][[4]])
  all = rbind(all, as.character(obs))
}

TSLS_Z1_100 <- monte_carlo_f(100, 100, method="tsls_z1")
TSLS_Z1_200 <- monte_carlo_f(200, 100, method="tsls_z1")
TSLS_Z1_400 <- monte_carlo_f(400, 100, method="tsls_z1")
TSLS_Z1_800 <- monte_carlo_f(800, 100, method="tsls_z1")
TSLS_Z1 = list(TSLS_Z1_100, TSLS_Z1_200, TSLS_Z1_400, TSLS_Z1_800)
for (i in 1:4){
  print(i)
  obs = c("TSLS Z1", size[i], TSLS_Z1[[i]][[1]], TSLS_Z1[[i]][[2]], 
          TSLS_Z1[[i]][[3]], TSLS_Z1[[i]][[4]])
  all = rbind(all, as.character(obs))
}

TSLS_allZ_100 <- monte_carlo_f(100, 100, method="tsls_allz")
TSLS_allZ_200 <- monte_carlo_f(200, 100, method="tsls_allz")
TSLS_allZ_400 <- monte_carlo_f(400, 100, method="tsls_allz")
TSLS_allZ_800 <- monte_carlo_f(800, 100, method="tsls_allz")
TSLS_allZ = list(TSLS_allZ_100, TSLS_allZ_200, TSLS_allZ_400, TSLS_allZ_800)
for (i in 1:4){
  print(i)
  obs = c("TSLS All Z", size[i], TSLS_allZ[[i]][[1]], TSLS_allZ[[i]][[2]], 
          TSLS_allZ[[i]][[3]], TSLS_allZ[[i]][[4]])
  all = rbind(all, as.character(obs))
}

Jackknife_Z1_100  <- monte_carlo_f(100, 100, method="jackknife_z1")
Jackknife_Z1_200  <- monte_carlo_f(200, 100, method="jackknife_z1")
Jackknife_Z1_400  <- monte_carlo_f(400, 100, method="jackknife_z1")
Jackknife_Z1_800  <- monte_carlo_f(800, 100, method="jackknife_z1")
Jackknife_Z1 = list(Jackknife_Z1_100, Jackknife_Z1_200, 
                    Jackknife_Z1_400, Jackknife_Z1_800)
for (i in 1:4){
  print(i)
  obs = c("Jackknife IV Z1", size[i], Jackknife_Z1[[i]][[1]], 
          Jackknife_Z1[[i]][[2]], Jackknife_Z1[[i]][[3]], 
          Jackknife_Z1[[i]][[4]])
  all = rbind(all, as.character(obs))
}

Jackknife_allZ_100  <- monte_carlo_f(100, 100, method="jackknife_allz")
Jackknife_allZ_200  <- monte_carlo_f(200, 100, method="jackknife_allz")
Jackknife_allZ_400  <- monte_carlo_f(400, 100, method="jackknife_allz")
Jackknife_allZ_800  <- monte_carlo_f(800, 100, method="jackknife_allz")
Jackknife_allZ = list(Jackknife_allZ_100, Jackknife_allZ_200, 
                      Jackknife_allZ_400, Jackknife_allZ_800)
for (i in 1:4){
  print(i)
  obs = c("Jackknife IV All Z", size[i], Jackknife_allZ[[i]][[1]], 
          Jackknife_allZ[[i]][[2]], Jackknife_allZ[[i]][[3]], 
          Jackknife_allZ[[i]][[4]])
  all = rbind(all, as.character(obs))
}


options(digits=3)
all[,2] <- as.numeric(all[,2])
all[,3] <- as.numeric(all[,3])
all[,4] <- as.numeric(all[,4])
all[,5] <- as.numeric(all[,5])
all[,6] <- as.numeric(all[,6])
all <- all[-1,]

stargazer(all, summary = FALSE, rownames = FALSE)






################################################################################
################################################################################
## ----
## Problem 7

data <- read.csv(file="Pset2/abadie.csv")

# Clean
data$constant = 1
data$nettfa = 1000*data$nettfa
data$age_adj = data$age - 25
data$age2_adj = data$age_adj^2

## Reproduce table 2

# Col 1: OLS
col1_x_var = c("p401k", "constant", "inc", "age_adj", "age2_adj", "marr", "fsize")
col1 <- lm_own(data=data, dep_var = "nettfa", indep_var = col1_x_var, robust=TRUE)
col1$names <- c("p401k", "constant", "inc", "age_adj", "age2_adj", "marr", "fsize")

# Col 2: First stage
col2_x_var = c("constant", "inc", "age_adj", "age2_adj", "marr", 
               "fsize", "e401k")
col2 <- lm_own(data=data, dep_var = "p401k", indep_var = col2_x_var, 
               robust=TRUE)
data$pred_1stage = as.matrix(data[,col2_x_var]) %*% 
  as.matrix(as.numeric(col2$beta_hat))
col2$names <- c("constant", "inc", "age_adj","age2_adj", "marr", "fsize", "e401k")

# Col 3: Second stage
# col3_x_var = c("pred_1stage","constant", "inc", "age_adj", "age2_adj", "marr", "fsize")
# col3 <- lm_own(data=data, dep_var = "nettfa", indep_var = col3_x_var, robust=TRUE)

# Alternatively: direct tsls
indep_var = c("inc", "marr", "age_adj", "age2_adj", "fsize")
dep_var = "nettfa"
treat_var = "p401k"
instru_var = "e401k"
sub = data[sample(nrow(data), nrow(data)),]
dep_varm = as.matrix(sub[,dep_var])
indep_varm = as.matrix(sub[,indep_var])
instru_varm = as.matrix(sub[,instru_var])
treat_varm = as.matrix(sub[,treat_var])
col3_tsls <- tsls_own(data="NA", dep_var=dep_varm, indep_var=indep_varm, 
                      treat_var=treat_varm, instru_var=instru_varm, robust=TRUE)
col3_tsls$names <- c("constant", "p401k", "inc", "marr", "age_adj", "age2_adj", 
                     "fsize")

# Col 4: Least Squares Treated
col4_x_var = c("inc", "age_adj", "age2_adj", "marr", "fsize")
col4 <- kappa_own(B = 50, S = nrow(data), data=data, dep_var = "nettfa", 
                  indep_var = col4_x_var, treat_var = "p401k", 
                  instrument_var = "e401k")
col4$names = c("constant", "p401k", "inc", "age_adj", "age2_adj", "marr", "fsize")

# betas_boot = matrix(, nrow = 1, ncol = (length(indep_var) + 2))
# B = 100
# for (b in 1:B){
#   print(b)
#   sampled = sample(nrow(data), S, replace = TRUE)
#   beta_boot = kappa_own_onlyb(data=data[sampled,], dep_var = "nettfa", 
#                               indep_var = col4_x_var, treat_var = "p401k", 
#                               instrument_var = "e401k")
#   betas_boot = rbind(betas_boot, beta_boot)
# }
# betas_boot = betas_boot[-1,]
# betas_boot = betas_boot %*% dplyr::filter(!is.nan())
# se = c()
# for (i in 1:ncol(betas_boot)){
#   se = c(se, std(betas_boot[!is.nan(betas_boot[,i]),i]))
# }
# col4$se = se
# 

### Make table
all <- data.frame(Participation = "",
                  se_p = "",
                  Constant = "",
                  se_c = "",
                  FamilyIncome = "",
                  se_f = "",
                  Age = "",
                  se_a = "",
                  Age2 = "", 
                  se_a2 = "",
                  Married = "",
                  se_m = "",
                  FamilySize = "",
                  se_size = "",
                  Eligibility = "",
                  se_e = "")
all[] <- lapply(all, as.character)

all_rows = c("p401k", "constant", "inc", "age_adj", "age2_adj", "marr", "fsize",
            "e401k")
all_cols = list(col1, col2, col3_tsls, col4)
names_cols = c("OLS", "First Stage", "Second Stage", "Least Squares Treated")
r = c(2, 4, 2, 2)
for (j in 1:length(all_cols)){
  print(j)
  for (i in 1:length(all_rows)){
    print(i)
    col = all_cols[j][[1]]
    n = as.vector(col$names)
    index = which(n==all_rows[i])
    if (length(index)>0){
      all[j,(i-1)*2+1] = as.character(round(col$beta_hat[index], r[j]))
      all[j,(i-1)*2+2] = paste("(", 
                               as.character(round(col$se[index], r[j])),
                               ")", sep="")}
  }
}
stargazer(t(all), summary=FALSE, 
          notes = c("Robust Standard Errors in Parenthesis."))







### 7.2 95% Anderson Rubin CI for the coefficient on the endogenous variable.
grid = linspace(7000,17000, 1000)
indep_var = c("inc", "marr", "age_adj", "age2_adj", "fsize")
dep_var = "nettfa"
treat_var = "p401k"
instru_var = "e401k"
dep_varm = as.matrix(data[,dep_var])
indep_varm = as.matrix(data[,indep_var])
instru_varm = as.matrix(data[,instru_var])
treat_varm = as.matrix(data[,treat_var])
ar_ci = anderson_rubin_own(data="NA", dep_varm, indep_varm, 
                           treat_varm, instru_varm, around=5000, steps=1000)
ar_ci

tsls_ci = c(col3_tsls$beta_hat[2] - 1.96 * col3_tsls$se[2], 
            col3_tsls$beta_hat[2] + 1.96 * col3_tsls$se[2])
tsls_ci




  
 

### 7.3 : Compute a jackknife IV estimator with bootstrapped standard error...
indep_var = c("inc", "marr", "age_adj", "age2_adj", "fsize")
dep_var = "nettfa"
treat_var = "p401k"
instru_var = "e401k"

dep_varm = as.matrix(data[,dep_var])
indep_varm = as.matrix(data[,indep_var])
instru_varm = as.matrix(data[,instru_var])
treat_varm = as.matrix(data[,treat_var])

output_jack = jackknife_iv_own(dep_varm, indep_varm, treat_varm, 
                                    instru_varm, robust=TRUE)
output_jack$se_boot = se_bootstrap_own(B = 100, 
                                       S = nrow(dep_varm),
                                       dep_var = dep_varm,
                                       indep_var = indep_varm,
                                       treat_var = treat_varm,
                                       instru_var = instru_varm, 
                                       method="jackknifeIV")

output_jack$beta_hat
output_jack$se
output_jack$se_boot








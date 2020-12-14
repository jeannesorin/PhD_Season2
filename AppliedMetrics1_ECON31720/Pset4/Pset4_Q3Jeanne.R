## ---------------------------
##
## Script name: Problem Set 3 - Question 3
##
## Author: Jeanne Sorin
##
## Date Created: 2020-12-11
##
## ---------------------------
##
## Notes: This code reproduces Figures 10 & 11 of Kellogg, Mogstad, Pouliot & 
## Torgovitsky 2020
## It follows the structure of the code on Kellogg's github 
## 
## I am thankful to Nadia Lucas for helpful comments and late night zoom 
## debugging sessions
## ---------------------------

## set working directory for Mac and PC

rm(list=ls())
setwd("~/Documents/PhD_Git/PhD_Season2/Econometrics1/")   

## ---------------------------

options(scipen = 6, digits = 4) 

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(data.table)
require(mvtnorm)
require(stargazer)
library(prioritizr)
library(ggplot2)
library(dplyr)
library(gurobi)
library(pracma)
## ---------------------------
#source("Pset_Functions.R") 


#### ---------------------------------------------------------------------------
#### The MASC Estimator needs
# The matching estimator and weights: nn_weight_f
# The SC estimator and weights: sc_weights_f
# The Cross Validation procedure: crossval_masc
# A routine function that runs over all m: routine_masc

### Find nearest neighbor weights (for gamma_matching)
nn_weight_f = function(donors, treated, treatment=1970, m=5){
  diff = (donors[1:nrow(donors)-1,] - as.vector(treated[1:nrow(donors)-1,]))^2
  dist = colSums(diff)
  Weights = rep(0, length(dist))
  Weights[which(dist %in% sort(dist)[1:m])] = 1 / length(which(dist %in% sort(dist)[1:m]))
  return(Weights)
}


### Find SC weights
sc_weights_f = function(donors, treated, treatment=16){
  # Set up beautiful Gurobi object that finds the weights such that treated = pred(controls)
  model = list()
  model$A = matrix(1, 1, ncol(donors))
  model$sense = "="
  model$rhs = c(1) # weights should sum to 1
  model$lb = rep(0, ncol(donors)) # min weight = 0
  model$ub = rep(1, ncol(donors)) # max weight = 1
  model$obj = sc_objective(donors, treated)
  model$objcon = sc_objconstant(donors, treated)
  model$Q = sc_quadratic(donors, treated)
  
  output = gurobi(model)
  result = list()
  result$sc_w <- output$x
  result$sc_obj <- output$objval
  return(result)
}
### For Gurobi
# The objective loss function
sc_objective = function(donors, treated){ 
  return(-2 * as.matrix((t(treated))) %*% as.matrix(donors))}
# The constant
sc_objconstant = function(donors, treated){ 
  return(t(treated) %*% treated)}
# The quadratic part
sc_quadratic = function(donors, treated){ 
  return(as.matrix((t(donors))) %*% as.matrix(donors))}

#### ---- 
all_weight = function(donors, treated, treatment=16, m){
  ### Helper function that puts together the nn weights and the sc weights
  donors <- donors[1:(treatment - 1), ]
  treated <- as.matrix(treated[1:(treatment - 1), ])
  nn_weights =  nn_weight_f(donors, treated, treatment=treatment, m=m)
  sc_results = sc_weights_f(donors, treated, treatment=treatment)
  sc_weights = sc_results[[1]]
  return(list(sc_weights, nn_weights))
}

#### ----
crossval_masc = function(treated, donors, treatment, tune_m){
  ### For a given m, find the phi that minimizes the sum(errors^2)
  ### Prep
  set_f = 8:14
  min_preperiods = 8
  minl = 1
  maxl = 1
  treat = treatment
  Ysc = NULL
  Ynn = NULL
  foldweights = list()
  Ytreated = NULL
  objweight = NULL
  weights_f = rep(1,7)/7
  
  ### Fold
  # Get the weights
  weights = all_weight(donors = donors,  treated = treated,
                       treatment = treat, m=tune_m)
  
  # Solve both nn and sc estimators for each fold
  for (i in 1:length(set_f)){
    treatinter = (set_f[i] + 1):min(set_f[i]+1,treatment-1)
    treat = set_f[i]+1
    foldweights[[i]] = all_weight(donors = donors, treatment = treat,
                                  treated = treated, m=tune_m)
    # Append the vectors
    Ysc <- c(Ysc, as.numeric(donors[treatinter,])%*%foldweights[[i]][[1]])
    Ynn<- c(Ynn,as.numeric(donors[treatinter,])%*%foldweights[[i]][[2]])
    Ytreated <- c(Ytreated, as.numeric(treated[treatinter,]))
    objweight <- c(objweight, rep(weights_f[i]/length(treatinter), length(treatinter)))
  }
  # Compute phi analytically
  phi = min(max((objweight*(Ytreated-Ysc)) %*% 
                  (Ynn - Ysc)/((objweight*(Ynn-Ysc)) %*% (Ynn - Ysc)),0),1)
  ### Compute implied errors 
  error_cv = sum(objweight*(Ytreated - (phi*Ynn + (1-phi)*Ysc))^2)
  ### Final weights
  ww = phi*weights[[2]] + (1-phi)*weights[[1]]
  
  output = list(phi_hat = phi,  m_hat = tune_m, weights = ww, cverror = error_cv)
  return(output)
}

routine_masc = function(treated, donors, 
                        treatment=16, all_m = c(1:10)){
  # All results: run the cross validation function for each m in 1:10
  all <- lapply(all_m, function(x) crossval_masc(treated = treated,
                                                   donors=donors,
                                                   treatment=treatment, 
                                                   tune_m = x))
  
  # Keep only the one with the lowest cv error
  smallest = all[[which.min(lapply(all, function(x) x$cverror))]]
  return(smallest)
}



#### ---------------------------------------------------------------------------
#### The SC Penalized estimator needs
# The SC pen estimator and weights: sc_weights_f & corresponding obj functions
# The Cross Validation procedure: crossval_scpen
# A routine function that runs over all m: routine_scpen

## ----
sc_pen_weights_f = function(donors, treated, treatment=16, pi){
  # Computes the weights for the SC pen using beautiful gurobi
  donors = donors[1:treatment-1,]
  treated = treated[1:treatment-1,]
  
  # Set up
  model = list()
  model$A = matrix(1, 1, ncol(donors))
  model$sense = "="
  model$rhs = c(1) # weights should sum to 1
  model$lb = rep(0, ncol(donors)) # min weight = 0
  model$ub = rep(1, ncol(donors)) # max weight = 1
  model$obj = sc_pen_objective(donors, treated, pi)
  model$objcon = sc_pen_objconstant(donors, treated, pi)
  model$Q = sc_pen_quadratic(donors, treated, pi)
  
  output = gurobi(model)
  result = list()
  result$sc_pen_w <- output$x
  result$sc_pen_obj <- output$objval
  return(result)
}
## Corresponding parts of the objective function
sc_pen_objective = function(donors, treated, pi){ 
  part_sc = (-2) * as.matrix((t(treated))) %*% as.matrix(donors)
  # Additional part: the extra interpolation bias
  part_extra = NULL
  for (i in 1:ncol(donors)){
    extra = sum((donors[,i] - treated)^2)
    #extra = sqrt(sum((donors[,i] - treated)^2))
    part_extra[i] = extra
  }
  return((1-pi)*part_sc + pi*part_extra)}
sc_pen_objconstant = function(donors, treated, pi){ 
  return((1-pi)* (t(treated) %*% treated))}
sc_pen_quadratic = function(donors, treated, pi){ 
  return((1-pi)*as.matrix((t(donors))) %*% as.matrix(donors))}


## ----
crossval_scpen = function(treated, donors, treatment, pi){
  # For a given pi, find the weights that minimizes the sum(errors^2)
  
  ### Prepare folding
  set_f = 8:14
  min_preperiods = 8
  minl = 1
  maxl = 1
  treat = treatment
  Yscpen = NULL
  foldweights = list()
  Ytreated = NULL
  objweight = NULL
  weights_f = rep(1,7)/7
  
  ### Get full weights corresponding to pi
  weights = sc_pen_weights_f(donors = donors, treated = treated, pi=pi)
  
  ### Fold
  for (i in 1:length(set_f)){
    treatinter = (set_f[i] + 1):min(set_f[i]+1,treatment-1)
    treat = set_f[i]+1
    # get weights corresponding to fold
    foldweights[[i]] = sc_pen_weights_f(donors = donors, treated = treated,
                                        treatment=treat, pi=pi)
    # Predicted counterfactuals
    Yscpen = c(Yscpen, as.numeric(donors[treatinter,]) %*% foldweights[[i]][[1]])
    Ytreated = c(Ytreated, as.numeric(treated[treatinter,]))
    objweight = c(objweight, rep(weights_f[i]/length(treatinter), 
                                 length(treatinter)))
  }
  
  # Solve error
  error_cv = sum(objweight*(Ytreated - Yscpen)^2)
  
  output = list(weights = weights,
                pi_hat = pi,
                cverror = error_cv)
  return(output)
}

## ----
routine_scpen = function(treated,  donors,  treatment=16, 
                         all_pi = linspace(0,1,100)){
  # Cross Validation, for each pi compute cv error
  all <- lapply(all_pi, function(x) 
    crossval_scpen(treated = treated, donors=donors,
                   treatment=treatment, pi = x))
  # Choose smallest cverror
  smallest = all[[which.min(lapply(all, function(x) x$cverror))]] 
  return(smallest)
}




#### ---------------------------------------------------------------------------
#### ---------------------------------------------------------------------------
data = read.csv("Pset4/basque.csv")
data = as.data.frame(data)
data = data %>% filter(regionno != 1)
donors = data[data$regionno!=17 & data$year,c(1,3,4)]
donors = reshape(donors, idvar = "year", timevar="regionno", direction="wide")
donors = donors[,-1]
treated = data[data$regionno==17 & data$year, c(1,3,4)]
treated = reshape(treated, idvar = "year", timevar="regionno", direction="wide")
treated = as.matrix(treated[,-1])
treatment=16

### Figure 10
output10 = routine_masc(treated = as.matrix(treated[1:treatment-1,]),
                      donors = as.matrix(donors[1:treatment-1,]),
                      treatment = 16,
                      all_m = c(1:10))

prediction10 = as.matrix(donors) %*% as.matrix(output10$weights)
treatment_effect = treated - prediction10
ggplot() +
  geom_line(aes(unique(data$year), y=treatment_effect*1000)) +
  xlab("Year") + ylab("Treatment effect") + theme_minimal() + 
  geom_hline(yintercept= mean(treatment_effect[16:43])*1000, col='blue', lty=2)+
  geom_hline(yintercept=0, lty=2)+
  geom_vline(xintercept = 1970, lty=2)
ggsave("Pset4/Problem4_Figure10.png")


### Figure 11
weights_sc_nn = all_weight(donors, treated, treatment = 16, m=3)
# matching
weights_nn = weights_sc_nn[[2]]
pred_nn = as.matrix(donors) %*% weights_nn
treatment_effect_nn = treated - pred_nn
diff_matching = treatment_effect_nn - treatment_effect
# sc
weight_sc = weights_sc_nn[[1]]
pred_sc = as.matrix(donors) %*% weight_sc
treatment_effect_sc = treated - pred_sc
diff_sc = treatment_effect_sc - treatment_effect
# sc pen
pen_output = routine_scpen(donors=donors, treated=as.matrix(treated), treatment=16)
pen_counterfactual = as.matrix(donors) %*% as.matrix(pen_output$weight$sc_pen_w)
treatment_pen = treated - pen_counterfactual
diff_pen = treatment_pen - treatment_effect


ggplot() +
  geom_line(aes(unique(data$year[data$year >= 1970]), 
                y=diff_matching[16:43]*1000, col="Matching")) +
  geom_line(aes(unique(data$year[data$year >= 1970]), 
                y=diff_sc[16:43]*1000, col="SC")) +
  geom_line(aes(unique(data$year[data$year >= 1970]), 
                y=diff_pen[16:43]*1000, col="Penalized SC")) +
  xlab("Year") + ylab("Difference in Effect (per capita GDP)") + theme_minimal() + 
  geom_hline(yintercept = 0, col="black")+
  geom_vline(xintercept = 1970) + theme(legend.title = element_blank()) 
ggsave("Pset4/Problem4_Figure11.png")

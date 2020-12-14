## ---------------------------
##
## Script name: Pset 4 - Question 2
##
##
## Author: Jeanne Sorin
##
## Date Created: 2020-12-11
##
## ---------------------------
##
## Notes: This code corresponds to problem 2 of problem set 4
## I am thankful to Yixin Sun and Tanya Rajan for helpful discussions & comments
##   
##
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
library(matrixStats)
library(tictoc)
library(future)
library(Rcpp)
library(RcppArmadillo)
library(RcppEigen)
library(Rfast)
library(Matrix)

## ---------------------------


## ---------------------------

### DGP - parameters and functions
valE = c(2,3,4,5)

Y0f = function(u, e){-0.2 + 0.5*e + u}
Y1f = function(u, e, t, v, theta){-0.2 + 0.5*e + sin(t - theta*e) + u + v}

Uallt <- function(eps){
  t=5
  uallt = matrix(0, t)
  uallt[1] = eps[1]
  for (i in 2:t){ uallt[i] = rho*uallt[i-1] + eps[i]}
  return(uallt)
}


## Helper Functions
beta_ols_f <- function(y,x){
  # beta OLS
  x = cbind(1,as.matrix(x))
  y = as.matrix(y)
  xTx = t(x) %*% x
  xTy = t(x) %*% y
  return((solve(xTx, tol=1e-20) %*% xTy))
}  


dgp_simulation <- function(N, theta = -2, rho = .5, t = 5){
  
  # id FE (needed later for cluster)
  id <-  c(matrix(rep(1:N, t), ncol = t))
  
  # GDP
  V <- matrix(rnorm(N*t), ncol = t)
  eps <-  matrix(rnorm(N*t), ncol = t)
  Us = t(map(split(eps, 1:N), Uallt) %>% reduce(cbind))
  Es <- matrix(rep(sample(2:5, N, replace = TRUE), t), ncol = t)
  tsm <- as.matrix(map_dfc(1:t, rep, N))
  Y0 = Y0f(Us, Es)
  Y1 = Y1f(Us, Es, tsm, V, theta)

  # Everything as vectors because easier to work with
  Y0 <- c(Y0)
  Y1 <- c(Y1)
  Es <- c(Es)
  tsm <- c(tsm)
  
  # Build D and Y in potential outcome framework
  D <- as.numeric(Es <= tsm)
  Y <- D*Y1 + (1-D)*Y0
  
  # create cohort, time, and relative time dummies, with 1 cohort and 1 time
  ### Create Fixed Effects
  tfe = cbind(c(rep(1, N), rep(0,4*N)),
              c(rep(0, N), rep(1, N), rep(0,3*N)),
              c(rep(0, 2*N), rep(1, N), rep(0,2*N)),
              c(rep(0, 3*N), rep(1, N), rep(0,N)),
              c(rep(0, 4*N), rep(1, N)))
  colnames(tfe) = c("Time1", "Time2", "Time3", "Time4", "Time5")
  tfe = tfe[,-5]
  
  cfe <- map_dfr(Es, function(x) as.data.frame(t(as.numeric(x == 2:4)))) 
  colnames(cfe) = paste0("cfe", 2:4)

  # tfe <-
  #   map_dfr(t_all, function(x) as.data.frame(t(as.numeric(x == 1:(t-1))))) %>%
  #   setNames(paste0("tfe", 1:(t-1)))

  relfe <- map_dfr(tsm - Es, function(x) as.data.frame(t(as.numeric(x == -4:3)))) 
  colnames(relfe) = paste0("rlfe", -4:3)
  relfe = relfe[,-c(1,4)]
    #dplyr::select(-c("rlfe-4", "rlfe-1"))
  
  xv <- as.matrix(cbind(cfe, tfe, relfe))
  return(list(xv, Y, Es, tsm, id))
}


### Monte Carlo Function
montecarlo_f <- function(M=50, N=1000, theta=-2, rho=0.5, t=5){
  beta = matrix(0, 14, M)
  beta2 = matrix(0, 14, M)
  for (m in 1:M){
    print(m)
    output = dgp_simulation(N=N, theta=theta)
    beta[,m] = beta_ols_f(as.matrix(output[[2]]), as.matrix(output[[1]]))
  }
  return(list(beta))
}

### Plot relative time coefficients
plot_mcresult = function(run1, run2, theta="2"){
  xs = c(-3, -2, 0, 1, 2, 3)
  
  keep_1 = run1[9:14,]
  keep_2 = run2[9:14,]
  
  mean_1 = rowMeans(keep_1)
  mean_2 = rowMeans(keep_2)
  
  q2p5_1 = rowQuantiles(keep_1, probs=0.025)
  q2p5_2 = rowQuantiles(keep_2, probs=0.025)
  
  q97p5_1 = rowQuantiles(keep_1, probs=0.975)
  q97p5_2 = rowQuantiles(keep_2, probs=0.975)  
  
  pp <- ggplot() +
    geom_point(aes(x = xs, y=mean_1, col="Sim 1000 - mean")) +
    geom_line(aes(x = xs, y=mean_1, col="Sim 1000 - mean")) +
    geom_ribbon(aes(x=xs, ymin=q2p5_1, ymax=q97p5_1, fill="Sim 1000 - 95% CI"), linetype=2, alpha=0.1) +
    xlab("Relative Time") + ylab("Coefficient") + theme_minimal() +
    theme(legend.title = element_blank()) +
    ggtitle(paste("Coefficients on Relative Time (Theta = ", theta, ")", sep=""))
  pp= pp + geom_point(aes(x = xs, y=mean_2, col="Sim 10000 - mean"))+
    geom_line(aes(x = xs, y=mean_2, col="Sim 10000 - mean")) +
    geom_ribbon(aes(x=xs, ymin=q2p5_2, ymax=q97p5_2, fill="Sim 10000 - 95% CI"), linetype=2, alpha=0.1)
  print(pp)
  ggsave(paste("Pset4/Plot_Q2_theta", theta, ".png", sep="") ,plot=pp)
}

### Part b
rho=0.5
run1000A = montecarlo_f(M=50, N=1000, theta=-2, rho=0.5, t=5)
run10000A = montecarlo_f(M=50, N=10000, theta=-2, rho=0.5, t=5)
plot_mcresult(run1000A[[1]], run10000A[[1]], theta="-2")

### Part c
run1000B = montecarlo_f(M=50, N=1000, theta=0, rho=0.5, t=5)
run10000B = montecarlo_f(M=50, N=1000, theta=0, rho=0.5, t=5)
plot_mcresult(run1000B[[1]], run10000B[[1]], theta="0.0")

run1000C = montecarlo_f(M=50, N=1000, theta=1, rho=0.5, t=5)
run10000C = montecarlo_f(M=50, N=1000, theta=1, rho=0.5, t=5)
plot_mcresult(run1000C[[1]], run10000C[[1]], theta="0.0")





### part d
### Devise a consistent estimator of ATE_3(2) = E[Y_i3(1) - Y_i3(0) | E_i=2]
simulation_ate <- function(theta=1, rho=0.5, N=100, tt=3, ee=2){
  
  run <- dgp_simulation(N, theta)
  Ys = run[[2]]
  Es = run[[3]]
  ts = run[[4]]
  part1 = mean(Ys[Es==ee & ts==tt])
  part2 = mean(Ys[Es==ee & ts==1])
  part3 = mean(Ys[Es==tt + 1 & ts==tt])
  part4 = mean(Ys[Es==tt + 1 & ts==1])
  sim_return = part1 - part2 + part3 - part4
  ### True value of ATE
  true_return = sin(tt - theta*ee)
  
  return(list(sim_return, true_return))
}

rho=0.5
simD2 = simulation_ate(theta=-2, rho=0.5, N=10000, t=3, e=2)
simD0 = simulation_ate(theta=0, rho=0.5, N=10000, t=3, e=2)
simD1 = simulation_ate(theta=1, rho=0.5, N=10000, t=3, e=2)

sim = data.table(Theta = c(-2.0, 0.0, 1.0),
                 Simulation = c(simD2[[1]],simD0[[1]],simD1[[1]]),
                 True = c(simD2[[2]],simD0[[2]],simD1[[2]]))
stargazer(sim, summary=FALSE)
                 
                   



se_ols_cluster <- function(x, res, cl){
  # cluster structure
  clusters = unique(cl)
  k = ncol(x)
  n = nrow(x)
  C = length(clusters)
  meat_cluster <- lapply(1:C, function(c){
    # Find which obs fall into the cluster, select the corresponding obs
    # compute each cluster's meat
    cindex = which(cl == clusters[c]) 
    if (length(cindex)==1){ Xc <- t(matrix(x[cindex, ])) }
    if (length(cindex) > 1){ Xc <- x[cindex, ]}
    resc <- matrix(res[cindex, ])
    meat <- t(Xc) %*% resc %*% t(resc) %*% Xc
    return(meat)})
  Meat <- (Reduce("+", meat_cluster)) / n
  Bread = solve((t(x)%*%x))
  #vcv = n * t(Bread) %*% Meat %*% Bread
  vcv = (C*(n-1) / ((C-1)*(n-k))) * n * t(Bread) %*% Meat %*% Bread
  return(sqrt(diag(vcv)))
}

adjust_rad = function(x, res, beta, cl){
  # Rademacher adjustment
  pred = cbind(1,x) %*% beta + res
  betarad = beta_ols_f(pred, x)
  resrad = pred - cbind(1,x) %*% betarad
  se_cl_rad = se_ols_cluster(cbind(1,x), resrad, cl=cl)
  wald = (betarad["rlfe1",] - true) / se_cl_rad["rlfe1"]
  return(wald)
}

wildbaby = function(y, x, cl, true){
  # Recover the test at 0.05 level for wild bootstrap + rademacher adjustment
  # Number of bootstraps for wild
  Mboot = 20
  
  # Get the constrained OLS (impose dr1 = true)
  xconstraint = cbind(x[,1:10], x[,12:13])
  betaconstraint = beta_ols_f(y, xconstraint)
  betaconstraint = c(betaconstraint[1:11], true, betaconstraint[12:13])
  resconstraint = y - cbind(1, x) %*% betaconstraint
  
  # Adjust bootstrap by Rademacher adjustment
  # randomize residuals = c(residuals, -residuals)
  draw = matrix(sample(c(-1,1), size=length(resconstraint)*Mboot, 
                       prob = c(0.5, 0.5), replace=T), ncol=Mboot)
  wiiild = apply(draw, 2, function(k){adjust_rad(x, resconstraint*k, 
                                                 betaconstraint, cl)})
  
  # find cluster SE (normal)
  beta = beta_ols_f(y, x)
  res = y - cbind(1,x) %*% beta
  se_cl = se_ols_cluster(cbind(1, x), res, cl=cl)
  wald = (beta["rlfe1",] - true) / se_cl["rlfe1"]
  # test
  return(1*(abs(wald) > quantile(abs(wiiild), .975)))
}


check_CI = function(x, true, se, N, thre=1.96){
  # Helper function to check that you are within the threshold
  reject = 1*(abs(x - true) / ((se)) > thre)
  return(reject)
}

test = function(N=20, theta=1, rho=0.5, t=5, true=(sin(1)-sin(-1)-sin(4))){
  ### Simulation
  run = dgp_simulation(N=N, theta=theta, rho=rho)
  y = as.matrix(run[[2]])
  x = as.matrix(run[[1]])
  xone = cbind(1, x)
  id = as.matrix(run[[5]])
  
  ### OLS Beta
  beta = beta_ols_f(y, x)
  
  ### SE
  # homo
  res = y - xone %*% beta
  se_homo = as.numeric(t(res) %*% res) * solve(t(xone) %*% xone)
  se_homo = sqrt(diag(se_homo)/(nrow(xone) - ncol(xone)))
  # cluster
  se_cl = se_ols_cluster(xone, res, cl=id)
  # robust
  res2 = diag(as.vector(res)^2, ncol=nrow(y))
  inside = t(xone) %*% res2 %*% xone
  se_rob = solve(t(xone) %*% xone) %*% 
    inside %*% solve(t(xone) %*% xone)
  se_rob = sqrt(diag(se_rob))
  # wild
  
  reject_wild = wildbaby(y, x, id, true)
  
  # Keep only relevant coefficient
  beta = beta[12]
  se_homo = se_homo["rlfe1"]
  se_cl = se_cl["rlfe2"]
  se_rob = se_rob["rlfe3"]

  reject = list(check_CI(beta, true, se_homo, N, 1.96),
              check_CI(beta, true, se_cl, N, 1.96),
              check_CI(beta, true, se_rob, N, 1.96),
              reject_wild)
  return(reject)
  
}

test_MC = function(N=20, theta=1, rho=0.5, t=5, M=100, true=(sin(1))){
  # Routine function for question E
  all_tests = matrix(rep(0,M*4), ncol=4)
  for (m in 1:M){
    print(m)
    results = tryCatch(test(N=N, theta=theta, rho=rho, t=t, true=true), 
                       error=function(cond) return(c(NA, NA, NA, NA)))
    all_tests[m,] = sapply(results, function(x) mean(x, na.rm=T))
  }
  all_tests = colmeans(all_tests[!is.na(all_tests[,1]),])
  return(all_tests)
}



all0 = lapply(c(20, 50, 200), test_MC, theta=1, rho=0, t=5, M=200, true=(sin(1)))
all05 = lapply(c(20, 50, 200), test_MC, theta=1, rho=0.5, t=5, M=200, true=(sin(1)))
all1 = lapply(c(20, 50, 200), test_MC, theta=1, rho=1, t=5, M=200, true=(sin(1)))

table = rbind(all0[[1]], all0[[2]], all0[[3]],
              all05[[1]], all05[[2]], all05[[3]],
              all1[[1]], all1[[2]], all1[[3]])
table = cbind(c(rep(c(20, 50, 200), 4)), as.matrix(table))
colnames(table) = c("Nb Obs", "Homoskedastic", "Cluster", "Robust", "Wild")
stargazer(table, summary=FALSE)



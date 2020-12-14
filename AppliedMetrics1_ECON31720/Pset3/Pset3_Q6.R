
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
library(prioritizr)
library(ggplot2)
library(splines2)
## ---------------------------

## load up our functions into memory

source("Pset_Functions.R") 

## ---------------------------

#### ------------
#### Problem 6: Identification and Extrapolation of Causal Effects with IV (Mogstad and Torgovitsky)
# Reproduce Figure 6 

# D = binary indicator for purchasing a mosquito net
# Z = experimentally varied subsidy for the net ==> binned {1, 2, 3, 4} (Z=4 most generous)
# Data from Dupas (2014) 
pz = 1/4
# pscore for each z
p1 = 0.12
p2 = 0.29
p3 = 0.48
p4 = 0.78
p = c(p1, p2, p3, p4)

### Compute Ez, Ed, Cov_dz etc such that consistent with DGP
n = 100
simD = c(rep(1, 12), rep(0, 88), rep(1, 29), rep(0, 71),
         rep(1,48), rep(0,52), rep(1,78), rep(0,22))
simZ = c(rep(1,n), rep(2,n), rep(3,n), rep(4,n))
Ez = (1+2+3+4)/4
Ed = (0.12+0.29+0.48+0.78)/4
cov_dz = mean(simD*simZ) - Ez*Ed



### the MTR (DGP)
m0 = function(u){0.9 - 1.1*u + 0.3*u^2}
m1 = function(u){0.35 - 0.3*u - 0.05*u^2}
md = function(u){-0.55 + 0.8*u - 0.35*u^2}

# Integrals of the MTRs
m1int = function(ul, uh){return(integrate(m1, ul, uh))}
m0int = function(ul, uh){return(integrate(m0, ul, uh))}


### THE BASIS
## BERNSTEIN
bernstein_own = function(ul, uh, k, K){
  # Mogstad, Santos, Torgovitsky 2018 version
  func = function(u){choose(K, k) * u^k * (1-u)^(K-k) }
  integral = integrate(func, lower=ul, upper=uh)$value
  return(integral)
}
## CONSTANT SPLINES
splines_own = function(limit){
  low = c(0, limit)
  high = c(limit, 1)
  spline = high - low
  return(spline)
}


### The s(d,x,z)
## Compute s_ds
sIVslope <- function(z){
  return((z - Ez) / cov_dz)
}
sTSLS <- function(z){
  Pi = matrix(c(1, 0.12, 1, 0.29, 1, 0.48, 1, 0.78), ncol=4)
  Zm = matrix(c(rep(pz, 4), p/4), ncol=2)
  Zmm = solve(Pi %*% Zm)
  w = Zmm %*% (Pi %*% diag(1, 4))
  return(w[2,z])
}

## Compute watt
w1att <- function(u){ifelse(u <= p[1], 1/Ed, 
                            ifelse(p[1] < u & u <= p[2], (1-pz)/Ed,
                                   ifelse(p[2] < u & u <= p[3], (1-2*pz)/Ed,
                                          ifelse(p[3] < u & u <= p[4], (1-3*pz)/Ed, 0))))}
w0att <- function(u){-1*w1att(u)}


### The weights
## Compute w_d from s_s
# Weights 1 defined as s(d,x,z) 1[u <= p(x,z)] 
# Therefore need to take the cumulative sum (and then average) for : if u <= p1, then also u <= p2, p3, p4
# s = (s1, s2, s3, s4)
# For D=1:
# w1(u | u <= p1) = 0.25 * (s1 + s2 + s3 + s4)
# w1(u | p1 < u <= p2) = 0.25 * (s4 + s3 + s2)
# w1(u | p2 < u <= p3) = 0.25 * (s4 + s3) ...

# For D=0:
# w0(u | u <= p1) = 0
# w0(u | p1 < u <= p2) = 0.25*s1
# w0(u | p2 < u <= p3) = 0.25*(s1+s2)
w1f <- function(sfunc){
  sall = sapply(1:4, sfunc)
  w1 = 0.25*(sall[1] + sall[2] + sall[3] + sall[4])
  w2 = 0.25*(sall[2] + sall[3] + sall[4])
  w3 = 0.25*(sall[3] + sall[4])
  w4 = 0.25*sall[4]
  return(c(w1, w2, w3, w4))}
w0f <- function(sfunc){
  sall = sapply(1:4, sfunc)
  w1 = 0.25*(sall[1])
  w2 = 0.25*(sall[1] + sall[2])
  w3 = 0.25*(sall[1] + sall[2] + sall[3])
  w4 = 0.25*(sall[1] + sall[2] + sall[3]+sall[4])
  return(c(w1, w2, w3, w4))}


### The gammas (do not depend on Theta)
## Compute Gamma_s
gamma_f = function(k, K, d=1, type=sIVslope, basis=bernstein_own, obj=FALSE, param=TRUE){
  if (obj==FALSE & d==1){
    w = w1f(type)
  }
  if (obj==FALSE & d==0){
    w = w0f(type)
  }
  if (obj==TRUE & d==1){
    w = w1att(p)
  }
  if (obj==TRUE & d==0){
    w = w0att(p)
  }
  if (param==TRUE){
    # We integrate over the whole range of pscores --> one gamma per k 
    b = mapply(FUN = basis, c(0, p[-4]), p, k, K)
    return(sum(b*w))
  }
  if (param==FALSE){
    b = splines_own(p)
    # Here we always return 5 gammas (but it only runs once! so equivalent to running 5 times
    #, one for each pscore)
    if (d==0){b = b[-1]}
    if (d==1){b = b[-5]}
    return(c(b*w, 0))
  }
}


### THE TRUE VALUES (using weights and the M1 and M0 from the DGP)
## Compute Beta (true)
beta_true <- function(type=sIVslope, obj=FALSE){
  if (obj==FALSE){
    w1 = w1f(type)
    w0 = w0f(type)
  }
  if (obj==TRUE){
    w1 = sapply(p, w1att)
    w0 = sapply(p, w0att)
  }
  mtr1 = unlist(mapply(m1int, c(0,p[-4]), p)['value',])*w1
  mtr0 = unlist(mapply(m0int, c(0,p[-4]), p)['value',])*w0
  return(sum(mtr1, mtr0))
}
betaIV = beta_true(type=sIVslope, obj=FALSE)
betaTSLS = beta_true(type=sTSLS, obj=FALSE)
betaATT = beta_true(obj=TRUE)

monoton_constraint = function(kk){
  # Constraint for the monotonicity of the thetas
  rows= kk*2
  cols = 2*(kk+1)
  mono = matrix(0, nrow=rows, ncol=cols)
  for (l in 1:kk){
    mono[l, l] = 1
    mono[kk+l, kk+l+1] = 1
    mono[l, l+1] = -1
    mono[kk+l,kk+l+2] = -1
  }
  return(mono)
}


## BOUNDS
# Max(Gamma_m(Theta)) such that Gamma_s(Theta) = Beta_s
# Min(Gamma_m(Theta)) such that Gamma_s(Theta) = Beta_s
# Write the function such that gurobi can do miracles
solvegurobi <- function(K, direction="max", monotone=FALSE, parametric=TRUE){
  
  if (parametric==TRUE){
    basis=bernstein_own
    K2 = K}
  
  if (parametric==FALSE){
    basis=splines_own
    K2 = 4}
  gamma_att1_all = sapply(0:K, gamma_f, K, 1, basis=basis, obj=TRUE, param=parametric)
  gamma_att0_all = -1*gamma_att1_all

  gamma_iv1_all = sapply(0:K, gamma_f,  K, 1, sIVslope, basis=basis, obj=FALSE, param=parametric)
  gamma_iv0_all = -1*gamma_iv1_all

  gamma_tsls1_all = sapply(0:K, gamma_f,  K, 1, sTSLS, basis=basis, obj=FALSE, param=parametric)
  gamma_tsls0_all = -1*gamma_tsls1_all

  # Gurobi at work
  beautiful = list()
  beautiful$obj = c(gamma_att1_all, gamma_att0_all)
  beautiful$lb = 0
  beautiful$ub = 1
  beautiful$modelsense = direction
  
  if (monotone==FALSE){
    beautiful$A = rbind(matrix(c(gamma_iv1_all, gamma_iv0_all), nrow=1), 
                        matrix(c(gamma_tsls1_all, gamma_tsls0_all), nrow=1))
    beautiful$sense = c("=", "=")
    beautiful$rhs = c(betaIV, betaTSLS)
  }
  if (monotone==TRUE){ #Thanks Sunny for "troubleshouting"
    # Basically creates additional constraints imposing that everything is monotone
    # Theta_k - Theta_{k+1} < 0       <=> Theta_k > Theta_{k+1}
    # As many constraints as theta differences

    mono = monoton_constraint(K2)
    
    beautiful$A = rbind(matrix(c(gamma_iv1_all, gamma_iv0_all), nrow=1), 
                        matrix(c(gamma_tsls1_all, gamma_tsls0_all), nrow=1),
                        mono)
    beautiful$sense = c("=", "=", rep(">", K2*2))
    beautiful$rhs = c(betaIV, betaTSLS, rep(0, K2*2))
  }
  output = gurobi(beautiful)
  return(output$objval)
}


## Compute the bounds
upper_param = mapply(solvegurobi, 1:19, direction="max", monotone=FALSE, parametric=TRUE)
lower_param = mapply(solvegurobi, 1:19, direction="min", monotone=FALSE, parametric=TRUE)
upper_param_mono = mapply(solvegurobi, 1:19, direction="max", monotone=TRUE, parametric=TRUE)
lower_param_mono = mapply(solvegurobi, 1:19, direction="min", monotone=TRUE, parametric=TRUE)
upper_nonparam = solvegurobi(0, direction="max", monotone=FALSE, parametric=FALSE)
lower_nonparam = solvegurobi(0, direction="min", monotone=FALSE, parametric=FALSE)
upper_nonparam_mono = solvegurobi(0, direction="max", monotone=TRUE, parametric=FALSE)
lower_nonparam_mono = solvegurobi(0, direction="min", monotone=TRUE, parametric=FALSE)

## Plot the ATT
ggplot() +
  geom_point(aes(x=1:19, y=upper_param, col="Polynomial")) +
  geom_line(aes(x=1:19, y=upper_param, col="Polynomial")) +
  geom_point(aes(x=1:19, y=lower_param, col="Polynomial")) +
  geom_line(aes(x=1:19, y=lower_param, col="Polynomial")) +
  geom_point(aes(x=1:19, y=upper_param_mono, col="Polynomial (Monotone)")) +
  geom_line(aes(x=1:19, y=upper_param_mono, col="Polynomial (Monotone)")) +
  geom_point(aes(x=1:19, y=lower_param_mono, col="Polynomial (Monotone)")) +
  geom_line(aes(x=1:19, y=lower_param_mono, col="Polynomial (Monotone)")) +
  geom_point(aes(x=1:19, y=upper_nonparam, col="Non Polynomial")) +
  geom_line(aes(x=1:19, y=upper_nonparam, col="Non Polynomial")) +
  geom_point(aes(x=1:19, y=lower_nonparam, col="Non Polynomial")) +
  geom_line(aes(x=1:19, y=lower_nonparam, col="Non Polynomial")) +
  geom_point(aes(x=1:19, y=upper_nonparam_mono, col="Non Polynomial (Monotone)")) +
  geom_line(aes(x=1:19, y=upper_nonparam_mono, col="Non Polynomial (Monotone)")) +
  geom_point(aes(x=1:19, y=lower_nonparam_mono, col="Non Polynomial (Monotone)")) +
  geom_line(aes(x=1:19, y=lower_nonparam_mono, col="Non Polynomial (Monotone)")) +
  geom_hline(aes(yintercept=betaATT, col="ATT")) +
  theme_minimal() + theme(legend.title = element_blank())+
  ylab("Bounds on the ATT") +
  xlab("Polynomial degree") 



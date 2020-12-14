
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
## ---------------------------

## load up our functions into memory

source("Pset_Functions.R") 

## ---------------------------


################################################################################
################################# PROBLEM 5 ####################################
################################################################################

##### FUNCTIONS

# Recover the MTR (or MTE for Specification 2) from the regression result
adjusts_mtr = function(reg, d=1, spec="1"){
  namesdelta = c("d_age_u", "d_ageat1st_u", "d_agekid1_u",
                 "d_agekid2_u", "d_boy1st_u", "d_boy2nd_u", "d_black_u",
                 "d_hispanic_u", "d_otherrace_u")
  namesgamma = c("g_age", "g_ageat1st", "g_agekid1",
                 "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
                 "g_hispanic", "g_otherrace")
  
  # Copy regression results, so that no need to go through coef that are directly identified
  regadj = reg
  
  if (spec=="1"){
    if (d==1){
      regadj["b"] = 2*reg["b"]
    }
    if (d==0){
      regadj["a"] = reg["a"] - reg["b"]
      regadj["b"] = 2*reg["b"]
    }
  }
  
  if (spec=="2"){
      regadj = c()
      regadj["a1ma0"] = reg["a1"]
      regadj["b1mb0"] = reg["b1"]
      #for (g in namesgamma){
      #  regadj[g] = reg[g]
      #}
  }
  
  if (spec=="3"){
    if (d==1){
      regadj["b"] = 2*reg["b"]
      for (d in namesdelta){
        regadj[d] = 2*reg[d]
      }
    }
    if (d==0){
      regadj["a"] = reg["a"] - reg["b"]
      regadj["b"] = 2*reg["b"]
      for (g in 1:length(namesgamma)){
        regadj[namesgamma[g]] = reg[namesgamma[g]] - reg[namesdelta[g]]
        regadj[namesdelta[g]] = 2*reg[namesdelta[g]]
      }
    }
  }
  
  if (spec=="4"){
    if (d==1){
      regadj["b"] = 2*reg["b"]
      regadj["b2"] = 3*reg["b2"]
    }
    if (d==0){
      regadj["a"] = reg["a"] - reg["b"] 
      regadj["b"] = 2*(reg["b"] - reg["b2"])
      regadj["b2"] = 3*reg["b2"]
    }
  }
  
  if (spec=="5"){
    if (d==1){
      regadj["b"] = 2*reg["b"]
      regadj["b2"] = 3*reg["b2"] 
      regadj["b3"] = 4*reg["b3"]
      }
    if (d==0){
      regadj["a"] = reg["a"] - reg["b"] 
      regadj["b"] = 2*(reg["b"] - reg["b2"])
      regadj["b2"] = 3*(reg["b2"] - reg["b3"])
      regadj["b3"] = 4*(reg["b3"])
    }
  }
  return(regadj)
}




# Names of coefficients for each specification
namesi = c("a", "b", "g_age", "g_ageat1st", "g_agekid1",
           "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
           "g_hispanic", "g_otherrace")

namesii = c("a", "b", "zeta", "g_age", "g_ageat1st", "g_agekid1",
             "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
             "g_hispanic", "g_otherrace")

namesii = c("a0", "a1", "b0", "b1", "g_age", "g_ageat1st", "g_agekid1",
            "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
            "g_hispanic", "g_otherrace")

namesiii = c("a", "b", "g_age", "g_ageat1st", "g_agekid1",
             "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
             "g_hispanic", "g_otherrace",
             "d_age_u", "d_ageat1st_u", "d_agekid1_u",
             "d_agekid2_u", "d_boy1st_u", "d_boy2nd_u", "d_black_u",
             "d_hispanic_u", "d_otherrace_u")

namesiv = c("a", "b", "b2", "g_age", "g_ageat1st", "g_agekid1",
            "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
            "g_hispanic", "g_otherrace")

namesv = c("a", "b", "b2", "b3", "g_age", "g_ageat1st", "g_agekid1",
           "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
           "g_hispanic", "g_otherrace")


# Compute the MTE from MTR1 and MTR0 and x_avg
# mte_from_mtr = function(m1, m0, avgx, uuu, spe="1"){
#   iii = cbind(1, uuu)
#   for (n in 1:length(avgx)){
#     iii = cbind(iii, avgx[n])
#   }
#   if (spec=="3"){
#     ugridp = t(kronecker(t(uuu), avgx))
#     iii = cbind(iii, ugridp)
#   }
#   mte_u = iii %*% (m1 - m0)
#   return(mte_u)
# }

# Graph the MTE on U Grid


minp <- function(u, p){
  # Computes P(u > pz)
  # (1 - minp) = P(u <= pz)
  return(length(which(p < u))/length(p))
}

weightATE_f <- function(u, p){
  return(ones(length(u), 1))
}

weightATT_f <- function(u, p){
  pmin = sapply(u, minp, p)
  return((1-pmin)/Ed)
}

weightATU_f <- function(u, p){
  pmin = sapply(u, minp, p)
  return((pmin)/(1-Ed))
}

weightLATE_f <- function(u, p){
  low = mean(p[z_data==0])
  high = mean(p[z_data==1])
  weight = (u < high & u > low) / (high - low)
  return(weight)
}








### Compute each param of interest
param_f <- function(u, weight, mte, p, v=1, D=2, m1=mtr1, m0=mtr0, x=indep, spec=1){
  if (v==1){ # Version 1: integrate the MTE with the appropriate weights
    inside = weight(u, p)*mte
    return(mean(inside))
  }
  if (v==2 & spec != "2"){ # Version 2: directly from the imputed potential outcomes
    # Impute Y(1) and Y(0) from the MTRs
    Y0imputed = x %*% m0
    Y1imputed = x %*% m1
    # Replace by observed data when observed
    Y0imputed[iud,] = y_data[iud,]
    Y1imputed[id,] = y_data[id]
    if (D==2){ return(mean(Y1imputed) - mean(Y0imputed))} #ATE 
    if (D==1){ return(mean(Y1imputed[id,]) - mean(Y0imputed[id,]))} #ATT
    if (D==0){ return(mean(Y1imputed[iud,]) - mean(Y0imputed[iud,]))} #ATU
  }
    if (v==2 & spec == "2"){
      Diffimputed = x %*% m0
      if (D==2){return(mean(Diffimputed))}
      if (D==1){ return(mean(Diffimputed[id,]))}
      if (D==0){ return(mean(Diffimputed[iud,]))}
    }
  }




  
### Routine
spec_routine <- function(dep, indep, names, 
                             qq = "a", spec="1", 
                             x_avg,
                             id=index_d, iud=index_ud){
  
  uuu = linspace(0,1,100)

  if (spec=="2"){
    # Compute mte params and mte
    reg0 = beta_ols_f(y=dep, x=indep)
    names(reg0) = names
    mte_est = adjusts_mtr(reg0, spec="2")
    xx = cbind(1, uuu)
    reg1 = copy(reg0)
    #for (n in 1:length(x_avg)){
    #  xx = cbind(xx, x_avg[n])
    #}
    mte = xx %*% mte_est
  }
  
  if (spec!="2"){
    # Compute mt1 and mt0
    reg1 = beta_ols_f(y=dep[id,], x=indep[id,])
    names(reg1) = names
    reg0 = beta_ols_f(y=dep[iud,], x=indep[iud,])
    names(reg0) = names
    
    # Get the mtrs
    mtr1 = adjusts_mtr(reg1, d=1, spec=spec)
    mtr0 = adjusts_mtr(reg0, d=0, spec=spec)
    
    # Compute the MTE for the grid of u, evaluated at the average X & plot it
    iii = cbind(1, uuu)
    if (spec=="4" | spec=="5"){
      iii = cbind(iii, uuu^2)
    }
    if (spec=="5"){
      iii = cbind(iii, uuu^3)
    }
    for (n in 1:length(x_avg)){
      iii = cbind(iii, x_avg[n])
    }
    if (spec=="3"){
      ugridp = t(kronecker(t(uuu), x_avg))
      iii = cbind(iii, ugridp)
    }
    mte = iii %*% (mtr1 - mtr0)
  }
  
  tit = paste("MTE for specification ", spec, " evaluated at avg X", sep="")
  print(ggplot() +
    geom_line(aes(x = uuu, y=mte))+
    theme_minimal() + ylab("MTE") + xlab("u") + ylim(-0.8, 0.6)+
    ggtitle(tit)  )
  ggsave(paste(paste("Pset3/Problem5Graphs/GraphMTE_Spec", spec, qq, sep="_"), ".png", sep=""))
  

  # Compute the parameters ATE, ATT, ATU (evaluated at pmean (the pscore for the average X))
  # Potentially replace pmean by p (the propensity score)
  if (ncol(z_data)==1){
    param = c(param_f(uuu, weightATE_f, mte, p, v=1, spec), # Integrating the MTE
              param_f(uuu, weightATT_f, mte, p, v=1, spec),
              param_f(uuu, weightATU_f, mte, p, v=2, spec),
              param_f(ugrid, weightLATE_f, mte, p, v=1, spec),
              param_f(uuu, weightATE_f, mte, p, v=2, D=2, reg1, reg0, indep, spec), # Taking the average over treated / untreated
              param_f(uuu, weightATT_f, mte, p, v=2, D=1, reg1, reg0, indep, spec),
              param_f(uuu, weightATU_f, mte, p, v=2, D=0, reg1, reg0, indep, spec))
              
  }
  if (ncol(z_data)==2){ # LATE doesn't make sense in itself if multiple instruments
    param = c(param_f(uuu, weightATE_f, mte, p, v=1, spec),
              param_f(uuu, weightATT_f, mte, p, v=1, spec),
              param_f(uuu, weightATU_f, mte, p, v=1, spec),
              param_f(uuu, weightATE_f, mte, p, v=2, D=2, reg1, reg0, indep, spec), # Taking the average over treated / untreated
              param_f(uuu, weightATT_f, mte, p, v=2, D=1, reg1, reg0, indep, spec),
              param_f(uuu, weightATU_f, mte, p, v=2, D=0, reg1, reg0, indep, spec))
  }  
  
  return(param)
}


#### General
data <- read_csv("Pset3/angrist_evans_clean.csv")

x_data = attrapemoi(data, c("age", "ageat1st", "agekid1", "agekid2",
                            "boy1st", "boy2nd", "black", "hispanic",
                            "otherrace"))
d_data = attrapemoi(data, c("more2kids"))
y_data = attrapemoi(data, c("worked"))

Ed = mean(d_data)

# treated
index_d = data[,"more2kids"] == 1
index_ud = data[,"more2kids"] == 0
# Compute the average of each X (needed for the MTE)
xmean = colMeans(x_data)
# Grid of u
ugrid = linspace(0,1,100)

#### Question a
instrument="samesex"
z_data = attrapemoi(data, instrument)
index_z = data[,instrument]==1
index_uz = data[,instrument]==0

# Compute the propensity Score using home-made logit
allvec = cbind(1, z_data, x_data)
logitown = maxLik(logit_f, start=rep(.01, ncol(allvec)), yv = d_data, xv=allvec )
p = exp(allvec %*% logitown$estimate) / (1 + exp(allvec %*% logitown$estimate))
# Compute the propensity score evaluated at mean(x)
#vec_mean = cbind(1, z_data, repmat(xmean, nrow(z_data), 1))
#pmean <- exp(vec_mean %*% logitown$coefficients) / (1 + exp(vec_mean %*% logitown$coefficients))

# logit <- glm(d_data ~ z_data + x_data, family = "binomial") # Alternatively using R (same results, faster)
# vec = cbind(1, z_data, x_data)
# p <- exp(vec%*%logit$coefficients) / (1 + exp(vec %*% logit$coefficients))


### For each specification
## Spec i
indepi = cbind(1, p, x_data) 
paramiA = spec_routine(dep=y_data, indep=indepi, names=namesi, 
                          qq="a", spec="1", x_avg=xmean,
                       id=index_d, iud = index_ud)

## Spec ii
indepii = cbind(1, d_data, p, p*d_data, x_data)
#indepii = cbind(1, p, p^2, x_data)
paramiiA = spec_routine(dep=y_data, indep=indepii, names=namesii,
                        qq="a", spec="2", x_avg=xmean)

## Spec iii
x_data_u = x_data*repmat(p, 1, ncol(x_data))
indepiii = cbind(1, p, x_data, x_data_u)
paramiiiA = spec_routine(dep=y_data, indep=indepiii, names=namesiii,
                         qq="a", spec="3", x_avg=xmean)

## Spec iv
p2 = p^2
indepiv = cbind(1, p, p2, x_data)
paramivA = spec_routine(dep=y_data, indep=indepiv, names=namesiv,
                        qq="a", spec="4", x_avg=xmean)
## Spec v
p3 = p^3
indepv = cbind(1, p, p2, p3, x_data)
paramvA = spec_routine(dep=y_data, indep=indepv, names=namesv,
                       qq="a", spec="5", x_avg=xmean)


paramallA = cbind(paramiA, paramiiA, paramiiiA, paramivA, paramvA)
paramallA = as.data.table(paramallA,
                          row.names = c("ATE", "ATT", "ATU", "LATE", "ATE (V2)", "ATT (V2)", "ATU (V2)"))

stargazer(paramallA, summary = FALSE)



#### Question b
instrument="twins"
index_z = data[,instrument]==1
index_uz = data[,instrument]==0
z_data = attrapemoi(data, instrument)

# Pscore
# allvec = cbind(1, z_data, x_data)
# logitown = maxLik(logit_f, start=rep(.01, ncol(allvec)), yv = d_data, xv=allvec )
# p = exp(allvec %*% logitown$estimate) / (1 + exp(allvec %*% logitown$estimate))

logit <- glm(d_data ~ z_data + x_data, family = "binomial") # Alternatively using R (same results, faster)
vec = cbind(1, z_data, x_data)
p <- exp(vec%*%logit$coefficients) / (1 + exp(vec %*% logit$coefficients))


### For each specification
## Spec i
indepi = cbind(1, p, x_data) 
paramiB = spec_routine(dep=y_data, indep=indepi, names=namesi, 
                       qq="b", spec="1", x_avg=xmean,
                       id=index_d, iud = index_ud)

## Spec ii
indepii = cbind(1, d_data, p, p*d_data, x_data)
paramiiB = spec_routine(dep=y_data, indep=indepii, names=namesii,
                        qq="b", spec="2", x_avg=xmean)

## Spec iii
x_data_u = x_data*repmat(p, 1, ncol(x_data))
indepiii = cbind(1, p, x_data, x_data_u)
paramiiiB = spec_routine(dep=y_data, indep=indepiii, names=namesiii,
                         qq="b", spec="3", x_avg=xmean)

## Spec iv
p2 = p^2
indepiv = cbind(1, p, p2, x_data)
paramivB = spec_routine(dep=y_data, indep=indepiv, names=namesiv,
                        qq="b", spec="4", x_avg=xmean)
## Spec v
p3 = p^3
indepv = cbind(1, p, p2, p3, x_data)
paramvB = spec_routine(dep=y_data, indep=indepv, names=namesv,
                       qq="b", spec="5", x_avg=xmean)


paramallB = cbind(paramiB, paramiiB, paramiiiB, paramivB, paramvB)
paramallB = as.data.table(paramallB)

stargazer(paramallB, summary=FALSE)




#### Question c
instrument=c("samesex", "twins")
#index_z = data[,instrument]==1
#index_uz = data[,instrument]==0
z_data = attrapemoi(data, instrument)
index_00z = data[,instrument[1]]==0 & data[,instrument[2]]==0
index_10z = data[,instrument[1]]==1 & data[,instrument[2]]==0
index_01z = data[,instrument[1]]==0 & data[,instrument[2]]==1
index_11z = data[,instrument[1]]==1 & data[,instrument[2]]==1



# Compute the propensity Score using home-made logit
allvec = cbind(1, z_data, x_data)
logitown = maxLik(logit_f, start=rep(.01, ncol(allvec)), yv = d_data, xv=allvec )
p = exp(allvec %*% logitown$estimate) / (1 + exp(allvec %*% logitown$estimate))
 
# logit <- glm(d_data ~ z_data + x_data, family = "binomial") # Alternatively using R (same results, faster)
# vec = cbind(1, z_data, x_data)
# pR <- exp(vec%*%logit$coefficients) / (1 + exp(vec %*% logit$coefficients))
# 

### For each specification
## Spec i
indepi = cbind(1, p, x_data) 
paramiC = spec_routine(dep=y_data, indep=indepi, names=namesi, 
                       qq="c", spec="1", x_avg=xmean,
                       id=index_d, iud = index_ud)

## Spec ii
indepii = cbind(1, d_data, p, p*d_data, x_data)
paramiiC = spec_routine(dep=y_data, indep=indepii, names=namesii,
                        qq="c", spec="2", x_avg=xmean)

## Spec iii
x_data_u = x_data*repmat(p, 1, ncol(x_data))
indepiii = cbind(1, p, x_data, x_data_u)
paramiiiC = spec_routine(dep=y_data, indep=indepiii, names=namesiii,
                         qq="c", spec="3", x_avg=xmean)

## Spec iv
p2 = p^2
indepiv = cbind(1, p, p2, x_data)
paramivC = spec_routine(dep=y_data, indep=indepiv, names=namesiv,
                        qq="c", spec="4", x_avg=xmean)
## Spec v
p3 = p^3
indepv = cbind(1, p, p2, p3, x_data)
paramvC = spec_routine(dep=y_data, indep=indepv, names=namesv,
                       qq="c", spec="5", x_avg=xmean)


paramallC = cbind(paramiC, paramiiC, paramiiiC, paramivC, paramvC)
paramallC = as.data.table(paramallC)

stargazer(paramallC, summary=FALSE)





#### TSLS Estimates

z_dataA = attrapemoi(data=data, "samesex")
z_dataB = attrapemoi(data=data, "twins")
z_dataC = attrapemoi(data=data, c("samesex", "twins"))

betatslsA = beta_tsls_f_withcov(y_data, d_data, z_dataA, x_data)[2]
betatslsB = beta_tsls_f_withcov(y_data, d_data, z_dataB, x_data)[2]
betatslsC = beta_tsls_f_withcov(y_data, d_data, z_dataC, x_data)[2]


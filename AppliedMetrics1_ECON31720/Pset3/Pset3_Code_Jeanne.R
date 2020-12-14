## ---------------------------
##
## Script name: Problem Set 2
##
## Purpose of script: 
##
## Author: Jeanne Sorin
##
## Date Created: 2020-11-20
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

data <- read_csv("Pset3/angrist_evans_clean.csv")
data = data[sample(nrow(data), 10000), ]

## ---------------------------
################################################################################
################################# PROBLEM 5 ####################################
################################################################################

x_data = attrapemoi(data, c("age", "ageat1st", "agekid1", "agekid2", 
                              "boy1st", "boy2nd", "black", "hispanic",
                              "otherrace"))
d_data = attrapemoi(data, c("more2kids"))
z_data = attrapemoi(data, c("samesex"))
y_data = attrapemoi(data, c("worked"))

#rm(data)


### TSLS
tsls_results <- tsls_own(data="NA", dep_var = y_data, indep_var = x_data, 
                         treat_var = d_data, instru_var = z_data,
                         robust = FALSE)

### The beast

# 0. Obtain the propensity score for each observation through logit
p <- logit_own(cbind(x_data, z_data), d_data)
treated_index = d_data==1
untreated_index = d_data==0
ptreated = p[treated_index,]
puntreated = p[untreated_index,]

ggplot() +
  geom_histogram(aes(ptreated, fill="Treated"), alpha = 0.8) +
  geom_histogram(aes(puntreated, fill="Untreated"), alpha = 0.8) +
  xlab("propensity score") + 
  ylab("Count") 


### How do you get from p to u ??
Fp = function(u){length(p[p <= u]) / length(p)}
u = sapply(p, Fp)
utreated = u[treated_index]
uuntreated = u[untreated_index]
ggplot() +
  geom_histogram(aes(utreated, fill="Treated"), alpha = 0.8) +
  geom_histogram(aes(uuntreated, fill="Untreated"), alpha = 0.8) +
  xlab("U") + 
  ylab("Count") 

plot(z_data, p)

# 1. For each u recover E(Y(d) | U=u, X=x) (see Heckman Pset 4)
# What is the difference between p / ud
pL = 0.0
pH = 1.0
bins = 5

# Creates pscore domain
p_values = linspace(pL, pH, bins+1)
valuesY1 = zeros(bins+1, 1)
valuesY0 = zeros(bins+1, 1)
ME = zeros(bins+1, 1)

# Get binned results (non parametric MTR)
for (i in 1:(bins+1)){
  pv = p_values[i]
  indexes = ((pv - 1/(2*bins) <= p) & (p <= pv + 1/(2*bins)))[,1]
  df = data[indexes,]
  df1 = df[df$more2kids==1,]
  df0 = df[df$more2kids==0,]
  valuesY1[i,] = mean(df1$worked, na.rm=T)
  valuesY0[i,] = mean(df0$worked, na.rm=T)
  ME[i,] = valuesY1[i,] - valuesY0[i,]
}

ggplot() +
  geom_line(aes(x=p_values, y=ME))



# From E(Y(1) | U <= p) = a1 + b1/2p
# to E(Y(1) | U = u) = a1 + b1 u 
# ie = b1/2 = b1^ => b1 = 2*b1^ 
# b1/2

compute_mtr = function(data = data, instrument="more2kids"){
  index_t = data[,instrument]==1
  index_ut = data[,instrument]==0
}



## Specification (i) m(d | u, x) = \alpha_d + \beta_d u + \gamma_d' x
index_t = data$more2kids==1
index_ut = data$more2kids==0
names = c("a", "b", "g_age", "g_ageat1st", "g_agekid1",
           "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
           "g_hispanic", "g_otherrace")


indep_t = cbind(1, u[index_t], x_data[index_t,])
dep_t = y_data[index_t,]
mi_1 = lm_own(data="NA", dep_var = dep_t, indep_var = indep_t)
mi_1$names = names
mi_1 = adjusts_mtr(mi_1)


indep_ut = cbind(1, u[index_ut], x_data[index_ut,])
dep_ut = y_data[index_ut,]
mi_0 = lm_own(data="NA", dep_var = dep_ut, indep_var = indep_ut)
mi_0$names = names
mi_0 = adjusts_mtr(mi_0)


## Specification (ii) m(d | u, x) = \alpha_d + \beta_d u + \gamma' x
constant_t = 1*d_data
constant_ut = 1*(1-d_data)
u_t = u*d_data
u_ut = u*(1-d_data)
names = c("a1", "a0", "b1", "b0", "g_age", "g_ageat1st", "g_agekid1",
          "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
          "g_hispanic", "g_otherrace")

indep = cbind(constant_t, constant_ut, u_t, u_ut, x_data)
dep = y_data
mii = lm_own(data="NA", dep_var = dep, indep_var = indep)
mii$names = names
mii = adjusts_mtr(mii)



## Specification (iii) m(d | u, x) = \alpha_ + \beta_d u + \gamma_d'x + \delta_d' xu
index_t = data$more2kids==1
index_ut = data$more2kids==0
names = c("a", "b", "g_age", "g_ageat1st", "g_agekid1",
          "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
          "g_hispanic", "g_otherrace",
          "d_age_u", "d_ageat1st_u", "d_agekid1_u",
          "d_agekid2_u", "d_boy1st_u", "d_boy2nd_u", "d_black_u",
          "d_hispanic_u", "d_otherrace_u")

x_data_u = t(t(x_data) %*% diag(u[,1]))
indep_t = cbind(1, u[index_t], x_data[index_t,], x_data_u[index_t,])
dep_t = y_data[index_t,]
miii_1 = lm_own(data="NA", dep_var = dep_t, indep_var = indep_t)
miii_1$names = names
miii_1 = adjusts_mtr(miii_1)

indep_ut = cbind(1, u[index_ut], x_data[index_ut,], x_data_u[index_ut,])
dep_ut = y_data[index_ut,]
miii_0 = lm_own(data="NA", dep_var = dep_ut, indep_var = indep_ut)
miii_0$names = names
miii_0 = adjusts_mtr(miii_0)



## Specification (iv) m(d | u, x) = \alpha_ + \beta_d u + \beta2_d u^2 + \gamma_d'x 
u2 = u^2
index_t = data$more2kids==1
index_ut = data$more2kids==0
names = c("a", "b", "b2", "g_age", "g_ageat1st", "g_agekid1",
          "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
          "g_hispanic", "g_otherrace")

indep_t = cbind(1, u[index_t], u2[index_t], x_data[index_t,])
dep_t = y_data[index_t,]
miv_1 = lm_own(data="NA", dep_var = dep_t, indep_var = indep_t)
miv_1$names = names
miv_1 = adjusts_mtr(miv_1)

indep_ut = cbind(1, u[index_ut], u2[index_ut], x_data[index_ut,])
dep_ut = y_data[index_ut,]
miv_0 = lm_own(data="NA", dep_var = dep_ut, indep_var = indep_ut)
miv_0$names = names
miv_0 = adjusts_mtr(miv_0)


### Specification (v)  m(d | u, x) = \alpha_ + \beta_d u + \beta2_d u^2 + \beta3_d u^3 + \gamma_d'x 
u3 = u^3
index_t = data$more2kids==1
index_ut = data$more2kids==0
names = c("a", "b", "b2", "b3", "g_age", "g_ageat1st", "g_agekid1",
          "g_agekid2", "g_boy1st", "g_boy2nd", "g_black",
          "g_hispanic", "g_otherrace")

indep_t = cbind(1, u[index_t], u2[index_t], u3[index_t], x_data[index_t,])
dep_t = y_data[index_t,]
mv_1 = lm_own(data="NA", dep_var = dep_t, indep_var = indep_t)
mv_1$names = names
mv_1 = adjusts_mtr(mv_1)

indep_ut = cbind(1, u[index_ut], u2[index_ut], u3[index_ut], x_data[index_ut,])
dep_ut = y_data[index_ut,]
mv_0 = lm_own(data="NA", dep_var = dep_ut, indep_var = indep_ut)
mv_0$names = names
mv_0 = adjusts_mtr(mv_0)



### For each specification estimate
# i
indepv = cbind(1, u, x_data)
parami = cbind(estimate_manual(model0 = mi_0, model1 = mi_1, param="ATE", indepv = indepv),
               estimate_manual(model0 =mi_0, model1 = mi_1, param="ATT", indepv = indepv),
               estimate_manual(model0 =mi_0, model1 = mi_1, param="ATU", indepv = indepv),
               estimate_manual(model0 =mi_0, model1 = mi_1, param="LATE", indepv = indepv))
# ii
constant_t = 1*d_data
constant_ut = 1*(1-d_data)
u_t = u*d_data
u_ut = u*(1-d_data)
indepv = cbind(constant_t, constant_ut, u_t, u_ut, x_data)
paramii = cbind(estimate_manual(modelboth = mii, param="ATE", indepv = indepv),
                estimate_manual(modelboth= mii, param="ATT", indepv = indepv),
                estimate_manual(modelboth= mii, param="ATU", indepv = indepv),
                estimate_manual(modelboth= mii, param="LATE", indepv = indepv))
# iii
x_data_u = t(t(x_data) %*% diag(u))
indepv = cbind(1, u, x_data, x_data_u)
paramiii = cbind(estimate_manual(model0 =miii_0, model1 = miii_1, param="ATE", indepv = indepv),
                 estimate_manual(model0 =miii_0, model1 = miii_1, param="ATT", indepv = indepv),
                 estimate_manual(model0 =miii_0, model1 = miii_1, param="ATU", indepv = indepv),
                 estimate_manual(model0 =miii_0, model1 = miii_1, param="LATE", indepv = indepv))
# iv
u2 = u^2
indepv = cbind(1, u, u2, x_data)
paramiv = cbind(estimate_manual(model0 = miv_0, model1 = miv_1, param="ATE", indepv = indepv),
                estimate_manual(model0 =miv_0, model1 = miv_1, param="ATT", indepv = indepv),
                estimate_manual(model0 =miv_0, model1 = miv_1, param="ATU", indepv = indepv),
                estimate_manual(model0 =miv_0, model1 = miv_1, param="LATE", indepv = indepv))
# v
u3 = u^3
indepv = cbind(1, u, u2, u3, x_data)
paramv = cbind(estimate_manual(model0 =mv_0, model1 = mv_1, param="ATE", indepv = indepv),
               estimate_manual(model0 =mv_0, model1 = mv_1, param="ATT", indepv = indepv),
               estimate_manual(model0 =mv_0, model1 = mv_1, param="ATU", indepv = indepv),
               estimate_manual(model0 =mv_0, model1 = mv_1, param="LATE", indepv = indepv))


allestimates = cbind(t(parami), t(paramii), t(paramiii),t(paramiv), t(paramv))
allestimates[1:3,] = round(as.numeric(allestimates[1:3,]), 3)
### Construct m(u = E[Y(1) - Y(0) | U = u]) 
# Average over xs









#### ------------
#### Problem 6: Identification and Extrapolation of Causal Effects with IV (Mogstad and Torgovitsky)
# Reproduce Figure 6 







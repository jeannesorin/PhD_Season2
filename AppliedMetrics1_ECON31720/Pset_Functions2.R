## This code contains all home-made functions used for psets in 
## Applied Econometrics

## ------------------------------------------------
## Import Packages
require(tidyverse)
require(data.table)
require(readstata13)
require(pracma)
require(geodist)
require(tidyselect)
require(knitr)
library(Matrix)
library(maxLik)


################################################################################
############################# RESHAPING ETC ####################################
################################################################################

## ----
## extract relevant data columns from data frame
attrapemoi <- function(data, varnames){ 
  matrixObject <- data %>%
    dplyr::select(.dots = varnames) %>%
    as.matrix()
}

## ----
## Make Pretty Regression tables
make_tables <- function(est, se, N, 
                        names, dep_var, 
                        title,
                        format="latex", r=3){
  output = data.frame(Coef = names,
                      dep_var = paste(round(est, r), paste("(",round(se, r), ")", sep=""), sep=" "))
  return(xtable(output, type=format, caption=title))
}




################################################################################
############################# BASICS ###########################################
################################################################################

std_own <- function(vector){
  ## standard deviation
  n = length(vector)
  mean = mean(vector)
  return(sqrt((1/(n-1))*sum((vector - mean)^2)))
}

logitlikf <- function(theta, y, x){
  #thanks Tanya for the helo on this
  z = x%*%theta
  v1 = log(exp(z)/(1 + exp(z)))
  v0 = log(1/(1 + exp(z)))
  inside = y*v1 + (1-y)*v0
  return(sum(inside))
}

probit_own <- function(z_data, d_data){
  z_data2 = cbind(1, z_data)
  output = beta_ols_f(y=d_data, x=z_data2)
  pred = z_data2 %*% output
  return(pnorm(pred))
}



logit_own <- function(x, z){
  x_withone = cbind(1, x)
  #reg <- lm_own(data="NA", dep_var = z, indep_var = x_withone)
  # parametric
  reg <-maxLik(logitlikf, start=0.1*ones(ncol(x_withone), 1), y=z, x=x_withone)
  pred <- x_withone %*% as.matrix(reg$estimate)
  logit = 1/(1 + exp(-pred))
  return(logit)
}

################################################################################
######################### REGRESSION MODELS ####################################
################################################################################

##### Estimators
beta_ols_f <- function(y,x){
  # Beta OLS
  xTx = t(x) %*% x
  xTy = t(x) %*% y
  return(solve(xTx) %*% xTy)
}  

beta_tsls_f <- function(y,d,z){
  # Beta TSLS
  zTz = t(z) %*% z
  zTd = t(z) %*% d
  Pi = solve(zTz) %*% zTd
  Pred = t(Pi) %*% t(z)
  return(solve(Pred %*% d) %*% Pred %*% y)
}

beta_wls_f <- function(y,x,w){
  # Beta WLS, given the weighting MATRIX w
  xTx = t(x) %*% w %*% x
  xTy = t(x) %*% w %*% y
  return(solve(xTx) %*% xTy)
}


beta_jackknife_iv_f <- function(y,x,z){
  ## Note that if covariates: X = D + 1 + X ; Z = Z + 1 + X
  
  ### Compute the h (Thanks Sunny for the efficient trick!)
  zi <- split(z, 1:nrow(z))
  zTz <- solve(t(z) %*% z)
  h <- map(zi, function(zi) as.matrix(t(zi) %*% zTz %*% zi))
  h <- unlist(h)
  
  ### Compute beta_jack
  Pi = t(x) %*% z %*% solve(t(z) %*% z)
  x_jack = (1 / (1 - h)) * (t(Pi %*% t(z)) - x * h)
  beta_jack = solve(t(x_jack) %*% x) %*% t(x_jack) %*% y 
  return(beta_jack)
}

### Standard Errors

se_ols <- function(x, res){
  n = nrow(x)
  k = ncol(x)
  xTx = t(x) %*% x
  resTres = as.numeric(t(res) %*% res) / n
  vcv = resTres * solve (xTx)
  return(sqrt(diag(vcv)))
}

se_ols_robust <- function(x, res){
  n = nrow(x)
  k = ncol(x)
  xTx = t(x) %*% x
  invxTx = solve(t(x) %*% x)
  sig = Diagonal(n, res^2)
  vcv = t(invxTx) %*% t(x) %*% sig %*% x %*% invxTx
  return(sqrt(n* diag(vcv) / (n-k)))
}

se_ols_cluster <- function(x, res, cl){
  n = nrow(x)
  k = ncol(x)
  xTx = t(x) %*% x
  
  # cluster structure
  clusters = unique(cl)
  C = length(clusters)
  meat_cluster <- lapply(1:C, function(c){
    # Find which obs fall into the cluster, select the corresponding obs
    # compute each cluster's meat
    cindex = which(cl == clusters[c]) 
    if (length(cindex)==1){
      Xc <- t(matrix(x[cindex, ]))
    }
    if (length(cindex) > 1){
      Xc <- x[cindex, ]
    }
    resc <- matrix(res[cindex, ])
    meat <- t(Xc) %*% resc %*% t(resc) %*% Xc
    return(meat)})
  Meat <- (Reduce("+", meat_cluster)) / n
  Bread = solve(xTx)
  
  vcv = (C*(n-1) / ((C-1)*(n-k))) * n * t(Bread) %*% Meat %*% Bread
  return(sqrt(diag(vcv)))
}


se_tsls_f <- function(d,z,res, x="NA"){
  # if using regressions: cbind d and z
  if (x!="NA"){
    d = cbind(d, x)
    z = cbind(z, x)
  }
  n = nrow(z)
  k = ncol(z)
  tz = t(z)
  zTz = t(z) %*% z
  zTz_inv = solve(zTz)
  Pz = z %*% zTz_inv 
  Pz = Pz %*% tz
  resTres = as.numeric(t(res) %*% res)
  vcv = resTres * solve(t(d) %*% Pz %*% d)
  return(sqrt(diag(vcv) / (n)))
}

se_tsls_robust_f <- function(d, z, res, x = "NA"){
  # if using regressions: cbind d and z
  if (x!="NA"){
    d = cbind(d, x)
    z = cbind(z, x)
  }
  n = nrow(z)
  k = ncol(z)
  d_res2 = diag(as.vector(res^2))
  meat = t(z) %*% d_res2 %*% z
  vcv = solve(t(z) %*% d) %*% meat %*% solve(t(z) %*% d)
  return(sqrt(n * diag(vcv) / (n-k)))  
  }
  

  
### Full Routine 


lm_own <- function(data="NA", dep_var, indep_var, cluster_var = NA, 
                   robust=FALSE){
  ## This function performs the OLS estimation with clusterest SE
  ## Following mostly Azeem Shaikh's notes & Fiona Burlig's notes 
  ## https://www.fionaburlig.com/teaching/are212
  ## Accomodates both dataframe and vector / matrices structures
  
  if (data != "NA"){
    # Dataframe
    sub <- data %>% dplyr::select(all_of(dep_var), all_of(indep_var))
    sub <- sub[complete.cases(sub),]
    n <- nrow(sub)
    k <- length(indep_var)
    y_data <- attrapemoi(sub, dep_var)
    x_data <- attrapemoi(sub, indep_var)
    if (!is.na(cluster_var)){
      cluster_data = attrapemoi(data, cluster_var)
    }
  }
  if (data == "NA"){
    # Matrices
    y_data = dep_var
    x_data = indep_var
    if (!is.na(cluster_var)){
      cluster_data = cluster_var
    }
    n <- length(y_data)
    k <- ncol(indep_var)
    k <- ifelse(isempty(k), 1, k)
  }
  
  ### Compute Beta OLS
  beta_hat <- beta_ols_f(y_data, x_data)
  res <- y_data - x_data %*% beta_hat
  
  ### Compute SE
  if (robust==FALSE & is.na(cluster_var)){
    se = se_ols(x_data, res)
  }
  if (robust==TRUE & is.na(cluster_var)){
    se = se_ols_robust(x_data, res)
  }  
  if (!is.na(cluster_var)){
    se = se_ols_cluster(x_data, res, cluster_data)
  }

  ### Output
  p_value <- 2*pt(abs(beta_hat/se),df=n-k,
                  lower.tail= FALSE)
  output <- list(beta_hat, se, p_value) 
  names(output) <- c("beta_hat", "se", "p_value") 
  return(output)
}


dep_var = y_data
indep_var = x_data
instru_var = z_data
treat_var = d_data


tsls_own <- function(data="NA", dep_var, indep_var = NA, treat_var, instru_var, 
                     robust=FALSE){
  ## This function performs the TSLS estimation 
  if (data != "NA"){
    print("Please write in terms of matrices!!")
    break
  }
  if (data == "NA"){
    y_data = dep_var
    # Reformat such that all regressors included in 2nd stage also in 1st
    if (!is.na(indep_var)){
      z_data = as.matrix(cbind(1, instru_var, indep_var))
      d_data = as.matrix(cbind(1, treat_var, indep_var))
    }
    if (is.na(indep_var)){
      z_data = as.matrix(cbind(1, instru_var))
      d_data = as.matrix(cbind(1, treat_var))      
    }
    n <- length(y_data)
    k <- max(ncol(z_data), 1)
  }
  
  # Compute beta, residual, and standard errors
  beta_hat_tsls <- beta_tsls_f(y_data, d_data, z_data)
  res <- y_data - d_data %*% beta_hat_tsls
  if (robust==FALSE){
    se = se_tsls_f(d_data, z_data, res, x="NA")
  }
  if (robust==TRUE){
    se = se_tsls_robust_f(d_data, z_data, res, x="NA")
  }
  
  ci <- c(beta_hat_tsls[2] - 1.96*se[2], beta_hat_tsls[2] + 1.96*se[2])
  output <- list(beta_hat_tsls, se, ci[1], ci[2]) 
  names(output) <- c("beta_hat", "se", "low95", "high95") 
  return(output)
}


jackknife_iv_own <- function(dep_var, indep_var, treat_var, instru_var, 
                             robust=FALSE){
  ### Prepare Data
  Y = dep_var
  n = dim(Y)[1]
  if (!is.na(indep_var)){
    Z = cbind(ones(n, 1), instru_var, indep_var)
    X = cbind(ones(n, 1), treat_var, indep_var)
  }
  if (is.na(indep_var)){
    Z = cbind(ones(n, 1), instru_var)
    X = cbind(ones(n, 1), treat_var)
  }
  k = dim(Z)[2]
  
  beta_jack <- beta_jackknife_iv_f(Y,X,Z)
  res_jack <- Y - X %*% beta_jack
  
  ### Compute Standard errors and CI
  if (robust==FALSE){
    se <- se_tsls_f(X, Z, res_jack)
  }
  if (robust==TRUE){
    se <- se_tsls_robust_f(X, Z, res_jack)
  }
  CI <- c(beta_jack[2] - 1.96*se[2], beta_jack[2] + 1.96*se[2])
  
  ### Output
  output <- list(beta_jack, se, CI[1], CI[2])
  names(output) <- c("beta_hat", "se", "low95", "high95") 
  return(output)
}


kappa_own <- function(B = 100, S = nrow(dep_var), data="NA", dep_var, indep_var, 
                      treat_var, instrument_var, robust=FALSE){
  if (data == "NA"){
    print("Write everything in Matrices!!")
    break
  }
  if (data != "NA"){
    # Dataframe
    sub <- data %>% dplyr::select(c(all_of(dep_var), all_of(treat_var), 
                                    all_of(indep_var), all_of(instrument_var)))
    sub <- sub[complete.cases(sub),]
    n <- nrow(sub)
    k <- length(indep_var)
    y_data <- attrapemoi(sub, dep_var)
    x_data <- attrapemoi(sub, indep_var)
    d_data <- attrapemoi(sub, treat_var)
    z_data <- attrapemoi(sub, instrument_var)
    xall_data = cbind(ones(nrow(x_data), 1), d_data, x_data)
  }
  
  ### Compute the p score using logit
  p <- logit_own(x_data, z_data)
 
  ### Compute kappa & put it in a diag matrix
  kappa = 1 - d_data*(1- z_data) / (1 - p) - (1 - d_data)*z_data / p
  diag_kappa = diag(as.vector(kappa), nrow=length(kappa), ncol=length(kappa))
  
  ## Compute beta_hat from WLS, with weight = kappa
  beta_hat <- beta_wls_f(y_data, xall_data, diag_kappa)
  

  # Bootstrap Standard Errors
  se_boot <- kappa_bootstrap(B = B, S = nrow(data), data=data, 
                             dep_var = dep_var, 
                             indep_var = col4_x_var, 
                             treat_var = treat_var, 
                             instrument_var = instrument_var)
  
  ### Output
  output <- list(beta_hat, se_boot) 
  names(output) <- c("beta_hat", "se") 
  return(output)
}


kappa_bootstrap <- function(B = 100, S = nrow(dep_var), data="NA", dep_var, indep_var, 
                      treat_var, instrument_var){
  # Bootstrap Standard Errors
  print(0)
  betas_boot = matrix(, nrow = 1, ncol = (length(indep_var) + 2))
  for (b in 1:B){
    print(b)
    sampled = sample(nrow(data), S, replace = TRUE)
    beta_boot = kappa_own_onlyb(data=data[sampled,], dep_var = dep_var,
                                indep_var = indep_var, 
                                treat_var = treat_var,
                                instrument_var = instrument_var)
    betas_boot = rbind(betas_boot, beta_boot)
  }
  betas_boot = betas_boot[-1,]
  se = c()
  for (i in 1:ncol(betas_boot)){
    se = c(se, std(betas_boot[!is.nan(betas_boot[,i]),i]))
  }
  
  ### Output
  return(se)
}

kappa_own_onlyb <- function(data="NA", dep_var, indep_var, 
                      treat_var, instrument_var){
  if (data == "NA"){
    print("Write everything in Matrices!!")
    break
  }
  if (data != "NA"){
    # Dataframe
    sub <- data %>% dplyr::select(c(all_of(dep_var), all_of(treat_var), 
                                    all_of(indep_var), all_of(instrument_var)))
    sub <- sub[complete.cases(sub),]
    n <- nrow(sub)
    k <- length(indep_var)
    y_data <- attrapemoi(sub, dep_var)
    x_data <- attrapemoi(sub, indep_var)
    d_data <- attrapemoi(sub, treat_var)
    z_data <- attrapemoi(sub, instrument_var)
    xall_data = cbind(ones(nrow(x_data), 1), d_data, x_data)
  }
  
  ### Compute the p score using logit
  p <- logit_own(x_data, z_data)
  
  ### Compute kappa & put it in a diag matrix
  kappa = 1 - d_data*(1- z_data) / (1 - p) - (1 - d_data)*z_data / p
  diag_kappa = diag(as.vector(kappa), nrow=length(kappa), ncol=length(kappa))
  
  ## Compute beta_hat from WLS, with weight = kappa
  beta_hat <- beta_wls_f(y_data, xall_data, diag_kappa)
  return(t(beta_hat))
}


################################################################################
############################ BOOTSTRAPPING SE ##################################
################################################################################

se_bootstrap_own <- function(B=100, S=nrow(indep_var), 
                             dep_var,
                             indep_var = NA,
                             treat_var = NA,
                             instru_var = NA, 
                             w = NA, # The weights (eg kappa)
                             beta_init = NA,
                             method="jackknifeIV"){
  
  valid = c("ols", "wls", "tsls", "jackknifeIV", "iv")
  if ((method %in% valid)==FALSE){
    print("The method is not valid")
    break
  }
  
  ### Bootstrap
  for (b in 1:B){
    
    ## Initiate
    print(b)
    sampled = sample(nrow(dep_var), S, replace = TRUE)
    dep_varm = as.matrix(dep_var[sampled,])
    if (!is.na(indep_var)){
      indep_varm = as.matrix(indep_var[sampled,])
    }
    if (!is.na(instru_var)){
      instru_varm = as.matrix(instru_var[sampled,])
    }
    if (!is.na(treat_var)){
      treat_varm = as.matrix(treat_var[sampled,])
    }
    if (!is.na(w)){
      wm = w[sampled,]
    }
    
    ### Compute beta
    if (method=="ols"){
      beta = beta_ols_f(dep_varm, indep_varm)
    }
    
    if (method=="tsls" | method=="iv") {
      if (!is.na(indep_var)){
        d = cbind(ones(nrow(treat_varm), 1), treat_varm, indep_varm)
        z = cbind(ones(nrow(treat_varm), 1), instru_varm, indep_varm)
      }
      if (is.na(indep_var)){
        d = cbind(ones(nrow(treat_varm), 1), treat_varm, indep_varm)
        z = cbind(ones(nrow(treat_varm), 1), instru_varm, indep_varm)
      }
      beta = beta_tsls_f(dep_varm, d, z)
    }
    
    if (method=="wls"){
      beta = beta_wls_f(dep_varm, indep_varm, wm)
    }
    
    if (method=="jackknifeIV"){
      if (!is.na(indep_var)){
        d = cbind(ones(nrow(treat_varm), 1), treat_varm, indep_varm)
        z = cbind(ones(nrow(treat_varm), 1), instru_varm, indep_varm)
      }
      if (is.na(indep_var)){
        d = cbind(ones(nrow(treat_varm), 1), treat_varm, indep_varm)
        z = cbind(ones(nrow(treat_varm), 1), instru_varm, indep_varm)
      }
      beta = beta_jackknife_iv_f(dep_varm, d, z)
    }

    beta_init = rbind(beta_init, t(beta))
  }
  
  
  beta_init = beta_init[-1,]
  se = c()
  for (i in 1:ncol(beta_init)){
    se = c(se, std_own(beta_init[,i]))
  }
  return(se)
}



################################################################################
########################## CONFIDENCE INTERVAL #################################
################################################################################

anderson_rubin_own <- function(data="NA", dep_var, indep_var, treat_var, 
                               instru_var, around=100, steps=1000){
  
  # Obtain the "target" coefficient from the tsls_own function
  # Note that here the coef of interest is in 2nd position
  # (tsls_own puts constant (1), treat_var (2) etc)
  all_coefs =  tsls_own(data="NA", dep_var, indep_var, treat_var, 
                        instru_var, robust=FALSE)
  coefs = all_coefs$beta_hat[-2]
  target = all_coefs$beta_hat[2]
  print(paste("Target is ", target, sep=" "))
  print(paste("Min considered is ", target-around, sep=""))
  print(paste("Max considered is ", target + around, sep=""))
  # Prepare the target, grid etc
  keep_index = c()
  grid = linspace(target-around, target+around, steps)
  z_data = instru_var
  d_data = treat_var
  y_data = dep_var
  x_data = indep_var
  n <- length(y_data)
  k <- 1
  
  # For each element of the grid, compute whether falls within 95% AR
  for (b in 1:length(grid)){
    print(b)
    target_art = grid[b]
    all_betas = c(target_art, coefs)
    
    # Compute residual and E(ZU)
    res = y_data - as.matrix(cbind(d_data, 1, x_data)) %*% all_betas
    e_z_res = t(z_data) %*% res
    g_hat = solve(t(z_data) %*% z_data) %*% e_z_res
    
    # Compute robust se (the inside etc)
    resmm <- diag(as.vector(res - z_data %*% g_hat)^2, ncol=length(y_data))
    inside <- t(z_data) %*% resmm %*% z_data
    VCV <- solve(t(z_data) %*% d_data) %*% inside %*% solve(t(z_data) %*% z_data)
    AR = t(g_hat) %*% solve(VCV) %*% g_hat
    test = 1*(AR <= qchisq(.95, ncol(z_data)))
    keep_index = c(keep_index, test)
  }
  
  min_ind = min(which(keep_index==1))
  max_ind = max(which(keep_index==1))
  return(c(grid[min_ind], grid[max_ind]))
}




###############################################################################
###################### MATCHING & PSCORE ######################################
###############################################################################

# note: the functions below may need to be adjusted from improvement on the 
# basic functions made along pset 2. (but are NOT used in pset 2)

distance_own <- function(x1, x2, VCV){
  ## This function computes the mahalanobis distance
  return(sqrt(t(x1-x2) %*% solve(VCV) %*% (x1-x2)))
}

matching_own <- function(data, dep_var, treat_var, indep_var, nb){
  ## This function performs k nearest neighbors
  
  ## Subset data & split between treated & control
  sub <- data %>% dplyr::select(all_of(dep_var), all_of(treat_var), all_of(indep_var), index)
  sub <- sub[complete.cases(sub),]
  subT <- sub[sub[,treat_var]==1,]
  subNT <- sub[sub[,treat_var]==0,]
  n <- nrow(sub) 
  
  ## Prep final dataset
  final = sub[1,]
  final$D2 = 0
  final$Y1 = 0
  final$Y0 = 0
  final$diff = 0
  final = final[-1,]
  
  ## extract data matrices & compute var-cov matrix for mahalanobis distance
  x_data <- attrapemoi(sub, all_of(indep_var))
  x_data1 <- attrapemoi(subT, all_of(indep_var))
  x_data0 <- attrapemoi(subNT, all_of(indep_var))
  n = length(x_data[,1])
  VCV <-  1/(n-1) * t(x_data) %*% x_data
  
  ## Compute distances between each treated and each control observation
  # Treated
  for (i in 1:nrow(subT)){
    # For each i treated obs, rbind with all untreated obs
    tokeep = rbind(subT[i,], subNT)
    tokeep$D2 = NA
    for (j in 1:nrow(subNT)){
      # Compute distance between i treated and each untreated obs
      x1 = x_data1[i,]
      x2 = x_data0[j,]
      D2 = distance_own(x1, x2, VCV)
      tokeep$D2[j+1] = D2
    }
    ## keep only nb closest
    min = sort(tokeep$D2)
    min = min[1:nb]
    tokeep <- rbind(tokeep[1,], tokeep %>% filter(D2 %in% min))
    ## Compute counterfactuals
    tokeep$Y1 = tokeep$pog20s[tokeep[,treat_var]==1]
    tokeep$Y0 = mean(tokeep$pog20s[tokeep[,treat_var]==0])
    tokeep = tokeep %>% dplyr::select(all_of(dep_var), all_of(treat_var),
                                      all_of(indep_var), index, Y1, Y0)
    tokeep = tokeep[1,]
    tokeep$diff = tokeep$Y1 - tokeep$Y0
    final <- rbind(final, tokeep)
  }
  
  ## Untreated (same as for treated)
  for (i in 1:nrow(subNT)){
    tokeep = rbind(subNT[i,], subT)
    tokeep$D2 = NA
    for (j in 1:nrow(subT)){
      x1 = x_data0[i,]
      x2 = x_data1[j,]
      D2 = distance_own(x1, x2, VCV)
      tokeep$D2[j+1] = D2
    }
    min = sort(tokeep$D2)
    min = min[1:nb]
    tokeep <- rbind(tokeep[1,], tokeep %>% filter(D2 %in% min))
    tokeep$Y1 = mean(tokeep[,dep_var][tokeep[,treat_var]==1])
    tokeep$Y0 = tokeep[,dep_var][tokeep[,treat_var]==0]
    tokeep = tokeep %>% dplyr::select(all_of(dep_var), all_of(treat_var),
                                      all_of(indep_var), index, Y1, Y0)
    tokeep = tokeep[1,]
    tokeep$diff = tokeep$Y1 - tokeep$Y0
    final <- rbind(final, tokeep)
  }
  return(final)
}

pscorematching_own <- function(data, dep_var, treat_var, indep_var, nb){
  ## This function performs pscore matching and outputs a dataset with the original data + counterfactual
  data$constant = 1
  ## Subset data appropriately
  sub <- data %>% dplyr::select(all_of(dep_var), all_of(treat_var), constant, all_of(indep_var), index)
  sub <- sub[complete.cases(sub),]
  n <- nrow(sub) 
  
  ## Compute the p score using logit function (allowed as per the assignment instructions)
  form = paste(treat_var, paste(indep_var, collapse=" + "), sep=" ~ ")
  mylogit = glm(as.character(form), sub, family="binomial")
  coef = mylogit$coefficients
  x_data = attrapemoi(sub, c("constant", all_of(indep_var)))
  sub$pscore = x_data %*% coef
  
  ## Prepare matching & final dataset
  subT <- sub[sub[,treat_var]==1,]
  subNT <- sub[sub[,treat_var]==0,]
  
  final = sub[1,]
  final$D2 = 0
  final$Y1 = 0
  final$Y0 = 0
  final$diff = 0
  final = final[-1,]
  
  ## For each treated & untreated observation, find nb observation from other group with closest p, compute counterfactual
  ## Treated
  for (i in 1:nrow(subT)){
    tokeep = rbind(subT[i,], subNT)
    # Find the distance between pscore of treated observation & all untreated obs pscore
    tokeep$D2 = abs(tokeep$pscore[1] - tokeep$pscore)
    tokeep$D2[1] = NA
    # keep nb closest observations & compute counterfactual (average Y)
    min = sort(tokeep$D2)[1:nb]
    tokeep$Y1 = tokeep$pog20s[1]
    tokeep$Y0 = mean(tokeep$pog20s[tokeep$D2 %in% min])
    # Compute difference
    tokeep$diff = tokeep$Y1 - tokeep$Y0
    final <- rbind(final, tokeep[1,])
  }
  ## Untreated (similar as treated)
  for (i in 1:nrow(subNT)){
    tokeep = rbind(subNT[i,], subT)
    tokeep$D2 = abs(tokeep$pscore[1] - tokeep$pscore)
    tokeep$D2[1] = NA
    min = sort(tokeep$D2)[1:nb]
    tokeep$Y0 = tokeep$pog20s[1]
    tokeep$Y1 = mean(tokeep$pog20s[tokeep$D2 %in% min])
    tokeep$diff = tokeep$Y1 - tokeep$Y0
    final <- rbind(final, tokeep[1,])
  }
  return(final)
}

estimate_matching <- function(data, dep_var, treat_var, indep_var, 
                              clustervar, k, est, pscore=FALSE){
  ## This function is the routine to compute the \beta with matching
  ## it accomodates ATT, ATE, ATU, and matching on covariates or pscore
  
  ## Subset the data as appropriate
  datatouse <- data %>% dplyr::select(all_of(dep_var), all_of(treat_var), all_of(indep_var), all_of(clustervar))
  datatouse <- datatouse[complete.cases(datatouse),]
  datatouse$index <- 1:nrow(datatouse)
  
  ## Obtain the matched datasets
  if (pscore==FALSE){
    matched_data <- matching_own(datatouse, dep_var, treat_var, indep_var, k)
  }
  
  if (pscore==TRUE){
    matched_data <- pscorematching_own(datatouse, dep_var, treat_var, indep_var, k)
  }
  
  ## Prepare
  matched_data <- merge(matched_data, datatouse %>% dplyr::select(index, all_of(clustervar)), by="index")
  subT = matched_data %>% filter(pog1349==1)
  subNT = matched_data %>% filter(pog1349==0)
  
  ## Compute parameters of interest
  if (est=="ATT"){
    return(mean(subT$Y1) - mean(subT$Y0))
  }
  if (est=="ATE"){
    return(mean(matched_data$Y1) - mean(matched_data$Y0))
  }
  if (est=="ATU"){
    return(mean(subNT$Y1) - mean(subNT$Y0))
  }  
}





################################################################################
#################### Standard Errors (old) #####################################
################################################################################




# se_bootstrap_pscorematching <- function(B, S, data, dep_var, treat_var, 
#                                         indep_var, clustervar,
#                                         k, est, pscore){
#   ## This function computes the se using bootstrap
#   ## it accomodates ATE, ATT, ATU, matching on pscore or on covariates
#   hat = c()
#   for (b in 1:B){
#     print(b)
#     sub = data[sample(nrow(data), S),]
#     hat = append(hat, estimate_matching(sub, dep_var, treat_var, 
#                    indep_var, clustervar, k, est, pscore))
#   }
#   se = std_own(hat)
#   return(se)
# }






## ------------------------------------------------
## Home made functions 
require(tidyverse)
require(data.table)
require(readstata13)
require(pracma)
require(geodist)
require(tidyselect)
require(knitr)


## ----
## extract relevant data columns
attrapemoi <- function(data, varnames){ 
  matrixObject <- data %>%
    dplyr::select(.dots = varnames) %>%
    as.matrix()
}

## ----
## Pretty tables
make_tables <- function(est, se, N, 
                        names, dep_var, 
                        title,
                        format="latex", r=3){
  output = data.frame(Coef = names,
                      dep_var = paste(round(est, r), paste("(",round(se, r), ")", sep=""), sep=" "))
  return(xtable(output, type=format, caption=title))
}




## ----
lm_own <- function(data="NA", dep_var, indep_var, robust=FALSE){
  ## This function performs the OLS estimation with clusterest SE
  ## Following mostly Azeem Shaikh's notes & Fiona Burlig's notes https://www.fionaburlig.com/teaching/are212
  ## Accomodates both dataframe and vector / matrices structures
  
  if (data != "NA"){
    # Dataframe
    sub <- data %>% dplyr::select(all_of(dep_var), all_of(indep_var))
    sub <- sub[complete.cases(sub),]
    n <- nrow(sub)
    k <- length(indep_var)
    y_data <- attrapemoi(sub, dep_var)
    x_data <- attrapemoi(sub, indep_var)
  }
  if (data == "NA"){
    # Matrices
    y_data = dep_var
    x_data = indep_var
    n <- length(y_data)
    k <- length(indep_var)
  }
  
  ## Compute beta_hat
  beta_hat <- solve(t(x_data) %*% x_data) %*% t(x_data) %*% y_data 
  res <- y_data - x_data %*% beta_hat
  ## Compute se
  if (robust==FALSE){
    VCV <- (1/(n-k))* as.numeric((t(res) %*% res)) * solve(t(x_data) %*% x_data)
  }
  if (robust==TRUE){
    print("Sorry I don't handle robust SE yet, WIP")
  }
  se <- sqrt(diag(VCV))
  p_value <- 2*pt(abs(beta_hat/se),df=n-k,
                  lower.tail= FALSE)
  output <- list(beta_hat, se, p_value) 
  names(output) <- c("beta_hat", "se", "p_value") 
  return(output)
}


## ----
lm_cluster_own <- function(data, dep_var, indep_var, clustervar = NA){
  ## This function performs the OLS estimation with clusterest SE
  ## Following mostly Fiona Burlig's notes https://www.fionaburlig.com/teaching/are212
  
  ## Subset data & keep only non missing
  sub <- data %>% dplyr::select(dep_var, indep_var, clustervar)
  sub <- sub[complete.cases(sub),]
  n <- nrow(sub)
  k <- length(indep_var)
  
  ## as vector / matrices
  y_data <- attrapemoi(sub, dep_var)
  x_data <- attrapemoi(sub, indep_var)
  
  ## Compute beta_hat & the residual
  beta_hat <- solve(t(x_data) %*% x_data) %*% t(x_data) %*% y_data 
  res <- y_data - x_data %*% beta_hat
  
  ### CLUSTERED
  ## Figure out the cluster structure
  clusterdata = attrapemoi(sub, clustervar)
  clusters <- unique(clusterdata)
  C <- length(clusters)
  ## For each cluster
  clusterInside <- lapply(1:C, function(c) {
    # Find which observations fall into the cluster
    cindex <- which(clusterdata == clusters[c]) 
    # Select the corresponding X
    if (length(cindex)==1){
      Xc <- t(matrix(x_data[cindex, ]))
    }
    if (length(cindex) > 1){
      Xc <- x_data[cindex, ]
    }
    # Compute each cluster's "meat" by selecting residuals for the cluster
    resc <- matrix(res[cindex, ])
    middle <- t(Xc) %*% resc %*% t(resc) %*% Xc
    return(middle)})
  
  ### Put things back together
  Inside <- (Reduce("+", clusterInside)) / n
  Outside <- solve(t(x_data) %*% x_data)
  # Compute se matrix
  se_all <- (C/(C-1))*((n-1)/(n-k)) * n * t(Outside) %*% Inside %*% Outside
  # se as per usual
  se <- sqrt(diag(se_all))
  p_value <- 2*pt(abs(beta_hat/se),df=n-k,
                  lower.tail= FALSE)
  output <- list(beta_hat, se, p_value) 
  names(output) <- c("betahat", "se", "p_value") 
  return(output)
}




## -----------------------------------------------

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




estimate_matching <- function(data, dep_var, treat_var, indep_var, clustervar, k, est, pscore=FALSE){
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


se_own <- function(vector){
  ## This function computes the standard deviation of a vector (for ex a vector of coef from bootstrapping)
  n = length(vector)
  mean = mean(vector)
  return(sqrt((1/n)*sum((vector - mean)^2)))
}


se_bootstrap <- function(B, S, data, dep_var, treat_var, indep_var, clustervar, k, est, pscore){
  ## This function computes the se using bootstrap
  ## it accomodates ATE, ATT, ATU, matching on pscore or on covariates
  hat = c()
  for (b in 1:B){
    print(b)
    sub = data[sample(nrow(data), S),]
    hat = append(hat, estimate_matching(sub, dep_var, treat_var, indep_var, clustervar, k, est, pscore))
  }
  se = se_own(hat)
  return(se)
}




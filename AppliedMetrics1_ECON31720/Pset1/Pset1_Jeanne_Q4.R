## ---------------------------
##
## Script name: Applied Econometrics pset 1 - Problem 4
##
## Purpose of script: 
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

## load up the packages we will need:  (uncomment as required)

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
################################# Question 4 ##################################
###############################################################################
## -----
## Specify the DGP
Yf <- function(X, U){return(sin(2*X) + 2*exp(-16*X^2) + U)}
Xf <- function(n, minX = -2, maxX = 2){runif(n, min = minX, max = maxX)}
Uf <- function(n, meanX=0, sdX=1){rnorm(n, meanX, sdX)}


## -----
monte_carlo_f<- function(M, N, method="localkernel", h=0.2, breaks=10, xmin=-2, xmax=2){
  ## This function performs the MC simulations for problem 4
  # M = number of loops in MC
  # N = number of observations simulated each time
  
  ## ----
  ## Check that the inputs are correct
  valid_methods = c("localkernel", "locallinreg", "sieve", "nne", "sieveBernstein", "sievelinearsplines")
  if ((method %in% valid_methods)==FALSE){
    print("Invalid method")
    break
  }
  if (breaks > N){
    print("ratio break/N too large")
    break
  }
  
  ## ----
  ## Initialize MC
  mux_hat = matrix(, nrow = 1, ncol = breaks)
  
  ## As many time as specified by the number of MC simulations M
  for (m in 1:M){
    print(m)
    
    # Simulate a N-dimensional sample
    X = Xf(N, xmin, xmax)
    U = Uf(N, 0, 0.3)
    Y = Yf(X, U)
    
    # discretize x
    xd = linspace(xmin, xmax, breaks)
    
    ## Perform deterministic computation on this sample & store results for each method
    
    ## localkernel: h is the bandwidth determining how "close" is close to "x"
    if (method=="localkernel"){
      # Uniform kernel : compute the average Y for X close to x, for each x in xd
      muX = sapply(xd, function(x){
        mean(Y[X <= x+h & X >= x-h])
      })
      mux_hat = rbind(mux_hat, muX)
    }
    
    ## locallinreg: h is the bandwidth determining how "close" is close to "x"
    if (method=="locallinreg"){
      # Compute the local OLS on observations with X close to x, for each x in xd
      muX = sapply(xd, function(x){
        Ysub = Y[X <= x+h & X >= x-h]
        Xsub = X[X <= x+h & X >= x-h]
        vones = ones(length(Xsub))[1,]
        coef = lm_own(data="NA", dep_var=Ysub, indep_var=cbind(vones, Xsub))$beta_hat
        return(mean(coef[1] + coef[2]*Xsub))
      })
      mux_hat = rbind(mux_hat, muX)
    }
    
    ## sieve: h is the number of polyn degrees to consider        
    if (method=="sieve"){
      # Prepare the dataframe (format handled by OLS function)
      vones = ones(N, 1)
      dataframe <- data.frame(yvar = Y,
                              constant = vones,
                              xvar = X)
      for (k in 2:(h)){
        # Take x at the power
        name = paste("xvar", k, sep="_")
        dataframe[,name] = dataframe$xvar^k
      }
      all = names(dataframe)[2:length(names(dataframe))]
      # OLS
      coef = lm_own(data=dataframe, dep_var="yvar", indep_var=all)$beta_hat
      dataxd <- data.frame(ones(breaks, 1),
                           xd)
      for (k in 2:(h)){
        name = paste("xd", k, sep="_")
        dataxd[,name] = dataxd$xd^k
      }
      # Prediction
      pred_xn = as.matrix(dataxd) %*% coef
      mux_hat = rbind(mux_hat, pred_xn[,1])
    }
    
    ## nne: h is the number of neighbors to keep      
    if (method=="nne"){
      # Compute the difference between x and X, keeep the h closest observations, take the average Y
      muX = sapply(xd, function(x){
        absdiff = abs(x - X)
        minindex = c()
        for (j in 1:h){
          minindex <- append(minindex, which(absdiff == Rfast::nth(absdiff, j, descending = F))[[1]])
        }
        mean(Y[minindex])
      })
      mux_hat = rbind(mux_hat, muX)      
    }
    
    ## sieveBernstein:        
    if (method=="sieveBernstein"){
      # Normalize X to U[0,1]
      Z = (X - xmin) / (xmax - xmin)
      dataframe <- data.frame(yvar = Y)
      # Prepare factorial
      for (k in 0:h){
        name = paste("bk", k, sep="_")
        dataframe[,name] <- (factorial(h) / (factorial(k)*factorial(h-k))) * Z^k * (1-Z)^(h-k)
      }
      all = names(dataframe)[2:length(names(dataframe))]
      # Run appropriate regression, obtains the coef
      coef = lm_own(data=dataframe, dep_var="yvar", indep_var=all)$beta_hat
      
      # predict on xd
      xd_z = (xd - xmin) / (xmax - xmin)
      dataxd <- data.frame(xd_z = xd_z)
      for (k in 0:h){
        name = paste("xd_z", k, sep="_")
        dataxd[,name] = (factorial(h) / (factorial(k)*factorial(h-k))) * xd_z^k * (1-xd_z)^(h-k)
      }
      dataxd <- dataxd[,2:(h+2)]
      pred_xn = as.matrix(dataxd) %*% coef
      mux_hat = rbind(mux_hat, pred_xn[,1])
    }
    
    ## sievelinearsplines:        
    if (method=="sievelinearsplines"){
      # Prepare the dataframe
      vones = ones(N, 1)
      dataframe <- data.frame(yvar = Y,
                              constant = vones,
                              xvar = X)
      # Quantiles / Knots
      threshold = c(xmin)
      for (k in 1:h){
        threshold = append(threshold, xd[breaks*(k/(h+1))])
      }
      for (k in 3:(h+2)){
        name = paste("xvar", k-1, sep="_")
        dataframe[,name] = ifelse(dataframe$xvar >= threshold[k-1], dataframe$xvar - threshold[k-1], 0)
      }
      all = names(dataframe)[2:length(names(dataframe))]
      # Run the appropriate regression
      coef = lm_own(data=dataframe, dep_var="yvar", indep_var=all)$beta_hat
      # Predict
      dataxd <- data.frame(constant = ones(breaks, 1),
                           xd = xd)
      for (k in 3:(h+2)){
        name = paste("xd", k-1, sep="_")
        dataxd[,name] = ifelse(dataxd$xd >= threshold[k-1], dataxd$xd - threshold[k-1], 0)
      }
      pred_xn = as.matrix(dataxd) %*% coef
      mux_hat = rbind(mux_hat, pred_xn[,1])
    }
  }
  
  ## Put everything together, compute the mean (for each x in xd) & the sd
  mux_hat = mux_hat[-1,]
  mean = colMeans(mux_hat)
  std = sqrt(M/(M-1)*(colMeans(mux_hat^2) - colMeans(mux_hat)^2))
  return(rbind(mean, std))
}    

print_monte_carlo_f <- function(M, N, method="localkernel", hsmall = 2, hlarge = 6,breaks=100, xmin=-2, xmax=2){
  ## This function takes the output of the function above and make nice plots (with ggplot bc that's the best)
  xd = linspace(xmin, xmax, breaks)
  Y = Yf(xd, 0)
  
  # MC with small bandwidth
  monte_carlo <- monte_carlo_f(M, N, method=method, h=hsmall, breaks=breaks, xmin=xmin, xmax=xmax)
  mean = monte_carlo[1,]
  sd = monte_carlo[2,]
  sdabove = mean+sd
  sdbelow = mean-sd
  sd_left = sd

  
  plot = ggplot() +
    theme(legend.position="bottom", legend.box = "horizontal") +
    geom_line(aes(xd, Y, col="True Y")) +
    geom_point(aes(xd, mean, col="Mean Projected Y"), size=0.2) +
    geom_line(aes(xd, sdabove, col="Mean +/- 1 sd"), size=0.1) +
    geom_line(aes(xd, sdbelow), col="blue", size=0.1) +
    ggtitle(paste("Method:", method, "; h =", hsmall, "Avg sd deviation =", round(mean(sd_left[!is.nan(sd_left)]),3), sep=" ")) +
    labs(x="x", y = "E(Y|X=x)") +
    scale_colour_manual("", 
                        breaks = c("True Y", "Mean Projected Y", "Mean +/- 1 sd"),
                        values = c("black", "red", "blue")) 
  print(plot) 
  name = paste(method, "small", "pb4.png", sep="_")
  ggsave(filename = name, plot, dpi = 300, device='png')  
  
  
  # MC with large bandwidth
  monte_carlo <- monte_carlo_f(M, N, method=method, h=hlarge, breaks=breaks, xmin=xmin, xmax=xmax)
  mean = monte_carlo[1,]
  sd = monte_carlo[2,]
  sdabove = mean+sd
  sdbelow = mean-sd
  sd_right = sd
  
  plot = ggplot() +
    theme(legend.position="bottom", legend.box = "horizontal") +
    geom_line(aes(xd, Y, col="True Y")) +
    geom_point(aes(xd, mean, col="Mean Projected Y"), size=0.2) +
    geom_line(aes(xd, sdabove, col="Mean +/- 1 sd"), size=0.1) +
    geom_line(aes(xd, sdbelow), col="blue", size=0.1) +
    ggtitle(paste("Method:", method, "; h =", hlarge, "Avg sd deviation =", round(mean(sd_right[!is.nan(sd_left)]),3), sep=" ")) +
    labs(x="x", y = "E(Y|X=x)") +
    scale_colour_manual("", 
                        breaks = c("True Y", "Mean Projected Y", "Mean +/- 1 sd"),
                        values = c("black", "red", "blue")) 
  print(plot) 
  name = paste(method, "large", "pb4.png", sep="_")
  ggsave(filename = name, plot, dpi = 300, device='png')  
  sd_right = sd
  
}


## Finally simulateeeeeee
M = 500
N = 500
print_monte_carlo_f(M, N, method="localkernel", hsmall = 0.05, hlarge = 0.3,breaks=100, xmin=-2, xmax=2)
print_monte_carlo_f(M, N, method="locallinreg", hsmall = 0.1, hlarge = 1,breaks=80, xmin=-2, xmax=2)
print_monte_carlo_f(M, N, method="sieve", hsmall = 15, hlarge = 3,breaks=100, xmin=-2, xmax=2)
print_monte_carlo_f(M, N, method="nne", hsmall = 3, hlarge = 100,breaks=100, xmin=-2, xmax=2)
print_monte_carlo_f(M, N, method="sieveBernstein", hsmall = 3, hlarge = 20,breaks=100, xmin=-2, xmax=2)
print_monte_carlo_f(M, N, method="sievelinearsplines", hsmall = 2, hlarge = 20,breaks=100, xmin=-2, xmax=2)






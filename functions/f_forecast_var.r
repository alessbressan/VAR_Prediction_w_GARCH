f_forecast_var <- function(y, level) {
  ### Compute the VaR forecast of a GARCH(1,1) model with Normal errors at the desired risk level
  #  INPUTS
  #   y     : [vector] (T x 1) of observations (log-returns)
  #   level : [scalar] risk level (e.g. 0.95 for a VaR at the 95# risk level)
  #  OUTPUTS
  #   VaR   : [scalar] VaR forecast 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  #   theta : [vector] GARCH parameters
  #  NOTE
  #   o the estimation is done by maximum likelihood 
  
  # Fit a GARCH(1,1) model with Normal errors
  # Starting values and bounds
  theta0 <- c(0.1 * var(y), 0.1, 0.8)
  LB     <- c(1e-10, 1e-10, 1e-10) ## lower bound
  
  # Stationarity condition
  A      <- cbind(c(1, 0, 0, 0),c(0, 1, 0, -1), c(0, 0, 1, -1))
  b      <- matrix(c(LB,-1), nrow = 4, ncol = 1)

  # Run the optimization
  ## optimize parameters for the nll
  theta <- constrOptim(theta = theta0,
                       f = f_nll,
                       grad = NULL,
                       #method = "L-BFGS-B",
                       ui = A,
                       ci = b,
                       y = y)$par
  # Recompute the conditional variance
  sig2 <- f_ht(theta, y)
  
  #Take sigma2 T to make prediction
  sig <- sig2[length(sig2)]
  sig <- sqrt(sig)
  
  # Compute the next-day ahead VaR for the Normal model
  ## ALESSIO ADDED THIS found from problem statement 
  VaR <- qnorm(1-level)*sig
  
  out <- list(VaR_Forecast = VaR, 
              ConditionalVariances = sig2, 
              GARCH_param = theta)
  
  return(out)
}

f_nll <- function(theta, y) {
  ### Function which computes the negative log likelihood value 
  ### of a GARCH model with Normal errors
  #  INPUTS
  #   theta  : [vector] of estimated GARCH parameters
  #   y      : [vector] (T x 1) of observations
  #  OUTPUTS
  #   nll    : [scalar] negative log likelihood value
  
  T <- length(y)
  
  # Compute the conditional variance of a GARCH(1,1) model
  sig2 <- f_ht(theta, y) #sigma prime from the formula
  
  # Consider the T values
  sig2 <- sig2[1:T]
  
  # Compute the loglikelihood - ALESSIO ADDED 
  error <- y - mean(y)
  nll = - T/2 * log(2*pi) - sum(log(sig2)) - 1/2*sum(error^2/sig2)
  
  # Output the negative value
  return(-nll)
}

f_ht <- function(theta, y)  {
  ### Function which computes the vector of conditional variance
  #  INPUTS
  #   x0 : [vector] (3 x 1) (w, a, b)
  #   y     : [vector] (T x 1) log-returns
  #  OUTPUTS 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  
  # Extract the parameters
  a0 <- theta[1]  #omega - Ioan added
  a1 <- theta[2]  #alpha - Ioan added
  b1 <- theta[3]  #beta - Ioan added
  
  T <- length(y)+1
  
  # Initialize the conditional variances
  sig2 <- rep(NA, T)
  
  # Start with unconditional variances
  sig2[1] <- a0 / (1 - a1 - b1)
  
  #Prediction error - Ioan added
  e = y - mean(y)
  e2 = e ^ 2
  
  # Compute conditional variance at each step - Ioan added
  for(t in 2:T)
  {
    sig2[t] = a0 + a1 * e2[t-1] + b1 * sig2[t-1] 
  }
  
  return(sig2)
}

f_backtestVAR <- function(sample, T, level){
  ###functions###
  
  f_forecast_var <- function(y, level) {
    ### Compute the VaR forecast of a GARCH(1,1) model with Normal errors at the desired risk level
    #  INPUTS
    #   y     : [vector] (T x 1) of observations (log-returns)
    #   level : [scalar] risk level (e.g. 0.95 for a VaR at the 95# risk level)
    #  OUTPUTS
    #   VaR   : [scalar] VaR forecast 
    #   sig2  : [vector] (T+1 x 1) conditional variances
    #   theta : [vector] GARCH parameters
    #  NOTE
    #   o the estimation is done by maximum likelihood 
    
    # Fit a GARCH(1,1) model with Normal errors
    # Starting values and bounds
    theta0 <- c(0.1 * var(y), 0.1, 0.8)
    LB     <- c(1e-10, 1e-10, 1e-10) ## lower bound
    
    # Stationarity condition
    A      <- cbind(c(1, 0, 0, 0),c(0, 1, 0, -1), c(0, 0, 1, -1))
    b      <- matrix(c(LB,-1), nrow = 4, ncol = 1)
    
    # Run the optimization
    ## optimize parameters for the nll
    theta <- constrOptim(theta = theta0,
                         f = f_nll,
                         grad = NULL,
                         #method = "L-BFGS-B",
                         ui = A,
                         ci = b,
                         y = y)$par
    # Recompute the conditional variance
    sig2 <- f_ht(theta, y)
    
    #Take sigma2 T to make prediction
    sig <- sig2[length(sig2)]
    sig <- sqrt(sig)
    
    # Compute the next-day ahead VaR for the Normal model
    ## ALESSIO ADDED THIS found from problem statement 
    VaR <- qnorm(1-level)*sig
    
    out <- list(VaR_Forecast = VaR, 
                ConditionalVariances = sig2, 
                GARCH_param = theta)
    
    return(out)
  }
  
  f_nll <- function(theta, y) {
    ### Function which computes the negative log likelihood value 
    ### of a GARCH model with Normal errors
    #  INPUTS
    #   theta  : [vector] of estimated GARCH parameters
    #   y      : [vector] (T x 1) of observations
    #  OUTPUTS
    #   nll    : [scalar] negative log likelihood value
    
    T <- length(y)
    
    # Compute the conditional variance of a GARCH(1,1) model
    sig2 <- f_ht(theta, y) #sigma prime from the formula
    
    # Consider the T values
    sig2 <- sig2[1:T]
    
    # Compute the loglikelihood - ALESSIO ADDED 
    error <- y - mean(y)
    nll = - T/2 * log(2*pi) - sum(log(sig2)) - 1/2*sum(error^2/sig2)
    
    # Output the negative value
    return(-nll)
  }
  
  f_ht <- function(theta, y)  {
    ### Function which computes the vector of conditional variance
    #  INPUTS
    #   x0 : [vector] (3 x 1) (w, a, b)
    #   y     : [vector] (T x 1) log-returns
    #  OUTPUTS 
    #   sig2  : [vector] (T+1 x 1) conditional variances
    
    # Extract the parameters
    a0 <- theta[1]  #omega - Ioan added
    a1 <- theta[2]  #alpha - Ioan added
    b1 <- theta[3]  #beta - Ioan added
    
    T <- length(y)+1
    
    # Initialize the conditional variances
    sig2 <- rep(NA, T)
    
    # Start with unconditional variances
    sig2[1] <- a0 / (1 - a1 - b1)
    
    #Prediction error - Ioan added
    e = y - mean(y)
    e2 = e ^ 2
    
    # Compute conditional variance at each step - Ioan added
    for(t in 2:T)
    {
      sig2[t] = a0 + a1 * e2[t-1] + b1 * sig2[t-1] 
    }
    
    return(sig2)
  }
  
  ### End ###
  
  library(doParallel)
  
  max <- T
  hist_var <- rep(NA, T)
  in_sample <- rep(NA,T)
  
  print("starting function")
  
  
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  
  
  hist_var <- foreach(i = 1:max, .combine = 'c') %dopar% {
    #cat("I am creating", i, "th window\n")
    in_sample <- sample[i:(T+i-1)]
    #print(in_sample)
    
    #VaR forecast
    return(f_forecast_var(in_sample, level = 0.95)$VaR_Forecast)
    
  }
  
  stopCluster(cl)
  
  return(hist_var)
}

f_risky <- function(a,b){
  if(abs(a) < abs(b)){
    cat("\n",a," is less risky than ",b)
  }else{
    cat("\n",b," is less risky than ",a)
  }
}

sim_mydata_outcomes <- function(n = 25000,
                       mu = c(3, 27, 15), 
                       sd = c(2, 5, 4), 
                       r = c(0.3, 0.15, 0.55), 
                       varnames = c("g", "x", "y"),
                       seed = 12345
                       ){ 
  
  ########################
  ## Simulate the data 
  ########################
  set.seed(seed)
  
  ########################
  ## Simulate the data 
  ########################
  mydata <- faux::rnorm_multi(n = 25000, 
                              mu = mu,
                              sd = sd,
                              r = r, 
                              varnames = varnames,
                              empirical = FALSE)
  
  ########################
  ## produce transformed 
  ## versions of the exposure data
  ########################
  ## UNIFORM
  a = mydata$x
  a = (a - mean(a) )/ sd(a)
  mydata$x_uni = pnorm( a, mean = 0, sd = 1)
  
  ## GAMMA
  a = a + abs( min(a) )
  mydata$x_gamma = pgamma( a , shape = 3)
  
  ## EXPONENTIAL
  mydata$x_exp = pexp(mydata$x_uni, rate = 4)
  
  ## BETA
  a = a / max(a)
  mydata$x_beta = pbeta(a, shape1 = 4, shape2 = 3)
  
  ########################
  ## produce transformed 
  ## versions of the outcome data
  ########################
  
  
  ## UNIFORM
  a = mydata$y
  a = (a - mean(a) )/ sd(a)
  mydata$y_uni = pnorm( a, mean = 0, sd = 1)
  
  ## GAMMA
  a = a + abs( min(a) )
  mydata$y_gamma = pgamma( a , shape = 3)
  
  ## EXPONENTIAL
  mydata$y_exp = pexp(mydata$y_uni, rate = 4)
  
  ## BETA
  a = a / max(a)
  mydata$y_beta = pbeta(a, shape1 = 4, shape2 = 3)
  
  ########################
  ## return the sims
  ########################
  return(mydata)
}




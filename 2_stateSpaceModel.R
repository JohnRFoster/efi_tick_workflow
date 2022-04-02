library(nimble)

model.code <- nimbleCode({
  
  tau.proc ~ dexp(1)  # process error prior
  
  # regression coefficient priors
  for (i in 1:2) {
    beta[i] ~ dnorm(0, tau = 1 / 3 ^ 2) # standard deviation of 3
  }
  
  for(i in 1:n.months){
    alpha.month[i] ~ dnorm(0, tau = 1)
  }
  
  for (s in 1:n.sites) {
    
    tau.obs[s] ~ dexp(1)   # observation error prior
    
    # first latent state at each site prior
    x[s, first.drag[s]] ~ dnorm(10, 2)
    
    # process model
    for (t in first.drag[s]:(last.drag - 1)) {
      
      # error in variables for temp and rh
      # observed ~ N(true, observed error)
      temp.o[s, t] ~ dnorm(temp[s, t], var = temp.var[s, t])
      rh.o[s, t] ~ dnorm(rh[s, t], var = rh.var[s, t])
      
      # priors for true weather, which has been centered and scaled
      temp[s, t] ~ dnorm(0, tau = 1) 
      rh[s, t] ~ dnorm(0, tau = 1)
      
      # population growth a function of temperature and relative humidity
      # with month specific intercepts
      lambda[s, t] <- alpha.month[month[t]] + 
        beta[1] * temp[s, t] +
        beta[2] * rh[s, t] 
        
      
      ex[s, t] <- x[s, t] * lambda[s, t]        # expected number of ticks 
      m[s, t] ~ dnorm(ex[s, t], tau = tau.proc) # process error
      x[s, t+1] <- max(0, m[s, t])              # can't be negative! 
    }  
    
    # data model
    for (t in first.drag[s]:last.drag) {
      y[s, t] ~ dnorm(x[s, t], tau = tau.obs[s])
    }  
    
  }
}) 



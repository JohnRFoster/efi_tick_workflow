library(nimble)

model.code <- nimbleCode({
  
  for (s in 1:n.sites) {
    
    # priors
    for (i in 1:3) {
      # regression coefficients 
      beta[s, i] ~ dnorm(0, tau = 1 / 3 ^ 2) # standard deviation of 3
    }
    tau.proc[s] ~ dinvgamma(0.1, 0.1) # process error
    tau.obs[s] ~ dinvgamma(0.1, 0.1)  # observation error
    
    # first latent state at each site
    x[s, first.drag[s]] ~ dexp(1)
    
    # process model
    for (t in first.drag[s]:(last.drag - 1)) {
      
      # error in variables for temp and rh
      # observed ~ N(true, error)
      temp.o[s, t] ~ dnorm(temp[s, t], tau = tau.temp[s, t])
      rh.o[s, t] ~ dnorm(rh[s, t], tau = tau.rh[s, t])
      
      # priors for true weather, which has been centered and scaled
      temp[s, t] ~ dnorm(0, tau = 1)
      rh[s, t] ~ dnorm(0, tau = 1)
      
      # population growth a function of temperature and relative humidity
      lambda[s, t] <- beta[s, 1] + 
        beta[s, 2] * temp[s, t] +
        beta[s, 3] * rh[s, t]
      
      ex[s, t] <- x[s, t] * lambda[s, t]
      x[s, t] ~ dnorm(ex[s, t], tau = tau.proc[s])
    }  
    
    # data model
    for (t in first.drag[s]:last.drag) {
      y[s, t] ~ dnorm(x[s, t], tau = tau.obs[s])
    }  
    
  }
}) 

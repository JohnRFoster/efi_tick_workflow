# fitting the mcmc 
# check parameters for convergence
# extract samples

run_mcmc <- function(model.code, constants, data, inits, thin, n.iter, n.burnin, n.chains){
  library(coda)
  
  samples <- nimbleMCMC(
    code = model.code,
    constants = constants,
    data = data,
    inits = inits(),
    thin = thin,
    niter = n.iter,
    nburnin = n.burnin,
    nchains = n.chains,
    samplesAsCodaMCMC = TRUE
  )
  
  # remove state, temp, and rh samples to check convergence on parameters
  state.cols <- grep("x[", colnames(samples[[1]]), fixed = TRUE)
  rh.cols <- grep("rh[", colnames(samples[[1]]), fixed = TRUE)
  temp.cols <- grep("temp[", colnames(samples[[1]]), fixed = TRUE)
  check.mcmc <- samples[,-c(state.cols, rh.cols, temp.cols), drop = TRUE]
  
  g.diag <- gelman.diag(check.mcmc, multivariate = FALSE)$psrf
  print(g.diag)
  
  out <- list(
    params = check.mcmc,
    states = samples[,grep("x[", colnames(samples[[1]]), fixed = TRUE), drop = TRUE],
    rh = samples[,grep("rh[", colnames(samples[[1]]), fixed = TRUE), drop = TRUE],
    temp = samples[,grep("temp[", colnames(samples[[1]]), fixed = TRUE), drop = TRUE]
  )
  
  return(out)
  
}




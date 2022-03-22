# main script for running the forecast

# step 0 - forecast identifiers 
# step 1 - get data
# step 2 - fit model (which includes forecast period)
# step 3 - create metadata
# step 4 - submit

library(MMWRweek)
library(lubridate)
library(tidyverse)
library(nimble)

# ========================================================== #
#           0. forecast identifiers
# ========================================================== #

team_name <- "tbd"
check.neon <- FALSE # do we want to check neon for new (or updated) weather observations?

team_list <- list(
  list(
    individualName = list(givenName = "John",
                          surName = "Foster"),
    organizationName = "Boston University",
    electronicMailAddress = "fosterj@bu.edu"
  )
)

run.date <- today()
run.month <- month(run.date)

# the forecast is for the mmwr weeks that are during the current month, but for 2021
sundays <- seq.Date(ymd("2021-01-03"), by = 7, length.out = 52) # all Sundays in 2021

# forecast time (the sunday date stamp that maps to mmwrWeek)
fx.time <- sundays[month(sundays) == run.month]
forecast.date <- fx.time[1]

# forecast mmwrWeeks
forecast.weeks <- MMWRweek(fx.time)$MMWRweek
n.fx.weeks <- length(forecast.weeks)

# ========================================================== #
#           1. get data
# ========================================================== #

# can get all data we need for the forecast by running 1_getData.R
source("1_getData.R")

# ========================================================== #
#           2. fit model (which includes forecast period)
# ========================================================== #

# NIMBLE model code is in 2_stateSpaceModel.R
source("2_stateSpaceModel.R")

# need to give an initial condition for model parameters
# dimensions of parameters in inits must match dimensions in the model!
n.beta <- constants$n.sites*3
inits <- function(){
  list(
    beta = matrix(rnorm(n.beta, 0, 1), constants$n.sites, 3),
    tau.proc ~ runif(constants$n.sites, 1, 3), 
    tau.obs ~ runif(constants$n.sites, 1, 3)  
  )
}

myModel <- nimbleModel(
  code = model.code, # model.code defined in 2_stateSpaceModel.R
  constants = constants,
  data = data,
  inits = inits())


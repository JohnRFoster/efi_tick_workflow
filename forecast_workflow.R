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

team_name <- "state_space"
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
# we can provide inits for any stochastic node, data or parameters
# we have NAs in y, temp, and rh, so let's specify those
# dimensions of inits must match dimensions in the model!


# temperature and relative humidity have been centered and scaled, their inits are straightforward
temp.init <- data$temp.o
temp.o.na <- which(is.na(temp.init))
temp.init[temp.o.na] <- rnorm(length(temp.o.na), 0, 1)

rh.init <- data$rh.o
rh.o.na <- which(is.na(rh.init))
rh.init[rh.o.na] <- rnorm(length(rh.o.na), 0, 1)

# for the observed nodes, we want inits where they are NA, and NA where there are data
temp.o.init <- data$temp.o
temp.o.init[temp.o.na] <- rnorm(length(temp.o.na), 0, 1)
temp.o.init[-temp.o.na] <- NA

rh.o.init <- data$rh.o
rh.o.init[rh.o.na] <- rnorm(length(rh.o.na), 0, 1)
rh.o.init[-rh.o.na] <- NA

y.init <- data$y
y.na <- which(is.na(y.init))
y.init[y.na] <- rnorm(length(y.na), mean(y.init, na.rm = T), sd(y.init, na.rm = T))
y.init[y.init < 0] <- 0 # density is zero-bound

# these have the same dimensions and are closely related
# best guess of the latent density is the data
x.init <- y.init 
y.init[-y.na] <- NA

n.sites <- constants$n.sites
n.drags <- constants$last.drag
n.beta <- constants$n.sites*3
inits <- function(){
  list(
    beta = matrix(rnorm(n.beta, 0, 1), n.sites, 3),
    lambda = matrix(rnorm(n.sites*n.drags, 0, 1), n.sites, n.drags),
    tau.proc = runif(constants$n.sites, 1, 3), 
    tau.obs = runif(constants$n.sites, 1, 3),
    y = jitter(y.init),
    x = jitter(x.init),
    ex = jitter(x.init[,-1]),
    temp = jitter(temp.init),
    rh = jitter(rh.init),
    temp.o = jitter(temp.o.init),
    rh.o = jitter(rh.o.init)
  )
}

# model.code defined in 2_stateSpaceModel.R
# constants and data from 1_getData.R
model <- nimbleModel(
  code = model.code, 
  constants = constants,
  data = data,
  inits = inits())

model$initializeInfo()


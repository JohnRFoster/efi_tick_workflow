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

theme <- "ticks"
team_name <- "state_space"
check.neon <- FALSE # do we want to check neon for new (or updated) weather observations?

# mcmc configuration
Nmc <- 500                   # the number of mcmc iterations we want to save
n.iter <- 50000               # the total number of mcmc iterations to run
n.burnin <- 2000              # burn-in iterations to discard
n.chains <- 3                 # number of chains
thin <- round(n.iter / Nmc)   # thinning interval to get back to Nmc

team.list <- list(
  list(
    individualName = list(givenName = "John",
                          surName = "Foster"),
    organizationName = "Boston University",
    electronicMailAddress = "fosterj@bu.edu"
  )
)

run.date <- today()
# run.date <- ymd("2022-03-30")
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
n.beta <- 3
inits <- function(){
  list(
    beta = rnorm(2, 0, 0.1),
    alpha = rnorm(n.sites, 0, 1),
    alpha.month = rnorm(constants$n.months, 0, 1),
    lambda = matrix(runif(n.sites*n.drags, 0, 2), n.sites, n.drags),
    tau.proc = runif(1, 1, 3), 
    tau.obs = runif(n.sites, 1, 3),
    x = jitter(x.init),
    ex = jitter(x.init[,-1]),
    m = jitter(x.init[,-1]),
    y = jitter(y.init),
    temp = jitter(temp.init),
    rh = jitter(rh.init),
    temp.o = jitter(temp.o.init),
    rh.o = jitter(rh.o.init)
  )
}

# NIMBLE model code is in 2_stateSpaceModel.R
source("2_stateSpaceModel.R")


# constants and data from 1_getData.R
source("3_fitModel.R")
out <- run_mcmc(
  model.code = model.code,
  constants = constants,
  data = data,
  inits = inits,
  thin = thin,
  n.iter = n.iter,
  n.burnin = n.burnin,
  n.chains = n.chains
)

mcmc.file <- paste0(as.character(forecast.date), ".RData")
save(out, file = file.path("mcmc", mcmc.file))


load(file = file.path("mcmc", mcmc.file))

states <- as.matrix(out$states)

draws <- sample(nrow(states), Nmc, replace = TRUE)


states <- states[draws,]

latent.ticks <- tibble()
for(j in 1:n.sites){
  for(i in 1:ncol(data$y)){
    col <- paste0("x[", j, ", ", i, "]")
    samps <- states %>% 
      as_tibble() %>% 
      pull(col)
    
    site.week <- tibble(
      siteID = site[j],
      time = time[i],
      amblyomma_americanum = samps,
      ensemble = 1:length(samps),
      obs = data$y[j, i]
    )  
    
    latent.ticks <- bind_rows(latent.ticks, site.week)
    
  }  
}

tick.stats <- latent.ticks %>% 
  filter(!is.na(amblyomma_americanum)) %>% 
  # mutate(amblyomma_americanum = amblyomma_americanum + 1,
  #        obs = obs + 1) %>% 
  group_by(siteID, time) %>% 
  summarise(mean = median(amblyomma_americanum),
            low90 = quantile(amblyomma_americanum, 0.1),
            high90 = quantile(amblyomma_americanum, 0.9),
            obs = mean(obs))

tick.stats %>% 
  ggplot() +
  aes(x = time, y = mean) +
  geom_ribbon(aes(x = time, ymin = low90, ymax = high90), fill = "lightblue") +
  geom_line(aes(y = mean)) +
  geom_point(aes(y = obs)) +
  # scale_y_log10() +
  coord_cartesian(xlim = c(ymd("2018-01-01"), ymd("2021-04-07"))) +
  facet_wrap(~ siteID,
             scales = "free_y") +
  theme_bw()

# extract parameters to check convergence and ecological interpretation
# extract states for plotting
# extract forecast period for submission
# make file for submission
# make metadata file
# submit


# ========================================================== #
#           2. forecast file
# ========================================================== #

# extract just the forecast period
forecast <- latent.ticks %>%
  select(-obs) %>% 
  filter(time %in% fx.time) %>% 
  mutate(forecast = 1,
         data_assimilation = 0)

fx.file.name <- paste0(theme, "-", as.character(forecast.date), "-", team_name, ".csv.gz")
forecast.file <- file.path("forecast", fx.file.name)
write_csv(forecast, forecast.file)
neon4cast::forecast_output_validator(forecast.file) # check


# ========================================================== #
#           2. metadata file
# ========================================================== #

md.file.name <- paste0(theme, "-", as.character(forecast.date), "-", team_name, ".xml")
metedata.file <- file.path("forecast", md.file.name)

model.metadata <- list(
  forecast = list(
    model_description = list(
      forecast_model_id = team_name,
        name = "A simple state-space model", 
        type =  "process",
        repository =  "JohnRFoster/efi_tick_workflow"
    ),
    initial_conditions = list(
      status = "present", 
      complexity = n.sites
    ),
    drivers = list(
      status = "propagates", 
      complexity = 2, 
      propagation = list( 
        type = "ensemble", 
        size = Nmc 
      )
    ),
    parameters = list(
      status = "propagates", 
      complexity = 22,
      propagation = list(
        type = "ensemble", 
        size = Nmc)
    ),
    random_effects = list(
      status = "absent"
    ),
    process_error = list(
      status = "propagates", 
      complexity = 1, 
      propagation = list(
        type = "ensemble", 
        size = Nmc 
      )
    ),
    obs_error = list(
      status = "present", 
      complexity = n.sites 
    )
  )
)

metadata.file <- neon4cast::generate_metadata(forecast.file, team.list, model.metadata)

# ========================================================== #
#           2. submit forecast and metadata
# ========================================================== #

neon4cast::submit(forecast_file = forecast.file,
                  metadata = metadata.file,
                  ask = FALSE)

neon4cast::check_submission(forecast.file)

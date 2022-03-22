## get data for challenge
## 1. tick data
## 2. historical weather (for calibration)
## 2. noaa weather forecasts (for forecasts)

library(tidyverse)
library(lubridate)
library(neon4cast)
library(neonstore)

# need this to access data from s3 bucket
Sys.setenv("AWS_DEFAULT_REGION" = "data",
           "AWS_S3_ENDPOINT" = "ecoforecast.org")

# initialize the two main objects we need to build a NIMBLE model
data <- list()
constants <- list()

# ========================================================== #
#           1. Tick data
# ========================================================== #

# the tick data is relatively easy to get:
data.tick <- read_csv("https://data.ecoforecast.org/targets/ticks/ticks-targets.csv.gz", guess_max = 1e6)
targets <- data.tick %>%
  arrange(siteID, time) %>% 
  mutate(year = year(time),
         year.fac = as.numeric(as.factor(year)),
         site.fac = as.numeric(as.factor(siteID)))

# get the target file into something usable for NIMBLE
# make it wide, where each row is a site and each column is a week
nimble.data <- targets %>% 
  arrange(time) %>% 
  select(time, siteID, `Amblyomma americanum`) %>% 
  pivot_wider(names_from = time, 
              values_from = `Amblyomma americanum`) %>% 
  arrange(siteID)

# what we actually use in nimble is a matrix with only numeric and NA values
y <- nimble.data %>% 
  select(-siteID) %>% 
  as.matrix()

# need to pad the end of the matrix with NAs to represent the forecast period
# n.fx.weeks set in forecast_workflow.R
fx.na <- matrix(NA, nrow(y), n.fx.weeks)
data$y <- cbind(y, fx.na) # put into our data list

# some useful vectors that we may need later
site <- nimble.data$siteID
time <- ymd(colnames(y))
year <- year(time)
week <- MMWRweek(time)$MMWRweek

# we need to index year, as each year is being modeled as its own cohort, place in constants
year.index <- year %>% as.factor() %>% as.numeric()
constants$year <- year.index

# also need to index sites in the model code, place in constants
site.index <- site %>% as.factor() %>% as.numeric()
constants$site <- site.index
constants$n.sites <- length(site.index)

# there is no reason to simulate what happens before the first observation event
# so let's get a vector that has the week of the first tick drag at each site
first.drag <- apply(y, 1, function(x) min(which(!is.na(x))))
last.drag <- ncol(data$y) # the total number of drags

constants$first.drag <- first.drag
constants$last.drag <- last.drag

# to forecast each week, we need to calibrate to each week of each year
# create a data frame that has a week for each site
first.year <- min(targets$year)
last.year <- max(targets$year)

tick.site.weeks <- expand_grid(
  siteID = site,
  year = first.year:last.year,
  mmwrWeek = 10:44 # the challenge includes these weeks at all sites
) %>% 
  mutate(time = MMWRweek2Date(year, mmwrWeek))

# ========================================================== #
#           2. NEON weather
# ========================================================== #

# shouldn't have to download from NEON again, but can if we want to 
# check.neon is defined in forecast_workflow.R
if(check.neon){
  source("0_neonWeather.R")
} else {
  air.temp.weekly <- read_csv("drivers/airTemp.csv") %>% 
    mutate(time = MMWRweek2Date(year, mmwrWeek)) 
  rel.hum.weekly <- read_csv("drivers/rh.csv") %>% 
    mutate(time = MMWRweek2Date(year, mmwrWeek))
}

air.temp.past <- left_join(tick.site.weeks, air.temp.weekly, by = c("siteID", "time", "year", "mmwrWeek"))
rh.past <- left_join(tick.site.weeks, rel.hum.weekly, by = c("siteID", "time", "year", "mmwrWeek"))

# ========================================================== #
#           3. Future NOAA
# ========================================================== #

for (i in 1:length(site)) {
  neon4cast::get_noaa_forecast_s3(
    ".",
    model = "NOAAGEFS_1hr",
    site = site[i],
    date = forecast.date,
    cycle = "00"
  )
}

dir.create("drivers", showWarnings = FALSE)
noaa.future <- neon4cast::stack_noaa(dir = "drivers",
                                     model = "NOAAGEFS_1hr",
                                     forecast_date = forecast.date)

# will need to create mmwrWeek and time column multiple times; we lose them when we call summarise()
create_mmwr <- function(df){
  df %>% 
    mutate(time = ymd(ceiling_date(time, unit = "day")),
           year = year(time),
           mmwrWeek = MMWRweek::MMWRweek(time)$MMWRweek,
           time = MMWRweek2Date(year, mmwrWeek))
}

noaa.future.weekly <- noaa.future %>% 
  select(time, siteID, ensemble, air_temperature, relative_humidity) %>% 
  create_mmwr() %>% 
  group_by(siteID, year, time)

air.temp.future <- noaa.future.weekly %>%
  mutate(air_temperature = air_temperature - 273.15) %>% # convert to C
  summarise(mean = mean(air_temperature),
            variance = var(air_temperature)) %>% 
  create_mmwr()

rh.future <- noaa.future.weekly %>%
  summarise(mean = mean(relative_humidity),
            variance = var(relative_humidity)) %>% 
  create_mmwr()

# combine past and future temp and rh and
# scale and center mean, scale variance appropriately 
scale_center <- function(df){
  df %>% 
    arrange(siteID, time) %>% 
    group_by(siteID) %>% 
    mutate(sd = sd(mean, na.rm = TRUE),
           mu = mean(mean, na.rm = TRUE),
           mean = (mean - mu)/sd,
           variance = variance * (1/sd)^2) %>% 
    select(-sd, -mu) %>% 
    ungroup()
}
temp <- bind_rows(air.temp.past, air.temp.future) %>% 
  scale_center()
rh <- bind_rows(rh.past, rh.future) %>% 
  scale_center()

# before we can put the temp and rh matrices into data,
# we need to make sure that all weeks in the tick data are 
# in the weather data (in case any weather weeks are missing)
# make a tibble that has every week for every site
all.site.weeks <- expand_grid(
  siteID = site,
  time = time
)

# need all sundays in calibration and forecast period
# fx.time set in forecast_workflow.R
all.time <- c(time, fx.time)

#' @param df is our combined weather data, either temp or rh
#' @param stat is the statistic to grab, either "mean" or "variance"
make_matrix <- function(df, stat){
  df.mmwr <- all.site.weeks %>% 
    left_join(df, by = c("siteID", "time")) %>% 
    create_mmwr() %>% 
    filter(time %in% all.time) %>% 
    select(siteID, time, all_of(stat)) %>% 
    arrange(time) %>% 
    pivot_wider(names_from = time,
                values_from = .data[[stat]]) %>% 
    arrange(siteID) %>% 
    select(-siteID) %>% 
    as.matrix
}

temp.mean <- make_matrix(temp, "mean")
temp.var <- make_matrix(temp, "variance")
rh.mean <- make_matrix(rh, "mean")
rh.var <- make_matrix(rh, "variance")

# place observed weather and associated error into data
data$temp.o <- temp.mean
data$temp.var <- temp.var
data$rh.o <- rh.mean
data$rh.var <- rh.var

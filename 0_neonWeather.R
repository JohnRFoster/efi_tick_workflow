# this is a stand alone script to download and process the observed
# weather from the neon sites used in the tick challenge

# because we are forecasting 2021, all weather data is observed
# we can preprocess the data into a csv so that we don't have to 
# download it every month, then reveal the "new" observations 
# for each month's forecast. this is particularly advanteguous because
# the initial download will take a while


library(tidyverse)
library(lubridate)
library(neonstore)

# first we need to set where you want neon data to be stored
# or you can set directly in .Renviron
# Sys.setenv("NEONSTORE_HOME" = "/path/to/my/neonstore")
# Sys.setenv("NEONSTORE_DB" = "/path/to/my/neonstore")

# the products we want are:
neon.products <- c(
  "DP1.00003.001", # air temperature
  "DP1.00098.001"  # relative humidity
)

# this download will take a while...
neon_download(product = neon.products,
              site = tick.sites)

air.temp <- neon_read(table = "TAAT_30min-basic", site = tick.sites)
rel.hum <- neon_read(table = "RH_30min-basic", site = tick.sites) 

# need the same thing from both data sets:
# mean weekly value (mean of mean column)
# variance from the weather observation (maximum variance recorded in a week)
# grabbing the max variance so that if we include error in variables we don't overfit
# temp and rh data have different column names so need to specify

#' @param df the data frame to subset and summarise
#' @param qf.col the name of the quality flag column
#' @param mean.col the name of the mean value column
#' @param variance.col the name of the variance column

weekly_summary <- function(df, qf.col, mean.col, variance.col){
  df %>% 
    filter(.data[[qf.col]] == 0,
           siteID %in% tick.sites) %>% 
    mutate(time = ymd(ceiling_date(endDateTime, unit = "day")),
           year = year(time),
           mmwrWeek = MMWRweek::MMWRweek(time)$MMWRweek) %>% 
    group_by(siteID, year, mmwrWeek) %>% 
    summarise(mean = mean(.data[[mean.col]]),
              variance = max(.data[[variance.col]])) %>% 
    arrange(siteID, year, mmwrWeek)
}

air.temp.weekly <- weekly_summary(air.temp, "finalQF", "tempTripleMean", "tempTripleVariance")
rel.hum.weekly <- weekly_summary(rel.hum, "RHFinalQF", "RHMean", "RHVariance") %>% 
  mutate(mean = mean*0.01, # scale rh to percent
         variance = variance*(0.01^2)) # scale variance by the square of the scaling factor

write_csv(air.temp.weekly, path = "drivers/airTemp.csv")
write_csv(rel.hum.weekly, path = "drivers/rh.csv")


# plots to see coverage / sanity check

# plot_weather <- function(df){
#   df %>% 
#     mutate(time = MMWRweek::MMWRweek2Date(year, mmwrWeek)) %>% 
#     ggplot() +
#     aes(x = time, y = mean) +
#     geom_point() +
#     facet_wrap(~ siteID) +
#     theme_bw()
# }
# 
# plot_weather(air.temp.weekly) + labs(title = "Weekly Mean Air Temperature")
# plot_weather(rel.hum.weekly) + labs(title = "Weekly Mean RH")




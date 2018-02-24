# Just a training R forecast script...

# Clear all output from the console 
cat("\014") 

# Removing all objects from the current workspace.
rm(list=ls())

# Providing the name of the demo data file
name_of_your_file = "IMF DATA.xlsx"

# Load the relevant libraries
library(readxl)
library(ggplot2)
library(forecast)

# Print out the names of spreadsheets
excel_sheets(name_of_your_file)

# Read the second sheet of the uploaded data file:
mydata <- read_excel(name_of_your_file, sheet = 2)

# Display the structure of the data
#View(mydata)
str(mydata)

# Creating a time series object containing the GDP_current_prices
my_ts <- ts(mydata[, 2], start=1991, end=2017)
my_ts

#Plotting the GDP_current_prices
autoplot(my_ts)

# they dont look like white noise, they seem to be
# correlated and they are not normally distributed
# The Ljung-Box test is below 0.05


### DIDN'T USE IT ###

# Creating a subset function (look here:
# https://stats.stackexchange.com/questions/6239/subsetting-r-time-series-vectors, 
# by the way subset.ts not working
# https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/subset.ts)

#subset_a_ts <- function(data, start, end) {
#  ks <- which(time(data) >= start & time(data) < end)
#  vec <- data[ks]
#  ts(vec, start=start(data) + c(0, ks[1] - 1), frequency=frequency(data))
#}

# Isolating the dates from the time series
#dates_of_ts = time(my_ts)

# Create a training set using subset_a_ts containing data up to 3 years before the last date 
# of the time series (be careful, we are using minus 2 as the end date, not minus 3)
#train <- subset_a_ts(my_ts, start = dates_of_ts[1], end = dates_of_ts[length(dates_of_ts)-2])

###

# Checking the residuals
my_ts %>% naive(h=3) %>% checkresiduals()
my_ts %>% meanf(h=3) %>% checkresiduals()
my_ts %>% forecast(h=3) %>% checkresiduals()

# Creating a function that automatically computes time series cross-validation errors
my_forecast <- function(my_timeseries, for_model, n_period_for){
e <- matrix(NA_real_, nrow = length(my_ts), ncol = n_period_for)
for (h in 1:n_period_for)
  e[, h] <- tsCV(my_ts, forecastfunction = for_model, h = h)
mse <- colMeans(e^2, na.rm = TRUE)
print(mse)
}

my_forecast(my_ts, naive, 3)
my_forecast(my_ts, meanf, 3)

# If model=NULL,the function forecast.ts makes 
# forecasts using ets models (if the data are 
# non-seasonal or the seasonal period is 12 or less) 
# or stlf (if the seasonal period is 13 or more).
my_forecast(my_ts, forecast, 3)

# Plot the forecasts for different the above models
my_ts %>% naive() %>% forecast(h=3) %>% autoplot()
my_ts %>% meanf() %>% forecast(h=3) %>% autoplot()
my_ts %>% ets() %>% forecast(h=3) %>% autoplot()
 
# Getting the forecast
my_ts %>% naive() %>% forecast(h=3)
my_ts %>% meanf() %>% forecast(h=3)
my_ts %>% ets() %>% forecast(h=3)

# Clear all output from the console 
#cat("\014")


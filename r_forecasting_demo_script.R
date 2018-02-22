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

# Read the fist sheet of the uploaded data that contains the data: 
mydata <- read_excel(name_of_your_file, sheet = 2)

# Display the structure of the data
#View(mydata)
str(mydata)

# Creating a time series object containing the GDP_current_prices
my_ts <- ts(mydata[, 2], start=1991, end=2017)
my_ts

#Plotting the GDP_current_prices
autoplot(my_ts)

# Using the naive forecast to get naive results for the next 3 years..
summary(naive(my_ts, h=3))

# Checking the residuals..hmm...
checkresiduals(naive(my_ts, h=3))

# they dont look like white noise, they seem to be
# correlated and they are not normally distributed
# The Ljung-Box test is below 0.05

# more simply using the pipe function:

my_ts %>% naive(h=3) %>% checkresiduals() 

# Create the training data as train
train <- window(my_ts, end = 2011)


# Compute naive forecasts and save to naive_fc
naive_fc <- naive(train, h = 5)

# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = 5)

# Use accuracy() to compute measures of forecast accuracy
accuracy(naive_fc, my_ts)
accuracy(mean_fc, my_ts)


# Clear all output from the console 
cat("\014") 
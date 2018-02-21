# Just random R code..


# Clear all output from the console 
cat("\014") 

# Removing all objects from the current workspace.
rm(list=ls())

# Providing the name of 
name_of_your_file = "IMF DATA.xlsx"

# Load the readxl package
library(readxl)
library(ggplot2)

# Print out the names of  spreadsheets
excel_sheets(name_of_your_file)

# Read the fist sheet of the uploaded data: 
mydata <- read_excel(name_of_your_file, sheet = 2)

# Display the structure of the data
#View(mydata)
str(mydata)

autoplot(mydata)
gglagplot(mydata[ ,1])
ggAcf(mydata[ ,1])


# Checking the correlation matrix for our data.
#mydata$Year <- NULL
#View(round(cor(mydata, use="complete.obs", method="kendall"),2))

my_ts <- ts(mydata[ ,-1], start=1991, end=2017)

autoplot(my_ts, facets = FALSE)

fit <- tslm(GDP_current_prices ~ ., data=my_ts)
summary(fit)

f <- forecast(fit, h=3,level=c(80,95))
f

plot(f, ylab="GDP_current_prices",
     xlab="t")
lines(fitted(fit),col="blue")
summary(fit)


par(mfrow=c(2,2))
res <- ts(resid(fit),s=1990)
plot.ts(res,ylab="res (GDP)")
abline(0,0)
Acf(res)


# Clear all output from the console 
cat("\014") 
#LOAD PACKAGES
packages =  c("readxl", "ggplot2", "dplyr", "tidyr", "data.table", 'corrplot', 'gridExtra', 'forecast', 'tseries', 'TSA', 'tibble', 'TTR', 'xts', 'assertthat')

my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}

purrr::walk(packages, my.install, character.only = TRUE, warn.conflicts = FALSE)

sample_num = 5

install.packages("ggseasonplot")
install.packages("imputeTS")
install.packages("reshape2")
install.packages("ggfortify")
install.packages("knitr")

library(knitr)
library(ggfortify)
library(reshape2)
library(imputeTS)
library(forecast)


#LOAD DATA
data  <- read_excel("data.xlsx", sheet = "Month")

head(data)

# Convert data into time series dataset
attach(data)

data<-ts(data,c(2017,1),c(2022,10),12)

## Take a look at the class of the dataset 
str(data)

# Check for missing values
sum(is.na(data))
# Check the frequency of the time series data
frequency(data)
# Check the cycle of the time series
cycle(data)

summary(data)

# Plot the raw data using the base plot function
plot(data,xlab="Date", ylab = "Call count as (%) of 2017",main="Monthly Call Count from 2017 and 2022")


data  <- read_excel("data.xlsx", sheet = "month1")

data <- na.approx(data)
data = as.data.frame(data)


# Fill NAs with the last non-missing value
myts_filled <- na.fill(data, "last")

# Fill NAs using linear interpolation
myts_interpolated <- na_interpolation(data)
myts_interpolated

# Create a date column
myts_interpolated$Date <- as.Date(paste(myts_interpolated$Year, "01", "01", sep = "-"))

# Reshape data from wide to long format
myts_long <- reshape2::melt(myts_interpolated, id.vars = "Date", variable.name = "Month", value.name = "Count")
myts_long = myts_long[-(1:6), , drop = FALSE]
# Plot the data as a line
ggplot(myts_long, aes(x = Date, y = Count, color = Month)) +
  geom_line() +
  labs(x = "Date", y = "Count", title = "Count by Month") +
  scale_color_discrete(name = "Month", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_bw()


myvector = c(99, 99, 95, 82, 86, 82, 63, 62, 46, 77, 67, 91, 79, 58, 57, 42, 63, 82, 60, 57, 70, 53, 73, 48,
          84, 35, 75, 75, 81, 71, 65, 58, 57, 50, 55, 0, 75, 54, 56, 21, 16, 14, 28, 32, 35, 29, 25, 11,
          28, 23, 27, 41, 57, 90, 129, 96, 50, 22, 29, 21, 45, 22, 38, 30, 23, 21, 17, 50, 82, 8)


# from Jan 2009 to Dec 2014 as a time series object
month_data <- ts(myvector, start=c(2017, 1), end=c(2022, 10), frequency=12)

# plot series
plot(month_data)

# Seasonal decomposition
fit <- stl(month_data, s.window="period")
plot(fit)

# additional plots
monthplot(month_data)
seasonplot(month_data)


#Loading Initial Data

data  <- read_excel("data.xlsx", sheet = "transform_data")

# Data with null values
myvector <- data$`Count of Incoming_Call_Time`
# from Jan 2017 to Dec 2022 as a time series object
weeks_nul <- ts(myvector, start=c(2017, 1), end=c(2022, 43), frequency=52)
weeks_null <- na.approx(weeks_nul)

summary(weeks_null)

# Calculate the basic statistics for the data
mean <- round(mean(weeks_null),2)
sd <- round(sd(weeks_null),2)
min <- round(min(weeks_null),2)
max <- round(max(weeks_null),2)
range <- round((max - min),2)
median <- round(median(weeks_null),2)
quantiles <- round(quantile(weeks_null, probs = c(0.25, 0.50, 0.75)),2)
iqr_data <- round(quantiles[3] - quantiles[1],2)
var_data <- round(var(weeks_null), 2)
cv_data <- round((sd / mean(weeks_null)),2)
mad_data <- round(mean(abs(weeks_null - mean(weeks_null))),2) # Compute mean absolute deviation
mad_median_data <- round(median(abs(weeks_null - median(weeks_null))),2) # Compute median absolute deviation

# Create a data frame with the results
results_df <- data.frame(
  `Statistics_of_Data` = c("Mean","Minimum","Maximum","Range", "IQR","Q1","Q2","Q3", "Variance", "Standard Deviation", "Coefficient of Variation", "Mean Absolute Deviation", "Median Absolute Deviation"),
  `Value` = c(mean,min, max,range, iqr_data, quantiles[1],quantiles[2],quantiles[3],var_data, sd, cv_data, mad_data, mad_median_data)
)

# Print the table using the knitr package
kable(results_df, row.names = FALSE)
library(imputeTS)

plot(weeks_null, main="Weekly call data with missing values")
statsNA(weeks_null)


#Loading the transformed values with increase of 7
data  <- read_excel("data.xlsx", sheet = "model")
data
# remove first column
data <- data[, -1]

# create box plots for each column
par(mfrow = c(1, ncol(data)))
for (i in 1:ncol(data)) {
  boxplot(data[, i], main = colnames(data)[i])
}
# Extract the values from the time series
ts_data <- as.vector(data)
ts_data <- ts_data$`Transform data`[1:302]

# Identify and replace outliers using Z-score method
z_scores <- (ts_data - mean(ts_data)) / sd(ts_data)
threshold <- 3  # adjust threshold as needed
outliers <- abs(z_scores) > threshold
ts_data[outliers] <- round(mean(ts_data))  # replace outliers with NA or another value

# Plot original and cleaned time series data
par(mfrow = c(2, 1))
plot(ts_data, main = "Cleaned Time Series Data")
points(ts_data[outliers], col = "red", pch = 20)
plot(ts(rnorm(100, mean = 10, sd = 2)), main = "Original Time Series Data")
par(mfrow = c(1, 1))

boxplot(ts_data, main = "Box Plot of Cleaned Data", ylab = "Values")

ts_data
# from Jan 2009 to Dec 2014 as a time series object
weeks <- ts(ts_data, start=c(2017, 1), end=c(2022, 42), frequency=52)
weeks
library(zoo)
weeks <- na.approx(weeks)

# Extract the values from the time series
myvalues <- as.vector(weeks)

# Create a bar plot
barplot(myvalues, main = "Weekly Time Series Data", xlab = "Week", ylab = "Value")

# plot series
plot(weeks, main="Weekly call data with no outliers and NULL values")


# display seasonal plots
ggseasonplot(weeks, year.labels = TRUE)

# display trend plots
ggsubseriesplot(weeks)


# Seasonal decomposition
fit <- stl(weeks, s.window="period")
plot(fit)

# additional plots
monthplot(weeks)
seasonplot(weeks)


# Decompose the time series by additive

decompose_data_add <- decompose(weeks, type = "additive")
plot(decompose_data_add, col = "red")
# Plot the decomposed time series using autoplot
autoplot(decompose_data_add)


# Decompose the time series by multiplicative
decompose_data_mul <- decompose(weeks, type = "multiplicative")
plot(decompose_data_mul, col = "blue")
# Plot the decomposed time series using autoplot
autoplot(decompose_data_mul)

# Set up the plot grid with 2 rows and 1 column
par(mfrow = c(2, 1))

# Plot the original time series with a legend
plot(weeks, type = "l", col = "blue", main = "Original Time Series", ylab = "Values")
legend("topleft", legend = c("Original"), col = "blue", lty = 1)

# Decompose the time series into its components
decomp <- decompose(weeks)

# Calculate the rolling average and standard deviation of the seasonal component
k <- 7
roll_mean <- rollapply(decomp$seasonal, width = k, FUN = mean, align = "center", fill = NA)
roll_sd <- rollapply(decomp$seasonal, width = k, FUN = sd, align = "center", fill = NA)

# Plot the seasonal component and the rolling mean and standard deviation with a legend
plot(decomp$seasonal, type = "l", col = "blue", main = "Seasonal Component", ylab = "Values")
lines(roll_mean, col = "red")
lines(roll_sd, col = "green")
legend("topleft", legend = c("Seasonal", "Rolling Mean", "Rolling SD"), col = c("blue", "red", "green"), lty = 1)

par(mfrow = c(1, 1))

# Forecast from Naive method
adjust<-seasadj(decompose_data_mul)
plot(naive(adjust))
# Forecast from Seasonal Naive method
adjust<-seasadj(decompose_data_mul)
plot(snaive(adjust))

#Autocorrelation with acf and pacf
tsdisplay(weeks)

# Test stationary of the time series (ADF)
adf.test(weeks)

#Differencing is a common solution used to stationarize the variable. We will perform differencing using R function diff.

Autocorrelation <- diff(weeks, differences=1)
plot(Autocorrelation)

library(forecast)

tsdisplay(Autocorrelation)

weeks
# Divide data between training and test time series objects
b1<-window(weeks, end=c(2021, 52))
b2<-window(weeks, start=c(2022,1))
h=length(b2)

# Forecast from 2022 onwards using mean, naive, drift and seasonal methods
par(mfrow = c(1, 1))
plot(weeks, type = 'n')
lines(b1)
lines(b2, col = "red")
abline(v = end(b1) + 1, lty = 2, lwd = 2)

# Mean (based on the overall mean value)
f1 <- meanf(b1, h = h)
lines(f1$mean, lwd = 2, col = "yellow")

# Naive (based on the last value)
f2 <- rwf(b1, h = h)
lines(f2$mean, lwd = 2, col = "green")

# Drift (based on 1st and last value)
f3 <- rwf(b1, drift = TRUE, h = h)
lines(f3$mean, lwd = 2, col = "orange")

# Seasonal naive forecast
f4 <- snaive(b1, h = h)
lines(f4$mean, lwd = 2, col = "blue")
legend("bottomleft", legend = c("Data", "Trend", "Mean", "Naive", "Drift", "S Naive"),
       col = c("black", "red", "yellow", "green", "orange", "blue"), lwd = 2)

kable(accuracy(f1, b2)) # display accuracy for mean method
kable(accuracy(f2, b2)) # display accuracy for naive method
kable(accuracy(f3, b2)) # display accuracy for drift method
kable(accuracy(f4, b2)) # display accuracy for seasonal method

# COMMENT :naive method is the one that minimizes RMSE on test set with the value of 6.187545. Our intuition on the seasonal structure of the data has been confirmed by accuracy measures. naive method is then the best method to use for our prediction task

#Analyse residuals for seasonal series
res <- residuals(f2)
plot(res)
hist(res, breaks = "FD", col = "steelblue") # The bell shape of the histogram suggests that residuals are normal and not correlated.
acf(res, na.action = na.omit)

f4 <- snaive(weeks, h = 8)
f3 <- rwf(weeks, drift = TRUE, h = 8)
f2 <- rwf(weeks, h = 8)
f1 <- meanf(weeks, h = 8)
print(f1)
print(f2)
print(f3)
print(f4)


# 7.Exponential Models
# SES - models level
fit <- HoltWinters(weeks, beta=FALSE, gamma=FALSE)
simple_exponential<-forecast(fit, h=8)
simple_exponential

#Extrapolation Models (Holt Linear)
# double exponential - models level and trend
fit <- HoltWinters(weeks, gamma=FALSE)
double_exponential<-forecast(fit, h=8)
double_exponential

#Holt Winters
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(weeks)
triple_exponential<-forecast(fit, h=8)
triple_exponential

accuracy(simple_exponential)
accuracy(double_exponential)
accuracy(triple_exponential)

# Plot all three models in a single graph
par(mfrow = c(1, 1), bg = "#F2F2F2")
# Plot all three models in the same figure
plot(simple_exponential, lwd = 1, col = "blue", main = "Exponential Models")
lines(double_exponential$mean, lwd = 2, col = "red")
lines(triple_exponential$mean, lwd = 2, col = "orange")

# Add legend
legend("bottomleft", legend = c("SES", "Double Exp.", "Triple Exp."), col = c("blue", "red", "green"), lty = 1)
par(mfrow = c(1, 1), bg="#FFFFFF")

# 8.Regression Model
fit<-tslm(weeks~trend)
f<-forecast(fit, h=8)
plot(f)
acf(residuals(f))

# fit the model based on the seasonality
fit2<-tslm(weeks~trend+season)
f2<-forecast(fit2, h=8)
plot(f2)
acf(residuals(f2))

# Print accuracy measures for the trend-only model
accuracy(f1)
# Print accuracy measures for the trend and seasonality model
accuracy(f2)

# 9.ARIMAs including an examination of autocorrelation.
#TEST STATIONARITY OF THE TIME SERIES
library(tseries)

#1. Test stationary of the time series (ADF)
adf.test(weeks)
#As a rule of thumb, where the p-value is less than 5%, we have a strong evidence against the null hypothesis, so we reject the null hypothesis. As per the test results above, the p-value is 0.016 which is <0.05 therefore we reject the null in favour of the alternative hypothesis that the time series is stationary.

#2. Test stationary of the time series (Autocorrelation)
library(ggplot2)
autoplot(acf(weeks,plot=FALSE))+ labs(title="Correlogram of Weeks calls from 2017 to 2022")
ggplot(data = weeks, aes(x = index(weeks), y = weeks)) +
  geom_line(color = "steelblue") +
  labs(title = "Correlogram of Weeks calls from 2017 to 2022")

decompose_data <- decompose(weeks, type = "additive")

# Review random time series for any missing values
decompose_data$random 

# Autoplot the random time series which exclude the NA values
autoplot(acf(na.remove(decompose_data$random),plot=FALSE))+ labs(title="Correlogram of weekly call data from 2017 to 2022")

#We can see that the acf of the residuals is centered around 0

#FIT A TIME SERIES MODEL
arima_weeks <- auto.arima(weeks)
arima_weeks
#The ARIMA(1,1,1)(1,0,0)[52] model parameters are lag 1 differencing (d), and an autoregressive term of first lag (p). Then the seasonal model has an autoregressive term of first lag (D) at model period 52 units, in this case weeks.

library(ggfortify)
ggtsdiag(arima_weeks)
#The residual plots appear to be centered around 0 as noise, with no pattern. The arima model is a fairly good fit.
weeks


#CALCULATE FORECASTS
#Finally we can plot a forecast of the time series using the forecast function, again from the forecast R package, with a 95% confidence interval where h is the forecast horizon periods in weeks.

forecast_weeks <- forecast(arima_weeks, level = c(95), h = 8)
autoplot(forecast_weeks,forecast.colors = "red")
summary(forecast_weeks)

rm(list = ls())

# Packages Installation ---------------------------------------------------

packages <- c("fpp2", "tidyverse", "GGally","lmtest","sandwich","quantreg","strucchange","tseries")


for (package_name in packages) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  }
}
# Loading Packages -------------------------------------------------------------------------

library(fpp2)
library(tidyverse)
library(GGally)
library(lmtest)
library(sandwich)
library(quantreg) 
library(strucchange)
library(tseries)

# Loading Data ------------------------------------------------------------

getwd()
# setwd("C:/Users/gabri/OneDrive/Desktop/Trapin Project/Time Series")
df <- read.csv("time_series.csv", sep = ";")
head(df)
tail(df)
names(df)
str(df)

# Missing Values?
sum(is.na(df))

# DATA MANIPULATION -------------------------------------------------------

# 1) Scaling Indexes ------------------------------------------------------

scale <- function(x) {
  a = (x/x[1])*100
  return(a)
}

# We decide to re-scale these variable (CPI and CpiCig)
# because their starting value is 100 and refers to
# December 1984. This scaled variables won't be included
# in the models but they are useful to get an idea 
# of how the situation evolved over time

df$CPI_scaled <- scale(df$CPI)
df$CpiCig_scaled <- scale(df$CpiCig)

# 2) Fixing Classes -------------------------------------------------------

# as.Date(Date)
df$Date <- as.Date(paste0("01-", df$Date), format = "%d-%b-%y")

# Data.frame -> Time.series Transformation 

# The transformation from data frame into time series
# is used only in the data exploration section of the project.
# The analysis will be carried on treating the data
# as data frame.

cigarettes <- ts(df[,-1], start = c(2004,1),
                 end = c(2022,12), frequency = 12)
head(cigarettes)

# Splitting Variables

# NB* -> cigarettes sold and cigarettes produced 
# data are stored in billions. For clarity, we converted them
# in a 0000 format, so a number such as 2000 is read as
# 2000 millions of cigarettes sold/produced.

cig_sold <- cigarettes[,1] # in Millions
cig_prod <- cigarettes[,2] # in Millions
cpi      <- cigarettes[,3]
gdp      <- cigarettes[,5]
unemp    <- cigarettes[,7]

str(df)

# DATA EXPLORATION -------------------------------------------------------------------------

# Correlation 

df[,-1] %>%
  ggpairs()

df %>% 
  keep(is.numeric) %>% 
  cor()

# 1) VISUALIZATION -----------------------------------------------------------------------

# Cigarettes Sold -------------------------------------------------------------------------

cig_sold %>% 
  autoplot() +
  ggtitle("Cigarettes Sold in Canada per month") +
  xlab("Year") +
  ylab("Quantity (in Bilions)") 

acf(cig_sold, lag.max = 96) 
pacf(cig_sold, lag.max = 96) 

# Cigarettes Produced -----------------------------------------------------

cig_prod %>% 
  autoplot() +
  ggtitle("Cigarettes Produced in Canada per month") +
  xlab("Year") +
  ylab("Quantity (in Bilions)") 

acf(cig_prod, lag.max = 96) 
pacf(cig_prod, lag.max = 96) 

# Cpi  ---------------------------------------------------

cpi %>% 
  autoplot()+
  ggtitle("Cigarettes Consumer price index in Canada per month") +
  xlab("Year") +
  ylab("Index Value") 

acf(cpi, lag.max = 96) 
pacf(cpi, lag.max = 96) 

# Gdp  ---------------------------------------------------------

gdp %>% 
  autoplot()+
  ggtitle("Gross domestic product in Canada") +
  xlab("Year") +
  ylab("GDP(*100'000)") 

acf(gdp, lag.max = 96) 
pacf(gdp, lag.max = 96) 

# Unemployment Rate ---------------------------------------------------

unemp %>% 
  autoplot()+
  ggtitle("Unemployment rate in Canada per month") +
  xlab("Year") +
  ylab("Unemployment rate") 

acf(unemp, lag.max = 96) 
pacf(unemp, lag.max = 96) 

# 2) TIME SERIES DECOMPOSITION -----------------------------------------------

# 2.a) Moving Average ----------------------------------------------------------

cig_sold %>% 
  autoplot(series = "Data") +
  autolayer(ma(cig_sold, order = 12), series = "MA(12)") +
  ggtitle("Moving Average Line Chart") +
  scale_colour_manual(values=c("Data"="grey","MA(12)"="blue"))

# 2.b) Trend & Seasonality  ----------------------------------------------------------

cig_sold  %>%    # series by month  
  ggsubseriesplot()

# Additive Model
# Yt = Tt + St + et
cig_sold %>% 
  decompose(type = "additive") %>% 
  autoplot()

# {or}
plot(stl(cig_sold, "periodic"))

dev.off()
# 2.c) Structural Breaks ----------------------------------------------------------

# Historical Information:
# 2005: for the 1st time a state banned smoking inside restaurants
# 2005: increase in taxes 
# 2006: forbidden to smoke inside bars

# Due to this changes in regulations, the series has a strong
# drop. Considered that, we decided to add calculate a 
# structural break that will be included in the model

breaks <- breakpoints(cig_sold ~ 1,
                      h = 12 , breaks = 1)
summary(breaks)

plot(cig_sold)
lines(breakpoints(breaks), col = "red")

# MODELS: -----------------------------------------------------------------

# TRAIN-TEST SPLIT -----------------------------------------

# We decide to take 70% for train and 30% for test

# data.frame
n       <- nrow(df)
ix.is   <- 1:161 # about 70%
ix.oos  <- 162:228
t <- ix.is # trend

df.is  <- df[ix.is,]
nrow(df.is)
df.oos <- df[ix.oos,]
nrow(df.oos)

# time.series
train <- window(cig_sold, start = 2003, )
test  <- window(cig_sold, start = 2021)
# Using "window" instead of "sample" keeps the ts object
class(train)
class(test)

# DETERMINISTIC COMPONENTS ------------------------------------------------

# 1) TREND ----------------------------------------------------------------

# Our approach will consist in:
# - Fitting a linear trend for the entire series
# - Fitting a trend with one break
# - Fitting a trend with two breaks

# 1.1) Linear Trend ----------------------------------------------------

fit.trend <- lm(Sales ~ t, data = df.is)
summary(fit.trend)
plot(df$Sales, type = "l")
abline(fit.trend, col = "red", lwd = 2, lty = "dashed")
abline(v=end(ix.is), lty=2)

# 1.2) Structural Breaks --------------------------------------------------------

# 1.2.A) One break --------------------------------------------------------

breaks_1 <- breakpoints(Sales ~ 1, data = df.is, breaks = 1)
summary(breaks_1) 
plot(df.is$Sales, type = "l")
abline(v = breaks_1$breakpoints, col = "red", lty = 2)

# creating dummy trend matrix for 1 break

d1 <- c(rep(1,24), rep(0,137))
d2 <- c(rep(0,24), rep(1,137))
breaks_1_matrix <- cbind(d1,d2)
trend_1_matrix <- breaks_1_matrix * t

# fitting linear model with two trends

fit <- lm(Sales ~ trend_1_matrix, data = df.is)
summary(fit)
acf(residuals(fit), lag.max = 150)
plot(residuals(fit), type = "l")

# residuals analysis

res <- residuals(fit)

par(mfrow = c(1,2))
acf(df.is$Sales, lag.max = 96) # original data
acf(res, lag.max = 96) 

# plots

plot(df.is$Sales, type = "l")
abline(a = fit$coefficients[1], b = fit$coefficients[2],
       col = "green", lwd = 2)
abline(a = fit$coefficients[1], b = fit$coefficients[3],
       col = "blue", lwd = 2)
legend("topright", legend=c("Trend B_1", "Trend B_2"),
       col=c("green", "blue"), lty=1, cex=0.8)
abline(v = breaks_1$breakpoints, col = "red", lty = 2)

plot(df.is$Sales, type = "l")
lines(fit$fitted.values, col = "red", lwd = 2)

# 1.3.B) Two breaks -------------------------------------------------------

breaks_2 <- breakpoints(Sales ~ 1, data = df.is, breaks = 2)
summary(breaks_2) 
plot(df.is$Sales, type = "l")
abline(v = breaks_2$breakpoints, col = "red", lty = 2)

# creating dummy trend matrix for 2 breaks

d1 <- c(rep(1,30), rep(0,131))
d2 <- c(rep(0,30), rep(1,35), rep(0,96))
d3 <- c(rep(0,65), rep(1, 96))
breaks_2_matrix <- cbind(d1,d2,d3)
trend_2_matrix <- breaks_2_matrix * t

# fitting linear model with three trends

fit <- lm(Sales ~ trend_2_matrix, data = df.is)
summary(fit)
acf(residuals(fit), lag.max = 150)

# residuals analysis

res <- residuals(fit)

par(mfrow = c(1,2))
acf(df.is$Sales, lag.max = 96) # original data
acf(res, lag.max = 96) 

# plots

plot(df.is$Sales, type = "l")
abline(a = fit$coefficients[1], b = fit$coefficients[2],
       col = "green", lwd = 2)
abline(a = fit$coefficients[1], b = fit$coefficients[3],
       col = "blue", lwd = 2)
abline(a = fit$coefficients[1], b = fit$coefficients[4],
       col = "orange", lwd = 2)
legend("topright", 3500000, legend=c("Trend B_1", "Trend B_2", "Trend B_3"),
       col=c("green", "blue", "orange"), lty=1, cex=0.8)
abline(v = breaks_2$breakpoints, col = "red", lty = 2)

plot(df.is$Sales, type = "l")
lines(fit$fitted.values, col = "red", lwd = 2)


# 2) SEASONALITY  -------------------------------------------------------------

# We will consider the seasonality as follows:
# - Monthly
# - Seasons

# 2.1) Monthly  -----------------------------------------------------------

# Creating a dummy matrix 

season_monthly <- diag(1, nrow = 12, ncol = 12)
season_monthly2 <- diag(1, nrow = 12, ncol = 12)

while(nrow(season_monthly) < 228) {
  season_monthly <- rbind(season_monthly,season_monthly2)
}

colnames(season_monthly) <- c("January", "February", "March", "April", "May", "June",
                   "July", "August", "Semptember", "October", "November", "December")


# * without intercept -------------------------------------------

fit <- lm(Sales ~ -1 + season_monthly[ix.is,], data = df.is)
summary(fit)
plot(df.is$Sales, type = "l")
lines(fit$fitted.values, col = "red")
res <- residuals(fit)
acf(res, lag.max = 96)

# * with intercept ----------------------------------------------

season_monthly_int <- season_monthly[,-1]  # we remove January to avoid dummy trap
fit <- lm(Sales ~ season_monthly_int[ix.is,], data = df.is)
summary(fit)
plot(df.is$Sales, type = "l")
lines(fit$fitted.values, col = "red")
res <- residuals(fit)
acf(res, lag.max = 96)

# It doesn't fit well right now, but the ideal scenario 
# is combining both trend and seasonality, and later on 
# we see that they will work well combined

# We now proceed to compare each of the trends calculated
# above (one trend, two trend and one break, three 
# trends and two breaks) to understand which fit the
# better to make forecasts.

season_monthly.oos <- season_monthly_int[ix.oos,]
Trend_3 <- 162:228
X <- cbind(rep(1,67), season_monthly.oos, Trend_3)
X

# 1 column = vectors of 1 (for intercept)
# 2:12 columns = seasonal dummies
# remaining columns = trend \ trends

dev.off()

# Fit with one trend and seasonality -------------------------------------------

fit_full_trend <- lm(Sales ~ season_monthly_int[ix.is,] +
                       t, data = df.is)
summary(fit_full_trend)
plot(df.is$Sales, type = "l")
lines(fit_full_trend$fitted.values, col = "red", lwd = 2)
res <- residuals(fit_full_trend)
plot(res, type = "l")
acf(res, lag.max = 96)

beta <- coef(fit_full_trend)
forecasts <- X %*% beta
mse_trend <- mean((df.oos$Sales - forecasts)^2)
mse_trend # 84785.4

# Fit with one break and seasonality -------------------------------------------

fit_one_break <- lm(Sales ~ season_monthly_int[ix.is,] +
                      trend_1_matrix, data = df.is)
summary(fit_one_break)
plot(df.is$Sales, type = "l")
lines(fit_one_break$fitted.values, col = "red", lwd = 2)
res <- residuals(fit_one_break)
plot(res, type = "l")
acf(res, lag.max = 96)

beta <- coef(fit_one_break)[-13]
forecasts <- X %*% beta
mse_one_break <- mean((df.oos$Sales - forecasts)^2)
mse_one_break # 33965.77

# Fit with two breaks and seasonality -------------------------------------------

fitt_two_breaks <- lm(Sales ~ season_monthly_int[ix.is,] +
                        trend_2_matrix, data = df.is)
summary(fitt_two_breaks)
plot(df.is$Sales, type = "l")
lines(fitt_two_breaks$fitted.values, col = "red", lwd = 2)
res <- residuals(fitt_two_breaks)
plot(res, type = "l")
acf(res, lag.max = 96)

beta <- coef(fitt_two_breaks)[-c(13,14)]
forecasts <- X %*% beta
mse_trend <- mean((df.oos$Sales - forecasts)^2)
mse_trend # 65603.56


# 2.2) Quarterly ---------------------------------------------------------------

# Trying to fit different seasonality terms (by season)

season_quarterly <- matrix(c(rep(1,3), rep(0,9),
                             rep(0,3), rep(1,3), rep(0,6),
                             rep(0,6), rep(1,3), rep(0,3),
                             rep(0,9), rep(1,3)),
                           nrow = 12, ncol = 4)
season_quarterly2 <- matrix(c(rep(1,3), rep(0,9),
                              rep(0,3), rep(1,3), rep(0,6),
                              rep(0,6), rep(1,3), rep(0,3),
                              rep(0,9), rep(1,3)),
                            nrow = 12, ncol = 4)

while(nrow(season_quarterly) < 228) {
  season_quarterly <- rbind(season_quarterly,season_quarterly2)
}
colnames(season_quarterly) <- c("Winter", "Spring", "Summer", "Fall")
season_quarterly

# * without intercept -------------------------------------------

fit <- lm(Sales ~ -1 + season_quarterly[ix.is,], data = df.is)
summary(fit)
plot(df.is$Sales, type = "l")
lines(fit$fitted.values, col = "red")
res <- residuals(fit)
acf(res, lag.max = 96)

# * with intercept ----------------------------------------------

season_quarterly_int <- season_quarterly[,-1]  # we remove january to avoid dummy trap
fit <- lm(Sales ~ season_quarterly_int[ix.is,], data = df.is)
summary(fit)
plot(df.is$Sales, type = "l")
lines(fit$fitted.values, col = "red")
res <- residuals(fit)
acf(res, lag.max = 96)

# + trend without intercept -----------------------------------------------------

fit <- lm(Sales ~  -1 + season_quarterly[ix.is,] + trend_2_matrix,
          data = df.is)
summary(fit)
plot(df.is$Sales, type = "l")
lines(fit$fitted.values, col = "red", lwd = 2)
res <- residuals(fit)
acf(res, lag.max = 96)

# + trend with intercept -------------------------------------------

fit_quart <- lm(Sales ~ season_quarterly_int[ix.is,] + trend_2_matrix, data = df.is)
summary(fit_quart)
plot(df.is$Sales, type = "l")
lines(fit_quart$fitted.values, col = "red", lwd = 2)
res <- residuals(fit_quart)
acf(res, lag.max = 96)

# forecasts with deterministic trend and deterministic seasonality

season_quarterly.oos <- season_quarterly_int[ix.oos,]
Trend_3 <- 162:228
X <- cbind(rep(1,67), season_quarterly.oos, Trend_3)
X

# 1st column = vectors of 1 (for intercept)
# 2nd column = seasonal dummies
# 3rd column = trend post 2nd break

beta <- coef(fit_quart)[-c(5,6)]
beta # we leave only the trend for post 2nd break

forecasts <- X %*% beta
mse_quart <- mean((df.oos$Sales - forecasts)^2)
mse_quart #72263.89

# plots
plot(df$Sales, type = "l")
lines(ix.oos, forecasts, col = "red", lwd = 2)

# FINAL CONCLUSION ABOUT DETERMINISTIC COMPONENTS:
# we decided at the end that the model that fits better
# include a intercept, two trends and eleven dummies.

# DICKEY-FULLER  --------------------------------------------------

# Let's now proceed to the stationarity check.

# Case 1: No intercept, No trend ------------------------------------------

fit <- lm(Sales[2:161] ~ -1 + Sales[1:160], data = df.is)
summary <- summary(fit)
summary
adf_statistic <- (fit$coefficients[1]-1)/summary$coefficients[2]
adf_statistic
# test statistic = -1.051493
# critical value(alpha = 0.05) = -1.943
# we don't reject H0
# NON STATIONARY

# As expected we don't reject the null if we build a 
# model with only the y lagged 1. Let's try to fit
# the model with the deterministic components
# estimated above.

# Case 2: Intercept, Trend and Season ------------------------------------------
fit <- lm(Sales[2:161] ~ Sales[1:160] + trend_1_matrix[2:161,] + season_monthly[2:161, -1], data = df.is)
summary <- summary(fit)
adf_statistic <- (fit$coefficients[2]-1)/summary$coefficients[2,2]
adf_statistic
# test statistic =  -5.308356 
# critical value(alpha = 0.05) = -3.43
# we reject H0
# STATIONARITY

res <- residuals(fit)
acf(res, lag.max = 96)
pacf(res, lag.max = 96)

# COVARIATES STATIONARITY --------------------------------------------------

# 1) GDP ---------------------------------------------------------------------
plot(df$GDP, type = "l")
plot(diff(df$GDP), type = "l")

# original series
fit <- lm(GDP[2:161] ~ -1 + GDP[1:160], data = df.is)
summary(fit)
# parameter ~1 so we don't reject H0 
# gdp non stationary

# integrating
fit <- lm(diff(df.is$GDP[2:161]) ~ -1 + GDP[2:160], data = df.is)
summary <- summary(fit)
summary
adf_statistic <- (fit$coefficients[1]-1)/summary$coefficients[2]
adf_statistic
# parameter close to 0 => GDP stationary if differentiated

# 2) CpiCig ---------------------------------------------------------------------
plot(df$CpiCig, type = "l")
plot(diff(df$CpiCig), type = "l")

# original series
fit <- lm(CpiCig[2:161] ~ -1 + CpiCig[1:160], data = df.is)
summary(fit)
# parameter ~1 so we don't reject H0 
# CpiCig non stationary

# integrating
fit <- lm(diff(df.is$CpiCig[2:161]) ~ -1 + CpiCig[2:160], data = df.is)
summary <- summary(fit)
summary
adf_statistic <- (fit$coefficients[1]-1)/summary$coefficients[2]
adf_statistic
# parameter close to 0 => CpiCig stationary if differentiated

# 3) Unemp_rate ---------------------------------------------------------------------
plot(df$Unemp_rate, type = "l")
plot(diff(df$Unemp_rate), type = "l")

# original series
fit <- lm(Unemp_rate[2:161] ~ -1 + Unemp_rate[1:160], data = df.is)
summary(fit)
# parameter ~1 so we don't reject H0 
# Unemp_rate non stationary

# integrating
fit <- lm(diff(df.is$Unemp_rate[2:161]) ~ -1 + Unemp_rate[2:160], data = df.is)
summary <- summary(fit)
summary
adf_statistic <- (fit$coefficients[1]-1)/summary$coefficients[2]
adf_statistic
# parameter close to 0 => Unemp_rate stationary if differentiated



# ARIMA ------------------------------------------------------

# This function returns the acf and pacf plots
# and the summary of the model. Again, it was built to 
# speed up the process.

arima.analysis <- function(arima_model, train) {
  par(mfrow = c(1,2))
  res <- residuals(arima_model)
  acf(res, lag.max = 160)
  pacf(res, lag.max = 160)
  summary <- summary(arima_model)
  return(summary)
}

# xreg matrix
xreg_season <- season_monthly[,-1] # dummy matrix no January
xreg_trend  <- matrix(c(1:24, rep(0, 204),
                         rep(0,24), 25:228), ncol = 2)
colnames(xreg_trend) <- c("Trend_1", "Trend_2")

xreg <- cbind(xreg_trend, xreg_season)

sales_is <- df.is$Sales

arima_1 <- Arima(sales_is, order = c(1,0,0), xreg = xreg[ix.is,])
arima_2 <- Arima(sales_is, order = c(1,0,1), xreg = xreg[ix.is,])
arima_3 <- Arima(sales_is, order = c(1,1,1), xreg = xreg[ix.is,])
arima_4 <- Arima(sales_is, order = c(2,1,1), xreg = xreg[ix.is,])
arima_5 <- Arima(sales_is, order = c(2,1,2), xreg = xreg[ix.is,])
arima_6 <- Arima(sales_is, order = c(3,1,1), xreg = xreg[ix.is,])
arima_7 <- Arima(sales_is, order = c(2,1,0), xreg = xreg[ix.is,])

arima.analysis(arima_1, sales_is) # aic = 2185.51
arima.analysis(arima_2, sales_is) # aic = 2139.52   
arima.analysis(arima_3, sales_is) # aic = 2120.69   
arima.analysis(arima_4, sales_is) # aic = 2108.88   
arima.analysis(arima_5, sales_is) # aic = 2107.34   
arima.analysis(arima_6, sales_is) # aic = 2110.66   
arima.analysis(arima_7, sales_is) # aic = 2109.65   

BIC(arima_1) #2234.808
BIC(arima_2) #2191.901
BIC(arima_3) #2169.896
BIC(arima_4) #2161.157
BIC(arima_5) #2162.695 
BIC(arima_6) #2166.009
BIC(arima_7) #2158.854 lower 

forecast_arima_1 <- forecast(arima_1, xreg = xreg[ix.oos,])
forecast_arima_2 <- forecast(arima_2, xreg = xreg[ix.oos,])
forecast_arima_3 <- forecast(arima_3, xreg = xreg[ix.oos,])
forecast_arima_4 <- forecast(arima_4, xreg = xreg[ix.oos,])
forecast_arima_5 <- forecast(arima_5, xreg = xreg[ix.oos,])
forecast_arima_6 <- forecast(arima_6, xreg = xreg[ix.oos,])
forecast_arima_7 <- forecast(arima_7, xreg = xreg[ix.oos,])

# computing mse
mse_arima_1 <- mean((forecast_arima_1$mean - df.oos$Sales)^2) # 26998.9
mse_arima_2 <- mean((forecast_arima_2$mean - df.oos$Sales)^2) # 27823.66
mse_arima_3 <- mean((forecast_arima_3$mean - df.oos$Sales)^2) # 18828.27
mse_arima_4 <- mean((forecast_arima_4$mean - df.oos$Sales)^2) # 17733.84
mse_arima_5 <- mean((forecast_arima_5$mean - df.oos$Sales)^2) # 19216.63
mse_arima_6 <- mean((forecast_arima_6$mean - df.oos$Sales)^2) # 17643.73
mse_arima_7 <- mean((forecast_arima_7$mean - df.oos$Sales)^2) # 18171.08

mse_arima <- c(mse_arima_1, mse_arima_2,
               mse_arima_3, mse_arima_4,
               mse_arima_5, mse_arima_6,
               mse_arima_7)

cat("The lowest mse is:", min(mse_arima),",which refers to
    the model number", which.min(mse_arima))

# plot
dev.off()

plot(forecast_arima_6, 
     main = "Arima(3,1,1)",
     sub = "Best Arima Model for MSE") 

plot(ix.oos, df.oos$Sales, type = "l",
     main = "Out of Sample Forecasts",
     xlab = "")
lines(162:228, forecast_arima_6$mean, col = "blue")

# ARIMAX with CPI------------------------------------------------------------------

# We try to add as covariate the CPI variable differentiated.
# as we saw in the stationarity check of the coavariates,
# in this way it results stationary

# xreg_2 = c(Deterministic Trend,
#            Deterministic Seasonality,
#            Cigarettes Price Index)

CpiCig_x <- c(NA, diff(df$CpiCig))
xreg_2 <- cbind(xreg, CpiCig_x)

arimax_1 <- Arima(sales_is[2:161], order = c(1,0,0), xreg = xreg_2[2:161,])
arimax_2 <- Arima(sales_is[2:161], order = c(1,0,1), xreg = xreg_2[2:161,])
arimax_3 <- Arima(sales_is[2:161], order = c(1,1,1), xreg = xreg_2[2:161,])
arimax_4 <- Arima(sales_is[2:161], order = c(2,1,1), xreg = xreg_2[2:161,])
arimax_5 <- Arima(sales_is[2:161], order = c(2,1,2), xreg = xreg_2[2:161,])
arimax_6 <- Arima(sales_is[2:161], order = c(3,1,1), xreg = xreg_2[2:161,])
arimax_7 <- Arima(sales_is[2:161], order = c(2,1,0), xreg = xreg_2[2:161,])

arima.analysis(arimax_1, sales_is) 
arima.analysis(arimax_2, sales_is) 
arima.analysis(arimax_3, sales_is) 
arima.analysis(arimax_4, sales_is) 
arima.analysis(arimax_5, sales_is) 
arima.analysis(arimax_6, sales_is) 
arima.analysis(arimax_7, sales_is) 

BIC(arimax_1) # 2218.714
BIC(arimax_2) # 2179.723
BIC(arimax_3) # 2158.408
BIC(arimax_4) # 2151.435
BIC(arimax_5) # 2154.046
BIC(arimax_6) # 2156.407
BIC(arimax_7) # 2148.682

forecast_arimax_1 <- forecast(arimax_1, xreg = xreg_2[162:228,])
forecast_arimax_2 <- forecast(arimax_2, xreg = xreg_2[162:228,])
forecast_arimax_3 <- forecast(arimax_3, xreg = xreg_2[162:228,])
forecast_arimax_4 <- forecast(arimax_4, xreg = xreg_2[162:228,])
forecast_arimax_5 <- forecast(arimax_5, xreg = xreg_2[162:228,])
forecast_arimax_6 <- forecast(arimax_6, xreg = xreg_2[162:228,])
forecast_arimax_7 <- forecast(arimax_7, xreg = xreg_2[162:228,])

mse_arimax_1 <- mean((forecast_arimax_1$mean - df.oos$Sales)^2) # 27669.76
mse_arimax_2 <- mean((forecast_arimax_2$mean - df.oos$Sales)^2) # 30979.29
mse_arimax_3 <- mean((forecast_arimax_3$mean - df.oos$Sales)^2) # 20130.72
mse_arimax_4 <- mean((forecast_arimax_4$mean - df.oos$Sales)^2) # 17904.15
mse_arimax_5 <- mean((forecast_arimax_5$mean - df.oos$Sales)^2) # 17614.1
mse_arimax_6 <- mean((forecast_arimax_6$mean - df.oos$Sales)^2) # 17654.05
mse_arimax_7 <- mean((forecast_arimax_7$mean - df.oos$Sales)^2) # 18885.34

mse_arimax <- c(mse_arimax_1,mse_arimax_2,
                mse_arimax_3,mse_arimax_4,
                mse_arimax_5,mse_arimax_6,mse_arimax_7)

cat("The lowest mse is:", min(mse_arimax),",which refers to
    the model number", which.min(mse_arimax))

res_cip <- forecast_arimax_5$mean - df.oos$Sales
acf(res_cip, lag.max = 60)
pacf(res_cip, lag.max = 60)

# Plotting best ARIMAX model
par(mfrow = c(2,2))

plot(forecast_arimax_5, 
     main = "ArimaX(2,1,2)",
     sub = "Best ArimaX Model for MSE")
a <- 161:227
plot(a, df.oos$Sales, type = "l",
     main = "Out of Sample Forecasts",
     xlab = "")
lines(forecast_arimax_5$mean, col = "blue")

dev.off()
# ARIMAX with PROD -------------------------------------------------------

Production <- df$Prod
xreg_2 <- cbind(xreg, Production)

arimax_1 <- Arima(sales_is[1:161], order = c(1,0,0), xreg = xreg_2[1:161,])
arimax_2 <- Arima(sales_is[1:161], order = c(1,0,1), xreg = xreg_2[1:161,])
arimax_3 <- Arima(sales_is[1:161], order = c(1,1,1), xreg = xreg_2[1:161,])
arimax_4 <- Arima(sales_is[1:161], order = c(2,1,1), xreg = xreg_2[1:161,])
arimax_5 <- Arima(sales_is[1:161], order = c(2,1,2), xreg = xreg_2[1:161,])
arimax_6 <- Arima(sales_is[1:161], order = c(3,1,1), xreg = xreg_2[1:161,])
arimax_7 <- Arima(sales_is[1:161], order = c(2,1,0), xreg = xreg_2[1:161,])

arima.analysis(arimax_1, sales_is) 
arima.analysis(arimax_2, sales_is) 
arima.analysis(arimax_3, sales_is) 
arima.analysis(arimax_4, sales_is) 
arima.analysis(arimax_5, sales_is) 
arima.analysis(arimax_6, sales_is) 
arima.analysis(arimax_7, sales_is) 

BIC(arimax_1) # 2196.587
BIC(arimax_2) # 2170.727
BIC(arimax_3) # 2151.081
BIC(arimax_4) # 2150.028
BIC(arimax_5) # 2154.01
BIC(arimax_6) # 2155.223
BIC(arimax_7) # 2145.486

forecast_arimax_1 <- forecast(arimax_1, xreg = xreg_2[162:228,])
forecast_arimax_2 <- forecast(arimax_2, xreg = xreg_2[162:228,])
forecast_arimax_3 <- forecast(arimax_3, xreg = xreg_2[162:228,])
forecast_arimax_4 <- forecast(arimax_4, xreg = xreg_2[162:228,])
forecast_arimax_5 <- forecast(arimax_5, xreg = xreg_2[162:228,])
forecast_arimax_6 <- forecast(arimax_6, xreg = xreg_2[162:228,])
forecast_arimax_7 <- forecast(arimax_7, xreg = xreg_2[162:228,])

mse_arimax_prod_1 <- mean((forecast_arimax_1$mean - df.oos$Sales)^2) # 32923.23
mse_arimax_prod_2 <- mean((forecast_arimax_2$mean - df.oos$Sales)^2) # 23083.07
mse_arimax_prod_3 <- mean((forecast_arimax_3$mean - df.oos$Sales)^2) # 18040.55
mse_arimax_prod_4 <- mean((forecast_arimax_4$mean - df.oos$Sales)^2) # 16901.68
mse_arimax_prod_5 <- mean((forecast_arimax_5$mean - df.oos$Sales)^2) # 17082.56
mse_arimax_prod_6 <- mean((forecast_arimax_6$mean - df.oos$Sales)^2) # 16919.56
mse_arimax_prod_7 <- mean((forecast_arimax_7$mean - df.oos$Sales)^2) # 16721.28

mse_arimax_prod <- c(mse_arimax_prod_1,mse_arimax_prod_2,
                mse_arimax_prod_3,mse_arimax_prod_4,
                mse_arimax_prod_5,mse_arimax_prod_6,mse_arimax_prod_7)

cat("The lowest mse is:", min(mse_arimax_prod),",which refers to
    the model number", which.min(mse_arimax_prod)) # 16721.28 
acf(arimax_4$residuals, lag.max = 96)
pacf(arimax_4$residuals, lag.max = 96)

# Plotting the best model with the prediction intervals

df %>%
  ggplot(aes(x = Date, y = Sales)) +
  geom_ribbon(data = df[-ix.is,], aes(x = Date, ymin = forecast_arima_6$lower[,1],
                                      ymax = forecast_arimax_7$upper[,1]), fill = "#F39C12", alpha = 0.25) +
  geom_line(color = "black", alpha = 0.6) +
  geom_line(data = df[-ix.is,], aes(x = Date, y = forecast_arimax_7$mean), col = "#F39C12", size = 0.5) +
  xlab("") +
  geom_vline(aes(xintercept = Date[162]),
             linetype = "dashed") +
  scale_y_continuous(labels = comma_format(scale = 1E-3, suffix = "B")) +
  labs(title = "II. ARIMAX (2,1,0) forecasts",
       x = "",
       y = "Sales ($)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size= 10),
        axis.title.y = element_text(size = 9),
        panel.border = element_rect(fill = NA, color = "black", linewidth =1)
  )


# Short Term Forecast -----------------------------------------------------
# no production

ix.is <- 1:161
xreg_2 <- xreg


one_step_ahead <- function(p,d,q) {
  
  sales.is <- df$Sales[ix.is]
  sales.oos <- df$Sales[-ix.is]
  Production <- df$Prod
  xreg_2 <- cbind(xreg, Production)
  
  t <- length(sales.oos)
  
  # Let's define empty vectores to store forecasts and prediction intervals
  pred1.ahead <- numeric(length(sales.oos))
  lb_80 <- numeric(length(sales.oos))
  lb_95 <- numeric(length(sales.oos))
  ub_80 <- numeric(length(sales.oos))
  ub_95 <- numeric(length(sales.oos))
  
  for (i in 1:t) {
    model <- Arima(sales.is, order=c(p,d,q),
                   xreg = as.matrix(xreg_2[1:(161-1+i),]))
    
    # train starts from 1:161, and then it's updated with the 
    # first value of the test set for each interaction in the loop.
    
    forecast <- forecast(model, h = 1,
                         xreg = as.matrix(xreg_2[(161+i):228,, drop = F]))
    
    # contrary to the train, test is reduced from one value
    # each interaction (because that value goes into the train)
    
    pred1.ahead[i] <- forecast$mean[1]
    lb_80[i] <- forecast$lower[1]
    lb_95[i] <- forecast$lower[2]
    ub_80[i] <- forecast$upper[1]
    ub_95[i] <- forecast$upper[2]
    
    # train update
    sales.is <- c(sales.is, sales.oos[i])
  }
  metrics <- list(pred1.ahead = pred1.ahead,
                  lb_80 = lb_80,
                  lb_95 = lb_95,
                  ub_80 = ub_80,
                  ub_95 = ub_95)
  return(metrics)
}


trial_1.2 <- one_step_ahead(1,0,0)
trial_2.2 <- one_step_ahead(1,0,1)
trial_3.2 <- one_step_ahead(1,1,1)
trial_4.2 <- one_step_ahead(2,1,1)
trial_5.2 <- one_step_ahead(2,1,2)
trial_6.2 <- one_step_ahead(3,1,1)
trial_7.2 <- one_step_ahead(2,1,0)


mse_osa_1.2 <- mean((trial_1.2$pred1.ahead - df.oos$Sales)^2) # 31515.22
mse_osa_2.2 <- mean((trial_2.2$pred1.ahead - df.oos$Sales)^2) # 21053.75
mse_osa_3.2 <- mean((trial_3.2$pred1.ahead - df.oos$Sales)^2) # 20473.03
mse_osa_4.2 <- mean((trial_4.2$pred1.ahead - df.oos$Sales)^2) # 21600.78
mse_osa_5.2 <- mean((trial_5.2$pred1.ahead - df.oos$Sales)^2) # 23334.04
mse_osa_6.2 <- mean((trial_6.2$pred1.ahead - df.oos$Sales)^2) # 21630.77
mse_osa_7.2 <- mean((trial_7.2$pred1.ahead - df.oos$Sales)^2) # 20340.03

mse_osa.2 <- c(mse_osa_1.2, mse_osa_4.2,
               mse_osa_2.2, mse_osa_5.2,
               mse_osa_3.2, mse_osa_6.2, mse_osa_7.2)

cat("The lowest mse is:", min(mse_osa.2),",which refers to
    the model number", which.min(mse_osa.2))


# Short Term Forecast -----------------------------------------------------
# with production

ix.is <- 1:161
xreg_2 <- xreg

one_step_ahead <- function(p,d,q) {
  
  sales.is <- df$Sales[ix.is]
  sales.oos <- df$Sales[-ix.is]
  Production <- df$Prod
  xreg_2 <- cbind(xreg, Production)
  
  t <- length(sales.oos)
  
  # Let's define empty vectores to store forecasts and prediction intervals
  pred1.ahead <- numeric(length(sales.oos))
  lb_80 <- numeric(length(sales.oos))
  lb_95 <- numeric(length(sales.oos))
  ub_80 <- numeric(length(sales.oos))
  ub_95 <- numeric(length(sales.oos))
  
  for (i in 1:t) {
    model <- Arima(sales.is, order=c(p,d,q),
                   xreg = as.matrix(xreg_2[1:(161-1+i),]))
    
    # train starts from 1:161, and then it's updated with the 
    # first value of the test set for each interaction in the loop.
    
    forecast <- forecast(model, h = 1,
                         xreg = as.matrix(xreg_2[(161+i):228,, drop = F]))
    
    # contrary to the train, test is reduced from one value
    # each interaction (because that value goes into the train)
    
    pred1.ahead[i] <- forecast$mean[1]
    lb_80[i] <- forecast$lower[1]
    lb_95[i] <- forecast$lower[2]
    ub_80[i] <- forecast$upper[1]
    ub_95[i] <- forecast$upper[2]
    
    # train update
    sales.is <- c(sales.is, sales.oos[i])
  }
  metrics <- list(pred1.ahead = pred1.ahead,
                  lb_80 = lb_80,
                  lb_95 = lb_95,
                  ub_80 = ub_80,
                  ub_95 = ub_95)
  return(metrics)
}


trial_1 <- one_step_ahead(1,0,0)
trial_2 <- one_step_ahead(1,0,1)
trial_3 <- one_step_ahead(1,1,1)
trial_4 <- one_step_ahead(2,1,1)
trial_5 <- one_step_ahead(2,1,2)
trial_6 <- one_step_ahead(3,1,1)
trial_7 <- one_step_ahead(2,1,0)

mse_osa_1 <- mean((trial_1$pred1.ahead - df.oos$Sales)^2) # 30014.17
mse_osa_2 <- mean((trial_2$pred1.ahead - df.oos$Sales)^2) # 20319.66
mse_osa_3 <- mean((trial_3$pred1.ahead - df.oos$Sales)^2) # 19058.08
mse_osa_4 <- mean((trial_4$pred1.ahead - df.oos$Sales)^2) # 19430.2
mse_osa_5 <- mean((trial_5$pred1.ahead - df.oos$Sales)^2) # 19004.19
mse_osa_6 <- mean((trial_6$pred1.ahead - df.oos$Sales)^2) # 18805.4
mse_osa_7 <- mean((trial_7$pred1.ahead - df.oos$Sales)^2) # 19315.12

mse_osa <- c(mse_osa_1, mse_osa_4,
             mse_osa_2, mse_osa_5,
             mse_osa_3, mse_osa_6, mse_osa_7)

cat("The lowest mse is:", min(mse_osa),",which refers to
    the model number", which.min(mse_osa))

df %>%
  ggplot(aes(x = Date, y = Sales)) +
  geom_ribbon(data = df[-ix.is,], aes(x = Date, ymin = trial_6$lb_95,
                                      ymax = trial_6$ub_95), fill = "#F39C12", alpha = 0.25) +
  geom_line(color = "black", alpha = 0.6) +
  geom_line(data = df[-ix.is,], aes(x = Date, y = trial_6$pred1.ahead), col = "#F39C12", size = 0.5) +
  xlab("") +
  geom_vline(aes(xintercept = Date[162]),
             linetype = "dashed") +
  scale_y_continuous(labels = comma_format(scale = 1E-3, suffix = "B")) +
  labs(title = " I. ARIMAX (3,1,1) forecasts",
       x = "",
       y = "Sales ($)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size= 10),
        axis.title.y = element_text(size = 9),
        panel.border = element_rect(fill = NA, color = "black", linewidth =1)
  ) 

library(fpp)
library(dplyr)
PG <- read.csv("/Users/brettscroggins/Downloads/IPG2211N.csv") %>%
  select(-DATE) %>%
  ts(start=c(1972,1), frequency=12)

PG1.tr <- window(PG, end=c(1995,12))
PG1.te <- window(PG, start=c(1996,1), end=c(2000,12))



### Problem 1

# BoxCox Lambda for PG1
L = BoxCox.lambda(PG1.tr)
L

# d = 2
tsdisplay(diff(BoxCox(PG1.tr,L)), lag=48)

# D = 1
seasonal_PG1.tr = diff(BoxCox(PG1.tr,L),12)
tsdisplay(seasonal_PG1.tr, lag=48)

# d = 1, D = 1
both_PG1.tr = diff(seasonal_PG1.tr)
tsdisplay(both_PG1.tr, lag=48)

# adf test
adf.test(both_PG1.tr)

# Max values of p, P, q, Q with combinations of d and D

# d = 1
## p = 5, q = 3, P = 1, Q = 4
# D = 1
## p = 2, q = 7, P = 1, Q = 1
# d = 1, D = 1
## p = 2, q = 2, P = 3, Q = 3



### Problem 2

# auto arima
auto_arima = auto.arima(PG1.tr, lambda = L)
summary(auto_arima)

# tsdiag from Problem 1
tsdiag(auto_arima, gof.lag = 24)

# 60 month forecast
forecast_60months = forecast(auto_arima, h = 60)
plot(forecast_60months,xlim=c(1990,2001),ylim=c(60,140))
lines(PG1.te, col='red')

# forecast accuracy
accuracy(forecast_60months, PG1.te)



### Problem 3

# manual arima for seasonal
P3_arima = auto.arima(seasonal_PG1.tr, d=0, D=1, max.p = 2, max.q = 6, max.P = 1, max.Q = 1)
summary(P3_arima)

# Compare P3_arima with auto_arima
##



### Problem 4

# manual arima for non-seasonal
P4_arima = auto.arima(diff(BoxCox(PG1.tr,L)), d=1, D=0, max.p = 5, max.q = 3, max.P = 1, max.Q = 4)
summary(P4_arima)

# Compare P3, P4 and auto arima
##



### Problem 5

PG2.tr <- window(PG, end=c(2011,12))
PG2.te <- window(PG, start=c(2012,1))

# BoxCox for PG2
L2 = BoxCox.lambda(PG2.tr)
L2

# Both differenced
both_PG2.tr = diff(diff(BoxCox(PG2.tr,L2),12))
adf.test(both_PG2.tr)

# Values of p, q, P, and Q
tsdisplay(both_PG2.tr,lag=48)
## p = 3, q = 3, P = 4, Q = 2



### Problem 6

# Auto arima for PG2
auto_arima2 = auto.arima(PG2.tr, lambda = L2)
summary(auto_arima2)

# Residual of PG2
tsdiag(auto_arima2)

# 2nd forecast
forecast_69months = forecast(auto_arima2, h = 69)
plot(forecast_69months, xlim=c(2000,2018), ylim=c(60,140))
lines(PG2.te, col='red')

# Check accuracy
accuracy(forecast_69months, PG2.te)



### Problem 7

PG3.tr <- window(PG, start=c(2005,1), end=c(2011,12))
L3 = BoxCox.lambda(PG3.tr)
L3

# Auto arima
auto_arima3 =  auto.arima(PG3.tr,lambda=L3)
summary(auto_arima3)

# Residuals of arima 3
tsdiag(auto_arima3)

# Create better model
best_arima = auto.arima(PG3.tr, max.d=2, max.D=2, max.p = 1, max.q = 1, max.P = 2, max.Q = 1)
summary(best_arima)

# Best fit model forecast
best_forecast = forecast(best_arima, h = 69)
plot(best_forecast, xlim=c(2000,2018), ylim=c(60,140))
lines(PG2.te, col='red')
accuracy(best_forecast, PG2.te)

# Compare PG3 best forecast vs. PG2 best forecast



### Problem 8

PG.tr <- window(PG, start=c(2005,1))

# Arima to project the next 63 months
best_arima_fit = arima(PG.tr, order = c(0,0,1), seasonal=c(2,1,1))
summary(best_arima_fit)
final_forecast = forecast(best_arima_fit, h = 63)
plot(final_forecast, xlim=c(2005,2023), ylim=c(60,140))

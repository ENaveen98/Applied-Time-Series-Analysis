# Section 1 [Loading Dataset] ---------------------------------
setwd('H:/F - Applied Time Series Analysis/Assignments/5')

# Load the TSA library and retail data
library(TSA)

# Load the RData file after changing to the directory containing it.
load('sarima_data.RData')

# Plot the time-series
par(mfrow = c(3, 1))
plot(yk, main='sarima_data - Time Series')
# From the plot we can observe a trend and possibly a periodic component. There would be some stationary stochastic component as well

# summary statistics
summary(yk)
acf(yk)
pacf(yk)

# Store the attributes - start time, end time, frequency, index of the given dataset
tattr = tsp(yk)
cat('start =',tattr[1],'end =',tattr[2],'period =',tattr[3],'\n')
tvec = time(yk)

# Section 2 [Trend Fitting] ---------------------------------
# Fit an appropriate polynomial of degree n
Tmat = poly(tvec, degree =1, raw = T)
modlm1 = lm(yk ~ Tmat)
summary(modlm1)
# From the significance of the coefficients for 2nd degree polynomial trend it was evident that a linear trend would suffice

reslm1 = ts(residuals(modlm1), start = tattr[1], frequency = tattr[3])
linear_y = tvec*modlm1$coefficients[2] + modlm1$coefficients[1]
additive_seasonal = ts(yk-linear_y, start = tattr[1], frequency = tattr[3])
multiplicative_seasonal = as.ts(as.vector(yk)/linear_y, start = tattr[1], frequency = tattr[3])
par(mfrow = c(2, 1))
plot(additive_seasonal, ylab = "Residuals", main = "Additive - Linear fit")
plot(multiplicative_seasonal, ylab = "Residuals", main = "Multiplicative - Linear fit")
# From the plots above we can see that for Multiplicative models the amplitude of the seasonal component dies down with time. Hence choosing an Additive model which upon visual inspection resembles that of constant amplitude.

# Section 3 [Spline Fit] ---------------------------------
# Using Spline fit
splinefit = smooth.spline(time(yk), yk, spar = 1)
res_spfit = yk - ts(splinefit$y, start = tattr[1], frequency = tattr[3])
par(mfrow = c(1, 1))
plot(res_spfit, ylab = "Residuals", main = "Residuals from spline fit")
# Extract the series assuming multiplicative model
swk <- as.ts(as.vector(yk)/splinefit$y, start = tattr[1], frequency = tattr[3])
# Plot the series
par(mfrow = c(3, 1))
plot(swk, main = "Mutiplicative - Trend removed series using Spline")
acf(swk)
pacf(swk)
# Using a multiplicative model results in the trend removed series to be non-stationary with variance changing with time.
# Hence, hereon going with an Additive model.

# Section 4 [Seasonal Component Analysis] ---------------------------------
par(mfrow = c(2, 1))
periodogram(additive_seasonal, ylab = "Periodogram", main = "Additive - Raw periodogram",xlab = "Frequency (cycles/sample)")
spec.ar(x = additive_seasonal, ylab = "PSD", main = "Additive - AR(15): PSD", xlab = "Frequency (cycles/year)")
# From both the above plots we can conclude that the period for seasonality is 10 samples/cycle or 1.2 cycle/year

# Using stl to decompose the original series
stl_model = stl(yk, s.window = 10)
plot(stl_model)
model_arma_stl = arima(stl_model$time.series[,3], order = c(5,0,5))
tsdiag(model_arma_stl)

# Section 5 [Seasonal fit] ---------------------------------
k = seq(tvec)
model_seasonal = lm(additive_seasonal ~ 0 + I(cos(2*pi*k/10)) + I(sin(2*pi*k/10)))
summary(model_seasonal)
model_seasonal_resid = model_seasonal$residuals
par(mfrow = c(3, 1))
plot(model_seasonal_resid, type ='l', main = 'Seasonal fit - residuals')
acf(model_seasonal_resid)
pacf(model_seasonal_resid)
# After fitting the residuals from linear trend using sin and cosine of period 10 the acf and pacf of the resulting residuals

# Section 6 [ARMA fit] ---------------------------------
model_arma = arima(model_seasonal_resid, order = c(2,0,0))
tsdiag(model_arma)
par(mfrow = c(2, 1))
pacf(model_arma$residuals)
hist(model_arma$residuals, probability = T, xlab = "Residuals", ylab = "Probability", main = "Histogram of residuals", col = "gray")


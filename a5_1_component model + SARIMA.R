# Section 1 [Loading Dataset] ---------------------------------
setwd('H:/F - Applied Time Series Analysis/Assignments/5')

# Load the TSA library and retail data
library(TSA)
data(retail)

# Plot the time-series
par(mfrow = c(3, 1))
plot(retail, main='Retail - Time Series')
# From the plot we can observe a trend and periodic component. Also additionally there would be some stationary stochastic component as well

# summary statistics
summary(retail)
acf(retail)
pacf(retail)

# Store the attributes - start time, end time, frequency, index of the given dataset
tattr = tsp(retail)
tvec = time(retail)

# Section 2 [Trend fitting] ---------------------------------
# Fit a linear model for trend
modlm1 = lm(retail ~ tvec)
summary(modlm1)
reslm1 <- ts(residuals(modlm1), start = tattr[1], frequency = tattr[3])
par(mfrow = c(1, 1))
plot(reslm1, ylab = "Residuals", main = "From linear fit")
# Upon visual inspection of the residual plot there can be possibility of an higher order polynomial trend

# Trying out with a 2nd degree polynomial to check if the estimates are significant.
Tmat = poly(tvec, degree =2, raw = T)
modlm2 = lm(retail ~ Tmat)
summary(modlm2)
reslm2 <- ts(residuals(modlm2), start = tattr[1], frequency = tattr[3])
plot(reslm2, ylab = "Residuals", main = "From Quadratic fit")
# Upon looking at the summary of this model it is evident that the estimates are not significant and hence we'll remove this model and stick to linear trend fit

# Section 3 [Spline Trend fitting] ---------------------------------
# Using Spline fit
splinefit = smooth.spline(time(retail), retail, spar = 1)
# Compute the residual and coerce it to a TS object
res_spfit = retail - ts(splinefit$y, start = tattr[1], frequency = tattr[3])
plot(res_spfit, ylab = "Residuals", main = "Residuals from spline and quadratic fits")
lines(reslm2, col = "red")
# Both the methods of removing trend provide almost similar residuals so we can just pick one and proceed although we might have to consider multiplicative model as the amplitude of seasonal component changes with time.

par(mfrow = c(2, 1))
periodogram(res_spfit, ylab = "Periodogram", main = "Raw periodogram",xlab = "Frequency (cycles/sample)")
spec.ar(x = res_spfit, ylab = "PSD", main = "AR(15): PSD", xlab = "Frequency (cycles/year)")
# From both the above plots we can conclude that the period for seasonality is 12 samples/cycle or 1 cycly/year

# Section 4 [Seasonal component analysis] ---------------------------------
# Extract the series assuming multiplicative model with the spline fit that was done earlier.
swk <- as.ts(as.vector(retail)/splinefit$y, start = tattr[1], frequency = tattr[3])
# Plot the series
par(mfrow = c(3, 1))
plot(swk, main = "Trend removed series using Spline fit")
acf(swk)
pacf(swk)
# Here we can observe that the amplitude of the seasonal component is fairly same. Also, the period of the seasonal components is seen to be 12 directly from the acf plot.

# Section 5 [ARIMA model] ---------------------------------
modsarima <- stats::arima(swk, order = c(2, 0, 2), seasonal = list(order = c(2 ,0, 0), period = 12))
# Examine the diagnostics
tsdiag(modsarima)
confint(modsarima)
par(mfrow = c(2, 1))
pacf(modsarima$residuals)
hist(modsarima$residuals, probability = T, xlab = "Residuals", ylab = "Probability", main = "Histogram of residuals", col = "gray")
# Observations - When the seasonal order was first set to (1,0,0) the pacf showed a peak at lag = 12 indicating that seasonal component is still present
# Hence for seasonal (2,0,0) was used.
# When the MA order was increased from 0 to 1 the pvalues from Ljung-Bos statistic crossed the 0.05% mark indicating MA component. Upon further increasing MA order to 2 the pvalues all had significant values now.
# Hence an ARMA(2,2) was used

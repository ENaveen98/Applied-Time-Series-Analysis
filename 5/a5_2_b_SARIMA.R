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

# Section 2 [Removing trend by differencing] ---------------------------------
model_sarima = arima(yk,order=c(0,1,0))
model_sarima_resid = model_sarima$residuals
par(mfrow = c(2, 1))
periodogram(model_sarima_resid, ylab = "Periodogram", main = "Differencing - Raw periodogram",xlab = "Frequency (cycles/sample)")
spec.ar(x = model_sarima_resid, ylab = "PSD", main = "Differencing - AR(15): PSD", xlab = "Frequency (cycles/year)")
# From both the above plots we can conclude that the period for seasonality is 10 samples/cycle or 1.2 cycle/year

# Section 3 [SARIMA model] ---------------------------------
model_sarima = arima(model_sarima_resid,order=c(2,0,1),seasonal=list(period=10,order=c(1,0,1)))
tsdiag(model_sarima)
par(mfrow = c(2, 1))
pacf(model_sarima$residuals)
hist(model_sarima$residuals, probability = T, xlab = "Residuals", ylab = "Probability", main = "Histogram of residuals", col = "gray")


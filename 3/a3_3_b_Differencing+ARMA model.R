setwd('H:/F - Applied Time Series Analysis/Assignments/3')

# Load the RData file after changing to the directory containing it.
load('a3_q3.RData')

# Generate summary of the data
summary(xk)

# Plot the time-series plot
plot(xk, main = 'Q3: Time Series Plot')

tattr = tsp(xk)
tvec = time(xk)

# Fitting an ARIMA model
model_arima = arima(xk,order = c(2,1,3),optim.control=list(maxit = 2500))
cat('ARIMA Model (2,1,3) - AIC=',AIC(model_arima), '\n', sep = '')

residuals_arma = ts(residuals(model_arima), start = tattr[1], frequency = tattr[3])
plot(residuals_arma, ylab = "Residuals", main= 'ARMA model Residuals')
acf(residuals_arma, main = 'ARMA Residuals - ACF - Plot')
pacf(residuals_arma, main = 'ARMA Residuals - PACF - Plot')

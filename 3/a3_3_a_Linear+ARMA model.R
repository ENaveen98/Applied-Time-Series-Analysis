setwd('H:/F - Applied Time Series Analysis/Assignments/3')

# Load the RData file after changing to the directory containing it.
load('a3_q3.RData')

# Generate summary of the data
summary(xk)

# Plot the time-series plot
plot(xk, main = 'Q3: Time Series Plot')

tattr = tsp(xk)
tvec = time(xk)

Tmat = poly(tvec, degree=1, raw = T)
model_linear <-lm(xk~Tmat)
summary(model_linear)

residuals_linear <-ts(residuals(model_linear), start = tattr[1], frequency = tattr[3])
plot(residuals_linear, ylab = "Residuals", main = "Step - (1/2) Linear model fit Residuals")
acf(residuals_linear, main = 'Linear Residuals - ACF - Plot')
pacf(residuals_linear, main = 'Linear Residuals - PACF - Plot')

# Fitting an ARIMA model
model_arima = arima(residuals_linear,order = c(2,0,1),optim.control=list(maxit = 2500))
cat('ARIMA Model (2,0,1) - AIC=',AIC(model_arima), '\n', sep = '')

residuals_arma = ts(residuals(model_arima), start = tattr[1], frequency = tattr[3])
plot(residuals_arma, ylab = "Residuals", main= 'Step - (2/2) ARMA model Residuals')
acf(residuals_arma, main = 'ARMA Residuals - ACF - Plot')
pacf(residuals_arma, main = 'ARMA Residuals -PACF - Plot')

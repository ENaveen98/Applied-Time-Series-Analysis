setwd('H:/F - Applied Time Series Analysis/Assignments/3')

# Load the RData file after changing to the directory containing it.
load('a3_q1.RData')

# Generate summary of the data
summary(xk)

# Plot the time-series plot
plot(xk, main = 'Q1: (b) Time Series Plot')

# Check the acf and pacf to get a sense of the type of model to be used
acf(xk, main = 'ACF - Plot')
pacf(xk, main= 'PACF - Plot')

# Fitting an ARIMA model
# Finding out an optimal combination of AR(p) and MA(m) model using AIC as discussed in class.
for (p in 1:4) {
  for (m in 1:4) {
    xk_arima1 = arima(xk,
                      order = c(p-1,0,m-1),
                      optim.control=list(maxit = 1000))
    cat('AR:',p-1,' MA:',m-1,' AIC=',AIC(xk_arima1), '\n', sep = '')
  }
}

# Verifying that the residuals are white noise by plotting the ACF and PACF of 
# residuals after fitting ARIMA (3, 0, 1) model.
xk_arima = arima(xk,
                  order = c(3,0,1),
                  optim.control=list(maxit = 1000))
plot(xk_arima$residuals, main= 'Residuals after fitting ARIMA (3, 0, 1) model')
acf(xk_arima$residuals, main= 'ACF of Residuals - ARIMA (3, 0, 1) model')
pacf(xk_arima$residuals, main= 'PACF of Residuals - ARIMA (3, 0, 1) model')

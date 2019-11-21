setwd('H:/F - Applied Time Series Analysis/Assignments/3')

# Load the RData file after changing to the directory containing it.
load('a3_q4.RData')
library(forecast)

# Get a sense for the data
summary(wk)
plot(wk)

# Store the frequency and start/end index.
tattr <-tsp(wk)

# Splitting the data into Train/Test.
train_wk = ts(wk[1:800], start = tattr[1], frequency = tattr[3])
test_wk = ts(wk[801:1000], start = tattr[1]+800, frequency = tattr[3])

# Automated code to find the best AR(p) model using AIC for each lambda and then 
# choosing the best lambda using MSE metric on test dataset.
# number of divisions for lambda in the given range
n = 10;
# Initialize the following variables to store MSE and the best AR(p) model
MSE_lambda = numeric(n+1);
p_lambda = numeric(n+1);
# For loop to iterate over different values of lambda in the given range
for (i in 0:n){
  # Calculating the current lambda to choose and using BoxCox transformation in
  # Train/Test dataset
  lambda = -5+(10/n)*i;
  train_stationary = ts(BoxCox(train_wk, lambda),
                        start = tattr[1],
                        frequency = tattr[3]); 
  test_stationary = ts(BoxCox(test_wk, lambda),
                       start = tattr[1]+800,
                       frequency = tattr[3]);
  
  # For loop to iterate over different values of p in AR(p) for the current lambda
  for (p in 1:8){
    # Fit AR(p) model to the Train dataset
    ar_model = Arima(train_stationary,order = c(p,0,0));
    # Calculate the corresponding AIC
    ar_aic = AIC(ar_model);
    # Below few lines of code to store the minimum value of AIC over different p.
    if (p==1){
      min_aic = ar_aic;
      min_p = p;
    }
    if (ar_aic<min_aic){
      min_aic = ar_aic;
      min_p = p;
    }
  }
  # Print the results for the current lambda.
  cat('AR model of p=', min_p,
      ' with AIC=', min_aic,
      ' is the best model for lambda=', lambda, '\n', sep='');
  # Retrain the AR(p) model with the best p value as obtained from the previous step.
  best_ar_model = Arima(train_stationary,order = c(min_p,0,0));
  # Store the best p in AR(p) as determined using AIC for the current lambda.
  p_lambda[i+1] = min_p
  # Do one-step ahead forecast on the test dataset and store the RMSE value for current lambda.
  MSE_lambda[i+1] = accuracy(Arima(test_stationary, model=best_ar_model))[2];
}
cat('--------------------------------------------------------------------------------', '\n')
best_lambda = -5+(10/n)*(which.min(MSE_lambda)-1)
best_p = p_lambda[which.min(MSE_lambda)]
cat('The optimal lambda value as calculated in the above procedure is', best_lambda, 'with AR model of order', best_p)

# Using the optimal lambda and re-fittin the time-series with an appropriate model
train_stationary = ts(BoxCox(train_wk, best_lambda),
                      start = tattr[1],
                      frequency = tattr[3]);
test_stationary = ts(BoxCox(test_wk, best_lambda),
                     start = tattr[1]+800,
                     frequency = tattr[3]);

# Fitting an ARIMA model
# Finding out an optimal combination of AR(p) and MA(m) model using AIC as discussed in class.
for (p in 1:6) {
  for (m in 1:4) {
    arima_model = Arima(train_stationary,
                      order = c(p-1,0,m-1)
                      )
    arima_AIC = AIC(arima_model)
    cat('AR:',p-1,' MA:',m-1,' AIC=', arima_AIC, '\n', sep = '')
    if (p==1 & m==1){
      min_aic = arima_AIC
      min_model = c(p-1,m-1)
    }
    if (arima_AIC<min_aic){
      min_aic = arima_AIC
      min_model = c(p-1,m-1)
    }
  }
}
cat('The best model is given by ARMA(p,q); p =', min_model[1], ', q =', min_model[2])

# Recompute the best model found from previous stage and report the results
best_model = Arima(train_stationary,
                   order = c(min_model[1],0,min_model[2])
                   )
test_forecast = Arima(test_stationary,
                      model=best_model)
forecast_residuals = test_forecast$residuals
plot(forecast_residuals)
acf(forecast_residuals)
pacf(forecast_residuals)

# Trying BoxCox.ar
library(TSA)
BoxCox.ar(train_wk, lambda = seq(-5, 5, 0.5), plotit = TRUE)
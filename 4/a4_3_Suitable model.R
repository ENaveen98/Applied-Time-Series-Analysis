setwd('H:/F - Applied Time Series Analysis/Assignments/4')

# Load the RData file after changing to the directory containing it.
library("xlsx")
library('forecast')
data <- read.xlsx("seismic1.xlsx", sheetName = 'Sheet1', header=FALSE)
vk = ts(data)
plot(vk, main='Time Series Data')
tattr = tsp(vk)
tvec = time(vk)
k = seq(0,tattr[2]-1,1)

# Fitting a polynomial model and checking if the estimated coefficients are significant.
model_poly = lm(vk ~ I(k) + I(k^2) + I(k^3))
summary(model_poly)
model_poly_resid = model_poly$residuals
plot(model_poly_resid, type='l', main='Polynomial Fit - Residuals')

# Below few lines of code to find the dominant frequency present in time series
# using spectrum and findfrequency function separately.
period_1 = findfrequency(model_poly_resid)
period_1
x = spectrum(model_poly_resid)
second_highest_freq = sort(x$spec, decreasing = T)[2]
for (i in 1:(length(x$spec)/10)){
  if (abs(x$spec[i] - second_highest_freq) < 10) {
    period_2 = 1/x$freq[i]
  }
}
period_2

# using a Period of 78
model_seasonal = lm(model_poly_resid ~ 0 + I(cos(2*pi*k/78)) + I(sin(2*pi*k/78)))
summary(model_seasonal)
model_seasonal_resid = model_seasonal$residuals
# The model coefficients are significant. However both during the polynomial fit and
# seasonal fit the standard deviation of estimates were slightly predominant and 
# hence trying the decompose function directly on data.

# Using the period of 78, that was estimated earlier, in the decompose function
vk = ts(data, frequency = 78)
vkdec <-decompose(vk, type = "multiplicative")
plot(vkdec)
acf(vkdec$random, na.action = na.pass, main='ACF of residuals - Multiplicative model')
pacf(vkdec$random, na.action = na.pass, main='PACF of residuals - Multiplicative model')

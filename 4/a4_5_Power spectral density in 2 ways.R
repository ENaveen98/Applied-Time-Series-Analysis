v_k = arima.sim(n = 1500, list(ar = c(0, -0.25), ma = c(0.45)))

acvf_l = acf(v_k, type = "covariance", lag.max = 4, plot = F)$acf

omega = seq(-pi,pi,length.out = 10000)
gamma_w = c()
for (w in omega) {
  sums_pos = 0
  for (l in 1:4) {
    sums_pos = sums_pos + 2*acvf_l[l+1]*cos(w*l)
  }
  sums = (sums_pos+acvf_l[1]*exp(0))/(2*pi)
  gamma_w = c(gamma_w, sums)
}
par(mfrow=c(1,1))
plot(omega, abs(gamma_w), type='l',
     main = paste("max_lag = 4", sep=" "),
     xlab = 'omega', ylab = 'gamma(omega)')

max_lag = seq(2, 12, 2)
par(mfrow=c(3,2))
for (l in max_lag) {
  acvf_l = acf(v_k, type = "covariance", lag.max = l, plot = F)$acf
  
  omega = seq(-pi,pi,length.out = 10000)
  gamma_w = c()
  for (w in omega) {
    sums_pos = 0
    for (l in 1:l) {
      sums_pos = sums_pos + 2*acvf_l[l+1]*cos(w*l)
    }
    sums = (sums_pos+acvf_l[1]*exp(0))/(2*pi)
    gamma_w = c(gamma_w, sums)
  }
  plot(omega, abs(gamma_w), type='l',
       main = paste("max_lag =", l, sep=" "),
       xlab = 'omega', ylab = 'gamma(omega)')
}

### Alternate implementation of same method.
# omega = seq(-pi,pi,length.out = 10000)
# gamma_w = c()
# for (w in omega) {
#   sums_pos = complex(real=0, imaginary = 0)
#   sums_neg = complex(real=0, imaginary = 0)
#   for (l in 1:max_lag) {
#     sums_pos = sums_pos + acvf_l[l+1]*exp(complex(real=0, imaginary = -w*l))
#     sums_neg = sums_neg + acvf_l[l+1]*exp(complex(real=0, imaginary = +w*l))
#   }
#   sums = (sums_pos+sums_neg+acvf_l[1]*exp(0))/(2*pi)
#   gamma_w = c(gamma_w, sums)
# }

par(mfrow=c(1,1))
acf(v_k)
pacf(v_k)
# ACF and PACF indicate an ARMA model of (2,2) configuration. As mentioned in
# class an ARMA(2,2) model is more than sufficient in terms of parsimonious models.

model = arima(v_k, order = c(2,0,2))
summary(model)
# The Standard error for the coefficient of e[k-2] is high. Hence using ARMA(2,0,1).
model = arima(v_k, order = c(2,0,1))
summary(model)
residuals = model$residuals
acf(residuals)
pacf(residuals)
# The ACF and PACF plots suggest that the residual has White-noise like characteristics.

gamma_tsfit = c()
for (w in omega) {
  sums = abs(((1+0.3876*exp(complex(real = 0, imaginary = -w)))/
                (1-0.0585*exp((complex(real = 0, imaginary = -w)))+
                                0.2806*exp((complex(real = 0, imaginary = -2*w))))))^2
  sums = sums/(2*pi)
  gamma_tsfit = c(gamma_tsfit, sums)
}
par(mfrow=c(1,1))
plot(omega, abs(gamma_tsfit), type='l',main = 'Time-Series fit gamma function', xlab = 'omega', ylab = 'gamma(omega)')

gamma_exact = c()
for (w in omega) {
  sums = abs(((1+0.45*exp(complex(real = 0, imaginary = -w)))/(1+0.25*exp((complex(real = 0, imaginary = -2*w))))))^2
  sums = sums/(2*pi)
  gamma_exact = c(gamma_exact, sums)
}

par(mfrow=c(1,1))
plot(omega, abs(gamma_exact), type='l',
     main = 'Exact gamma function', xlab = 'omega', ylab = 'gamma(omega)')

# Initialize some paramters
N_simulations = 10000
N_points = 1000
sample_mean = numeric(length = N_simulations)

# Run Monte-Carlo simulations
for (i in 1:N_simulations) {
  xk = arima.sim(model = list(ma = c(0.4)), n = N_points, sd = 1)
  sample_mean[i] = mean(xk)
}

# Find the Variance of sample mean
var(sample_mean)
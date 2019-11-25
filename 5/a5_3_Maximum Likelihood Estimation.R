# Section 1 [Loading Dataset] ---------------------------------
setwd('H:/F - Applied Time Series Analysis/Assignments/5')

# Load the RData file after changing to the directory containing it.
load('mle_unif.RData')

# Plot the likelihood function
n = length(xk)
thetavec = seq(from = 2,to = 3,by=0.001)
Ltheta = 1/(thetavec**n)
plot(thetavec, Ltheta, type ='l',
     xlab = 'Theta', ylab = 'likelihood', main = 'Likelihood function')

# Find optimal estimate of theta
optimal_estimate_theta = max(xk)
cat('Optimal Estimate of theta is', optimal_estimate_theta)
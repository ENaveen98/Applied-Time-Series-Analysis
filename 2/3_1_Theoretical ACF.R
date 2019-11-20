th_acf = c(1:21)
th_acf[1] = 1
th_acf[2] = 1.4/1.45
th_acf[3] = ((1.4^2)/1.45)-0.45
for (i in 4:21) {
  th_acf[i] = 1.4*th_acf[i-1]-0.45*th_acf[i-2]
}

r_acf = ARMAacf(ar=c(1.4, -0.45), lag.max=20)

acf(th_acf,  xlab='index', ylab='ACF', main = 'Theroretical ACF calculated by hand')
acf(r_acf,  xlab='index', ylab='ACF', main = 'Theroretical ACF calculated using R')


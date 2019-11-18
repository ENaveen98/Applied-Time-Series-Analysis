r_pacf_1 = ARMAacf(ar=c(1.3, -0.24), lag.max=20, pacf=TRUE)
print('For AR(2) process')
cat('l=1',r_pacf_1[1])
cat('l=2',r_pacf_1[2])

r_pacf_2 = ARMAacf(ma=c(0.6), lag.max=20, pacf=TRUE)
print('For MA(1) process')
cat('l=1',r_pacf_2[1])
cat('l=2',r_pacf_2[2])
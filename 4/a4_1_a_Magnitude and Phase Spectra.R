k_1 = seq(0, 5, by = 1)
xk_1 = 4*sin(pi*(k_1-2)/3)
xkf_1 = numeric(6)
for (i in 1 :6) {
  for (j in 1:6) {
    xkf_1[i] = xkf_1[i] + (xk_1[j]*exp(-2i*pi*(i-1)*(j-1)/6))
  }
  xkf_1[i] = xkf_1[i]/6
}
plot(k_1, abs(xkf_1), type='h', main='(i) Magnitude', xlab='n', ylab='Magnitude')
plot(k_1, Arg(xkf_1), type='h', main='(i) Phase', xlab='n', ylab='Phase')

k_2 = seq(0, 14, by = 1)
xk_2 = cos(2*pi*k_2/3)+sin(2*pi*k_2/5)
xkf_2 = numeric(15)
for (i in 1 :15) {
  for (j in 1:15) {
    xkf_2[i] = xkf_2[i] + (xk_2[j]*exp(-2i*pi*(i-1)*(j-1)/15))
  }
  xkf_2[i] = xkf_2[i]/15
}
plot(k_2, abs(xkf_2), type='h', main='(ii) Magnitude', xlab='n', ylab='Magnitude')
plot(k_2, Arg(xkf_2), type='h', main='(ii) Phase', xlab='n', ylab='Phase')

k_3 = seq(0, 14, by = 1)
xk_3 = cos(2*pi*k_3/5)*sin(2*pi*k_3/3)
xkf_3 = numeric(15)
for (i in 1 :15) {
  for (j in 1:15) {
    xkf_3[i] = xkf_3[i] + (xk_3[j]*exp(-2i*pi*(i-1)*(j-1)/15))
  }
  xkf_3[i] = xkf_3[i]/15
}
plot(k_3, abs(xkf_3), type='h', main='(iii) Magnitude', xlab='n', ylab='Magnitude')
plot(k_3, Arg(xkf_3), type='h', main='(iii) Phase', xlab='n', ylab='Phase')

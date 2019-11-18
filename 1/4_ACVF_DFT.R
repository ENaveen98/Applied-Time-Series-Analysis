# For different SNR values.

A = 1;
f = 0.24;
N = 200;
k = 1:N;
SNR = c(20, 10, 1, 0.1);
sigma_e = (1/(SNR*2^0.5))^0.5;

for (i in 1:4){
  e = rnorm(N, 0, sigma_e[i]);
  v = A*cos(2*pi*f*k) + e;
  
  # ACVF
  plot(acvf(v,h=100), xlab='index', ylab='ACVF', main = c('ACVF:', 'SNR is',SNR[i]));
  
  # Frequency Domain - DFT
  plot(Mod(fft(v)), xlab='index', ylab='DFT', main = c('Frequency domain:', 'SNR is',SNR[i]))
}
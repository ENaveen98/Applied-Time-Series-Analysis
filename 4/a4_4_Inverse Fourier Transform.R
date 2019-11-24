library(pracma)
f = seq(-0.5, 0.5, length.out= 1000)
psd_f = 1.44/(1.16-0.8*cos(2*pi*f))
acvf_l = ifft(psd_f)
acvf_handcalculated = numeric(10)
for (i in 1:10) {
  acvf_handcalculated[i] = 1.7142*(0.4)^(i-1)
}

acvf_handcalculated
abs(acvf_l)[1:10]
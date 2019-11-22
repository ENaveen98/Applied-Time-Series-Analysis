w = seq(-pi, pi, length.out= 1000)
G_w = (1+0.5*exp(complex(real=0,imaginary =-w)))/(1-1.2*exp(complex(real=0,imaginary =-w))+0.27*exp(complex(real=0,imaginary =-2*w)))
plot(w, abs(G_w), type='l', main='|G(w)| vs w', xlab='w', ylab='|G(w)|')
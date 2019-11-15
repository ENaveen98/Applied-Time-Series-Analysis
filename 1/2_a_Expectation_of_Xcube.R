# Single Integration
Func <- function(x) x^3*((1/(4*2*pi)^0.5))*exp((-1/8)*(x-1)^2)
integrate(Func, -Inf, Inf)
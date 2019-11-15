# Double Integration
InnerFunc <- function(x, y) (x^2 + y^2)/26
InnerIntegral <- function(y) { sapply(y, function(z) { integrate(InnerFunc, 0.2, 0.4, y=z)$value }) }
integrate(InnerIntegral, 0.3, 0.8)

# Single Integration
Func <- function(y) (y + y^3)*3/14
integrate(Func, 0, 2)
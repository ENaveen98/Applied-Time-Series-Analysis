s_n <- function(x, y) (1/(length(x)))*sum((X-mean(X))*(Y-mean(Y)));

vec_s_n_1 <- vector();
vec_s_n <- vector();

R <- 400;
N <- 2000;

for (i in 1:R){
  X <- rnorm(N, 1, 2);
  Y <- (2*X^2) + (4*X);
  vec_s_n_1 <- c(vec_s_n_1, cov(X,Y));
  vec_s_n <- c(vec_s_n, s_n(X,Y));
}

cat('Simple Average of estimates: \n', 'S_n-1^2 =>', mean(vec_s_n_1), '\t', 'S_n^2 =>', mean(vec_s_n), '\n');
cat('Standard Deviation of estimates: \n', 'S_n-1^2 =>', sd(vec_s_n_1), '\t', 'S_n^2 =>', sd(vec_s_n))

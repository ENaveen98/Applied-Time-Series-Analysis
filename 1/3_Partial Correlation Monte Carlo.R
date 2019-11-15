R <- 300;
N <- 600;

vec_inv <- vector();
vec_reg <- vector();

for (i in 1:R){
  V <- rnorm(N, 0, 1);
  W <- rnorm(N, 0, 2);
  Z <- rnorm(N, 1, 3);
  X <- 3*Z + W;
  Y <- 1.5*Z + 4*X + V;
  
  cov_matrix = cov(cbind(X, Y, Z));
  corr_matrix = cor(cbind(X, Y, Z));
  
  inv_cov_matrix = solve(cov_matrix);
  par_corr_inv = (-inv_cov_matrix[1,2])/(inv_cov_matrix[1,1]*inv_cov_matrix[2,2])^0.5;
  
  b12 = ((cov_matrix[1,1]/cov_matrix[2,2])^0.5)*((corr_matrix[1,2]-corr_matrix[1,3]*corr_matrix[2,3])/(1-corr_matrix[2,3]^2))
  b21 = ((cov_matrix[2,2]/cov_matrix[1,1])^0.5)*((corr_matrix[1,2]-corr_matrix[1,3]*corr_matrix[2,3])/(1-corr_matrix[1,3]^2))
  par_corr_reg = (b12*b21)^0.5
  
  vec_inv <- c(vec_inv, par_corr_inv);
  vec_reg <- c(vec_reg, par_corr_reg);
  
  # cat('Partial Correlation using Inversion = ',par_corr_inv,'\nPartial Correlation using Regression = ',par_corr_reg)
}

cat('Simple Average of estimates: \n', 'Inversion =>', mean(vec_inv), '\t', 'Regression =>', mean(vec_reg), '\n');
cat('Standard Deviation of estimates: \n', 'Inversion =>', sd(vec_inv), '\t', 'Regression =>', sd(vec_reg))
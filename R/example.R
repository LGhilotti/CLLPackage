## generate a dataset
set.seed(8675309)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)

#beta0 = rnorm(3,2,1)
#beta0=beta_min
beta0=c(0,0,0)
X = cbind(rep(1,n),x1,x2)

beta_estimated_gd <- linear_gd_optim(beta0, X, y, maxit = 100000, stepsize = 1e-3, tol=1e-3)

beta_estimated_sd <- linear_sd_optim(beta0, X, y, maxit = 1000000, tol=1e-3)

beta_min = lm(y ~ x1 + x2)$coeff


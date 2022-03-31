## generate a dataset
set.seed(8675309)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)

#beta0 = rnorm(3,2,1)
beta0=c(0,0,0)
X = cbind(rep(1,n),x1,x2)

beta_estimated <- linear_gd_optim(beta0, X, y, maxit = 100000, stepsize = 1e-3, tol=1e-3)

lm(y ~ x1 + x2)

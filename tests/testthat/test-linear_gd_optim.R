test_that("gradient descend works", {
  set.seed(8675309)
  n = 1000
  x1 = rnorm(n)
  x2 = rnorm(n)
  y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)

  beta0=c(0,0,0)

  X = cbind(rep(1,n),x1,x2)

  beta_est_gd <- linear_gd_optim(beta0, X, y, maxit = 100, stepsize = 0.5e-3, tol=1e-3)

  beta_est_lm <- lm(y ~ x1 + x2)

  expect_equal(beta_est_gd, beta_est_lm, tol = 0.2 )
})

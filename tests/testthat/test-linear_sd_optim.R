test_that("steepest descend works", {

  #generate data
  set.seed(8675309)
  n = 1000
  x1 = rnorm(n)
  x2 = rnorm(n)
  y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)

  beta0=c(0,0,0)
  X = cbind(rep(1,n),x1,x2)

  #run linear_sd_optim
  beta_est_sd <- linear_sd_optim(beta0, X, y, tol=1e-3, maxit = 1000)

  #run lm
  beta_est_lm = lm(y ~ x1 + x2)$coeff

  expect_equal(beta_est_sd,
               as.vector(beta_est_lm), tolerance = 0.2)

})

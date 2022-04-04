test_that("linear sd optim works", {

  #generate data
  set.seed(8675309)
  n = 1000
  x1 = rnorm(n)
  x2 = rnorm(n)
  y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)

  beta0=c(0,0,0)
  X = cbind(rep(1,n),x1,x2)

  #run linear sd
  beta_estimated_sd <- linear_sd_optim(beta0, X, y, maxit = 1000, tol=1e-3)

  #run lm
  beta_lm = lm(y ~ x1 + x2)$coeff

  expect_equal(beta_estimated_sd,
               beta_lm, tolerance = 0.2)

})

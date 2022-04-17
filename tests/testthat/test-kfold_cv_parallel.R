test_that("kfold_cv_parallel works", {

  #generate data
  set.seed(8675309)
  n = 1000
  x1 = rnorm(n)
  x2 = rnorm(n)
  y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)

  beta0=c(0,0,0)
  X = cbind(rep(1,n),x1,x2)

  # MSE computed with kfold_cv_seq
  MSE_seq <- kfold_cv_parallel(X,y,k=1000)

  # MSE computed with cross validation using lm function
  MSE <- kfold_cv_lm_seq(X = X, y=y, k=5)

  expect_equal(MSE_seq,
               MSE, tolerance = 0.2)

})

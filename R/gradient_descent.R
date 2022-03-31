
linear_gd_optim <- function(beta0, # beta(0)
                            X, # data predictors
                            y, # response variable
                            tol=1e-3, # tolerance
                            maxit=1000, # max iteration, not to run forever
                            stepsize=1e-3, # stepsize parameter
                            verbose=T) # should the function write messages?

{
  it <- 0
  err <- tol +1
  beta_new <- beta0
  beta_old <- beta0
  while (err > tol & it < maxit){
    beta_new <- beta_old - stepsize * gradient(beta_old, X, y)
    it <- it + 1
    err <- max(abs(beta_new - beta_old))

  }
  if (verbose==TRUE){
    if (err > tol)  print(paste0("Error after ", maxit, " iterations is: ", err))
    else print(paste0("Error less than ", tol, " after ", it, " iterations"))
  }

  return (beta_new)

}

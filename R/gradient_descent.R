
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
    beta_old <- beta_new
    beta_new <- beta_old - stepsize * gradient(beta_old, X, y)
    it <- it + 1
    err <- norm(beta_new - beta_old, type="i")
    if (verbose == TRUE){
      #print(paste0("Gradient at iteration ",it-1," is: ",as.vector(grad_L)))
      print(paste0("Step_t at iteration ",it-1," is: ",step_t))
      print(paste0("Error at iteration ",it-1," is: ",err))
    }

  }
  if (verbose==TRUE){
    if (err > tol)  print(paste0("Error after ", maxit, " iterations is: ", err))
    else print(paste0("Error less than ", tol, " after ", it, " iterations"))
  }

  return (as.vector(beta_new))

}

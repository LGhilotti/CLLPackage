
#' Gradient descend method
#'
#' This function returns the minimum point via gradient descend algorithm
#'
#' @param beta0 [numeric] Starting point of the iterative algorithm
#' @param X [numeric] Design matrix
#' @param y [numeric] Observed response vector
#' @param tol [numeric] Tolerance for stopping criteria
#' @param maxit [numeric] Maximum number of iterations for stopping criteria
#' @param stepsize [numeric] Stepsize
#' @param verbose [logical]
#'
#' @return [numeric] Estimated minimum point via gradient descend algorithm
#' @export
#'
linear_gd_optim <- function(beta0, X, y, tol=1e-3, maxit=1000,
                            stepsize=1e-3, verbose=T) {
  it <- 0
  err <- tol +1
  beta_new <- beta0
  beta_old <- beta0
  while (err > tol & it < maxit){
    beta_old <- beta_new
    beta_new <- beta_old - stepsize * gradient(beta_old, X, y)
    it <- it + 1
    err <- norm(beta_new - beta_old, type="i")
    if(is.infinite(err)) break
  }
  if (verbose==TRUE){
    if ((!is.infinite(err)) & (err > tol))  print(paste0("Error after ", maxit, " iterations is: ", err))
    else if (is.infinite(err)) print(paste0("Infinite error after ", it, " iterations"))
    else print(paste0("Error less than ", tol, " after ", it, " iterations"))
  }

  return (as.vector(beta_new))

}




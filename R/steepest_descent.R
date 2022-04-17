#' Steepest descend algorithm
#'
#' This function returns the minimum point via steepest descend algorithm
#'
#' @param beta0 [numeric] Starting point of the iterative algorithm
#' @param X [numeric] Design matrix
#' @param y [numeric] Observed response vector
#' @param tol [numeric] Tolerance for stopping criteria
#' @param maxit [numeric] Maximum number of iterations for stopping criteria
#' @param verbose [character]
#'
#' @return [numeric] Estimated minimum point via steepest descend algorithm
#' @export
#'
linear_sd_optim <- function(beta0, X, y, tol=1e-3, maxit=1000, verbose=T){

  it <- 0
  err <- tol +1
  beta_new <- beta0
  beta_old <- beta0
  while (err > tol & it < maxit){
    grad_L <- gradient(beta_old, X,y)
    hess_L <- 4*t(X) %*% X
    step_t <- norm(grad_L, type="2")^2 / as.numeric(t(grad_L) %*% hess_L %*% grad_L)
    beta_old <- beta_new
    beta_new <- beta_old - step_t * grad_L
    it <- it + 1
    err <- max(abs(beta_new - beta_old))

  }
  if (verbose==TRUE){
    if (err > tol)  print(paste0("Error after ", maxit, " iterations is: ", err))
    else print(paste0("Error less than ", tol, " after ", it, " iterations"))
  }

  return (as.vector(beta_new))

}

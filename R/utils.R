#' Gradient
#'
#' @param beta [numeric] Point where to evaluate the gradient
#' @param X [numeric] Design matrix
#' @param y [numeric] Data
#'
#' @return [numeric] The gradient of the function
#'
#'
gradient <- function(beta, X,y) {
  2*t(X)%*%(X%*%beta -y)
}

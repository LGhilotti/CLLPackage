#' Gradient of the loss function
#'
#' @param beta [numeric] Point where to evaluate the gradient
#' @param X [numeric] Design matrix
#' @param y [numeric] Observed response vector
#'
#' @return [numeric] Gradient of loss function in linear models
#'
#' @export
#'
gradient <- function(beta, X,y) {
  2*t(X)%*%(X%*%beta -y)
}



#' Predict responses given estimated parameters and new covariates
#'
#' @param beta_hat [numeric] Estimated parameters of linear model
#' @param X_test [numeric] Design matrix for test covariates
#'
#' @return [numeric] Predicted responses of the linear model
#' @export
#'
predict_y <- function(beta_hat, X_test){

  return (X_test %*% beta_hat)

}




#' Compute MSE - Mean Squared Error
#'
#' @param y_observed [numeric] Observed values for the response variable
#' @param y_predicted [numeric] Predicted values for the response variable
#'
#' @return [numeric] The MSE produced by the linear model
#' @export
#'
compute_mse <- function(y_observed, y_predicted){

  return (mean((y_observed - y_predicted)^2))

}




#' Compute MSE for cross-validation on specified fold
#'
#' @param i_test [numeric] Index of the fold to be selected as test set
#' @param X_perm [numeric] Design matrix
#' @param y_perm [numeric] Observed response vector
#' @param end_index_folds [numeric]
#' @param algorithm [character] Type of algorithm to be run
#' @param tol [numeric] Tolerance for stopping criteria
#' @param maxit [numeric] Maximum number of iterations for stopping criteria
#' @param stepsize [numeric] Stepsize - not used for steepest descend method
#' @param verbose [logical]
#'
#' @return [numeric] The MSE on the specified test fold
#' @import dplyr
#'
mymap <- function(i_test, X_perm, y_perm, end_index_folds, algorithm, tol, maxit, stepsize, verbose){

  range_test <- (end_index_folds[i_test]+1):end_index_folds[i_test+1]
  X_test <-  X_perm[range_test,]
  X_train <- X_perm[-range_test,]
  y_test <-  y_perm[range_test]
  y_train <- y_perm[-range_test]

  beta0 <- rnorm(ncol(X_perm), 0, 1e+4)
  if (algorithm == "sd"){
    beta_est <- linear_sd_optim(beta0, X_train, y_train, tol, maxit, verbose)
  } else if (algorithm == "gd") {
    beta_est <- linear_gd_optim(beta0, X_train, y_train,  tol, maxit, stepsize, verbose)
  }
  else {
    print("Not valid option - use gd (gradient descend) or sd (steepest descend)")
    break
  }

  y_pred <- predict_y(beta_est, X_test)

  return (compute_mse(y_test, y_pred))
}










#' K-fold cross-validation for lm - sequential version
#'
#' @param X [numeric] Design matrix
#' @param y [numeric] Observed response vector
#' @param k [numeric] Number of folds
#'
#' @return [numeric] MSE provided by lm() R function through k-fold Cross-Validation
#' @export
#'
kfold_cv_lm_seq <- function(X, y, k=5){
  n <- length(y)
  p <- ncol(X)
  Xy <- cbind(X,y)
  Xy_perm <- Xy[sample(1:n, size = n, replace = FALSE ),]
  X_perm <- Xy_perm[,1:p]
  y_perm <- Xy_perm[,p+1]
  card_folds <- c(rep(floor(n/k)+1, n%%k ), rep(floor(n/k), k- n%%k) )
  end_index_folds <- c(0,cumsum(card_folds))
  mse <- 0
  for (i_test in 1:k){
    range_test <- (end_index_folds[i_test]+1):end_index_folds[i_test+1]
    X_test <-  X_perm[range_test,]
    X_train <- X_perm[-range_test,]
    y_test <-  y_perm[range_test]
    y_train <- y_perm[-range_test]


    beta_est <- lm(y_train ~ X_train[,-1])$coeff

    y_pred <- predict_y(beta_est, X_test)

    mse <- mse + compute_mse(y_test, y_pred)
  }

  mse <- mse/k

  return (mse)

}

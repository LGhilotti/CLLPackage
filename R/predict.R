predict_y <- function(beta_hat, X_test){

  return (X_test %*% beta_hat)

}


compute_mse <- function(y_observed, y_predicted){

  return (mean((y_observed - y_predicted)^2))

}


kfold_cv_seq <- function(X, y, k=5, algorithm = "sd", tol=1e-3, maxit= 1000, stepsize = 1e-3, verbose = FALSE){
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

    beta0 <- rnorm(p, 0, 1e+4)
    if (algorithm == "sd"){
      beta_est <- linear_sd_optim(beta0, X_train, y_train, tol, maxit, verbose)
    } else if (algorithm == "gd") {
      beta_est <- linear_gd_optim(beta0, X_train, y_train,  tol, maxit, stepsize, verbose)
    }
    else {
      print("Not valid option")
      break
    }

    y_pred <- predict_y(beta_est, X_test)

    mse <- mse + compute_mse(y_test, y_pred)
  }

  mse <- mse/k

  return (mse)

}



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

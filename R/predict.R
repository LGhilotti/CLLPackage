
#' kfold_cv_seq
#'
#' This function provides a linear model estimate through a k-fold cross validation method.
#'
#' @param X [matrix] matrix of the covariates
#' @param y [vector] vector of the respose variable
#' @param k [numeric] the number of folds
#' @param algorithm [string] specifies "sd" for the steepest descent method or "gd" for the gradient descent method
#' @param tol [numeric] tolerance for the stoppage criteria of the algorithm
#' @param maxit [numeric] maximum number of iterations of the algorithm when convergence is not reached
#' @param stepsize [numeric]
#' @param verbose [logical]
#'
#' @return [numeric] the function returns the Mean Square Error of the model
#' @export
#'
#' @examples
kfold_cv_seq <- function(X, y, k=5, algorithm = "sd", tol=1e-3, maxit= 1000, stepsize = 1e-3, verbose = FALSE){
  n <- length(y)
  p <- ncol(X)
  Xy <- cbind(X,y)
  set.seed(8675309)

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
    set.seed(8675309)

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






kfold_cv_parallel <- function(X, y, k=5, algorithm = "sd", tol=1e-3, maxit= 1000, stepsize = 1e-3, verbose = FALSE){
  set.seed(8675309)
  n <- length(y)
  p <- ncol(X)
  Xy <- cbind(X,y)
  Xy_perm <- Xy[sample(1:n, size = n, replace = FALSE ),]
  X_perm <- Xy_perm[,1:p]
  y_perm <- Xy_perm[,p+1]
  card_folds <- c(rep(floor(n/k)+1, n%%k ), rep(floor(n/k), k- n%%k) )
  end_index_folds <- c(0,cumsum(card_folds))

  n_cpus <- parallel::detectCores()
  cluster <- makeCluster(n_cpus-1, type = "SOCK")
  registerDoSNOW(cluster)

  # export dependencies in cluster
  clusterExport(cluster, list("linear_sd_optim","linear_gd_optim","predict_y","compute_mse","gradient"))

  mse <- snow::parLapply(cl = cluster,
                  x = 1:k,
                  fun = mymap, X_perm = X_perm , y_perm = y_perm, end_index_folds = end_index_folds,
                  algorithm = algorithm, tol = tol, maxit = maxit, stepsize = stepsize, verbose= verbose ) %>%
    unlist() %>% # list to vector
    mean()

  stopCluster(cluster)


  return (mse)

}


gradient <- function(beta, X,y) {
  2*t(X%*%beta -y) %*% X
}

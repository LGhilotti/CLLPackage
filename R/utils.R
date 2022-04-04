gradient <- function(beta, X,y) {
  2*t(X)%*%(X%*%beta -y)
}

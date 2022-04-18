# CLLPackage

<!-- badges: start -->
<!-- badges: end -->
The goal of CLLPackage is to provide alternative methods to estimate coefficients of a linear model. The first method implemented is based on descend gradient approach and the second one is based on steepest gradient approach. In order to compute MSE on predictions obtained with a linear model, implementation of a k-fold cross-validation function both in sequential and in parallel way is available. 

## Installation

You can install the released version of CLLPackage with the following link:

``` r
install.packages("CLLPackage")
remotes::install_github("LGhilotti/CLLPackage")
```

## Example

This is a basic example which shows you how to use the main functions implemented in the package in order to estimate linear model coefficients and to compute MSE on predictions:

``` r
library(CLLPackage)

set.seed(8675309)
n<-10000
x1<-rnorm(n)
x2<-rnorm(n)
y<-1 + 0.5*x1 + 0.2*x2 + rnorm(n)

beta0 <- rnorm(3,2,1)
X<-cbind(rep(1,n),x1,x2)

beta_estimated_gd <- linear_gd_optim(beta0, X, y, maxit = 1000, stepsize = 1e-5, tol=1e-5)
beta_estimated_sd <- linear_sd_optim(beta0, X, y, maxit = 1000, tol=1e-4)

MSE_seq <- kfold_cv_seq(X,y,k=1000)
MSE_parallel <- kfold_cv_parallel(X,y,k=1000)

```


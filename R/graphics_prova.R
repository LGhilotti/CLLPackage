#set.seed(8675309)
#n = 1000
#x1 = rnorm(n)
#y = 1 + 0.5*x1 + rnorm(n)

#X = cbind(rep(1,n),x1)

#L = function(beta_0, beta_1) norm(((X %*% as.vector(c(beta_0,beta_1)) - y)),type="2")^2

#L(1,0.5)

#plot(L)

###
#library(graphics)
#define x and y
#beta_0 <- seq(-10,10,by=0.005)
#beta_1 <- seq(-10,10,by=0.005)

#beta_0 <- c(-0.5,0,0.5)
#beta_1 <- c(-0.5,0,0.5)

#define function to create z-values

#prova <- cbind(beta_0,beta_1)

#create z-values
#z = mapply(L,beta_0,beta_1)

#library(plotly)
#plot_ly(x=beta_0, y=beta_1, z=z, type="scatter3d", mode="markers")

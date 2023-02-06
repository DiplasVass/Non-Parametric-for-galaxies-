#################################### Skew Normal ###################################### 
.libPaths("/home/testuser/Rlib")

  install.packages("timeDate")
  install.packages("timeSeries")
  install.packages("fBasics")
  install.packages("fMultivar")
  install.packages("pbkrtest")
  install.packages("sn")
  library(fMultivar)
  library(timeDate)
  library(timeSeries)
  library(fBasics) 
  library(sn)


  r1 = replicate(30000,rsn(5,omega=1/4))
  r2 = replicate(30000,rsn(5,omega=1/2))
  r3 = replicate(30000,rsn(5,omega=3/4))
  r4 = replicate(30000,rsn(5,omega=4/4))
  r5 = replicate(30000,rsn(5,omega=5/4))
  r6 = replicate(30000,rsn(5,omega=8/4))
  r7 = replicate(30000,rsn(5,omega=20/4))
  nn = replicate(30000,rnorm(5))
  
  par(mfrow=c(1,2))
  hist(r1,nclass=40,col='white',main = "Skew Normal")
  hist(nn,nclass=40,col='white',main = "Normal(0,1)")
  
  par(mfrow=c(1,2))
  hist(r2,nclass=40,col='white')
  hist(nn,nclass=40,col='white')
  
  par(mfrow=c(1,2))
  hist(r1,nclass=40,col='white') 
  hist(nn,nclass=40,col='white')
  
  par(mfrow=c(1,2))
  hist(r1,nclass=40,col='white') 
  hist(nn,nclass=40,col='white')
  
  par(mfrow=c(1,2))
  hist(r1,nclass=40,col='white') 
  hist(nn,nclass=40,col='white')

  par(mfrow=c(1,2))
  hist(r1,nclass=40,col='white')
  hist(nn,nclass=40,col='white')
 
  par(mfrow=c(1,2))
  hist(r1,nclass=40,col='white') 
  hist(nn,nclass=40,col='white')

## Make 2-D Grid Coordinates:
   N <- 101
   x <- y <- seq(-4, 4, l=N)
   X <- cbind(u=grid2d(x)$x, v=grid2d(x)$y)
   
## dmsn
   # Set Parameters:
   xi <- c(0, 0) 
   Omega <- diag(2); Omega[2,1] <- Omega[1,2] <- 0.5
   alpha <- c(2, 5)
   # Compute skew Normal Density:
   z <- sn::dmsn(X, xi, Omega, alpha)
   Z <- list(x=x, y=x, z=matrix(z, ncol = length(x)))
   # Plot:   
   image(Z)
   contour(Z,col="darkblue")
   grid(col="red")
   
## rmsn - 
   set.seed(4711)
   r <- sn::rmsn(n=30000, xi, Omega, alpha)
   plot(hexBinning(r))
   contour(Z, add=TRUE, col="darkblue", lwd=2)
   grid(col="red")



## snorm -
   # Ranbdom Numbers:
   par(mfrow = c(2, 2))
   set.seed(1953)
   r = rsn(n = 1000)
   plot(r, type = "l", main = "skew normal", col = "steelblue")
   
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   box()
   x = seq(min(r), max(r), length = 201)
   lines(x, dsnorm(x), lwd = 2)
   
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Plot df and compare with true df", col = "steelblue",
     ylab = "Probability")
   lines(x, psn(x), lwd = 2)

install.packages("fGarch")
library(fGarch)
#> Loading required package: timeDate
#> Loading required package: timeSeries
#> Loading required package: fBasics
install.packages("ggplot2")
library(ggplot2)

#> Loading required package: stats4
#> 
#> Attaching package: 'sn'
#> The following object is masked from 'package:fBasics':
#> 
#>     vech
#> The following object is masked from 'package:stats':
#> 
#>     sd

central_tendency_sim <- 0
dispersion_sim <- 1
skewness_sim <- 1.5

N_sim <- 10000

obs_sim <- seq(from = -5,
               to = 5,
               length.out = N_sim)

ggplot(data = data.frame(u = obs_sim),
       mapping = aes(x = u)) +
  stat_function(mapping = aes(colour = "fGarch"),
                fun = dsnorm,
                args = list(mean = central_tendency_sim,
                            sd = dispersion_sim,
                            xi = skewness_sim)) +
  stat_function(mapping = aes(colour = "sn"),
                fun = dsn,
                args = list(xi = central_tendency_sim,
                            omega = dispersion_sim,
                            alpha = skewness_sim)) +
  stat_function(mapping = aes(colour = "standard"),
                fun = dnorm) +
  scale_colour_manual(name = NULL,
                      values = c("red", "blue", "green"))   

###

 X <- seq(-1, 2, 0.01)

par(mfrow=c(3,4))
plot(X, dsn(X, xi = 0.1, omega = 0.3, alpha = 0.25), type = "l")
abline(v = 0.2)

plot(X, dsn(X, xi = 0.1, omega = 0.3, alpha = 0.5), type = "l")
abline(v = 0.2)

plot(X, dsn(X, xi = 0.1, omega = 0.3, alpha = 0.75), type = "l")
abline(v = 0.2)

plot(X, dsn(X, xi = 0.1, omega = 0.3, alpha = 1), type = "l")
abline(v = 0.2)

plot(X, dsn(X, xi = 0.1, omega = 0.3, alpha = 1.5), type = "l")
abline(v = 0.2)

plot(X, dsn(X, xi = 0.1, omega = 0.3, alpha = 2.0), type = "l")
abline(v = 0.2)

plot(X, dsn(X, xi = 0.1, omega = 0.3, alpha = 5.0), type = "l")
abline(v = 0.2)

####

## snorm -
   # Ranbdom Numbers:
   par(mfrow = c(2, 2))
   set.seed(1953)
   r = rsnorm(n = 1000)
   plot(r, type = "l", main = "snorm", col = "steelblue")
   
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   box()
   x = seq(min(r), max(r), length = 201)
   lines(x, dsnorm(x), lwd = 2)
   
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue",
     ylab = "Probability")
   lines(x, psnorm(x), lwd = 2)
   
   # Compute quantiles:
   round(qsnorm(psnorm(q = seq(-1, 5, by = 1))), digits = 6)


#### sn and fGarch

install.packages("ggplot2")
library(ggplot2)

set.seed(123)
data <- rsnorm(1000, mean = 0, sd = 1, xi = 1.5)
fit <- selm(data ~ 1, family = "SN")
fit@param$dp
snormFit(data)$par

ggplot(data = data.frame(x = data), aes(x = x)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(mapping = aes(colour = "fGarch"), size = 1.2, 
                fun = dsnorm, args = c(mean = as.numeric(snormFit(data)$par["mean"]), 
                                       sd = as.numeric(snormFit(data)$par["sd"]), 
                                       xi = as.numeric(snormFit(data)$par["xi"]))) +
  stat_function(mapping = aes(colour = "sn"), size = 1.2, 
                fun = dsn, args = c(omega = as.numeric(fit@param$dp["omega"]), 
                                    alpha = as.numeric(fit@param$dp["alpha"]), 
                                    xi = as.numeric(fit@param$dp["xi"]))) +
  scale_colour_manual(name = NULL, values = c("blue", "red"))

.libPaths("/home/testuser/Rlib")
install.packages("sn")
library(sn)
# plotting SN densities

x <- seq(-2, 4, length=301)
y1 <- dsn(x, xi=0, omega=1.2, alpha=10)
plot(x, y1, type="l", ylab="density",col="black")
y2 <- dsn(x, 0, 1.2, 5)
lines(x, y2, col="green", lty=2)
y3 <- dsn(x, dp=c(0, 1.2, 2))
lines(x, y3, col="red", lty=2)
y4 <- dsn(x,0,1.2,1.5)
lines(x, y4, col="yellow", lty=2)
y5 <- dsn(x,0,1.2,1)
lines(x, y5, col="blue", lty=2)
y6 <- dsn(x,0,1.2,0.5)
lines(x, y6, col="purple", lty=2)
y7 <- dsn(x,0,1.2,0.25)
lines(x, y7, col="brown", lty=2)
title("SN densities as shape parameter tends to zero")
legend("topright",legend=c("alpha=10","alpha=5","alpha=2","alpha=1.5","alpha=1","alpha=0.5","alpha=0.25"),
        col=c("black","green","red","yellow","blue","purple","brown"),lty=c(1,rep(2,7)))

# switch between parameterizations

cpST <- c(1, 1.5, 1.5, 5.1)

# CP = (mean, st.dev, gamma1, gamma2)

dpST <- cp2dp(cpST, family="ST")
print(dpST)

# back to cpST
# sampling from ST and plotting density

y <- rst(1000, dp=dpST)
hist(y, prob=TRUE, col="gray90")
x <- seq(min(y), max(y), length=301)
pdfST <- dst(x, dp=dpST)
lines(x, pdfST, col=4)
print(sd(y)) # about cpST[2]

# SGN of Arellano-Valle et al. (2004)

wSGN <- function(z, lambda)  z * lambda[1]/sqrt(1 + lambda[2]*z^2)
x    <- seq(-1, 14, length=301)
pdf  <- dSymmModulated(x, 5, 2, f0="normal", G0="normal", w=wSGN,lambda=c(3,5))
plot(x, pdf, type="l")

x <- matrix((1:12)/3, 4, 3)
S <- diag(1:3) + outer(1:3,1:3)/2
wMvTrigs <- function(z, p, q) sin(z %*% p)/(1 + cos(z %*% q))
pdf <- dmSymmModulated(x, xi=1:3, Omega=S, f0="t", G0="logistic",w=wMvTrigs, par.f0=5, par.G0=NULL, p=c(2,3,-2), q=c(1,1, 0))

# plotting when d=2

range <- cbind(c(-4,4), c(-4,4))
plot2D.SymmModulated(range, xi=c(0,0), Omega=S[1:2,1:2],f0="normal", G0="normal", w=wMvTrigs,par.f0=NULL, par.G0=NULL, p=c(1,-3), q=c(1,1), col=4)
y <- rmSymmModulated(2500, xi=c(0,0), Omega=S[1:2,1:2],f0="normal", G0="normal", w=wMvTrigs,par.f0=NULL, par.G0=NULL, p=c(1,-3), q=c(1,1))
points(y, cex=0.3, col="gray60")





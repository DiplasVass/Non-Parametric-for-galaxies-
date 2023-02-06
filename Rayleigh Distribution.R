################################################################################################
#################################### Other Distribution ########################################
################################### RAYLEIGH DISTRIBUTION ######################################

install.packages("extraDistr")
library(extraDistr)

x <- rrayleigh(1e5, 13)
hist(x, 100, freq = FALSE)
curve(drayleigh(x, 13), 0, 60, col = "red", add = TRUE)
hist(prayleigh(x, 13)) 
plot(ecdf(x))
curve(prayleigh(x, 13), 0, 60, col = "red", lwd = 2, add = TRUE) 


Scale <- 2
x <- seq(-1, 8, by = 0.1)
plot(x, drayleigh(x, sigma = Scale), type = "l", ylim = c(0,1),
     las = 1, ylab = "",
     main = "Rayleigh density divided into 10 equal areas; orange = cdf")
abline(h = 0, col = "blue", lty = 2)
qq <- qrayleigh(seq(0.1, 0.9, by = 0.1), sigma = Scale)
lines(qq, drayleigh(qq, sigma = Scale), col = "purple", lty = 3, type = "h")
lines(x, prayleigh(x, sigma = Scale), col = "orange") 


install.packages("VGAM")
library(VGAM)


nn <- 1000
Scale <- exp(2)
rdata <- data.frame(ystar = rrayleigh(nn, scale = Scale))
fit <- vglm(ystar ~ 1, rayleigh, data = rdata, trace = TRUE, crit = "coef")
head(fitted(fit))
with(rdata, mean(ystar))
coef(fit, matrix = TRUE)
Coef(fit)

# Censored data
rdata <- transform(rdata, U = runif(nn, 5, 15))
rdata <- transform(rdata, y = pmin(U, ystar))

par(mfrow = c(1, 2))
hist(with(rdata, ystar)); hist(with(rdata, y)) 

extra <- with(rdata, list(rightcensored = ystar > U))
fit <- vglm(y ~ 1, cens.rayleigh, data = rdata, trace = TRUE,
            extra = extra, crit = "coef")
table(fit@extra$rightcen)
coef(fit, matrix = TRUE)
head(fitted(fit))


########### Distribution Of Rayleigh for many scales parameters ############

###########             Probability Density Plots             ###############


curve(drayleigh(x,0.5,log=FALSE),0,15,col="blue")
curve(drayleigh(x,1,log=FALSE),0,15,col="red",add=TRUE)
curve(drayleigh(x,2,log=FALSE),0,15,col="yellow",add=TRUE)
curve(drayleigh(x,3,log=FALSE),0,15,col="purple",add=TRUE)
curve(drayleigh(x,4,log=FALSE),0,15,col="brown",add=TRUE)
curve(drayleigh(x,5,log=FALSE),0,15,col="green",add=TRUE)
legend("topright",legend=c("ó = 0.5","ó = 1.0","ó = 2.0","ó = 3.0","ó = 4.0","ó = 5.0"),
 	 col=c("blue","red","yellow","purple","brown","green"),lty = rep(1,6))


###########             Cumulative Distribution Plots          ###############

curve(prayleigh(x,0.5,log=FALSE),0,15,col="blue")
curve(prayleigh(x,1,log=FALSE),0,15,col="red",add=TRUE)
curve(prayleigh(x,2,log=FALSE),0,15,col="yellow",add=TRUE)
curve(prayleigh(x,3,log=FALSE),0,15,col="purple",add=TRUE)
curve(prayleigh(x,4,log=FALSE),0,15,col="brown",add=TRUE)
curve(prayleigh(x,5,log=FALSE),0,15,col="green",add=TRUE)
legend("bottomright",legend=c("ó = 0.5","ó = 1.0","ó = 2.0","ó = 3.0","ó = 4.0","ó = 5.0"),
 	 col=c("blue","red","yellow","purple","brown","green"),lty = rep(1,6))




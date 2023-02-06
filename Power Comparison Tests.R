######### Power Comparison Tests #########
############KOLMOGOROV-SMIRNOV############
.libPaths("/home/testuser/Rlib")

reps = 30000

as = c(0.25,0.5,0.75,1.0,1.5,2,5)

n = c(5,10,15,20,25,30,40,50,100)


U = runif(n=5,min=-1,max=1) 

X = replicate(reps,rnorm(5))

Y = replicate(reps,runif(5,-1,1))

Z = X + (1/4)*Y

install.packages("sn")
library(sn)

ks.test(Z,"pnorm")
Dt = D*(sqrt(n) + 0.12 + (0.11/sqrt(n)))


alpha = 0.10
n = c(5,10,15,20,25,30,40,50,100)
as = c(0.25,0.5,0.75,1.0,1.5,2,5)

pct = rep(0,7)

ksPower = function(n) {

for(i in 1:length(as)) {

pct[i] = mean(replicate(reps,ks.test(rsn(n,alpha = as[i]),"pnorm")$p.value)<alpha)

}
return(pct)
}

###ALLIWS ME D statistic

RR5 = rep(0,length(as))
RR10 = rep(0,length(as))
RR15 = rep(0,length(as))
RR20 = rep(0,length(as))
RR25 = rep(0,length(as))
RR30 = rep(0,length(as))
RR40 = rep(0,length(as))
RR50 = rep(0,length(as))
RR100 = rep(0,length(as))

for(i in 1:length(as)) {

DS5 = replicate(reps,ks.test(rsn(5,alpha = as[i]),"pnorm")$statistic)

DS5*(sqrt(5) + 0.12 + (0.11/sqrt(5)))

RR5[i] = length(which(DS5>0.51))/30000
}

for(i in 1:length(as)) {
DS10 = replicate(reps,ks.test(rsn(10,alpha = as[i]),"pnorm")$statistic)

DS5*(sqrt(10) + 0.12 + (0.11/sqrt(10)))

RR10[i] = length(which(DS10>0.368))/30000
}

for(i in 1:length(as)) {
DS15 = replicate(reps,ks.test(rsn(15,alpha = as[i]),"pnorm")$statistic)

DS5*(sqrt(15) + 0.12 + (0.11/sqrt(15)))

RR15[i] = length(which(DS15>0.304))/30000
}

for(i in 1:length(as)) {
DS20 = replicate(reps,ks.test(rsn(20,alpha = as[i]),"pnorm")$statistic)

DS20*(sqrt(20) + 0.12 + (0.11/sqrt(20)))

RR20[i] = length(which(DS20>0.264))/30000
}

for(i in 1:length(as)) {
DS25 = replicate(reps,ks.test(rsn(25,alpha = as[i]),"pnorm")$statistic)

DS25*(sqrt(25) + 0.12 + (0.11/sqrt(25)))

RR25[i] = length(which(DS25>0.24))/30000
}

for(i in 1:length(as)) {
DS30 = replicate(reps,ks.test(rsn(30,alpha = as[i]),"pnorm")$statistic)

DS30*(sqrt(30) + 0.12 + (0.11/sqrt(30)))

RR30[i] = length(which(DS30>0.22))/30000
}

for(i in 1:length(as)) {
DS40 = replicate(reps,ks.test(rsn(40,alpha = as[i]),"pnorm")$statistic)

DS40*(sqrt(40) + 0.12 + (0.11/sqrt(40)))

RR40[i] = length(which(DS40>0.21))/30000
}

for(i in 1:length(as)) {
DS50 = replicate(reps,ks.test(rsn(50,alpha = as[i]),"pnorm")$statistic)

DS50*(sqrt(50) + 0.12 + (0.11/sqrt(50)))

RR50[i] = length(which(DS50>0.172541))/30000
}

for(i in 1:length(as)) {
DS100 = replicate(reps,ks.test(rsn(100,alpha = as[i]),"pnorm")$statistic)

DS100*(sqrt(100) + 0.12 + (0.11/sqrt(100)))

RR100[i] = length(which(DS100>0.122))/30000

}

RR = c(RR5,RR10,RR15,RR20,RR25,RR30,RR40,RR50,RR100)
KSPW = cbind(RR5,RR10,RR15,RR20,RR25,RR30,RR40,RR50,RR100)
colnames(KSPW) = c("n=5","n=10","n=15","n=20","n=25","n=30","n=40","n=50","n=100")
rownames(KSPW) =  c("as=0.25","as=0.5","as=0.75","as=1.0","as=1.5","as=2.0","as=5.0")
KSPW

######################### DIAGRAMMATA ###########################

#########################  FIGURE 2   ###########################

as = c(1/4,1/2,3/4,1,1.5,2,5)
alevel = seq(0,1.0,0.2)
x = rnorm(1)
y = runif(1,-1,1)
z = rep(0,length(as))

for(i in 1:length(as)) {
z[i] = x + as[i]*y
}
z

Q = rep(0,length(z))

for(i in 1:length(z)) {
Q[i] = ks.test(z[i],"pnorm")
}

a1 =  ks.test(z[1],"pnorm")$p.value
a2 =  ks.test(z[2],"pnorm")$p.value
a3 =  ks.test(z[3],"pnorm")$p.value
a4 =  ks.test(z[4],"pnorm")$p.value
a5 =  ks.test(z[5],"pnorm")$p.value
a6 =  ks.test(z[6],"pnorm")$p.value
a7 =  ks.test(z[7],"pnorm")$p.value



ObservedLevel  = c(a1,a2,a3,a4,a5,a6,a7)
as



plot(as,ObservedLevel,ylab="p.value",main="KS TEST p.value in function with ä ")
lines(as,ObservedLevel,col='blue')

###############FIGURE 3#################

par(mfrow=c(3,3))
plot(as,ksPower(5),main='n=5')
lines(as,ksPower(5),col='blue')
plot(as,ksPower(10),main='n=10')
lines(as,ksPower(10),col='blue')
plot(as,ksPower(15),main='n=15')
lines(as,ksPower(15),col='blue')
plot(as,ksPower(20),main='n=20')
lines(as,ksPower(20),col='blue')
plot(as,ksPower(25),main='n=25')
lines(as,ksPower(25),col='blue')
plot(as,ksPower(30),main='n=30')
lines(as,ksPower(30),col='blue')
plot(as,ksPower(40),main='n=40')
lines(as,ksPower(40),col='blue')
plot(as,ksPower(50),main='n=50')
lines(as,ksPower(50),col='blue')
plot(as,ksPower(100),main='n=100')
lines(as,ksPower(100),col='blue')



#################################################################
##################### ANDERSON - DARLING ########################

install.packages("nortest")
library(nortest)

################################
install.packages("sn")
library(sn)
reps =30000

####################################

install.packages("fGarch")
library(fGarch)
al = c(1/4,1/2,3/4,1,1.5,2,5)
n=5

##########################Skewness###################


delta = rep(NA,7)

for(i in 1:length(al)) {

delta[i] = (al[i]/sqrt((al[i])^2 + 1))

}

delta

Mean = rep(NA,length(al))

Mean = delta*sqrt(2/pi) 

Mean


Variance = rep(NA,length(al))

Variance =  1 - Mean^2

Variance

Std = sqrt(Variance)

Std

Skewness = rep(NA,length(al))

Skewness = (((4-pi)/2)*((delta*sqrt(2/pi))^3)/(1 - (2*(delta)^2)/pi ))^(3/2)

Skewness


Kurtosis = rep(NA,length(al))

Kurtosis = (2*pi - 3)*((delta*sqrt(2/pi))^4/(1-(2/pi)*delta^2)^2)

Kurtosis


##If you want to hold the mean around zero with a higher alpha (shape), 
##you will have to decrease other parameters, e.g.: the omega (scale).


paron = 1 - (2*(delta^2)/pi)

omg = 1 / paron 

omga = sqrt(omg)

ksi = - omga*delta*sqrt(2/pi)
ksi


################################################


L = 1:length(al)

adTEST = function(L) {

AD = replicate(reps,ad.test(rsnorm(5,mean = ksi[L],sd = omga[L],xi = al[L]),null="pnorm")$p.value)

return(AD)

}
adTEST(1)
mean(adTEST(6)<0.1)
install.packages("goftest")
library(goftest)
###############################################
#### Kataskeuh Function Gia Euresh Power ######

A = rep(NA,reps)
n = c(5,10,15,20,25,30,40,50,100)
AD = rep(NA,length(al))

ADpwr = function(n)        {

for(j in 1:length(al)) {

Rp = replicate(reps,rsnorm(n,mean = ksi[j],sd = omga[j],xi = al[j]))

for(i in 1:reps) {

A[i] = ad.test(Rp[,i],null = "pnorm")$p.value

AD[j] = mean(A<0.1)

			}
				}
return(AD)                  }

########################################
##### Kataskeuh Pinaka Tvn Power #######

AD5   = rep(0,length(al))
AD10  = rep(0,length(al))
AD15  = rep(0,length(al))
AD20  = rep(0,length(al))
AD25  = rep(0,length(al))
AD30  = rep(0,length(al))
AD40  = rep(0,length(al))
AD50  = rep(0,length(al))
AD100 = rep(0,length(al))

for(j in 1:length(al)) {

ADS5 = replicate(reps,ad.test(rsnorm(5,mean = ksi[j],sd = omga[j],xi = al[j]),"pnorm")$p.value)

AD5[j] = mean(ADS5<0.10)
}

for(j in 1:length(al)) {

ADS10 = replicate(reps,ad.test(rsnorm(10,mean = ksi[j],sd = omga[j],xi = al[j]),"pnorm")$p.value)

AD10[j] = mean(ADS10<0.10)
}

for(j in 1:length(al)) {

ADS15 = replicate(reps,ad.test(rsnorm(15,mean = ksi[j],sd = omga[j],xi = al[j]),"pnorm")$p.value)

AD15[j] = mean(ADS15<0.10)
}

for(j in 1:length(al)) {

ADS20 = replicate(reps,ad.test(rsnorm(20,mean = ksi[j],sd = omga[j],xi = al[j]),"pnorm")$p.value)

AD20[j] = mean(ADS20<0.10)
}

for(j in 1:length(al)) {

ADS25 = replicate(reps,ad.test(rsnorm(25,mean = ksi[j],sd = omga[j],xi = al[j]),"pnorm")$p.value)

AD25[j] = mean(ADS25<0.10)
}


for(j in 1:length(al)) {

ADS30 = replicate(reps,ad.test(rsnorm(30,mean = ksi[j],sd = omga[j],xi = al[j]),"pnorm")$p.value)

AD30[j] = mean(ADS30<0.10)
}

for(j in 1:length(al)) {

ADS40 = replicate(reps,ad.test(rsnorm(40,mean = ksi[j],sd = omga[j],xi = al[j]),"pnorm")$p.value)

AD40[j] = mean(ADS40<0.10)
}

for(j in 1:length(al)) {

ADS50 = replicate(reps,ad.test(rsnorm(50,mean = ksi[j],sd = omga[j],xi = al[j]),"pnorm")$p.value)

AD50[j] = mean(ADS50 < 0.10)
}

for(j in 1:length(al)) {

ADS100 = replicate(reps,ad.test(rsnorm(100,mean = ksi[j],sd = omga[j],xi = al[j]),"pnorm")$p.value)

AD100[j] = mean(ADS100<0.10)
}

AD = c(AD5,AD10,AD15,AD20,AD25,AD30,AD40,AD50,AD100)
ADPW = cbind(AD5,AD10,AD15,AD20,AD25,AD30,AD40,AD50,AD100)
colnames(ADPW) =  c("n=5","n=10","n=15","n=20","n=25","n=30","n=40","n=50","n=100")
rownames(ADPW) =  c("as=0.25","as=0.5","as=0.75","as=1.0","as=1.5","as=2.0","as=5.0")
ADPW

##############################################################################
############################## shape = 1/4 ###################################

############################# n = 5,10,15,20 #################################
n.iter = 30000
par(mfrow=c(5,5))
sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[1],sd = omga[1],xi = al[1]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(5, 10, 15, 20)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



####################### n =  25 , 30 , 40 ###########################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[1],sd = omga[1],xi = al[1]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(25, 30, 40)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



######################## n = 50 , 100 ##############################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[1],sd = omga[1],xi = al[1]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(50 , 100)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



##############################################################################
############################## shape = 1/2 ###################################

############################# n = 5,10,15,20 #####################
n.iter = 30000

sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[2],sd = omga[2],xi = al[2]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(5, 10, 15, 20)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



####################### n =  25 , 30 , 40 ###########################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[2],sd = omga[2],xi = al[2]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(25, 30, 40)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



######################## n = 50 , 100 ##############################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[2],sd = omga[2],xi = al[2]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(50 , 100)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)

##############################################################################
############################## shape = 0.75 ##################################

############################# n = 5,10,15,20 #####################
n.iter = 30000

sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[3],sd = omga[3],xi = al[3]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(5, 10, 15, 20)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



####################### n =  25 , 30 , 40 ###########################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[3],sd = omga[3],xi = al[3]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(25, 30, 40)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



######################## n = 50 , 100 ##############################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[3],sd = omga[3],xi = al[3]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(50 , 100)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)

##############################################################################
############################## shape = 1.0 ###################################

############################# n = 5,10,15,20 #####################
n.iter = 30000

sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[4],sd = omga[4],xi = al[4]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(5, 10, 15, 20)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



####################### n =  25 , 30 , 40 ###########################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[4],sd = omga[4],xi = al[4]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(25, 30, 40)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



######################## n = 50 , 100 ##############################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[4],sd = omga[4],xi = al[4]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(50 , 100)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)

##############################################################################
############################## shape = 1.5 ###################################

############################# n = 5,10,15,20 #####################
n.iter = 30000

sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[5],sd = omga[5],xi = al[5]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(5, 10, 15, 20)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



####################### n =  25 , 30 , 40 ###########################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[5],sd = omga[5],xi = al[5]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(25, 30, 40)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



######################## n = 50 , 100 ##############################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[5],sd = omga[5],xi = al[5]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(50 , 100)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)

##############################################################################
############################## shape = 2.0 ###################################

############################# n = 5,10,15,20 #####################
n.iter = 30000

sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[6],sd = omga[6],xi = al[6]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(5, 10, 15, 20)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



####################### n =  25 , 30 , 40 ###########################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[6],sd = omga[6],xi = al[6]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(25, 30, 40)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



######################## n = 50 , 100 ##############################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[6],sd = omga[6],xi = al[6]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(50 , 100)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)

##############################################################################
############################## shape = 5.0 ###################################

############################# n = 5,10,15,20 #####################
n.iter = 30000

sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[7],sd = omga[7],xi = al[7]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(5, 10, 15, 20)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



####################### n =  25 , 30 , 40 ###########################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[7],sd = omga[7],xi = al[7]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(25, 30, 40)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)



######################## n = 50 , 100 ##############################


sim <- function(rdist, n, n.iter, prefix="",
                breaks=seq(0, 1, length.out=20), alpha=0.1,
                plot=TRUE, ...) {

  # The simulated P-values.
  # NB: The optional arguments "..." are passed to `rdist` to specify
  #     its parameters (if any).
  x <- apply(matrix(rsnorm(n*n.iter,mean = ksi[7],sd = omga[7],xi = al[7]), ncol=n.iter), 2 , function(y) ad.test(y,null="pnorm")$p.value)
             
  # The histogram of P-values, if requested.
  if (plot) {
    power <- mean(x <= alpha)
    round.n <- 1+ceiling(log(1 + n.iter * power * (1-power), base=10) / 2)
    hist(x[x <= max(breaks)], xlab=paste("P value (n=", n, ")", sep=""), 
         breaks=breaks, 
         main=paste(prefix, "(power=", format(power, digits=round.n), ")", sep=""))
    # Specially color the "significant" part of the histogram
    hist(x[x <= alpha], breaks=breaks, col="#e0404080", add=TRUE)
  }

  # Return the array of P-values for any further processing.
  return(x)
}


n.iter <- 30000                 # Number of samples to generate
n.spec <- c(50 , 100)         # Sample sizes to study
par(mfrow=c(1,length(n.spec))) # Organize subsequent plots into a tableau
system.time(
  invisible(sapply(n.spec, function(n) sim(rsn, n, n.iter, prefix="DF = Inf ",alpha=0.1)))
)


#######################################################################
################# Kataskeuh Pinaka Power Comparisons ##################
########### Gia ta Kolmogorov-Smirnov kai Anderson-Darling ############
################# Pou dinontai apo to dosmeno paper  ##################

n  = c(5,10,15,20,25,30,40,50,100)
as = c(0.25,0.5,0.75,1,1.5,2,5)
Test  = c("ANDERSON-DARLING" , "KOLMOGOROV-SMIRNOV")

#### KOLMOGOROV - SMIRNOV #####
KS    = matrix(NA,nrow=7,ncol=9)

KS[1,] = c(13,15,16,18,20,22,26,29,46)
KS[2,] = c(20,27,34,41,47,53,63,72,95)
KS[3,] = c(28,42,55,66,74,81,90,96,100)
KS[4,] = c(35,57,72,83,91,85,99,100,100)
KS[5,] = c(50,79,91,97,99,100,100,100,100)
KS[6,] = c(67,86,97,99,100,100,100,100,100)
KS[7,] = c(75,96,100,100,100,100,100,100,100)
KS

KSPWR = cbind()

par(mfrow=c(3,3))
plot(as,KS[,1],main='n=5',ylab="Frequency")
lines(as,KS[,1],col='blue')
plot(as,KS[,2],main='n=10',ylab="Frequency")
lines(as,KS[,2],col='blue')
plot(as,KS[,3],main='n=15',ylab="Frequency")
lines(as,KS[,3],col='blue')
plot(as,KS[,4],main='n=20',ylab="Frequency")
lines(as,KS[,4],col='blue')
plot(as,KS[,5],main='n=25',ylab="Frequency")
lines(as,KS[,5],col='blue')
plot(as,KS[,6],main='n=30',ylab="Frequency")
lines(as,KS[,6],col='blue')
plot(as,KS[,7],main='n=40',ylab="Frequency")
lines(as,KS[,7],col='blue')
plot(as,KS[,8],main='n=50',ylab="Frequency")
lines(as,KS[,8],col='blue')
plot(as,KS[,9],main='n=100',ylab="Frequency")
lines(as,KS[,9],col='blue')

######## ANDERSON-DARLING ########
AD  = matrix(NA,nrow=7,ncol=9)

AD[,1] = c(15,27,43,59,80,90,100)
AD[,2] = c(17,38,62,81,96,99,100)
AD[,3] = c(19,48,77,92,99,100,100)
AD[,4] = c(21,57,87,97,100,100,100)
AD[,5] = c(24,66,93,99,100,100,100)
AD[,6] = c(27,74,96,100,100,100,100)
AD[,7] = c(32,86,99,100,100,100,100)
AD[,8] = c(37,93,100,100,100,100,100)
AD[,9] = c(64,100,100,100,100,100,100)

AD

par(mfrow=c(3,3))
plot(as,AD[,1],main='n=5',ylab="Frequency")
lines(as,AD[,1],col='blue')
plot(as,AD[,2],main='n=10',ylab="Frequency")
lines(as,AD[,2],col='blue')
plot(as,AD[,3],main='n=15',ylab="Frequency")
lines(as,AD[,3],col='blue')
plot(as,AD[,4],main='n=20',ylab="Frequency")
lines(as,AD[,4],col='blue')
plot(as,AD[,5],main='n=25',ylab="Frequency")
lines(as,AD[,5],col='blue')
plot(as,AD[,6],main='n=30',ylab="Frequency")
lines(as,AD[,6],col='blue')
plot(as,AD[,7],main='n=40',ylab="Frequency")
lines(as,AD[,7],col='blue')
plot(as,AD[,8],main='n=50',ylab="Frequency")
lines(as,AD[,8],col='blue')
plot(as,AD[,9],main='n=100',ylab="Frequency")
lines(as,AD[,9],col='blue')
mtext("Anderson - Darling Rejection Rate")

#####################################

PWR = matrix(NA,nrow=14,ncol=9)

PWR[1,]  = AD[1,]
PWR[2,]  = KS[1,]
PWR[3,]  = AD[2,]
PWR[4,]  = KS[2,]
PWR[5,]  = AD[3,]
PWR[6,]  = KS[3,]
PWR[7,]  = AD[4,]
PWR[8,]  = KS[4,]
PWR[9,]  = AD[5,]
PWR[10,] = KS[5,]
PWR[11,] = AD[6,]
PWR[12,] = KS[6,]
PWR[13,] = AD[7,]
PWR[14,] = KS[7,]

AlphaSkew     = c(0.25,0.25,0.5,0.5,0.75,0.75,1.0,1.0,1.5,1.5,2.0,2.0,5.0,5.0)
rownames(PWR) = c("AD","KS","AD","KS","AD","KS","AD","KS","AD","KS","AD","KS","AD","KS")
colnames(PWR) = c(5,10,15,20,25,30,40,50,100)
PWR

PWRT = cbind(AlphaSkew,PWR)
PWRT


################ Power Comparisons Diagrams ###############

################      Alpha Skew = 0.25      ##############

plot(n, AD[1,] , type="o", col="blue", pch="o", lty=1,xlim=c(0,100),ylab = "Power of Tests" , xlab = "Sample Size")
title("Power Comparisons of Kolmogorov-Smirnov and Anderson-Darling tests for shape = 0.25")
# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(n , KS[1,], col="red", pch="*")
lines(n , KS[1,], col="red",lty=2)
# Adding a legend inside box at the location in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(1,60,legend=c("AD","KS"), col=c("blue","red"),pch=c("o","*"),lty=c(1,2), ncol=1)


################      Alpha Skew = 0.50      ##############

plot(n, AD[2,] , type="o", col="blue", pch="o", lty=1,xlim=c(0,100),ylab = "Power of Tests" , xlab = "Sample Size")
title("Power Comparisons of Kolmogorov-Smirnov and Anderson-Darling tests for shape = 0.50")
# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(n , KS[2,], col="red", pch="*")
lines(n , KS[2,], col="red",lty=2)
# Adding a legend inside box at the location in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(1,100,legend=c("AD","KS"), col=c("blue","red"),pch=c("o","*"),lty=c(1,2), ncol=1)

################      Alpha Skew = 0.75      ##############

plot(n, AD[3,] , type="o", col="blue", pch="o", lty=1,xlim=c(0,100),ylab = "Power of Tests" , xlab = "Sample Size")
title("Power Comparisons of Kolmogorov-Smirnov and Anderson-Darling tests for shape = 0.75")
# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(n , KS[3,], col="red", pch="*")
lines(n , KS[3,], col="red",lty=2)
# Adding a legend inside box at the location in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(1,100,legend=c("AD","KS"), col=c("blue","red"),pch=c("o","*"),lty=c(1,2), ncol=1)

################      Alpha Skew = 1.00      ##############

plot(n, AD[4,] , type="o", col="blue", pch="o", lty=1,xlim=c(0,100),ylab = "Power of Tests" , xlab = "Sample Size")
title("Power Comparisons of Kolmogorov-Smirnov and Anderson-Darling tests for shape = 1.00")
# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(n , KS[4,], col="red", pch="*")
lines(n , KS[4,], col="red",lty=2)
# Adding a legend inside box at the location in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(1,100,legend=c("AD","KS"), col=c("blue","red"),pch=c("o","*"),lty=c(1,2), ncol=1)

################      Alpha Skew = 1.50      ##############

plot(n, AD[5,] , type="o", col="blue", pch="o", lty=1,xlim=c(0,100),ylab = "Power of Tests" , xlab = "Sample Size")
title("Power Comparisons of Kolmogorov-Smirnov and Anderson-Darling tests for shape = 1.50")
# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(n , KS[5,], col="red", pch="*")
lines(n , KS[5,], col="red",lty=2)
# Adding a legend inside box at the location in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(80,96,legend=c("AD","KS"), col=c("blue","red"),pch=c("o","*"),lty=c(1,2), ncol=1)

################      Alpha Skew = 2.00      ##############

plot(n, AD[6,] , type="o", col="blue", pch="o", lty=1,xlim=c(0,100),ylab = "Power of Tests" , xlab = "Sample Size")
title("Power Comparisons of Kolmogorov-Smirnov and Anderson-Darling tests for shape = 2.00")
# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(n , KS[6,], col="red", pch="*")
lines(n , KS[6,], col="red",lty=2)
# Adding a legend inside box at the location in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(80,96,legend=c("AD","KS"), col=c("blue","red"),pch=c("o","*"),lty=c(1,2), ncol=1)

################      Alpha Skew = 5.00      ##############

plot(n, AD[7,] , type="o", col="blue", pch="o", lty=1,xlim=c(0,100),ylim=c(75,100),ylab = "Power of Tests" , xlab = "Sample Size")
title("Power Comparisons of Kolmogorov-Smirnov and Anderson-Darling tests for shape = 5.00")
# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(n , KS[7,], col="red", pch="*")
lines(n , KS[7,], col="red",lty=2)
# Adding a legend inside box at the location in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(80,96,legend=c("AD","KS"), col=c("blue","red"),pch=c("o","*"),lty=c(1,2), ncol=1)








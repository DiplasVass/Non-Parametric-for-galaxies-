#########################################################################################
########################   S Gapper and Canonical RMS Estimator  ########################

.libPaths("/home/testuser/Rlib")
install.packages("RColorBrewer")
library(RColorBrewer)
colsSG = brewer.pal(6, "Paired")
palSG = colorRampPalette(colsSG)

########################   S Gapper     Estimator  ##############################

#############################     n = 5    ######################################

n=5
reps = 30000

X5 = replicate(reps,rnorm(n,1,100))

X5

Xs5 = matrix(NA, nrow = n , ncol = reps)

for(i in 1:reps) {

Xs5[,i] = sort(X5[,i])
}

Xs5

Q5 = sqrt(pi)/(n*(n-1))

Sum5 = matrix(NA,nrow=n-1,ncol=reps)

for(j in 1:reps)       {

for(i in 1:n-1) { 

Sum5[i,j] = i*(n-i)*(Xs5[i+1,j]-Xs5[i,j])

			}

				}

Sgapper5 = Q5*colSums(Sum5)

h5 = hist(Sgapper5,nclass=60,xlab = " s intrinsic (n = 5) " , ylab = "count",col= palSG(1),ylim=c(0,2000),xlim=c(0,300))


#############################     n = 15    ######################################

n=15
reps = 30000

X15 = replicate(reps,rnorm(n,1,100))

X15

Xs15 = matrix(NA, nrow = n , ncol = reps)

for(i in 1:reps) {

Xs15[,i] = sort(X15[,i])
}

Xs15

Q15 = sqrt(pi)/(n*(n-1))

Sum15 = matrix(NA,nrow=n-1,ncol=reps)

for(j in 1:reps)       {

for(i in 1:n-1) { 

Sum15[i,j] = i*(n-i)*(Xs15[i+1,j]-Xs15[i,j])

			}

				}

Sgapper15 = Q15*colSums(Sum15)

h15 = hist(Sgapper15,nclass=30,xlab = " s intrinsic (n = 15) " , ylab = "count",col= palSG(1),ylim=c(0,3500),xlim=c(0,200))

#############################     n = 20    ######################################

n=20
reps = 30000

X20 = replicate(reps,rnorm(n,1,100))

X20

Xs20 = matrix(NA, nrow = n , ncol = reps)

for(i in 1:reps) {

Xs20[,i] = sort(X20[,i])
}

Xs20

Q20 = sqrt(pi)/(n*(n-1))

Sum20 = matrix(NA,nrow=n-1,ncol=reps)

for(j in 1:reps)       {

for(i in 1:n-1) { 

Sum20[i,j] = i*(n-i)*(Xs20[i+1,j]- Xs20[i,j])

			}

				}

Sgapper20 = Q20*colSums(Sum20)

h20 = hist(Sgapper20,nclass=30,xlab = " s intrinsic (n = 20) " , ylab = "count",col=palSG(1),ylim=c(0,4000),xlim=c(0,200))

#############################     n = 50    ######################################

n=50
reps = 30000

X50 = replicate(reps,rnorm(n,1,100))

X50

Xs50 = matrix(NA, nrow = n , ncol = reps)

for(i in 1:reps) {

Xs50[,i] = sort(X50[,i])
}

Xs50

Q50 = sqrt(pi)/(n*(n-1))

Sum50 = matrix(NA,nrow=n-1,ncol=reps)

for(j in 1:reps)       {

for(i in 1:n-1) { 

Sum50[i,j] = i*(n-i)*(Xs50[i+1,j]- Xs50[i,j])

			}

				}

Sgapper50 = Q50*colSums(Sum50)

h50 = hist(Sgapper50,nclass=20,xlab = " s intrinsic (n = 50) " , ylab = "count",col= palSG(1),ylim=c(0,6000),xlim=c(0,200))

#####################################################################################
############################# All the histograms ####################################

par(mfrow=c(2,2))

hist(Sgapper5,nclass=60,xlab = " s intrinsic (n = 5) " , ylab = "count",col= palSG(1),ylim=c(0,2000),xlim=c(0,300))
hist(Sgapper15,nclass=30,xlab = " s intrinsic (n = 15) " , ylab = "count",col= palSG(1),ylim=c(0,3500),xlim=c(0,200))
hist(Sgapper20,nclass=30,xlab = " s intrinsic (n = 20) " , ylab = "count",col=palSG(1),ylim=c(0,4000),xlim=c(0,200))
hist(Sgapper50,nclass=20,xlab = " s intrinsic (n = 50) " , ylab = "count",col= palSG(1),ylim=c(0,6000),xlim=c(0,200))


##############  Canonical RMS Standard Deviation   Estimator  ###################

#############################     n = 5    ######################################

n=5
reps = 30000

X5 = replicate(reps,rnorm(n,1,100))

SD5 = rep(NA,reps)
	
for(i in 1:reps) {

SD5[i] = sd(X5[,i])

}

breaks5 = seq(0,300,by=6)

H5 = hist(SD5,breaks5,ylim=c(0,2000),xlab = " s intrinsic (n = 5) " , ylab = "count",col= rgb(1,0,0,0.5),xlim=c(0,300))


#############################     n = 15    #####################################

n=15
reps = 30000

X15 = replicate(reps,rnorm(n,1,100))

SD15 = rep(NA,reps)
	
for(i in 1:reps) {

SD15[i] = sd(X15[,i])

}

breaks15 = seq(0,300,by=5.5)

H15 = hist(SD15,breaks15,ylim=c(0,3500),xlab = " s intrinsic (n = 15) " , ylab = "count",col= rgb(1,0,0,0.5),xlim=c(0,200))


#############################     n = 20    #####################################

n=20
reps = 30000

X20 = replicate(reps,rnorm(n,1,100))

SD20 = rep(NA,reps)
	
for(i in 1:reps) {

SD20[i] = sd(X20[,i])

}

breaks20 = seq(0,300,by=5.5)

H20 = hist(SD20,breaks20,ylim=c(0,4000),xlab = " s intrinsic (n = 20) " , ylab = "count",col= rgb(1,0,0,0.5),xlim=c(0,200))


#############################     n = 50    #####################################

n=50
reps = 30000

X50 = replicate(reps,rnorm(n,1,100))

SD50 = rep(NA,reps)
	
for(i in 1:reps) {

SD50[i] = sd(X50[,i])

}

breaks50 = seq(0,300,by=5)

H50 = hist(SD50,breaks50,ylim=c(0,6000),xlab = " s intrinsic (n = 20) " , ylab = "count",col= rgb(1,0,0,0.5),xlim=c(0,200))

#####################################################################################
############################# All the histograms ####################################

par(mfrow=c(2,2))

hist(SD5,breaks5,ylim=c(0,2000),xlab = " s intrinsic (n = 5) " , ylab = "count",col= rgb(1,0,0,0.5),xlim=c(0,300))
hist(SD15,breaks15,ylim=c(0,3500),xlab = " s intrinsic (n = 15) " , ylab = "count",col= rgb(1,0,0,0.5),xlim=c(0,200))
hist(SD20,breaks20,ylim=c(0,4000),xlab = " s intrinsic (n = 20) " , ylab = "count",col= rgb(1,0,0,0.5),xlim=c(0,200))
hist(SD50,breaks50,ylim=c(0,6000),xlab = " s intrinsic (n = 20) " , ylab = "count",col= rgb(1,0,0,0.5),xlim=c(0,200))


###################################################################################
########################### Comparisons of Estimators #############################

################################### n = 5 #########################################


par(mfrow=c(2,2))

hist(Sgapper5,nclass=60,main='',xlab = " s intrinsic (n = 5) " , ylab = "count",col='black',
     ylim=c(0,2000),xlim=c(0,300))
hist(SD5,breaks5,ylim=c(0,2000),add=TRUE,xlab = " s intrinsic (n = 5) " , ylab = "count",
       col= rgb(0,0,1,0.5),xlim=c(0,300))
legend("topright", legend=c("S Gapper", "S RMS"),col=c("black", "blue"),lty=c(1,1) , cex=0.8)
title("Comparisons of Parameters Estimators")



hist(Sgapper15,nclass=30,main='',xlab = " s intrinsic (n = 15) " , ylab = "count",col= 'black',ylim=c(0,3500),xlim=c(0,200))
hist(SD15,breaks15,add=TRUE,ylim=c(0,3500),xlab = " s intrinsic (n = 15) " , ylab = "count",col= rgb(0,0,1,0.5),xlim=c(0,200))
legend("topright", legend=c("S Gapper", "S RMS"),col=c("black", "blue"),lty=c(1,1) , cex=0.8)
title("Comparisons of Parameters Estimators")



hist(Sgapper20,nclass=30,main='',xlab = " s intrinsic (n = 20) " , ylab = "count",col='black',ylim=c(0,4000),xlim=c(0,200))
hist(SD20,breaks20,add=TRUE,ylim=c(0,4000),xlab = " s intrinsic (n = 20) " , ylab = "count",col= rgb(0,0,1,0.5),xlim=c(0,200))
legend("topright", legend=c("S Gapper", "S RMS"),col=c("black", "blue"),lty=c(1,1) , cex=0.8)
title("Comparisons of Parameters Estimators")



hist(Sgapper50,nclass=20,main='',xlab = " s intrinsic (n = 50) " , ylab = "count",col='black',ylim=c(0,6000),xlim=c(0,200))
hist(SD50,breaks50,add=TRUE,ylim=c(0,6000),xlab = " s intrinsic (n = 50) " , ylab = "count",col= rgb(0,0,1,0.5),xlim=c(0,200))
legend("topright", legend=c("S Gapper", "S RMS"),col=c("black", "blue"),lty=c(1,1) , cex=0.8)
title("Comparisons of Parameters Estimators")





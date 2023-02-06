.libPaths("/home/testuser/Rlib")
install.packages("nortest")
library(nortest)
reps = 30000
###################### n=5 ######################

n=5

P5 = rep(NA,30000)

M5 = replicate(reps,rnorm(5))

for(i in 1:ncol(M5)) {

P5[i] = pearson.test(M5[,i])$statistic
}

PT5 = as.numeric(P5)

hist(PT5,nclass=30)

###################### n=10 ######################

n=10

P10 = rep(NA,30000)

M10 = replicate(reps,rnorm(10))

for(i in 1:ncol(M10)) {

P10[i] = pearson.test(M10[,i])$statistic
}

PT10 = as.numeric(P10)

hist(PT10,nclass=30)

###################### n=15 ######################

n=15

P15 = rep(NA,30000)

M15 = replicate(reps,rnorm(15))

for(i in 1:ncol(M15)) {

P15[i] = pearson.test(M15[,i])$statistic
}

PT15 = as.numeric(P15)

hist(PT15,nclass=30)

###################### n=20 ######################

n=20

P20 = rep(NA,30000)

M20 = replicate(reps,rnorm(20))

for(i in 1:ncol(M20)) {

P20[i] = pearson.test(M20[,i])$statistic
}

PT20 = as.numeric(P20)

hist(PT20,nclass=30)

###################### n=25 ######################

n=25

P25 = rep(NA,30000)

M25 = replicate(reps,rnorm(25))

for(i in 1:ncol(M25)) {

P25[i] = pearson.test(M25[,i])$statistic
}

PT25 = as.numeric(P25)

hist(PT25,nclass=30)

###################### n=30 ######################

n=30

P30 = rep(NA,30000)

M30 = replicate(reps,rnorm(30))

for(i in 1:ncol(M30)) {

P30[i] = pearson.test(M30[,i])$statistic
}

PT30 = as.numeric(P30)

hist(PT30,nclass=30)

###################### n=35 ######################

n=35

P35 = rep(NA,30000)

M35 = replicate(reps,rnorm(35))

for(i in 1:ncol(M35)) {

P35[i] = pearson.test(M35[,i])$statistic
}

PT35 = as.numeric(P35)

hist(PT35,nclass=30)

###################### n=40 ######################

n=40

P40 = rep(NA,30000)

M40 = replicate(reps,rnorm(40))

for(i in 1:ncol(M40)) {

P40[i] = pearson.test(M40[,i])$statistic
}

PT40 = as.numeric(P40)

hist(PT40,nclass=30)

###################### n=50 ######################

n=50

P50 = rep(NA,30000)

M50 = replicate(reps,rnorm(50))

for(i in 1:ncol(M50)) {

P50[i] = pearson.test(M50[,i])$statistic
}

PT50 = as.numeric(P50)

hist(PT50,nclass=30)

#######################################################
###################### ALL TOGETHER ###################

par(mfrow=c(3,3))
H1 = hist(PT5,nclass=30,col="white",lty=0,main="n=5",xlab="X2/DOF",ylab="Number in Bin")
lines(c(H1$breaks, max(H1$breaks)),
       c(0,H1$counts,0), type='S')
H2 = hist(PT10,nclass=30,col="white",lty=0,main="n=10",xlab="X2/DOF",ylab="Number in Bin")
lines(c(H2$breaks, max(H2$breaks)),
       c(0,H2$counts,0), type='S')
H3 = hist(PT15,nclass=30,col="white",lty=0,main="n=15",xlab="X2/DOF",ylab="Number in Bin")
lines(c(H3$breaks, max(H3$breaks)),
       c(0,H3$counts,0), type='S')
H4 = hist(PT20,nclass=30,col="white",lty=0,main="n=20",xlab="X2/DOF",ylab="Number in Bin")
lines(c(H4$breaks, max(H4$breaks)),
       c(0,H4$counts,0), type='S')
H5 = hist(PT25,nclass=30,col="white",lty=0,main="n=25",xlab="X2/DOF",ylab="Number in Bin")
lines(c(H5$breaks, max(H5$breaks)),
       c(0,H5$counts,0), type='S')
H6 = hist(PT30,nclass=30,col="white",lty=0,main="n=30",xlab="X2/DOF",ylab="Number in Bin")
lines(c(H6$breaks, max(H6$breaks)),
       c(0,H6$counts,0), type='S')
H7 = hist(PT35,nclass=30,col="white",lty=0,main="n=35",xlab="X2/DOF",ylab="Number in Bin")
lines(c(H7$breaks, max(H7$breaks)),
       c(0,H7$counts,0), type='S')
H8 = hist(PT40,nclass=30,col="white",lty=0,main="n=40",xlab="X2/DOF",ylab="Number in Bin")
lines(c(H8$breaks, max(H8$breaks)),
       c(0,H8$counts,0), type='S')
H9 = hist(PT50,nclass=30,col="white",lty=0,main="n=50",xlab="X2/DOF",ylab="Number in Bin")
lines(c(H9$breaks, max(H9$breaks)),
       c(0,H9$counts,0), type='S')




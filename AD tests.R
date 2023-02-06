
###################n=5######################
n=5
reps = 30000

Y5 = replicate(reps,rnorm(n))

X5= matrix(0,nrow=5,ncol=30000)

for(i in 1:reps) {

X5[,i] = sort(Y5[,i])

}

F5 = pnorm(X5)

S5 = matrix(nrow=5,ncol=reps) ### THELW NA VRW STATISTIKA ANA STHLH

for(i in 1:ncol(X5)) {

for(j in 1:n)  {

S5[j,i] = ((2*j-1)/n)*(log(F5[j,i]) + log(1 - F5[n+1-j,i]))

}
}

 
SS5 = colSums(S5)
Astat5 = -n - SS5


hist(Astat5,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=5")
abline(v=1.933,col="blue",lty=2)

################n=10####################
n = 10
reps=30000
Y10 = replicate(reps,rnorm(10))

X10= matrix(0,nrow=10,ncol=30000)

for(i in 1:reps) {

X10[,i] = sort(Y10[,i])

}

F10 = pnorm(X10)

S10 = matrix(nrow=10,ncol=reps)

for(i in 1:ncol(X10)) {

for(j in 1:n)  {

S10[j,i] = ((2*j-1)/n)*(log(F10[j,i]) + log(1 - F10[n+1-j,i]))

}
}
 
SS10 = colSums(S10)
Astat10 = -n - SS10


hist(Astat10,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=10")
abline(v=1.933,col="blue",lty=2)

################n=15####################
n = 15
reps=30000
Y15 = replicate(reps,rnorm(15))

X15= matrix(0,nrow=15,ncol=30000)

for(i in 1:reps) {

X15[,i] = sort(Y15[,i])

}

F15 = pnorm(X15)

S15 = matrix(nrow=15,ncol=reps)

for(i in 1:ncol(X15)) {

for(j in 1:n)  {

S15[j,i] = ((2*j-1)/n)*(log(F15[j,i]) + log(1 - F15[n+1-j,i]))

}
}
 
SS15 = colSums(S15)
Astat15 = -n - SS15


hist(Astat15,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=15")
abline(v=1.933,col="blue",lty=2)

################n=20####################
n = 20
reps=30000
Y20 = replicate(reps,rnorm(20))

X20= matrix(0,nrow=20,ncol=30000)

for(i in 1:reps) {

X20[,i] = sort(Y20[,i])

}

F20 = pnorm(X20)

S20 = matrix(nrow=20,ncol=reps)

for(i in 1:ncol(X20)) {

for(j in 1:n)  {

S20[j,i] = ((2*j-1)/n)*(log(F20[j,i]) + log(1 - F20[n+1-j,i]))

}
}
 
SS20 = colSums(S20)
Astat20 = -n - SS20


hist(Astat20,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=20")
abline(v=1.933,col="blue",lty=2)

################n=25####################
n = 25
reps=30000
Y25 = replicate(reps,rnorm(25))

X25= matrix(0,nrow=25,ncol=30000)

for(i in 1:reps) {

X25[,i] = sort(Y25[,i])

}

F25 = pnorm(X25)

S25 = matrix(nrow=25,ncol=reps)

for(i in 1:ncol(X25)) {

for(j in 1:n)  {

S25[j,i] = ((2*j-1)/n)*(log(F25[j,i]) + log(1 - F25[n+1-j,i]))

}
}
 
SS25 = colSums(S25)
Astat25 = -n - SS25


hist(Astat25,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=25")
abline(v=1.933,col="blue",lty=2)

################n=30####################
n = 30
reps=30000
Y30 = replicate(reps,rnorm(30))

X30= matrix(0,nrow=30,ncol=30000)

for(i in 1:reps) {

X30[,i] = sort(Y30[,i])

}

F30 = pnorm(X30)

S30 = matrix(nrow=30,ncol=reps)

for(i in 1:ncol(X30)) {

for(j in 1:n)  {

S30[j,i] = ((2*j-1)/n)*(log(F30[j,i]) + log(1 - F30[n+1-j,i]))

}
}
 
SS30 = colSums(S30)
Astat30 = -n - SS30


hist(Astat30,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=30")
abline(v=1.933,col="blue",lty=2)

################n=35####################
n = 35
reps=30000
Y35 = replicate(reps,rnorm(35))

X35= matrix(0,nrow=35,ncol=30000)

for(i in 1:reps) {

X35[,i] = sort(Y35[,i])

}

F35 = pnorm(X35)

S35 = matrix(nrow=35,ncol=reps)

for(i in 1:ncol(X35)) {

for(j in 1:n)  {

S35[j,i] = ((2*j-1)/n)*(log(F35[j,i]) + log(1 - F35[n+1-j,i]))

}
}
 
SS35 = colSums(S35)
Astat35 = -n - SS35


hist(Astat35,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=35")
abline(v=1.933,col="blue",lty=2)

################n=40####################
n = 40
reps=30000
Y40 = replicate(reps,rnorm(40))

X40= matrix(0,nrow=40,ncol=30000)

for(i in 1:reps) {

X40[,i] = sort(Y40[,i])

}

F40 = pnorm(X40)

S40 = matrix(nrow=40,ncol=reps)

for(i in 1:ncol(X40)) {

for(j in 1:n)  {

S40[j,i] = ((2*j-1)/n)*(log(F40[j,i]) + log(1 - F40[n+1-j,i]))

}
}
 
SS40 = colSums(S40)
Astat40 = -n - SS40


hist(Astat40,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=40")
abline(v=1.933,col="blue",lty=2)

################n=50####################
n = 50
reps=30000
Y50 = replicate(reps,rnorm(50))

X50= matrix(0,nrow=50,ncol=30000)

for(i in 1:reps) {

X50[,i] = sort(Y50[,i])

}

F50 = pnorm(X50)

S50 = matrix(nrow=50,ncol=reps)

for(i in 1:ncol(X50)) {

for(j in 1:n)  {

S50[j,i] = ((2*j-1)/n)*(log(F50[j,i]) + log(1 - F50[n+1-j,i]))

}
}
 
SS50 = colSums(S50)
Astat50 = -n - SS50


hist(Astat50,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=50")
abline(v=1.933,col="blue",lty=2)


###################### OLA TA HISTOGRAMMATA ######################

par(mfrow=c(3,3))
h1 = hist(Astat5,nclass=50,xlim=c(0,5),xlab= "AD statistic"  , ylab="Number In Bin" ,main="n=5",col="white",lty=0)
lines(c(h1$breaks, max(h1$breaks)),
       c(0,h1$counts,0), type='S')
abline(v=1.933,col="blue",lwd = 2 ,lty=2)
h2 = hist(Astat10,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=10",col="white",lty=0)
lines(c(h2$breaks, max(h2$breaks)),
       c(0,h2$counts,0), type='S')
abline(v=1.933,col="blue",lwd = 2 ,lty=2)
h3 = hist(Astat15,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=15",col="white",lty=0)
lines(c(h3$breaks, max(h3$breaks)),
       c(0,h3$counts,0), type='S')
abline(v=1.933,col="blue",lwd = 2 ,lty=2)
h4 = hist(Astat20,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=20",col="white",lty=0)
lines(c(h4$breaks, max(h4$breaks)),
       c(0,h4$counts,0), type='S')
abline(v=1.933,col="blue",lwd = 2 ,lty=2)
h5 = hist(Astat25,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=25",col="white",lty=0)
lines(c(h5$breaks, max(h5$breaks)),
       c(0,h5$counts,0), type='S')
abline(v=1.933,col="blue",lwd = 2 ,lty=2)
h6 = hist(Astat30,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=30",col="white",lty=0)
lines(c(h6$breaks, max(h6$breaks)),
       c(0,h6$counts,0), type='S')
abline(v=1.933,col="blue",lwd = 2 ,lty=2)
h7 = hist(Astat35,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=35",col="white",lty=0)
lines(c(h7$breaks, max(h7$breaks)),
       c(0,h7$counts,0), type='S')
abline(v=1.933,col="blue",lwd = 2 ,lty=2)
h8 = hist(Astat40,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=40",col="white",lty=0)
lines(c(h8$breaks, max(h8$breaks)),
       c(0,h8$counts,0), type='S')
abline(v=1.933,col="blue",lwd = 2 ,lty=2)
h9 = hist(Astat50,nclass=50,xlim=c(0,5),xlab= "AD statistic" , ylab="Number In Bin",main="n=50",col="white",lty=0)
lines(c(h9$breaks, max(h9$breaks)),
       c(0,h9$counts,0), type='S')
abline(v=1.933,col="blue",lwd = 2 ,lty=2)


#################### TESTS ANDERSON - DARLING ####################

install.packages("nortest")
library(nortest)

ad.test(Y5)
ad.test(Y10)
ad.test(Y15)
ad.test(Y20)
ad.test(Y25)
ad.test(Y30)
ad.test(Y35)
ad.test(Y40)
ad.test(Y50)


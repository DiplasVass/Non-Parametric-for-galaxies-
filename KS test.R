
########################n=5#######################
n=5
x5 = rnorm(5)
reps = 30000
Z5 = replicate(reps,rnorm(5))

set.seed(1)
i = 1:n
u = i/n 
um1 = (i-1)/n

D5 = rep(0,30000)

for(i in 1:ncol(Z5)) {

t = Z5[,i]
T = sort(t)
F = pnorm(T,mean=0,sd=1)
D5[i] = max(F - um1 , u - F)

end }

Dtilde5 = D5*(sqrt(n) + 0.12 + (0.11)/sqrt(n))


h1 = hist(Dtilde5, nclass=30,ylim=c(0,3000))
abline(v = 1.226, col = "blue", lwd = 2)

########################n=10#####################

n = 10
x10 = rnorm(10)
reps = 30000
Z10 = replicate(reps,rnorm(10))

set.seed(1)
i = 1:n
u = i/n 
um1 = (i-1)/n

D10 = rep(0,30000)

for(i in 1:ncol(Z10)) {

t = Z10[,i]
T = sort(t)
F = pnorm(T,mean=0,sd=1)
D10[i] = max(F - um1 , u - F)

end }

Dtilde10 = D10*(sqrt(n) + 0.12 + (0.11)/sqrt(n))


h2 = hist(Dtilde10, nclass=30,ylim=c(0,3000))
abline(v = 1.226, col = "blue", lwd = 2)


########################n=15#####################

n = 15
x15 = rnorm(15)
reps = 30000
Z15 = replicate(reps,rnorm(15))

set.seed(1)
i = 1:n
u = i/n 
um1 = (i-1)/n

D15 = rep(0,30000)

for(i in 1:ncol(Z15)) {

t = Z15[,i]
T = sort(t)
F = pnorm(T,mean=0,sd=1)
D15[i] = max(F - um1 , u - F)

end }

Dtilde15 = D15*(sqrt(n) + 0.12 + (0.11)/sqrt(n))


h3 = hist(Dtilde15, nclass=30,ylim=c(0,3000))
abline(v = 1.226, col = "blue", lwd = 2)



########################n=20#####################

n = 20
x20 = rnorm(20)
reps = 30000
Z20 = replicate(reps,rnorm(20))

set.seed(1)
i = 1:n
u = i/n 
um1 = (i-1)/n

D20 = rep(0,30000)

for(i in 1:ncol(Z20)) {

t = Z20[,i]
T = sort(t)
F = pnorm(T,mean=0,sd=1)
D20[i] = max(F - um1 , u - F)

end }

Dtilde20 = D20*(sqrt(n) + 0.12 + (0.11)/sqrt(n))


h4 = hist(Dtilde20, nclass=30,ylim=c(0,3000))
abline(v = 1.226, col = "blue", lwd = 2)



########################n=25#####################

n = 25
x25 = rnorm(25)
reps = 30000
Z25 = replicate(reps,rnorm(25))

set.seed(1)
i = 1:n
u = i/n 
um1 = (i-1)/n

D25 = rep(0,30000)

for(i in 1:ncol(Z25)) {

t = Z25[,i]
T = sort(t)
F = pnorm(T,mean=0,sd=1)
D25[i] = max(F - um1 , u - F)

end }

Dtilde25 = D25*(sqrt(n) + 0.12 + (0.11)/sqrt(n))


h5 = hist(Dtilde25, nclass=30,ylim=c(0,3000))
abline(v = 1.226, col = "blue", lwd = 2)


########################n=30#####################

n = 30
x30 = rnorm(30)
reps = 30000
Z30 = replicate(reps,rnorm(30))

set.seed(1)
i = 1:n
u = i/n 
um1 = (i-1)/n

D30 = rep(0,30000)

for(i in 1:ncol(Z30)) {

t = Z30[,i]
T = sort(t)
F = pnorm(T,mean=0,sd=1)
D30[i] = max(F - um1 , u - F)

end }

Dtilde30 = D30*(sqrt(n) + 0.12 + (0.11)/sqrt(n))


h6 = hist(Dtilde30, nclass=30,ylim=c(0,3000))
abline(v = 1.226, col = "blue", lwd = 2)


########################n=35#####################

n = 35
x35 = rnorm(35)
reps = 30000
Z35 = replicate(reps,rnorm(35))

set.seed(1)
i = 1:n
u = i/n 
um1 = (i-1)/n

D35 = rep(0,30000)

for(i in 1:ncol(Z35)) {

t = Z35[,i]
T = sort(t)
F = pnorm(T,mean=0,sd=1)
D35[i] = max(F - um1 , u - F)

end }

Dtilde35 = D35*(sqrt(n) + 0.12 + (0.11)/sqrt(n))


h7 = hist(Dtilde35, nclass=60,ylim=c(0,3000))
abline(v = 1.226, col = "blue", lwd = 2)


########################n=40#####################

n = 40
x40 = rnorm(40)
reps = 30000
Z40 = replicate(reps,rnorm(40))

set.seed(1)
i = 1:n
u = i/n 
um1 = (i-1)/n

D40 = rep(0,30000)

for(i in 1:ncol(Z40)) {

t = Z40[,i]
T = sort(t)
F = pnorm(T,mean=0,sd=1)
D40[i] = max(F - um1 , u - F)

end }

Dtilde40 = D40*(sqrt(n) + 0.12 + (0.11)/sqrt(n))


h8 = hist(Dtilde40, nclass=30,ylim=c(0,3000))
abline(v = 1.226, col = "blue", lwd = 2)


########################n=50#####################

n = 50
x50 = rnorm(50)
reps = 30000
Z50 = replicate(reps,rnorm(50))

set.seed(1)
i = 1:n
u = i/n 
um1 = (i-1)/n

D50 = rep(0,30000)

for(i in 1:ncol(Z50)) {

t = Z50[,i]
T = sort(t)
F = pnorm(T,mean=0,sd=1)
D50[i] = max(F - um1 , u - F)

end }

Dtilde50 = D50*(sqrt(n) + 0.12 + (0.11)/sqrt(n))


h5 = hist(Dtilde50, nclass=30,ylim=c(0,3000))
abline(v = 1.226, col = "blue", lwd = 2)



################ola ta histograms################

par(mfrow=c(3,3))
h1 = hist(Dtilde5, nclass=30,ylim=c(0,3000),xlab="D*",ylab="Number In Bin" ,main = "n=5",col="white",lty=0)
lines(c(h1$breaks, max(h1$breaks)),
       c(0,h1$counts,0), type='S')
abline(v = 1.226, col = "blue", lwd = 2 , lty = 2)
h2 = hist(Dtilde10, nclass=30,ylim=c(0,3000),xlab="D*",ylab="Number In Bin",main = "n=10",col="white",lty=0)
lines(c(h2$breaks, max(h2$breaks)),
       c(0,h2$counts,0), type='S')
abline(v = 1.226, col = "blue", lwd = 2 , lty = 2)
h3 = hist(Dtilde15, nclass=30,ylim=c(0,3000),xlab="D*",ylab="Number In Bin",main = "n=15",col="white",lty=0)
lines(c(h3$breaks, max(h3$breaks)),
       c(0,h3$counts,0), type='S')
abline(v = 1.226, col = "blue", lwd = 2 , lty = 2)
h4 = hist(Dtilde20, nclass=30,ylim=c(0,3000),xlab="D*",ylab="Number In Bin",main = "n=20",col="white",lty=0)
lines(c(h4$breaks, max(h4$breaks)),
       c(0,h4$counts,0), type='S')
abline(v = 1.226, col = "blue", lwd = 2 , lty = 2)
h5 = hist(Dtilde25, nclass=30,ylim=c(0,3000),xlab="D*",ylab="Number In Bin",main = "n=25",col="white",lty=0)
lines(c(h5$breaks, max(h5$breaks)),
       c(0,h5$counts,0), type='S')
abline(v = 1.226, col = "blue", lwd = 2 , lty = 2)
h6 = hist(Dtilde30, nclass=30,ylim=c(0,3000),xlab="D*",ylab="Number In Bin",main = "n=30",col="white",lty=0)
lines(c(h6$breaks, max(h6$breaks)),
       c(0,h6$counts,0), type='S')
abline(v = 1.226, col = "blue", lwd = 2 , lty = 2)
h7 = hist(Dtilde35, nclass=60,ylim=c(0,3000),xlab="D*",ylab="Number In Bin",main = "n=35",col="white",lty=0)
lines(c(h7$breaks, max(h7$breaks)),
       c(0,h7$counts,0), type='S')
abline(v = 1.226, col = "blue", lwd = 2 , lty = 2)
h8 = hist(Dtilde40, nclass=30,ylim=c(0,3000),xlab="D*",ylab="Number In Bin",main = "n=40",col="white",lty=0)
lines(c(h8$breaks, max(h8$breaks)),
       c(0,h8$counts,0), type='S')
abline(v = 1.226, col = "blue", lwd = 2 , lty = 2)
h9 = hist(Dtilde50, nclass=30,ylim=c(0,3000),xlab="D*",ylab="Number In Bin",main = "n=50",col="white",lty=0)
lines(c(h9$breaks, max(h9$breaks)),
       c(0,h9$counts,0), type='S')
abline(v = 1.226, col = "blue", lwd = 2 , lty = 2)



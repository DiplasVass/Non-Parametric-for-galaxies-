######################## velocity dispersion distribution #######################

#########################             D' Agostino                   ############################

#########################         Non Gaussian Groups               ############################ 

ng1 
nnL[ng1] ## NON GAUSSIAN GROUPS
sig = LDC[,4]
vdng1 = LDC[ng1,4] ## Velocity Dispersion For Non Gaussian Groups
hist(vdng1,nclass=30,col="white",
     xlab="velocity[km/s]",freq=FALSE,
     main="Distribution of velocity dispersion of NG groups (D'Agostino)") 
lines(density(vdng1),col="blue")

nnL[g1] ## GAUSSIAN GROUPS

#########################          Gaussian Groups               ############################ 


vdg1 = LDC[g1,4] ## Velocity Dispersion For Non Gaussian Groups
vdg1 

hist(vdg1,nclass=40,col="white",
     xlab="velocity[km/s]",freq=FALSE,
     main="Distribution of velocity dispersion of G groups (D'Agostino)") 
lines(density(vdng1),col="blue")

#########################      All the groups                    ##########################

hist(LDC[,4],nclass=40,col="white",
     xlab="velocity[km/s]",freq=FALSE,
     main="Distribution of velocity dispersion of groups") 
lines(density(LDC[,4]),col="blue")
install.packages("circular")
library(circular)
rayleigh.test(LDC[,4])

########################### gaussian ################################
v = 2*length(vdg1)
shat1 = sqrt(1/v)*sqrt(sum(vdg1^2))
shat1

plot(ecdf(vdg1),col="black",main="Simulations")
curve(prayleigh(x,221,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,190,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,180,log=FALSE),0,1000,col="yellow",add=TRUE)
curve(prayleigh(x,160,log=FALSE),0,1000,col="purple",add=TRUE)
curve(prayleigh(x,150,log=FALSE),0,1000,col="brown",add=TRUE)
curve(prayleigh(x,140,log=FALSE),0,1000,col="green",add=TRUE)
legend("bottomright",legend=c("empirical","ó = 200","ó = 190","ó = 180","ó = 160","ó = 150","ó = 140"),
      col=c("black","blue","red","yellow","purple","brown","green"),lty = rep(1,7))

plot(ecdf(vdg1),col="black",main="Confidence Intervals of Gaussian Groups")
curve(prayleigh(x,200,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,210,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,220,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,225,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,230,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,235,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,240,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,250,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,260,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,280,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,290,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,300,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,shat1,log=FALSE),0,1000,col="green",lwd=3,add=TRUE)
curve(prayleigh(x,190,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,180,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,160,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,150,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,140,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,130,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,125,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,120,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,115,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,110,log=FALSE),0,1000,col="blue",add=TRUE)
text(600,0.2,"Blue->Low")
text(600,0.4,"Black->empirical values")
text(600,0.3,"Red->High")
text(600,0.5,"Green->MLE")


########################### non - gaussian ################################
v = 2*length(vdng1)
shat2 = sqrt(1/v)*sqrt(sum(vdng1^2))
shat2

plot(ecdf(vdng1),col="black",main="Simulations")
curve(prayleigh(x,221,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,190,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,180,log=FALSE),0,1000,col="yellow",add=TRUE)
curve(prayleigh(x,160,log=FALSE),0,1000,col="purple",add=TRUE)
curve(prayleigh(x,150,log=FALSE),0,1000,col="brown",add=TRUE)
curve(prayleigh(x,140,log=FALSE),0,1000,col="green",add=TRUE)
legend("bottomright",legend=c("empirical","ó = 200","ó = 190","ó = 180","ó = 160","ó = 150","ó = 140"),
      col=c("black","blue","red","yellow","purple","brown","green"),lty = rep(1,7))

plot(ecdf(vdng1),col="black",main="Confidence Intervals of Non-Gaussian Groups",pch=10)
lines(ecdf(vdng1))
curve(prayleigh(x,200,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,205,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,210,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,215,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,220,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,230,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,240,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,250,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,260,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,265,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,235,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,245,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,shat2,log=FALSE),0,1000,col="green",lwd=3,add=TRUE)
curve(prayleigh(x,120,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,130,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,140,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,145,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,150,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,155,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,160,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,165,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,170,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,180,log=FALSE),0,1000,col="blue",add=TRUE)
text(600,0.2,"Blue->Low")
text(600,0.4,"Black->empirical values")
text(600,0.3,"Red->High")
text(600,0.5,"Green->MLE")

#################### all ###########################
v = 2*length(LDC[,4])
shat3 = sqrt(1/v)*sqrt(sum(LDC[,4]^2))
shat3

plot(ecdf(LDC[,4]),col="black",main="Confidence Intervals of all the Groups")
curve(prayleigh(x,200,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,210,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,220,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,225,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,230,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,235,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,240,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,250,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,260,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,280,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,290,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,300,log=FALSE),0,1000,col="red",add=TRUE)
curve(prayleigh(x,shat1,log=FALSE),0,1000,col="green",lwd=3,add=TRUE)
curve(prayleigh(x,190,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,180,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,160,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,150,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,140,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,130,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,125,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,120,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,115,log=FALSE),0,1000,col="blue",add=TRUE)
curve(prayleigh(x,110,log=FALSE),0,1000,col="blue",add=TRUE)
text(600,0.2,"Blue->Low")
text(600,0.4,"Black->empirical values")
text(600,0.3,"Red->High")
text(600,0.5,"Green->MLE")








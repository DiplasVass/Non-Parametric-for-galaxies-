.libPaths("/home/testuser/Rlib")

data1 = read.table("NASA GROUPS 2.txt",sep="",fill=TRUE)
data1

data2 = read.table("NASA GROUPS.txt",sep="",fill=TRUE)
data2

data = cbind(data1,data2)
data

Data = data[,c(1,9,10)]
Data

colnames(Data) = c("group","velocity","type")
Data

############### Histogram Of Number Of Galaxies In groups ####################

r1 = which(Data[,1]==1)
d1 = Data[r1,]

g = rep(NA,485)

for(i in 1:485) {
g[i]= length(which(Data[,1]==i))
}

g20 = which(g<20)
G2  = g[g20]


my.hist =  hist(G2,xlim=c(0,20),ylim=c(0,200),xlab="Number of Galaxies",ylab="Number of Groups",
     nclass=20,col = "white",
     main = "Histogram of the Distribution of galaxies in groups ",
     lty=0)
lines(c(my.hist$breaks, max(my.hist$breaks)),
       c(0,my.hist$counts,0), type='S')
 
################# Normality Tests For Group Galaxies ##########################


gp = function(i) {

r = which(Data[,1]==i)
d = Data[r,2]
return(d)
}

zscore = function(j) {

Z = (gp(j) - mean(gp(j)))/sd(gp(j)) 
return(Z)
}
install.packages("goftest")
library(goftest)

AD = function(i) {

ad = ad.test(zscore(i),"pnorm")$p.value 

return(ad)
}

adtest=rep(NA,485)
for(i in 1:485) {

adtest[i] = AD(i)  

}
which(adtest<0.1)


CVM = function(i) {

cvm = cvm.test(zscore(i),"pnorm")$p.value 

return(cvm)
}

cvmtest=rep(NA,485)
for(i in 1:485) {

cvmtest[i] = CVM(i)  

}
which(cvmtest<0.1)

#################################################################
#################### Velocity Histograms  #######################
Data[2000,1]
par(mfrow=c(1,2))
h2 = hist(Data[which(Data[,1]==285),2],nclass=40,main="Velocity For Non-Gaussian Group 285",xlab = "radial velocity [km/s]",col="white",lty=0)
lines(c(h2$breaks, max(h2$breaks)),c(0,h2$counts,0), type='S')
h4 = hist(Data[which(Data[,1]==289),2],nclass=40,main="Velocity For Gaussian Group 289",xlab = "radial velocity [km/s]",col="white",lty=0)
lines(c(h4$breaks, max(h4$breaks)),c(0,h4$counts,0), type='S')

###################################################################
################### Morphological Types ###########################

###################       Elliptical    ###########################

el = which(Data[,3]=="E")
el
nel = length(el)
nel

E  = Data[el,2]

zel = (E  - mean(E))/ sd(E)

h5 = hist(zel,nclass=60,xlim=c(-2,2),main="Velocity For Elliptical Galaxies",xlab = "radial velocity",col="white",lty=0)
lines(c(h5$breaks, max(h5$breaks)),c(0,h5$counts,0), type='S')


###################        Spiral      #############################

sp = which(Data[,3]=="S")
sp
nsp = length(sp)
nsp
plot(sp)
S  = Data[sp,2]

zsp = (S  - mean(S))/ sd(S)

h6 = hist(zsp,nclass=50,xlim=c(-2,2),main="Velocity For Spiral Galaxies",xlab = "radial velocity",col="white",lty=0)
lines(c(h6$breaks, max(h6$breaks)),c(0,h6$counts,0), type='S')


###################      Lenticular      #############################

le = which(Data[,3]=="L")
le
nle = length(le)
nle

L  = Data[le,2]

zle = (L  - mean(L))/ sd(L)

h7 = hist(zle,nclass=50,xlim=c(-2,2),main="Velocity For Lenticular Galaxies",xlab = "radial velocity",col="white",lty=0)
lines(c(h7$breaks, max(h7$breaks)),c(0,h7$counts,0), type='S')

#################### Irregular #################################

ir =  which(Data[,3]=="I")
ir
nir = length(ir)
nir

I  = Data[ir,2]
I

##################      Percentage       ##############################

nel#322
nsp#2369
nle#640
nir#318

pel = nel/nrow(data)
pel
psp = nsp/nrow(data)
psp
ple = nle/nrow(data)
ple*100
pir = nir/nrow(data)
pir

median(E)
median(S)
median(L)
median(I)

hist(E)
hist(S)
rayleigh.test(E)
rayleigh.test(S)
rayleigh.test(L)


hist(S,nclass=30,col=rgb(0,0,1,0.5),main="Elliptical and Spiral Glaxies of the sample",
     xlab="Elliptical and Spiral velocities [km/s]")
hist(E,nclass=30,col=rgb(1,0,0,0.5),add=TRUE)
legend(2000,140,legend=c("Spiral Galaxies","Elliptical Galaxies"),
       col=c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)),lty=c(1,1),
       box.lty=0)

############ Rayleigh Tests for Unoformity ############
rayleigh.test(E)
rayleigh.test(S)
rayleigh.test(L)

############ Wilcoxon Tests for Unoformity (Monte Carlo method)############

r1 = runif(length(E),min=min(E),max=max(E))
r2 = runif(length(S),min=min(S),max=max(S))
r3 = runif(length(L),min=min(L),max=max(L))

MC1=replicate(30000,wilcox.test(E,runif(length(E),min=min(E),max=max(E)))$p.value)
MC2=replicate(30000,wilcox.test(S,runif(length(S),min=min(S),max=max(S)))$p.value)
MC3=replicate(30000,wilcox.test(L,runif(length(L),min=min(L),max=max(L)))$p.value)

###### Percentage of Rejection #######
length(which(MC1<0.05))/30000
length(which(MC2<0.05))/30000
length(which(MC3<0.05))/30000












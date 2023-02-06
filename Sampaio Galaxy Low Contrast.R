.libPaths("/home/testuser/Rlib")

data1 = read.table("2MASSONLYGROUPS.txt",sep="") 
data1
colnames(data1) = c("GROUP","ID","N200","sigma","R200","logM200","m12","v200","DP","MA","mod",
                   "RA","DA")

data1[1:10,]

data2 = read.table("2MASSMEMBERS.txt",sep="")
data2
colnames(data2) = c("n","Group","HV","Kmag","Dist")
data2

############### Histogram Of Number Of Galaxies In groups ####################
g = data1[,3]
###############                Nmember <= 50              ####################

G1 = g[g<50]

my.hist1 =  hist(G1,xlim=c(0,51),ylim=c(0,200),xlab="Number of Galaxies",ylab="Number of Groups",
     nclass=20,col = "white",
     main = "Histogram of the Distribution of galaxies in groups of N <=50 ",
     lty=0)
lines(c(my.hist1$breaks, max(my.hist1$breaks)),
       c(0,my.hist1$counts,0), type='S')


###############              50 < Nmember < 300            ####################

G2 = g[g<300 & g>50] 

my.hist2 =  hist(G2,xlim=c(51,300),ylim=c(0,200),xlab="Number of Galaxies",ylab="Number of Groups",
     nclass=20,col = "white",
     main = "Histogram of the Distribution of galaxies in groups ",
     lty=0)
lines(c(my.hist2$breaks, max(my.hist2$breaks)),
       c(0,my.hist2$counts,0), type='S')

###############             Number  = All                   ####################
 
my.hist =  hist(g,xlim=c(0,400),ylim=c(0,500),xlab="Number of Galaxies",ylab="Number of Groups",
     nclass=20,col = "white",
     main = "Histogram of the Distribution of galaxies in groups ",
     lty=0)
lines(c(my.hist$breaks, max(my.hist$breaks)),
       c(0,my.hist$counts,0), type='S')


##################################################################################
###################      Distinguish LDC and HDC Groups      #####################

####### Groups #########

ldc = which(data1[,1]=="LDC")
ldc
LDC = data1[ldc,]  
LDC

hdc = which(data1[,1]=="HDC")
hdc
HDC = data1[hdc,]  
HDC

####### Members ########


l   = which(data2[,1]=="L")
l
L   = data2[l,]
L

h   = which(data2[,1]=="H")
h
H   = data2[h,]
H

####################################################################################
###################   Construct Group Members Matrices Separated ###################
###################                    For L                     ################### 

nnL = LDC[,2]  


gpl = function(i) {

nl = which(L[,2]==nnL[i])
return(nl)
}

GL  = function(i) {

gl  = L[gpl(i),]

return(gl)

}

###################                    For H                     ################### 


nnH = HDC[,2]  


gph = function(i) {

nh = which(H[,2]==nnH[i])
return(nh)
}

GH  = function(i) {

gh  = H[gph(i),]

return(gh)

}


#####################################################################################
######################### Normality Tests For L #####################################

#########################   For the Velocity    #####################################

#########################   D' Agostino Test    #####################################   

install.packages("moments")
library(moments)
install.packages("fBasics")
library(fBasics)

dp = rep(NA,length(nnL))

ZL   = function(i) {

zscl = (GL(i)[,3] - mean(GL(i)[,3]))/sd(GL(i)[,3])

return(zscl)

}

for(i in 1:length(nnL)) {

dp[i] =  agostino.test(ZL(i))$p.value

}

length(which(dp<0.1))

g1   = which(dp>0.1)
ng1  = which(dp<0.1)
ng1
nnL[c(38,40,257,200)]## which group

par(mfrow=c(2,2))
h1 =  hist(ZL(38),xlab=" Velocity [km/s] ",nclass=length(ZL(38)),col = "white",main = " NG for the 160 group ID ",lty=0)
      lines(c(h1$breaks, max(h1$breaks)),c(0,h1$counts,0), type='S')
h2 =  hist(ZL(40),xlab=" Velocity [km/s] ",nclass=10,col = "white",main = " G for the 164 group ID ",lty=0)
      lines(c(h2$breaks, max(h2$breaks)),c(0,h2$counts,0), type='S')
h3 =  hist(ZL(257),xlab=" Velocity [km/s] ",nclass=30,col = "white",main = " NG for the 1172 group ID ",lty=0)
      lines(c(h3$breaks, max(h3$breaks)),c(0,h3$counts,0), type='S')
      abline(v=0,lty=2)
h4 =  hist(ZL(200),xlab=" Velocity [km/s] ",nclass=25,col = "white",main = " G for the 926 group ID ",lty=0)
      lines(c(h4$breaks, max(h4$breaks)),c(0,h4$counts,0), type='S')
      abline(v=0,lty=2)


#########################   Shapiro -  Wilk  Test   ##################################   

sh = rep(NA,length(nnL))

for(i in 1:length(nnL)) {

sh[i] =  shapiro.test(ZL(i))$p.value

}
sh
length(which(sh<0.1))

g2   = which(sh>0.1)
ng2  = which(sh<0.1)
ng2
nnL[c(47,45,147,67)]## which group
length(g2)
par(mfrow=c(2,2))
h5 =  hist(ZL(47),xlab=" Velocity [km/s] ",nclass=length(ZL(47)),col = "white",main = " NG for the 182 group ID ",lty=0)
      lines(c(h5$breaks, max(h5$breaks)),c(0,h5$counts,0), type='S')
h6 =  hist(ZL(45),xlab=" Velocity [km/s] ",nclass=15,col = "white",main = " G for the 179 group ID ",lty=0)
      lines(c(h6$breaks, max(h6$breaks)),c(0,h6$counts,0), type='S')
h7 =  hist(ZL(147),xlab=" Velocity [km/s] ",nclass=5,col = "white",main = " NG for the 653 group ID ",lty=0)
      lines(c(h7$breaks, max(h7$breaks)),c(0,h7$counts,0), type='S')
h8 =  hist(ZL(67),xlab=" Velocity [km/s] ",nclass=30,col = "white",main = " G for the 249 group ID ",lty=0)
      lines(c(h8$breaks, max(h8$breaks)),c(0,h8$counts,0), type='S')
      

#########################   Anderson -  Darling  Test   ##################################   

install.packages("nortest")
library(nortest)

ad = rep(NA,length(nnL))

for(i in 1:length(nnL)) {

ad[i] =  ad.test(ZL(i))$p.value

}
ad
length(which(ad<0.1))

g3   = which(ad>0.1)
ng3  = which(ad<0.1)
ng3
nnL[c(31,70,265,45)]## which group
length(g3)
par(mfrow=c(2,2))
h9 =  hist(ZL(31),xlab=" Velocity [km/s] ",nclass=length(ZL(31)),col = "white",main = " NG for the 122 group ID ",lty=0)
      lines(c(h9$breaks, max(h9$breaks)),c(0,h9$counts,0), type='S')
h10 =  hist(ZL(70),xlab=" Velocity [km/s] ",nclass=10,col = "white",main = " G for the 261 group ID ",lty=0)
      lines(c(h10$breaks, max(h10$breaks)),c(0,h10$counts,0), type='S')
h11 =  hist(ZL(265),xlab=" Velocity [km/s] ",nclass=5,col = "white",main = " NG for the 1230 group ID ",lty=0)
      lines(c(h11$breaks, max(h11$breaks)),c(0,h11$counts,0), type='S')
h12 =  hist(ZL(45),xlab=" Velocity [km/s] ",nclass=20,col = "white",main = " G for the 179 group ID ",lty=0)
      lines(c(h12$breaks, max(h12$breaks)),c(0,h12$counts,0), type='S')
      
#########################   Lilliefors  Test   ##################################   

install.packages("nortest")
library(nortest)

li = rep(NA,length(nnL))

for(i in 1:length(nnL)) {

li[i] =  lillie.test(ZL(i))$p.value

}
li
length(which(li<0.1))

g4   = which(li>0.1)
ng4  = which(li<0.1)
ng4
nnL[c(8,70,273,45)]## which group
length(ng4)

par(mfrow=c(2,2))
h13 =  hist(ZL(8),xlab=" Velocity [km/s] ",nclass=length(ZL(8)),col = "white",main = " NG for the 35 group ID ",lty=0)
      lines(c(h13$breaks, max(h13$breaks)),c(0,h13$counts,0), type='S')
h14 =  hist(ZL(70),xlab=" Velocity [km/s] ",nclass=10,col = "white",main = " G for the 261 group ID ",lty=0)
      lines(c(h14$breaks, max(h14$breaks)),c(0,h14$counts,0), type='S')
h15 =  hist(ZL(273),xlab=" Velocity [km/s] ",nclass=5,col = "white",main = " NG for the 1257 group ID ",lty=0)
      lines(c(h15$breaks, max(h15$breaks)),c(0,h15$counts,0), type='S')
h16 =  hist(ZL(45),xlab=" Velocity [km/s] ",nclass=20,col = "white",main = " G for the 179 group ID ",lty=0)
      lines(c(h16$breaks, max(h16$breaks)),c(0,h16$counts,0), type='S')

#########################   Pearson  Test   ##################################   


pe = rep(NA,length(nnL))

for(i in 1:length(nnL)) {

pe[i] =  pearson.test(ZL(i))$p.value

}
pe
length(which(pe<0.1))

g5   = which(pe>0.1)
ng5  = which(pe<0.1)
ng5
nnL[c(9,200,267,81)]## which group
length(ng5)
par(mfrow=c(2,2))
h17 =  hist(ZL(9),xlab=" Velocity [km/s] ",nclass=length(ZL(9)),col = "white",main = " NG for the 36 group ID ",lty=0)
      lines(c(h17$breaks, max(h17$breaks)),c(0,h17$counts,0), type='S')
h18 =  hist(ZL(200),xlab=" Velocity [km/s] ",nclass=25,col = "white",main = " G for the 926 group ID ",lty=0)
      lines(c(h18$breaks, max(h18$breaks)),c(0,h18$counts,0), type='S')
h19 =  hist(ZL(267),xlab=" Velocity [km/s] ",nclass=5,col = "white",main = " NG for the 1238 group ID ",lty=0)
      lines(c(h19$breaks, max(h19$breaks)),c(0,h19$counts,0), type='S')
h20 =  hist(ZL(81),xlab=" Velocity [km/s] ",nclass=20,col = "white",main = " G for the 293 group ID ",lty=0)
      lines(c(h20$breaks, max(h20$breaks)),c(0,h20$counts,0), type='S')


###############################################################################
############################ Cataloge Of the Tests ############################

RejL = matrix(NA,nrow=length(ng5),ncol=5)
RejL = cbind(ng1,ng2,ng3,ng4,ng5)
colnames(RejL) = c("D'Agostino","Shapiro-Wilk","Anderson-Darling","Lilliefors","Pearson")
RejL[35:49,1] =  NA
RejL[40:49,2] =  NA
RejL[43:49,3] =  NA
RejL[39:49,4] =  NA
matrix(RejL[,5],ncol=1)

###########################       Histograms       ############################

par(mfrow=c(1,3))
h21 =  hist(ZL(1),xlab=" Velocity [km/s] ",nclass=10,col = "white",lty=0)
      lines(c(h21$breaks, max(h21$breaks)),c(0,h21$counts,0), type='S')
legend("topleft",legend=c("failed D'Agostino","passed Shapiro-Wilk","passed Anderson-Darling",
        "passed Lilliefors" , "passed Pearson"),box.lty=0,cex=0.8)
h22 =  hist(ZL(8),xlab=" Velocity [km/s] ",nclass=15,col = "white",lty=0)
      lines(c(h22$breaks, max(h22$breaks)),c(0,h22$counts,0), type='S')
legend("topleft",legend=c("passed D'Agostino","failed Shapiro-Wilk","failed Anderson-Darling",
        "failed Lilliefors" , "passed Pearson"),box.lty=0,cex=0.8)
h23 =  hist(ZL(28),xlab=" Velocity [km/s] ",nclass=5,col = "white",lty=0)
      lines(c(h23$breaks, max(h23$breaks)),c(0,h23$counts,0), type='S')
legend("topleft",legend=c("failed D'Agostino","passed Shapiro-Wilk","failed Anderson-Darling",
        "failed Lilliefors" , "passed Pearson"),box.lty=0,cex=0.8)

par(mfrow=c(1,3))
h24 =  hist(ZL(146),xlab=" Velocity [km/s] ",col = "white",lty=0)
      lines(c(h24$breaks, max(h24$breaks)),c(0,h24$counts,0), type='S')
legend("topleft",legend=c("failed D'Agostino","failed Shapiro-Wilk","failed Anderson-Darling",
        "failed Lilliefors" , "passed Pearson"),box.lty=0,cex=0.8)
h25 =  hist(ZL(156),xlab=" Velocity [km/s] ",nclass=20,col = "white",lty=0)
      lines(c(h25$breaks, max(h25$breaks)),c(0,h25$counts,0), type='S')
legend("topleft",legend=c("passed D'Agostino","passed Shapiro-Wilk","failed Anderson-Darling",
        "failed Lilliefors" , "failed Pearson"),box.lty=0,cex=0.8)
h26 =  hist(ZL(257),xlab=" Velocity [km/s] ",nclass=20,col = "white",lty=0)
      lines(c(h26$breaks, max(h26$breaks)),c(0,h26$counts,0), type='S')
legend("topleft",legend=c("failed D'Agostino","failed Shapiro-Wilk","failed Anderson-Darling",
        "failed Lilliefors" , "failed Pearson"),box.lty=0,cex=0.8)


##################### The Group 1172(257) Specialization ########################

hist(ZL(257),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 1172 group ID")
lines(density(ZL(257)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
abline(v=0,col="black",lwd=4,lty=2)
legend("topleft",legend=c("empirical curve","standard normal curve","theoretical mean"),col=c("blue","red","black"),
        lty=c(1,1,2),box.lty=0,cex=0.8)




####################       Agreement Of Tests      ########################

agree = c(17,34,38,106,135,149,189,190,197,206,219,257)
agree

##Which Group 

agr = nnL[agree]
agr

## Showing their Densities 

par(mfrow=c(2,2))
hist(ZL(17),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 63 group ID")
lines(density(ZL(17)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
hist(ZL(34),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 130 group ID")
lines(density(ZL(34)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
hist(ZL(38),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 160 group ID")
lines(density(ZL(38)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
hist(ZL(106),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 407 group ID")
lines(density(ZL(106)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')

par(mfrow=c(2,2))
hist(ZL(135),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 561 group ID")
lines(density(ZL(135)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
hist(ZL(149),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 678 group ID")
lines(density(ZL(149)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
hist(ZL(189),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 864 group ID")
lines(density(ZL(189)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
hist(ZL(190),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 867 group ID")
lines(density(ZL(190)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')

par(mfrow=c(2,2))
hist(ZL(197),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 916 group ID")
lines(density(ZL(197)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
hist(ZL(206),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 955 group ID")
lines(density(ZL(206)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
hist(ZL(219),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 997 group ID")
lines(density(ZL(219)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')
hist(ZL(257),xlab=" Velocity [km/s] ",col="white",freq=FALSE,main="NG for the 1172 group ID")
lines(density(ZL(257)),col="blue")
curve(dnorm(x),-2,2,add=TRUE,col='red')


################################################################################################
#########################      Velocity Dispersion Histogram        ############################

#########################             D' Agostino                   ############################

#########################         Non Gaussian Groups               ############################ 

ng1 
nnL[ng1] ## NON GAUSSIAN GROUPS
sig = LDC[,4]
vdng1 = LDC[ng1,4] ## Velocity Dispersion For Non Gaussian Groups
vdng1 

####### Histogram For Non Gaussian ########

ngh1 = hist(vdng1,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh1$breaks, max(ngh1$breaks)),c(0,ngh1$counts,0), type='S',lty=2)



#########################           Gaussian Groups                  ############################
 
nnL[g1] ## GAUSSIAN GROUPS

vdg1 = LDC[g1,4] ## Velocity Dispersion For Non Gaussian Groups
vdg1 

####### Histogram For  Gaussian ########

gh1 = hist(vdg1,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh1$breaks, max(gh1$breaks)),c(0,gh1$counts,0), type='S')




#########################  Mixed Histograms For The Two Groups       ############################


gh1 = hist(vdg1,nclass=40,col = "white",main="Velocity Distribution",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh1$breaks, max(gh1$breaks)),c(0,gh1$counts,0), type='S')
ngh1 = hist(vdng1,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh1$breaks, max(ngh1$breaks)),c(0,ngh1$counts,0), type='S',lty=2)
mtext("  D' Agostino  Test ")

################################################################################################
#########################             Shapiro - Wilk                ############################

#########################           Non Gaussian Groups             ############################ 

ng2 
nnL[ng2] ## NON GAUSSIAN GROUPS
sig = LDC[,4]
vdng2 = LDC[ng2,4] ## Velocity Dispersion For Non Gaussian Groups
vdng2 

####### Histogram For Non Gaussian ########

ngh2 = hist(vdng2,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh2$breaks, max(ngh2$breaks)),c(0,ngh2$counts,0), type='S',lty=2)


#########################           Gaussian Groups                  ############################
 
nnL[g2] ## GAUSSIAN GROUPS

vdg2 = LDC[g2,4] ## Velocity Dispersion For Non Gaussian Groups
vdg2 

####### Histogram For  Gaussian ########

gh2 = hist(vdg2,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh2$breaks, max(gh2$breaks)),c(0,gh2$counts,0), type='S')




#########################  Mixed Histograms For The Two Groups       ############################


gh2 = hist(vdg2,nclass=40,col = "white",main="Velocity Distribution",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh2$breaks, max(gh2$breaks)),c(0,gh2$counts,0), type='S')
ngh2 = hist(vdng2,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh2$breaks, max(ngh2$breaks)),c(0,ngh2$counts,0), type='S',lty=2)
mtext("  Shapiro-Wilk  Test ")


################################################################################################
#########################             Anderson-Darling              ############################

#########################           Non Gaussian Groups             ############################ 

ng3 
nnL[ng3] ## NON GAUSSIAN GROUPS
sig = LDC[,4]
vdng3 = LDC[ng3,4] ## Velocity Dispersion For Non Gaussian Groups
vdng3 

####### Histogram For Non Gaussian ########

ngh3 = hist(vdng3,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh3$breaks, max(ngh3$breaks)),c(0,ngh3$counts,0), type='S',lty=2)


#########################           Gaussian Groups                  ############################
 
nnL[g3] ## GAUSSIAN GROUPS

vdg3 = LDC[g3,4] ## Velocity Dispersion For Non Gaussian Groups
vdg3 

####### Histogram For  Gaussian ########

gh3 = hist(vdg3,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh3$breaks, max(gh3$breaks)),c(0,gh3$counts,0), type='S')




#########################  Mixed Histograms For The Two Groups       ############################


gh3 = hist(vdg3,nclass=40,col = "white",main="Velocity Distribution",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh3$breaks, max(gh3$breaks)),c(0,gh3$counts,0), type='S')
ngh3 = hist(vdng3,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh3$breaks, max(ngh3$breaks)),c(0,ngh3$counts,0), type='S',lty=2)
mtext(" Anderson - Darling  Test ")


################################################################################################
#########################               Lilliefors                  ############################

#########################           Non Gaussian Groups             ############################ 

ng4 
nnL[ng4] ## NON GAUSSIAN GROUPS
sig = LDC[,4]
vdng4 = LDC[ng4,4] ## Velocity Dispersion For Non Gaussian Groups
vdng4 

####### Histogram For Non Gaussian ########

ngh4 = hist(vdng4,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh4$breaks, max(ngh4$breaks)),c(0,ngh4$counts,0), type='S',lty=2)


#########################           Gaussian Groups                  ############################
 
nnL[g4] ## GAUSSIAN GROUPS

vdg4 = LDC[g4,4] ## Velocity Dispersion For Non Gaussian Groups
vdg4 

####### Histogram For  Gaussian ########

gh4 = hist(vdg4,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh4$breaks, max(gh4$breaks)),c(0,gh4$counts,0), type='S')




#########################  Mixed Histograms For The Two Groups       ############################


gh4 = hist(vdg4,nclass=40,col = "white",main="Velocity Distribution",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh4$breaks, max(gh4$breaks)),c(0,gh4$counts,0), type='S')
ngh4 = hist(vdng4,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh4$breaks, max(ngh4$breaks)),c(0,ngh4$counts,0), type='S',lty=2)
mtext(" Lilliefors  Test ")

################################################################################################
#########################               Pearson                      ###########################

#########################           Non Gaussian Groups             ############################ 

ng5 
nnL[ng5] ## NON GAUSSIAN GROUPS
sig = LDC[,4]
vdng5 = LDC[ng5,4] ## Velocity Dispersion For Non Gaussian Groups
vdng5 

####### Histogram For Non Gaussian ########

ngh5 = hist(vdng5,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh5$breaks, max(ngh5$breaks)),c(0,ngh5$counts,0), type='S',lty=2)


#########################           Gaussian Groups                  ############################
 
nnL[g5] ## GAUSSIAN GROUPS

vdg5 = LDC[g5,4] ## Velocity Dispersion For Non Gaussian Groups
vdg5 

####### Histogram For  Gaussian ########

gh5 = hist(vdg5,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh5$breaks, max(gh5$breaks)),c(0,gh5$counts,0), type='S')




#########################  Mixed Histograms For The Two Groups       ############################


gh5 = hist(vdg5,nclass=40,col = "white",main="Velocity Distribution",
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(gh5$breaks, max(gh5$breaks)),c(0,gh5$counts,0), type='S')
ngh5 = hist(vdng5,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab = "s intrinsic",ylab="Number In Bin",lty=0)
       lines(c(ngh5$breaks, max(ngh5$breaks)),c(0,ngh5$counts,0), type='S',lty=2)
mtext(" Pearson  Test ")



################################################################################################
#################      Number Of Members Per Galaxy Group Histogram        #####################

#########################           D'Agostino                      ############################

#########################        Non Gaussian Groups                ############################# 
ngn1 = g[ng1]
ngn1
ng1 
nnL[ng1] ## NON GAUSSIAN GROUPS

####### Histogram For Non Gaussian ########

nngh1 = hist(ngn1,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh1$breaks, max(nngh1$breaks)),c(0,nngh1$counts,0), type='S',lty=2)


#########################           Gaussian Groups                  ############################
gn1 = g[g1] 
gn1
g1
nnL[g5] ## GAUSSIAN GROUPS

####### Histogram For  Gaussian ########

ggh1 = hist(gn1,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh1$breaks, max(ggh1$breaks)),c(0,ggh1$counts,0), type='S')




#########################  Mixed Histograms For The Two Groups       ############################


ggh1 = hist(gn1,nclass=40,col = "white",main=" Number Of Members Distribution",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh1$breaks, max(ggh1$breaks)),c(0,ggh1$counts,0), type='S')
nngh1 = hist(ngn1,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh1$breaks, max(nngh1$breaks)),c(0,nngh1$counts,0), type='S',lty=2)
mtext(" D'Agostino  Test ")


#########################               Shapiro - Wilk              ############################

#########################             Non Gaussian Groups           ############################# 
ngn2 = g[ng2]
ngn2
ng2 
nnL[ng2] ## NON GAUSSIAN GROUPS

####### Histogram For Non Gaussian ########

nngh2 = hist(ngn2,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh2$breaks, max(nngh2$breaks)),c(0,nngh2$counts,0), type='S',lty=2)


#########################           Gaussian Groups                  ############################
gn2 = g[g2] 
gn2
g2
nnL[g2] ## GAUSSIAN GROUPS

####### Histogram For  Gaussian ########

ggh2 = hist(gn2,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh2$breaks, max(ggh2$breaks)),c(0,ggh2$counts,0), type='S')




#########################  Mixed Histograms For The Two Groups       ############################


ggh2 = hist(gn2,nclass=20,col = "white",main=" Number Of Members Distribution",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh2$breaks, max(ggh2$breaks)),c(0,ggh2$counts,0), type='S')
nngh2 = hist(ngn2,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh2$breaks, max(nngh2$breaks)),c(0,nngh2$counts,0), type='S',lty=2)
mtext(" Shapiro - Wilk  Test ")

#########################              Anderson - Darling           #############################

#########################             Non Gaussian Groups           ############################# 
ngn3 = g[ng3]
ngn3
ng3
nnL[ng3] ## NON GAUSSIAN GROUPS

####### Histogram For Non Gaussian ########

nngh3 = hist(ngn3,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh3$breaks, max(nngh3$breaks)),c(0,nngh3$counts,0), type='S',lty=2)


#########################           Gaussian Groups                  ############################
gn3 = g[g3] 
gn3
g3
nnL[g3] ## GAUSSIAN GROUPS

####### Histogram For  Gaussian ########

ggh3 = hist(gn3,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh3$breaks, max(ggh3$breaks)),c(0,ggh3$counts,0), type='S')




#########################  Mixed Histograms For The Two Groups       ############################


ggh3 = hist(gn3,nclass=20,col = "white",main=" Number Of Members Distribution",xlim=c(0,110),
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh3$breaks, max(ggh3$breaks)),c(0,ggh3$counts,0), type='S')
nngh3 = hist(ngn3,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh3$breaks, max(nngh3$breaks)),c(0,nngh3$counts,0), type='S',lty=2)
mtext(" Anderson - Darling  Test ")
 

#########################              Lilliefors                   #############################

#########################             Non Gaussian Groups           ############################# 
ngn4 = g[ng4]
ngn4
ng4
nnL[ng4] ## NON GAUSSIAN GROUPS

####### Histogram For Non Gaussian ########

nngh4 = hist(ngn4,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh4$breaks, max(nngh4$breaks)),c(0,nngh4$counts,0), type='S',lty=2)


#########################           Gaussian Groups                  ############################
gn4 = g[g4] 
gn4
g4
nnL[g4] ## GAUSSIAN GROUPS

####### Histogram For  Gaussian ########

ggh4 = hist(gn4,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh4$breaks, max(ggh4$breaks)),c(0,ggh4$counts,0), type='S')




#########################  Mixed Histograms For The Two Groups       ############################


ggh4 = hist(gn4,nclass=20,col = "white",main=" Number Of Members Distribution",xlim=c(0,110),
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh4$breaks, max(ggh4$breaks)),c(0,ggh4$counts,0), type='S')
nngh4 = hist(ngn4,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh4$breaks, max(nngh4$breaks)),c(0,nngh4$counts,0), type='S',lty=2)
mtext(" Lilliefors  Test ")
 

#########################              Pearson                      #############################

#########################             Non Gaussian Groups           ############################# 
ngn5 = g[ng5]
ngn5
ng5
nnL[ng5] ## NON GAUSSIAN GROUPS

####### Histogram For Non Gaussian ########

nngh5 = hist(ngn5,nclass=20,col = "white",main = " Histogram For Non Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh5$breaks, max(nngh5$breaks)),c(0,nngh5$counts,0), type='S',lty=2)


#########################           Gaussian Groups                  ############################
gn5 = g[g5] 
gn5
g5
nnL[g5] ## GAUSSIAN GROUPS

####### Histogram For  Gaussian ########

ggh5 = hist(gn5,nclass=20,col = "white",main = " Histogram For Gaussian ",
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh5$breaks, max(ggh5$breaks)),c(0,ggh5$counts,0), type='S')


#########################  Mixed Histograms For The Two Groups       ############################


ggh5 = hist(gn5,nclass=20,col = "white",main=" Number Of Members Distribution",xlim=c(0,110),
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(ggh5$breaks, max(ggh5$breaks)),c(0,ggh5$counts,0), type='S')
nngh5 = hist(ngn5,nclass=40,col = "white",main = " Histogram For Non Gaussian ",add=TRUE,
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(c(nngh5$breaks, max(nngh5$breaks)),c(0,nngh5$counts,0), type='S',lty=2)
mtext(" Pearson  Test ")
 


####################################################################################################
#########################        Results of Tests in a table           #############################

### Table 2.L ###

Test    = c("D'Agostino","Shapiro-Wilk","Anderson-Darling","Lilliefors","Pearson's")

NumberG = rep(length(nnL),5) 
NumberG

LFail   = c(length(ng1),length(ng2),length(ng3),length(ng4),length(ng5))
LFail

LPerc   = (c(length(ng1),length(ng2),length(ng3),length(ng4),length(ng5)))/length(nnL)
LPerc

Signlvl = rep(0.10,5)
Signlvl

Table2L  = cbind(NumberG,LFail,LPerc,Signlvl)
colnames(Table2L) = c("Number Of Groups","Number Of Failed","Percent of Failed Groups(%)","Significance Level")
rownames(Table2L) = Test
Table2L

### Table 3.L   ###

### D'Agostino ###

### Table 3.1.L ###

agree ## Non Gaussian
agr   ## Non Gaussian Groups for all tests

NumberG1 = c(length(g1),length(ng1))
NumberG1

LPerc1   = NumberG1/length(nnL)
LPerc1

Lsngbar1 = mean(LDC[ng1,4])
Lsngbar1

Lsgbar1 = mean(LDC[g1,4])
Lsgbar1

Lsbar1  = c(Lsngbar1,Lsgbar1)

Table31L = cbind(NumberG1,LPerc1,Lsbar1)
colnames(Table31L) = c("Number Of Groups","Percent of Failed Groups(%)","Sigmabar")
rownames(Table31L) = c("Gaussian","Non-Gaussian")
Table31L


### Shapiro - Wilk ###

### Table 3.2.L ###

agree ## Non Gaussian
agr   ## Non Gaussian Groups for all tests

NumberG2 = c(length(g2),length(ng2))
NumberG2

LPerc2   = NumberG2/length(nnL)
LPerc2

Lsngbar2 = mean(LDC[ng2,4])
Lsngbar2

Lsgbar2 = mean(LDC[g2,4])
Lsgbar2

Lsbar2  = c(Lsngbar2,Lsgbar2)

Table32L = cbind(NumberG2,LPerc2,Lsbar2)
colnames(Table32L) = c("Number Of Groups","Percent of Failed Groups(%)","Sigmabar")
rownames(Table32L) = c("Gaussian","Non-Gaussian")
Table32L


### Anderson - Darling ###

### Table 3.3.L ###

agree ## Non Gaussian
agr   ## Non Gaussian Groups for all tests

NumberG3 = c(length(g3),length(ng3))
NumberG3

LPerc3   = NumberG3/length(nnL)
LPerc3

Lsngbar3 = mean(LDC[ng3,4])
Lsngbar3

Lsgbar3 = mean(LDC[g3,4])
Lsgbar3

Lsbar3  = c(Lsngbar3,Lsgbar3)

Table33L = cbind(NumberG3,LPerc3,Lsbar3)
colnames(Table33L) = c("Number Of Groups","Percent of Failed Groups(%)","Sigmabar")
rownames(Table33L) = c("Gaussian","Non-Gaussian")
Table33L

### Lilliefors ###

### Table 3.4.L ###

agree ## Non Gaussian
agr   ## Non Gaussian Groups for all tests

NumberG4 = c(length(g4),length(ng4))
NumberG4

LPerc4   = NumberG4/length(nnL)
LPerc4

Lsngbar4 = mean(LDC[ng4,4])
Lsngbar4

Lsgbar4 = mean(LDC[g4,4])
Lsgbar4

Lsbar4  = c(Lsngbar4,Lsgbar4)

Table34L = cbind(NumberG4,LPerc4,Lsbar4)
colnames(Table34L) = c("Number Of Groups","Percent of Failed Groups(%)","Sigmabar")
rownames(Table34L) = c("Gaussian","Non-Gaussian")
Table34L

### Pearson ###

### Table 3.5.L ###

agree ## Non Gaussian
agr   ## Non Gaussian Groups for all tests

NumberG5 = c(length(g5),length(ng5))
NumberG5

LPerc5   = NumberG5/length(nnL)
LPerc5

Lsngbar5 = mean(LDC[ng5,4])
Lsngbar5

Lsgbar5 = mean(LDC[g5,4])
Lsgbar5

Lsbar5  = c(Lsngbar5,Lsgbar5)

Table35L = cbind(NumberG5,LPerc5,Lsbar5)
colnames(Table35L) = c("Number Of Groups","Percent of Failed Groups(%)","Sigmabar")
rownames(Table35L) = c("Gaussian","Non-Gaussian")
Table35L


####################################################################################
##########################         Conclusions           ###########################


mngb1 = mean(LDC[ng1,3])
mngb1
mngb2 = mean(LDC[ng2,3])
mngb2
mngb3 = mean(LDC[ng3,3])
mngb3
mngb4 = mean(LDC[ng4,3])
mngb4
mngb5 = mean(LDC[ng5,3])
mngb5


NNG = c(mngb1,mngb2,mngb3,mngb4,mngb5)
NNG

mgb1 = mean(LDC[g1,3])
mgb1
mgb2 = mean(LDC[g2,3])
mgb2
mgb3 = mean(LDC[g3,3])
mgb3
mgb4 = mean(LDC[g4,3])
mgb4
mgb5 = mean(LDC[g5,3])
mgb5
 

NG = c(mgb1,mgb2,mgb3,mgb4,mgb5)
NG


ngr1  =  mean(LDC[ng1,5])
ngr1  
ngr2  =  mean(LDC[ng2,5])
ngr2  
ngr3  =  mean(LDC[ng3,5])
ngr3  
ngr4  =  mean(LDC[ng4,5])
ngr4  
ngr5  =  mean(LDC[ng5,5])
ngr5  

NGR200 = c(ngr1,ngr2,ngr3,ngr4,ngr5)
NGR200 

gr1  =  mean(LDC[g1,5])
gr1  
gr2  =  mean(LDC[g2,5])
gr2  
gr3  =  mean(LDC[g3,5])
gr3  
gr4  =  mean(LDC[g4,5])
gr4  
gr5  =  mean(LDC[g5,5])
gr5  

GR200 = c(gr1,gr2,gr3,gr4,gr5)
GR200 

TABLE5 = cbind(NNG,NG,NGR200,GR200)
colnames(TABLE5) = c("Number Of Galaxies for Non-Gaussian","Number Of Galaxies for Gaussian",
			   "Virial Radius For Non-Gaussian","Virial Radius For Gaussian")
rownames(TABLE5) = c("D'Agostino","Shapiro-Wilk","Anderson-Darling","Lilliefors","Pearson's")
TABLE5



#################################################################################################
############################          logM200 Histograms           ##############################

############################         For All the galaxies          ##############################

LM200  = LDC[,6]

mol  = hist(LM200,nclass=40,col = "white",main = " Histogram For Non Gaussian ",freq=FALSE,
       xlab="Number of Galaxies",ylab="Number of Groups",lty=0)
       lines(density(LM200))


#### N>20 ####

nu20 = which(LDC[,3]>20)
nu20

LMU20 = LDC[nu20,6]
LMU20
du[1]

mu20 = hist(LMU20,nclass=40,col = "white",main = " Mass Distributions in modes for systems with N>20 ",freq=FALSE,
       xlab="log(M200)",lty=0)
       lines(density(LMU20),col="red")
       abline(v=13.69957,col="black",lwd=1,lty=2)

#### N<=20 ####

nl20 = which(LDC[,3]<=20)
nl20

LML20 = LDC[nl20,6]
LML20

ml20 = hist(LML20,nclass=40,col = "white",main = " Mass Distributions in modes for systems with N<=20 ",freq=FALSE,
       xlab="log(M200)",lty=0)
       lines(density(LML20),col="blue")
       abline(v=13.55229,col="black",lwd=1,lty=2)


##################### Combination ######################

hist(LML20,nclass=40,col = "white",main = " Mass Distributions in modes for systems  ",freq=FALSE,
       xlab="log(M200)",lty=0,xlim=c(11,16))
       lines(density(LML20),col="blue")
       abline(v=13.55229,col="black",lwd=1,lty=2)
hist(LMU20,nclass=40,col = "white",main = " Mass Distributions in modes for systems ",freq=FALSE,
       xlab="log(M200)",lty=0,add=TRUE)
       lines(density(LMU20),col="red")
       abline(v=13.69957,col="black",lwd=1,lty=2)
legend("topleft",legend=c("N>20","N<=20"),col=c("red","blue"),box.lty=0,lty=c(1,1))

############################################# GAUSSIAN , NON-GAUSSIAN #######################################
hist(LDC[g1,6],nclass=40,col = "white",main = " Mass Distributions in modes for Gaussian and Non-Gaussian groups  ",
       freq=FALSE,
       xlab="log(M200)",lty=0,xlim=c(11,16))
       lines(density(LDC[g1,6]),col="blue")
       hist(LDC[ng1,6],nclass=40,col = "white",main = " Mass Distributions in modes for systems ",freq=FALSE,
       xlab="log(M200)",lty=0,add=TRUE)
       lines(density(LDC[ng1,6]),col="red")
       legend("topleft",legend=c("Gaussian Mass","Non-Gaussian Mass"),col=c("blue","red"),box.lty=0,lty=c(1,1))

hist(LDC[g1,4],nclass=40,col = "white",main = " Velocity Dispersion Distributions  for Gaussian and Non-Gaussian groups  ",
       freq=FALSE,
       xlab="Velocity Dispersion [km/s]",lty=0)
       lines(density(LDC[g1,4]),col="blue")
       hist(LDC[ng1,4],nclass=40,col = "white",main = "",freq=FALSE,
       xlab="Velocity Dispersion [km/s]",lty=0,add=TRUE)
       lines(density(LDC[ng1,4]),col="red")
       legend("topleft",legend=c("Gaussian Mass","Non-Gaussian Mass"),col=c("blue","red"),box.lty=0,lty=c(1,1))

############################################################################################################
###################       Comparison of the Mass Distributions  of NG and G groups       ###################
 

##################     D'Agostino  ###################

mang1 = LDC[ng1,6]
mang1

mag1  = LDC[g1,6]
mag1 

MOL   = hist(LM200,nclass=20,col = "white",main=" Mass Distribution",
        xlab="LogM200",lty=0)
        lines(c(MOL$breaks, max(MOL$breaks)),c(0,MOL$counts,0), type='S',col="black")
MAG1  = hist(mag1,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MAG1$breaks, max(MAG1$breaks)),c(0,MAG1$counts,0), type='S',col="red")
MANG1 = hist(mang1,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MANG1$breaks, max(MANG1$breaks)),c(0,MANG1$counts,0), type='S',col="blue")

legend("topleft",legend=c("All the Groups","240 G Groups ","34 NG Groups"),
       col=c("black","red","blue"),lty=rep(1,3),box.lty=0)
mtext("D'Agostino")

##################     Shapiro - Wilk  ###################

mang2 = LDC[ng2,6]
mang2

mag2  = LDC[g2,6]
mag2 

MOL   = hist(LM200,nclass=20,col = "white",main=" Mass Distribution",
        xlab="LogM200",lty=0)
        lines(c(MOL$breaks, max(MOL$breaks)),c(0,MOL$counts,0), type='S',col="black")
MAG2  = hist(mag2,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MAG2$breaks, max(MAG2$breaks)),c(0,MAG2$counts,0), type='S',col="red")
MANG2 = hist(mang2,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MANG2$breaks, max(MANG2$breaks)),c(0,MANG2$counts,0), type='S',col="blue")

legend("topleft",legend=c("All the Groups","235 G Groups ","39 NG Groups"),
       col=c("black","red","blue"),lty=rep(1,3),box.lty=0)
mtext("Shapiro-Wilk")

##################     Anderson - Darling  ###################

mang3 = LDC[ng3,6]
mang3

mag3  = LDC[g3,6]
mag3 

MOL   = hist(LM200,nclass=20,col = "white",main=" Mass Distribution",
        xlab="LogM200",lty=0)
        lines(c(MOL$breaks, max(MOL$breaks)),c(0,MOL$counts,0), type='S',col="black")
MAG3  = hist(mag3,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MAG3$breaks, max(MAG3$breaks)),c(0,MAG3$counts,0), type='S',col="red")
MANG3 = hist(mang3,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MANG3$breaks, max(MANG3$breaks)),c(0,MANG3$counts,0), type='S',col="blue")

legend("topleft",legend=c("All the Groups","232 G Groups ","42 NG Groups"),
       col=c("black","red","blue"),lty=rep(1,3),box.lty=0)
mtext("Anderson - Darling")


##################     Lilliefors  ###################

mang4 = LDC[ng4,6]
mang4

mag4  = LDC[g4,6]
mag4 

MOL   = hist(LM200,nclass=20,col = "white",main=" Mass Distribution",
        xlab="LogM200",lty=0)
        lines(c(MOL$breaks, max(MOL$breaks)),c(0,MOL$counts,0), type='S',col="black")
MAG4  = hist(mag4,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MAG4$breaks, max(MAG4$breaks)),c(0,MAG4$counts,0), type='S',col="red")
MANG4 = hist(mang4,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MANG4$breaks, max(MANG4$breaks)),c(0,MANG4$counts,0), type='S',col="blue")

legend("topleft",legend=c("All the Groups","236 G Groups ","38 NG Groups"),
       col=c("black","red","blue"),lty=rep(1,3),box.lty=0)
mtext("Lilliefors")

Table2L
274-49

##################     Pearson's  ###################

mang5 = LDC[ng5,6]
mang5

mag5  = LDC[g5,6]
mag5 

MOL   = hist(LM200,nclass=20,col = "white",main=" Mass Distribution",
        xlab="LogM200",lty=0)
        lines(c(MOL$breaks, max(MOL$breaks)),c(0,MOL$counts,0), type='S',col="black")
MAG5  = hist(mag5,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MAG5$breaks, max(MAG5$breaks)),c(0,MAG5$counts,0), type='S',col="red")
MANG5 = hist(mang5,nclass=20,col = "white",main=" Mass Distribution",add=TRUE,
        xlab="LogM200",lty=0)
        lines(c(MANG5$breaks, max(MANG5$breaks)),c(0,MANG5$counts,0), type='S',col="blue")

legend("topleft",legend=c("All the Groups","225 G Groups ","38 NG Groups"),
       col=c("black","red","blue"),lty=rep(1,3),box.lty=0)
mtext("Pearson's")




#######################################################################################
##########################       Magnitude Distribution       #########################

##########################          More Strict Test          ##########################

#####################     Cramer - von Mises Normality Test   ##########################

cv = rep(NA,length(nnL))

for(i in 1:length(nnL)) {

cv[i] =  cvm.test(GL(i)[,4])$p.value

}
cv

length(which(cv<0.1))

g6   = which(cv>0.1)
ng6  = which(cv<0.1)
length(g6)
# e.d.f., quantile and Q-Q plots for globular cluster magnitudes

### 6th Group ###
mtd1 = GL(1)[,4] 

plot(ecdf(mtd1),verticals=TRUE, xlab="K (mag)", ylab="e.d.f.", main="",col="red") 
text(9.8,0.8,lab="6th Non-Gaussian Group K-band Magnitutde",cex=0.8)  
curve(pnorm(x,mean(mtd1),sd(mtd1)),9,12,col="blue",add=TRUE)
legend("topleft",legend=c("empirical magnitude","theoretical normal"),col=c("red","blue"),box.lty=0,lty=c(1,1))

qqnorm(mtd1, pch=20, cex.axis=1.3, cex.lab=1.3, main="6th Non-Gaussian Group K-band Magnitutde") 
qqline(mtd1, lty=2, lwd=1.5,col="blue") 


### 1249th Group ###
mtd2 = GL(270)[,4] 

plot(ecdf(mtd2),verticals=TRUE, xlab="K (mag)", ylab="e.d.f.", main="",col="red") 
text(10.6,0.8,lab="1249th Gaussian Group K-band Magnitutde",cex=.8)  
curve(pnorm(x,mean(mtd2),sd(mtd2)),9,12,col="blue",add=TRUE)
legend("topleft",legend=c("empirical magnitude","theoretical normal"),col=c("red","blue"),box.lty=0,lty=c(1,1))

qqnorm(mtd2, pch=20, cex.axis=1.3, cex.lab=1.3, main="1249th Gaussian Group K-band Magnitutde") 
qqline(mtd2, lty=2, lwd=1.5,col="blue") 


### 1172th Group ###
mtd3 = GL(257)[,4] 

plot(ecdf(mtd3),verticals=TRUE, xlab="K (mag)", ylab="e.d.f.", main="",col="red") 
text(9,0.8,lab="1172th Non-Gaussian Group K-band Magnitutde",cex=0.8)  
curve(pnorm(x,mean(mtd3),sd(mtd3)),8,11.5,col="blue",add=TRUE)
legend("topleft",legend=c("empirical magnitude","theoretical normal"),col=c("red","blue"),box.lty=0,lty=c(1,1))

qqnorm(mtd3, pch=20, cex.axis=1.3, cex.lab=1.3, main="1172th Non-Gaussian Group K-band Magnitutde") 
qqline(mtd3, lty=2, lwd=1.5,col="blue") 

### 312th Group ###
mtd4 = GL(90)[,4] 

plot(ecdf(mtd4),verticals=TRUE, xlab="K (mag)", ylab="e.d.f.", main="",col="red") 
text(9,0.8,lab="312th Non-Gaussian Group K-band Magnitutde",cex=0.8)  
curve(pnorm(x,mean(mtd4),sd(mtd4)),8,11.5,col="blue",add=TRUE)
legend("topleft",legend=c("empirical magnitude","theoretical normal"),col=c("red","blue"),box.lty=0,lty=c(1,1))

qqnorm(mtd4, pch=20, cex.axis=1.3, cex.lab=1.3, main="312th Non-Gaussian Group K-band Magnitutde") 
qqline(mtd4, lty=2, lwd=1.5,col="blue") 

### 633th Group ###
mtd5 = GL(144)[,4] 

plot(ecdf(mtd5),verticals=TRUE, xlab="K (mag)", ylab="e.d.f.", main="",col="red") 
text(10,0.8,lab="633th Gaussian Group K-band Magnitutde",cex=0.8)  
curve(pnorm(x,mean(mtd5),sd(mtd5)),8,11.5,col="blue",add=TRUE)
legend("topleft",legend=c("empirical magnitude","theoretical normal"),col=c("red","blue"),box.lty=0,lty=c(1,1))

qqnorm(mtd5, pch=20, cex.axis=1.3, cex.lab=1.3, main="633th Gaussian Group K-band Magnitutde") 
qqline(mtd5, lty=2, lwd=1.5,col="blue") 

rej1  = matrix(NA,ncol=5,nrow=length(ng6))
rej1[1:49,1] = RejL[,1]
rej1[1:39,2] = RejL[1:39,2]
rej1[1:42,3] = RejL[1:42,3]
rej1[1:38,4] = RejL[1:38,4]
rej1[1:49,5] = RejL[1:49,5]

newRejL = cbind(rej1,ng6)
newRejL
colnames(newRejL) = c("D'Agostino","Shapiro-Wilk","Anderson-Darling","Lilliefors","Pearson's","Cramer-von Mises")
matrix(newRejL[,5],ncol=1)



########################################################################################################
#############################        Densities  of Groups        #######################################

#### Schechter #####

DenL = LDC[,8]

#### Density Hist #####

denl = hist(DenL,nclass=25,col="white",lty=0,main="Density of The Low Contrast Groups",
       xlab="Densities")
lines(c(denl$breaks, max(denl$breaks)),c(0,denl$counts,0), type='S',lty=1)

###### Theoretical and Empirical Curves ######

denl = hist(DenL,nclass=25,col="white",lty=0,main="Density of The Low Contrast Groups",
       xlab="Densities",freq=FALSE)
curve(dnorm(x,mean(DenL),sd(DenL)),0.5,4.5,add=TRUE,col="blue")
lines(density(DenL),lty=2)
legend(0.5,0.6,legend=c("Empirical","Normal"),col=c("black","blue"),lty=c(2,1),box.lty=0)

#### ECDF  ####
plot(ecdf(DenL),cex.points=0,lwd=2,col="green",main="Empirical VS Theoretical of The Low Contrast Groups")
curve(pnorm(x,mean(DenL),sd(DenL)),0.5,4.5,add=TRUE,col="blue")

c1 = ad.test(DenL)$p.value
c2 = cvm.test(DenL)$p.value
c3 = lillie.test(DenL)$p.value
c4 = shapiro.test(DenL)$p.value
c5 = pearson.test(DenL)$p.value
c6 = agostino.test(DenL)$p.value

normal = matrix(c(c1,c2,c3,c4,c5,c6),ncol=1) 
test   = c("passed","passed","passed","passed","passed","passed")
rownames(normal) = c("A-D","CVM","LILLIE","S-W","PEARSON","AGOSTINO")
norma = cbind(normal,test)
colnames(norma) = c("p.value","test")
norma



plot(normal,pch=7,main="p.values of the tests for the number density distribution"
     ,ylab="p.values",xlab="test")
text(1,0.31,"A-D",col="blue",cex=1)
text(2,0.37,"CVM",col="blue",cex=0.8)
text(3,0.38,"LILLIE",col="blue",cex=0.8)
text(4,0.24,"S-W",col="blue",cex=0.8)
text(5,0.33,"PEARSON",col="blue",cex=0.8)
text(5.7,0.10,"AGOSTINO",col="blue",cex=0.8)


###########################################################################################
#####################                Coordinates                ###########################

RA1 = LDC[,12]
DA1 = LDC[,13]
RA2 = HDC[,12]
DA2 = HDC[,13]

par(mfrow=c(2,2))
plot(RA1,DA1,xlim=c(0,300),ylim=c(-200,200),pch=20,col="blue",xlab="Right Ascension ", 
     ylab = "Declination",main="Low Contrast")
plot(RA2,DA2,xlim=c(0,300),ylim=c(-200,200),pch=20,col="red",xlab="Right Ascension ",
     ylab = "Declination",main="High Contrast")

#################### ALL TOGETHER #######################

plot(RA1, DA1,ylim = range(c(-200,200)),col="blue",pch=20,xlim=c(0,300),
     xlab = "Right Ascension (deg) " ,ylab="Declination (deg)")
points(RA2,DA2,col="red",pch=20)
legend("topright",legend=c("Low Contrast  -  274 Groups ","High Contrast  - 142  Groups"),
     col=c("blue","red"),lty=c(1,1),box.lty=0)


################### GAUSSIAN - NON GAUSSIAN GROUPS COORDINATES ####################

rgb(0,0,1,0.5)

###################       ACCORDING TO THE METANALYSIS          ###################           


mal = LDC[,10]
mal

nrl  = which(mal==0)
nrl
nnr1 = which(mal==1)
nnr1

RAMA1 = LDC[nrl,12]
DAMA1 = LDC[nrl,13]
NRAMA1 = LDC[nnr1,12]
NDAMA1 = LDC[nnr1,13]

mah  = HDC[,10]
mah

hnrl  = which(mah==0)
hnrl
hnnr1 = which(mah==1)
hnnr1

HRAMA1 = HDC[hnrl,12]
HDAMA1 = HDC[hnrl,13]
HNRAMA1 = HDC[hnnr1,12]
HNDAMA1 = HDC[hnnr1,13]

par(mfrow=c(2,2))
plot(RAMA1,DAMA1,xlim=c(0,300),ylim=c(-200,200),pch=20,col="blue",xlab="Right Ascension ", 
     ylab = "Declination",main="Gaussian Low Contrast")
plot(NRAMA1,NDAMA1,xlim=c(0,300),ylim=c(-200,200),pch=20,col="red",xlab="Right Ascension ",
     ylab = "Declination",main="Non Gaussian Low Contrast")
plot(HRAMA1,HDAMA1,xlim=c(0,300),ylim=c(-200,200),pch=20,col="blue",xlab="Right Ascension ", 
     ylab = "Declination",main="Gaussian High Contrast")
plot(HNRAMA1,HNDAMA1,xlim=c(0,300),ylim=c(-200,200),pch=20,col="red",xlab="Right Ascension ",
     ylab = "Declination",main="Non Gaussian High Contrast")


################### sepearated Gaussian and Non - Gaussian ######################

#### GAUSSIAN #####

plot(RAMA1, DAMA1,ylim = range(c(-200,200)),col=rgb(0,0.5,1,0.5),pch=20,xlim=c(0,300),
     xlab = "Right Ascension (deg) " ,ylab="Declination (deg)",main="GAUSSIAN")
points(HRAMA1,HDAMA1,col=rgb(1,0,1,0.5),pch=20)
legend("topright",legend=c("Low Contrast Gaussian Groups  -  164 Groups ",
                           "High Contrast Gaussian Groups -  98 Groups"),  
       col=c(rgb(0,0.5,1,0.5),rgb(1,0,1,0.5)),lty=c(1,1),box.lty=0)

#### NON - GAUSSIAN #####

plot(NRAMA1, NDAMA1,ylim = range(c(-200,200)),col=rgb(0,0,1,0.5),pch=20,xlim=c(0,300),
     xlab = "Right Ascension (deg) " ,ylab="Declination (deg)",main="NON-GAUSSIAN")
points(HNRAMA1,HNDAMA1,col=rgb(1,0,0,0.5),pch=20)
legend("topright",legend=c("Low Contrast  Non - Gaussian Groups  -  110 Groups ",
                           "High Contrast Non - Gaussian Groups -  44 Groups"),  
       col=c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)),
       lty=c(1,1),box.lty=0)

###

par(mfrow=c(2,2))
plot(RAMA1, DAMA1,ylim = range(c(-200,200)),col=rgb(0,0.5,1,0.5),pch=20,xlim=c(0,300),
     xlab = "Right Ascension (deg) " ,ylab="Declination (deg)",main="GAUSSIAN")
points(HRAMA1,HDAMA1,col=rgb(1,0,1,0.5),pch=20)
legend("topright",legend=c("Low Contrast Gaussian Groups  -  164 Groups ",
                           "High Contrast Gaussian Groups -  98 Groups"),  
       col=c(rgb(0,0.5,1,0.5),rgb(1,0,1,0.5)),lty=c(1,1),box.lty=0,cex=0.5)

plot(NRAMA1, NDAMA1,ylim = range(c(-200,200)),col=rgb(0,0,1,0.5),pch=20,xlim=c(0,300),
     xlab = "Right Ascension (deg) " ,ylab="Declination (deg)",main="NON-GAUSSIAN")
points(HNRAMA1,HNDAMA1,col=rgb(1,0,0,0.5),pch=20)

legend("topright",legend=c("Low Contrast  Non - Gaussian Groups  -  110 Groups ",
                           "High Contrast Non - Gaussian Groups -  44 Groups"),  
       col=c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)),
       lty=c(1,1),box.lty=0,cex=0.5)

#################### ALL TOGETHER #######################

plot(RAMA1, DAMA1,ylim = range(c(-200,200)),col=rgb(0,0.5,1,0.5),pch=20,xlim=c(0,300),
     xlab = "Right Ascension (deg) " ,ylab="Declination (deg)",main="METHOD METANALYSIS")
points(NRAMA1,NDAMA1,col=rgb(0,0,1,0.5),pch=20)
points(HRAMA1,HDAMA1,col=rgb(1,0,1,0.5),pch=20)
points(HNRAMA1,HNDAMA1,col=rgb(1,0,0,0.5),pch=20)
legend("topright",legend=c("Low Contrast Gaussian Groups  -  164 Groups ",
                           "High Contrast Gaussian Groups -  98 Groups",
                           "Low Contrast  Non - Gaussian Groups  -  110 Groups ",
                           "High Contrast Non - Gaussian Groups -  44 Groups"),  
       col=c(rgb(0,0.5,1,0.5),rgb(1,0,1,0.5),rgb(0,0,1,0.5),rgb(1,0,0,0.5)),
       lty=c(1,1),box.lty=0)


##########################################################
############ Dynamical Clusters and Relax ################

############  D'Agostino ###############

par(mfrow=c(2,2))
plot(LDC[ng1,5],LDC[ng1,3],pch=20,col="red",main="Non Gaussian",xlab="R200",ylab="Number of Members")
mtext("D'Agostino")
plot(LDC[g1,5],LDC[g1,3],pch=20,col="blue",main=" Gaussian",xlab="R200",ylab="Number of Members")
mtext("D'Agostino")

############  Shapiro - Wilk ###############

par(mfrow=c(2,2))
plot(LDC[ng2,5],LDC[ng2,3],pch=20,col="red",main="Non Gaussian",xlab="R200",ylab="Number of Members")
mtext("Shapiro-Wilk")
plot(LDC[g2,5],LDC[g2,3],pch=20,col="blue",main=" Gaussian",xlab="R200",ylab="Number of Members")
mtext("Shapiro-Wilk")
LDC[1,]
############  Anderson-Darling ###############

par(mfrow=c(2,2))
plot(LDC[ng3,5],LDC[ng3,3],pch=20,col="red",main="Non Gaussian",xlab="R200",ylab="Number of Members")
mtext("Anderson-Darling")
plot(LDC[g3,5],LDC[g3,3],pch=20,col="blue",main=" Gaussian",xlab="R200",ylab="Number of Members")
mtext("Anderson-Darling")

############ Lilliefors  ###############

par(mfrow=c(2,2))
plot(LDC[ng4,5],LDC[ng4,3],pch=20,col="red",main="Non Gaussian",xlab="R200",ylab="Number of Members")
mtext("Lilliefors")
plot(LDC[g4,5],LDC[g4,3],pch=20,col="blue",main=" Gaussian",xlab="R200",ylab="Number of Members")
mtext("Lilliefors")

############ Pearson  ###############

par(mfrow=c(2,2))
plot(LDC[ng5,5],LDC[ng5,3],pch=20,col="red",main="Non Gaussian",xlab="R200",ylab="Number of Members")
mtext("Pearson")
plot(LDC[g5,5],LDC[g5,3],pch=20,col="blue",main=" Gaussian",xlab="R200",ylab="Number of Members")
mtext("Pearson")

############ Cramer-von Misses ###############

par(mfrow=c(2,2))
plot(LDC[ng6,5],LDC[ng6,3],pch=20,col="red",main="Non Gaussian",xlab="R200",ylab="Number of Members")
mtext("Cramer-von Misses")
plot(LDC[g6,5],LDC[g6,3],pch=20,col="blue",main=" Gaussian",xlab="R200",ylab="Number of Members")
mtext("Cramer-von Misses")

############################# hubble diagram ###############################


nnL[g1]
#NG
dis1 = GL(1)[,5]
vel1 = GL(1)[,3]
vel1
#G
dis2 = GL(19)[,5]
vel2 = GL(19)[,3]
vel2

par(mfrow=c(1,2))
plot(dis1,vel1,main="Hubble Diagram of the 6th Non-Gaussian group",xlab="distance [Mpc]",ylab="velocity [km/s]")
plot(dis2,vel2,main="Hubble Diagram of the 74th Gaussian group",xlab="distance [Mpc]",ylab="velocity [km/s]")

par(mfrow=c(1,2))
plot(L[,5],L[,3],pch=20,xlab="distance [Mpc]",ylab="velocity [km/s]",main="Low Contrast galaxies")
plot(H[,5],H[,3],pch=20,xlab="distance [Mpc]",ylab="velocity [km/s]",main="High Contrast galaxies")

#########################################################################
########################## CONTOURS #####################################
install.packages("rgl")
library(rgl)
x = LDC[,12]
y= LDC[,13]
d= data.frame(LDC[,12:13])

kd <- ks::kde(d, compute.cont=TRUE)
contour_95 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                    z=estimate, levels=cont["5%"])[[1]])
contour_95 <- data.frame(contour_95)

contour()



###########################################
## drawcontour.R
## Written by J.D. Forester, 17 March 2008
###########################################


##This function draws an approximate density contour based on
##empirical, bivariate data.


##change testit to FALSE if sourcing the file
testit=TRUE


draw.contour<-function(a,alpha=0.95,plot.dens=FALSE, line.width=2, line.type=1, limits=NULL, density.res=300,spline.smooth=-1,...){
  ##a is a list or matrix of x and y coordinates (e.g., a=list("x"=rnorm(100),"y"=rnorm(100)))
  ## if a is a list or dataframe, the components must be labeled "x" and "y"
  ## if a is a matrix, the first column is assumed to be x, the second y
  ##alpha is the contour level desired
  ##if plot.dens==TRUE, then the joint density of x and y are plotted,
  ##   otherwise the contour is added to the current plot.
  ##density.res controls the resolution of the density plot

  ##A key assumption of this function is that very little probability mass lies outside the limits of
  ## the x and y values in "a". This is likely reasonable if the number of observations in a is large.
  
  require(MASS)
  require(ks)
  if(length(line.width)!=length(alpha)){
    line.width <- rep(line.width[1],length(alpha))
  }

  if(length(line.type)!=length(alpha)){
    line.type <- rep(line.type[1],length(alpha))
  }
  
  if(is.matrix(a)){
    a=list("x"=a[,1],"y"=a[,2])
  }
  ##generate approximate density values
  if(is.null(limits)){
    limits=c(range(a$x),range(a$y))
  }
  f1<-kde2d(a$x,a$y,n=density.res,lims=limits)

  ##plot empirical density
  if(plot.dens) image(f1,...)
  
  if(is.null(dev.list())){
    ##ensure that there is a window in which to draw the contour
    plot(a,type="n",xlim=limits[1:2],ylim=limits[3:4],...)
  }
    
  ##estimate critical contour value
  ## assume that density outside of plot is very small

  zdens <- rev(sort(f1$z))
  Czdens <- cumsum(zdens)
  Czdens <- (Czdens/Czdens[length(zdens)])
  for(cont.level in 1:length(alpha)){
    ##This loop allows for multiple contour levels
    crit.val <- zdens[max(which(Czdens<=alpha[cont.level]))]
   
    ##determine coordinates of critical contour
    b.full=contourLines(f1,levels=crit.val)
    for(c in 1:length(b.full)){
      ##This loop is used in case the density is multimodal or if the desired contour
      ##  extends outside the plotting region
      b=list("x"=as.vector(unlist(b.full[c][2])),"y"=as.vector(unlist(b.full[c][3])))
      
      ##plot desired contour
      line.dat<-xspline(b,shape=spline.smooth,open=TRUE,draw=FALSE)
      lines(line.dat,lty=line.type[cont.level],lwd=line.width[cont.level])
    }
  }
}
install.packages("ks")
library(ks)
draw.contour(a=d,alpha=c(0.95,0.5,0.05),line.width=c(2,1,2),
line.type=c(1,2,1),plot.dens=TRUE, xlab="X", ylab="Y")

if(testit){
    a<-list("x"=LDC[ng1,5],"y"=LDC[ng1,4])
  draw.contour(a=a,alpha=c(0.95,0.5,0.05),line.width=c(2,1,2),line.type=c(1,2,1),plot.dens=TRUE, xlab="X", ylab="Y")
}
LDC[1,]
################### Non Gaussian ################

d1 = data.frame(LDC[ng1,c(12,13)])
m <- ggplot(d1, aes(x = LDC[ng1,12], y = LDC[ng1,13])) +
 geom_point() +
 xlim(2, 300) +
 labs(title="Plot of Coordinates of Non-Gaussian low contrast groups",
        x ="RA (deg)", y = "DA (deg)")+
ylim(-100, 100)
m + geom_density_2d()

m + stat_density_2d(aes(fill = stat(level)), geom = "polygon")

################### Gaussian  ####################

d2 = data.frame(LDC[g1,c(12,13)])
m <- ggplot(d2, aes(x = LDC[g1,12], y = LDC[g1,13])) +
 geom_point() +
 xlim(2, 300) +
labs(title="Plot of Coordinates of Gaussian low contrast groups",
        x ="RA (deg)", y = "DA (deg)")+
 ylim(-100, 100)
m + geom_density_2d()

m + stat_density_2d(aes(fill = stat(level)), geom = "polygon")


################# LOW #########################

d3 = data.frame(LDC[,c(12,13)])
m <- ggplot(d3, aes(x = LDC[,12], y = LDC[,13])) +
 geom_point() +
 xlim(2, 300) +
labs(title="Plot of Coordinates of Low contrast groups",
        x ="RA (deg)", y = "DA (deg)")+
 ylim(-100, 100)
m + geom_density_2d()

m + stat_density_2d(aes(fill = stat(level)), geom = "polygon")

################# HIGH #########################

d4 = data.frame(HDC[,c(12,13)])
m <- ggplot(d4, aes(x = HDC[,12], y = HDC[,13])) +
 geom_point() +
 xlim(2, 300) +
 labs(title="Plot of Coordinates of Low contrast groups",
        x ="RA (deg)", y = "DA (deg)")+
ylim(-100, 100)
m + geom_density_2d()

m + stat_density_2d(aes(fill = stat(level)), geom = "polygon")

w1=matrix(ng1,ncol=1)
w2=matrix(ng2,ncol=1)
w3=matrix(ng3,ncol=1)
w4=matrix(ng4,ncol=1)
w5=matrix(ng5,ncol=1)
w6=matrix(ng6,ncol=1)




















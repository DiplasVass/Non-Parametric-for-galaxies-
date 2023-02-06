GC1 <- read.table("http://astrostatistics.psu.edu/MSMA//datasets/GlobClus_MWG.dat",header=T)

GC2 <- read.table("http://astrostatistics.psu.edu/MSMA/datasets/GlobClus_M31.dat",header=T)

K1 <- GC1[,2] 

K2 <- GC2[,2]

summary(K1) 

summary(K2)

# e.d.f. for globular cluster magnitudes

plot(ecdf(K1), cex.points=0, verticals=TRUE, xlab="K (mag)", ylab="e.d.f.", main="")
plot(ecdf(K2-24.90), cex.points=0, verticals=TRUE, add=TRUE)
text(-7.5, 0.8, lab="MWG") 
text(-10.5, 0.9, lab="M 31")

#Lorides Empistosynhs gia thn empeirikh 

install.packages("sfsmisc") 
library("sfsmisc") 
ecdf.ksCI(K1,ci.col="blue")
ecdf.ksCI(K2 - 24.90,ci.col="red")

## QQPLOTS

par(mfrow=c(1,3)) 
qqplot(K1, K2-24.90, pch=20, cex.axis=1.3, cex.lab=1.3, xlab="MWG", ylab="M31 - 24.90", main="")
qqnorm(K1, pch=20, cex.axis=1.3, cex.lab=1.3, main="")
qqline(K1, lty=2, lwd=1.5) 
text(-2.5, -6, pos=4, cex=1.3, "MWG normal QQ plot")
qqnorm(K2-24.90, pch=20, cex.axis=1.3, cex.lab=1.3, main="")
qqline(K2-24.90, lty=2, lwd=1.5)
text(-3, -7.5, pos=4, cex=1.3, "M31 normal QQ plot") 


##Tests For Normlality 

install.packages("nortest") 
library(nortest) 
cvm.test(K1)#  
cvm.test(K2)# 
ad.test(K1) #
ad.test(K2) #

## 

ks.test(K1, K2-24.90) # with distance modulus offset removed 

mood.test(K1, K2-24.90)  



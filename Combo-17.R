# Color-magnitude diagram for low-redshift COMBO-17 galaxies
COMBO_loz=read.table('http://astrostatistics.psu.edu/MSMA/datasets/COMBO17_lowz.dat', header=T, fill=T) 
dim(COMBO_loz)  
names(COMBO_loz) 
par(mfrow=c(1,2)) 
plot(COMBO_loz, pch=20, cex=0.5, xlim=c(-22,-7), ylim=c(-2,2.5), xlab=expression(M[B]~~(mag)), ylab=expression(M[280] - M[B]~~(mag)), main='')
# Two-dimensional kernel-density estimator
library(MASS) 
COMBO_loz_sm <- kde2d(COMBO_loz[,1], COMBO_loz[,2], h=c(1.6,0.4), lims = c(-22,-7,-2,2.5), n=500) 

image(COMBO_loz_sm, col=grey(13:0/15), xlab=expression(M[B]~~(mag)), ylab=expression(M[280] - M[B]~~(mag)), xlim=c(-22,-7), ylim=c(-2,2.5), xaxp=c(-20,-10,2)) 
par(mfrow=c(1,1))


# Standardize variables
Mag_std <- scale(COMBO_loz[,1]) 
Color_std <- scale(COMBO_loz[,2]) 
COMBO_std <- cbind(Mag_std,Color_std)

# Hierarchical clustering
COMBO_dist <- dist(COMBO_std) 
COMBO_hc <- hclust(COMBO_dist, method='complete') 
COMBO_coph <- cophenetic(COMBO_hc) 
cor(COMBO_dist, COMBO_coph)


# Cutting the tree at k=5 clusters

plclust(COMBO_hc, label=F) 
COMBO_hc5a <- rect.hclust(COMBO_hc, k=5, border='black') 
str(COMBO_hc5a) 
COMBO_hc5b <- cutree(COMBO_hc, k=5) 
str(COMBO_hc5b) 
plot(COMBO_loz, pch=(COMBO_hc5b+19), cex=0.7, xlab=expression(M[B]~~(mag)), ylab=expression(M[280] - M[B]~~(mag)), main='')




groups = read.table("groups.txt" , sep="")
groups

DA = groups[,1:4]
RA = groups[,5:7]

GP = groups[,8:15]
GP
colnames(GP) = c("z","s1","er_s1","M/L","er_M/L","Ng","M","er_M")
GP

hist(GP[,1])

################################
install.packages("MASS")

library(MASS)

length(galaxies)
plot(gal,pch=20,ylab="velocity [1000km/s]",xlab="galaxies")
gal <- galaxies/1000
c(width.SJ(gal, method = "dpi"), width.SJ(gal))
plot(x = c(0, 40), y = c(0, 0.3), type = "n", bty = "l",
     xlab = "velocity of galaxy (1000km/s)", ylab = "density")
rug(gal)
lines(density(gal, width = 3.25, n = 200), lty = 1)
lines(density(gal, width = 2.56, n = 200), lty = 3)

ad.test(gal)















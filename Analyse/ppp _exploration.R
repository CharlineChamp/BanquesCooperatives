library(spatstat)

vignette('getstart')
beginner

banque <- read.csv("Données/bdd_coordonnees_banques2022.csv")


mypattern <- ppp(banque[,4], banque[,5], c(-6,10), c(41,52))
class(mypattern)
plot(mypattern)

summary(mypattern)
plot(Kest(mypattern))
help(Kest)


plot(density(mypattern))
points(mypattern, cex=.1)
help(density.ppp)

quadratcount(mypattern,4,5)
domain(mypattern)

mypattern2 <- ppp(banque[,4], banque[,5],c(min(banque$Longitude),max(banque$Longitude)), c(min(banque$Latitude),max(banque$Latitude)))
plot(mypattern2)
plot(density(mypattern2))
points(mypattern2, cex=.1)

long <- fd_c[[2]][[1]][[1]][[1]][,1]
lat <- fd_c[[2]][[1]][[1]][[1]][,2]

ow <- owin(poly = list(x = long,y = lat))
plot(ow)
points(mypattern2[ow],col='red')
mypattern2[ow]$n #pour avoir le nombre de points dans le polygone

plot(mypattern2[ow]$x,mypattern2[ow]$y) #coordonnées des banques dans le polygone 

coord.test <- data.frame(long=mypattern2[ow]$x, lat=mypattern2[ow]$y)


#union.owin(ow1,ow2,ow3)
#glm(N~TxPauvrete+……, family=poisson)





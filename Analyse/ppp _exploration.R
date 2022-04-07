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
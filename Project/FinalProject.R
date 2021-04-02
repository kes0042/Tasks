# Hypothesis:
# There is a positive correlation between the mass and the basal metabolic rate in a wide range of mammals. #

# Import data ####
#rm (list = ls())
library(phytools)
library(nlme)
library(RRphylo)

setwd("~/Desktop/Evolution/Tasks/Project")
# read in data
mydata <- read.csv ("MassBMRproject.csv")
head(mydata)

# rename data to match phylogeny
mydata[,1] <- gsub(" ", "_", mydata[,1])

# read in trees
Trees <- read.nexus("output.nex")
tree <- Trees[[1]]

# find species missing from phylogeny
Drops <- setdiff(mydata[,1], tree$tip.label)
DropRows <- sapply(Drops, function(x) grep(x, mydata[,1]))
mydata2 <- mydata[-DropRows,]
colnames(mydata2) <- c("species", "raw_bm", "logbm", "raw_bmr", "logbmr")

Cor <- corBrownian(1, tree)

par(mar=c(4,5,1,1), mgp=c(2.5,0.3,0), tck=-0.01, las=1)
plot(mydata2[,5], mydata2[,3], pch=16, col=rgb(0,0,0,0.5), xlab="log10 BMR", ylab="log10 mass")

# incorrect regression line--No phylogeny
NoPhy <- lm(mydata2[,3]~mydata2[,5])
abline(NoPhy, col='red', lty=2)

# phylogenetically corrected linear regression
model01 <- gls(logbm~logbmr, data=mydata2, correlation=Cor)
abline(model01, col='blue', lty=2, lwd=2)

#Red= is the just data not corrected relationship

#blue= is the phylegentic corrected one 

logBMR <- setNames(mydata2[,"logbmr"], mydata2[,"species"])
RRbmr <- RRphylo(tree, logBMR)
logBM <- setNames(mydata2[,"logbm"], mydata2[,"species"])
RRbm <- RRphylo(tree, mydata2[,"logbm"], cov=RRbmr, x1=mydata2[,"logbmr"])

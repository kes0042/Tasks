# Hypothesis:
# There is a positive correlation between the total begging call time measured in seconds between Vidua macroua and their host species Extrilda astrid. #

# Import data ####
#rm (list = ls())
setwd("~/Desktop/Evolution/Tasks/Project")
mydata <- read.csv ("Evolutionproject.csv")
mydata


par(mar=c(4,5,1,1), las=1, tck=-0.01, mgp=c(3,0.5,0))
boxplot(mydata[,4]~mydata[,1], xlab="species", ylab="call time", boxwex=0.2, col=c("#b3e2cd", "#fdcdac"))

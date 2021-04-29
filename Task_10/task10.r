setwd("~/Desktop/Evolution/Tasks/Task_10")
install.packages("diversitree")
library("diversitree")
transition_0to1 <- 0.1
transition_1to0 <- 0.1
speciation_0 <- 0.2
extinction_0 <- 0.1
speciation_1 <- 0.2
extinction_1 <- 0.1
maxN <- 1e3
maxT <-50
tree.bisse(Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0))
simTree <- tree.bisse(Pars, max.taxa = maxN, max.t =maxT)
str(simTree)
help(tree.bisse)
stateTable <- table (simTree$tip.state)
stateTable / sum(stateTable)
#1
Frequencies <- c("State 0", "State 1")
Colors <- c("Yellow", "Blue")
Data <- matrix(c(0.66, 0.70, 0.67, 0.655,0.574,0.32, 0.4, 0.55,0.59, 0.67, 0.457), nrow = 2, ncol =6, byrow=TRUE)
Data
Difference <- c(0.11, 0.2, 0.02, 0.01, 0.3, 0.2)
Fre1 <- c(0.23, 0.33,0.56, 0.40, 0.53, 0.08)
Fre0 <- c(0.63, 0.45, 0.69, 0.546, 0.622, 0.53)
barplot(Data, names.arg= Difference, main= "Frequency Changes based on R values", xlab= "Difference", ylab= "Frequency", beside=TRUE, col=c("Yellow", "BLue"))

#2
Frequencies <- c("State 0", "State 1")
Colors <- c("Pink", "Grey")
Data <- matrix(c(0.80, 0.75, 0.98, 0.60, 0.54, 0.89, 0.50, 0.9, 0.89, 0.99, 0.50, 0.875, 0.342, 0.789, 0.99, 0.785, 0.675, 0.798, 0.33, 0.54, 0.675, 0.33, 0.567, 0.987, 0.99, 0.32, 0.1, 0.7, 0.4, 0.067, 0.54, 0.33, 0.1, 0.55), nrow= 2, ncol= 17, byrow=TRUE)
Data
Difference <- c(0.04, 0.33, 0.0,0.0,0.0,0.1, 0.0 ,0.2, 0.1, 0.23, 0.21, 0.12, 0.21, 0.5, 0.0, 0.0, 0.3)
barplot(Data, names.arg=Difference, main="How close to 0", xlab="Difference", ylab="Frequencies", col= c("Pink", "Grey"))

#3
Data
Freq1_1 <- Data [,2]
Freq1_2 <- Data [,4]
Variance1 <- Freq1_1
Variance2 <- Freq1_2
VarianceMatrix <- c(Variance1, Variance2)
trial <- c(1,2)
barplot(VarianceMatrix, main='Variance of Freq', ylim=c(0,0.8), xlab="trial", ylab="variance", col="orange")
dev.off()
#4

Data
Freq_6 <- Data [,6]
Freq_6
NDR_9 <- Data[,9]
plot (NDR_9, Freq_6, xlab="variation rate of State", ylab="Freq of State", main="The effects Variation rate has on frequency")
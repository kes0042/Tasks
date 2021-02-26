setwd("~/Desktop/Evolution/Tasks/Task_05")
install.packages("learnPopGen")
library("learnPopGen")
install.packages("coala")
library("coala")
install.packages("phytools")
library("phytools")
model <-coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy =2 ) +
feat_mutation (10) +
sumstat_trees() +
sumstat_nucleotide_div()
stats <- simulate(model, nsim = 1)
Diversity <- stats$pi
head(Diversity)
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees [[1]][1])
plot(t1)
axisPhylo()
# there may be identical species 
Age1 <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot (t2)
axisPhylo()
# 
# They do not look the same  at all.  In one plot, 2,4,5,8,10,9 are more closely related.  In the other, 4,8,5,2,6,1,3,10,9 are more closely related. 
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
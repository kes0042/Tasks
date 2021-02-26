

install.packages("learnPopGen")
library("learnPopGen")
install.packages("coala")
library("coala")
install.packages("phytools")
library("phytools")
model<- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
feat_mutation(10) +
feat_recombination(10) +
sumstat_trees() +
sumstat_nucleotide_div()
stats<- simulate(model, nsim = 1)
Diversity<- stats$pi
# no, the numbers are not the same it ranges from 16.644 to 4.46 the difference are caused by the differnt loci 
Nloci<- length(stats$trees)
t1<- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
# 6 may there is an exiction or identical alleles 
Age1 <- max(nodeHeights(t1))
t2<- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
# the first is 2 and the second is 1 so no it is not the same 
# no they do not match.  Thep plots differ in which is more closely related to one another and the timing differs
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees [[1]][1])
t1_2 <- read.tree(text=stats$trees [[1]][2])
compare.chronograms(t1_1, t1_2)
for (locus in 1:Nloci) {
	ntrees <- length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if (locus == 1 && n == 1) {
			outPhy <- read.tree(text=stats$trees[[locus]][n])
			}
			else {
				outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
				}
			}
		}		
par(mfrow=c(1,1))
densityTree(outPhy)
model3 <- coal_model(10, 50) +
feat_mutation(par_prior("theta", sample.int(100, 1))) +
sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta<- sapply(stats, function(x) x$pars [["theta"]])
plot(mean_pi, theta)
abline(lm(mean_pi ~ theta))

#1
coalescent.plot(n=5, ngen=30, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)
#so, the 
#2
coalescent.plot(n=12, ngen=30, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)
#3 
coalescent.plot(n=10, ngen=30, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)

#question 1: The first simulation started with 5.  The next started with 12.  The last started with 10. I modified these by changing the n= number in each. 

#1 
coalescent.plot(n=5, ngen=35, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test) 

#2
coalescent.plot(n=5, ngen=45, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)

#3
coalescent.plot(n=5, ngen=50, col.order="alternating")
#4
coalescent.plot(n=5, ngen=49, col.order="alternating")
#question number 2: 49 goes to fixation at 10 
#question number 3: 5 offspring 
coalescent.plot(n=5, ngen=5, col.order="alternating")
#question 4: it has a large impact because each simulation was different each time 
#question 5: I think it is alive in generation 0 
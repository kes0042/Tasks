# to make population
setwd('~/Desktop/Evolution/Tasks/Task_03')
trueMean1 <-5
trueSD1 <-5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)

# take sample of eah population
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)

#compare the samples using a box plot
boxplot(Sample1, Sample2)
# No, I dont think they are significantly different 


source("http://jonsmitchell.com/code/simFxn04.R")
# now to make grandparents 
MatGrandma <- makeFounder("grandma_mom")
head(MatGrandma)
MatGrandpa <- makeFounder("grandpa_mom")
head(MatGrandpa)
PatGrandma <- makeFounder("grandma_da")
head(PatGrandma)
PatGrandpa <- makeFounder("grandpa_da")
head(PatGrandpa)
# make grandparents make Alan which is the dad
Alan <- makeBaby(PatGrandma, PatGrandpa)
# make Brenda 
Brenda <-makeBaby(MatGrandma, MatGrandpa)
# focus is the first child from Brenda and Alan
Focus <- makeBaby(Brenda, Alan)
# can search to see how many genes came from one known ancestor using grep() 
ToMom <- length( grep("mom", Focus))/length (Focus)
ToMom
# the number will be .5 it does not match qhat I thought
ToMomMom <- length(grep("grandma_mom", Focus))/ length (Focus)
ToMomMom #the number was .06655
ToMomDad <- length(grep("grandpa_mom", Focus))/ length (Focus)
ToMomDad # the nimber was .43345
# No, the numbers are not equally realted 
# make sibling
Sibling_01 <- makeBaby(Brenda, Alan)
# I would expect maybe .5 the answer is 
ToSib <- length(intersect(Focus, Sibling_01))/ length(Focus)
ToSib
# the number was .59855
# make 1,000 siblings
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/ length(Focus))
# summarize the data 
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")

HWE<- function(p) {
aa<- p^2
ab<- 2 * p * (1-p)
bb<- (1-p)^2
return(c(aa=aa, ab=ab, bb=bb))	}
HWE(0.5)
# aa .25 ab .50 bb .25
plot(1, 1,type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="frep. allele a", ylab="geno. freq")# makes balnk plot
# calculate genotype frequ for a bunch pf allele
p <- seq(from = 0, to= 1, by= 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[, "aa"], lwd=2, col="red")    #added the aa line on the graph
# the graph shows an increase in geno freq    aa increases, ab increases reaches a peak in the middle then decreases, bb decreases
lines(p, GenoFreq[, "ab"], lwd=2, col="purple")     # added the ab line on the graph
lines(p, GenoFreq[, "bb"], lwd=2, col="blue")      #added the bb line on the graph
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd2, bty="n")
Pop <- simPop(500)
# add points to the HWE plot
points(Pop[,"freqa"], Pop[, "Genotypes.aa"]/ 500, pch=21, bg="red")

# no I do not think it matches to Hardy-Weinberg because from what I interpret from the graph the majority are aa instead ab

Pop <- simPop(50)
points(Pop[,"freqa"], Pop[, "Genotypes.aa"]/ 50, pch=22, bg="red")
# the amount in the lower frequencies aapear

install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)  # the graph data is recorded between .8 to .1 and includes multiple increases and decreases 
x <- genetic.drift(Ne=400, nrep=5, pause=0.01)  # the graph shrunk to the data falling between .2 and .8 but there are still many increases and decreases
x <- genetic.drift(Ne=700, nrep=5, pause=0.01)  # this shrunk the data to between .3 and .8 there are also not as drastic increases and decreases
PopSizes <- 5:50  #makes a bunch of populations of different sizes between 5 and 50 individuals 
Samples <- 5:50 #say that there are 5 populations with each given size		
#simulate all 230 of populations and get the time one of the two allels went extincit 
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)  # lm() used to fit a line to data 
summary(Line)
Line$coef # extract coefficients use $coef
plot(Samples, tExt)
abline(Line)
# graph has a gradual increasing line with data plots throughout the graph the highest at 45 samples at tExt 400 

#compare
Line2 <-lm(tExt~Samples+0)
summary(Line2)
Line2$coef
plot(Samples, tExt)
abline(Line2)
# there is not nearly as a steep increase as in the other graph the hightest is still at the same as the last the data is much more closer to the line overall than that first graph
# the data spots are father from the line overall in the first graph and closer to the line in the second graph


#extra credit
head(Line)
summary(Line)
Line.ols <- Line <- lm(tExt ~ Samples)
install.packages("ggplot2")
library("ggplot2")
ggplot(date= Line, aes(y= Samples, x= tExt)) + geom_point(col= "blue") + geom_abline(slope = 0)
rlang::last_error()




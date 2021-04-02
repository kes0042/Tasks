setwd("~/Desktop/Evolution/Tasks/Task_08")
library("phytools")
tree <-read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree, type="fan")
#question 1: 82 tips

data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
dim(data)
class(data)

#question2: there are 82 dimensions and 1 branch

svl <- setNames(data$svl, rownames(data))
svl

Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
#question 3: stored in the tips which is svl .     The CI95 elements are on the ancestral state.

#quesiton 4: the function computes the contrast state at the root and re-roots the tree at interval nodes


par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)

# labels the tips with points instead of words using pch 
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
# adds the ancestral sates 
nodelabels(pch=16, cex=0.25*Ancestors$ace)
# visiualize the data in different way to see the differences in the tips 
obj <- contMap(tree, svl, plot=F)
plot (obj, type="fan", legend= 0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
# question 5: 
fossilNodes <-c()
nodeN <-c()

plot(tree, cex=0.5)
# question 5:
for (i in 1:nrow(fossilData))	{
	Node <- fastMRCA(tree, fossilData [i,"tip1"], fossilData[i,"tip2"])
	fossilNodes[i] <- fossilData[i, "svl"]
	nodeN[i] <- Node
	nodelabels(node=Node, pch=21, bg="red", cex=1.2)
}
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)

# question 7: fossils change the estimated ancestral sizes because you can look at the fossils and determine based off of how far the species is on the tree the relative size you can also look at the speices between two fossils and do the same thing. 
# question 8-10:
install.packages("geiger")
library("geiger")
?fitContinuous
fitContinuous(tree, svl, model='EB')
fitContinuous(tree, svl, model='OU')
fitContinuous(tree, svl, model='BM')
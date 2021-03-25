setwd("~/Desktop/Evolution/Tasks/Task_07")
install.packages("phytools")
library("phytools")
install.packages("ape")
text.string <- "(((((((cow, pig), whale), (bat, (lemur, human))),(robin, iguana)), coelacenth), (gold_fish, trout)), shark) ;"
vert.tree <-read.tree (text=text.string)
plot(vert.tree, edge.width=2)
nodelabels (frame="circle", bg='white', cex=1)
# question 1: I think a shark would be more closely related to goldfish because they both come from node 13, then node 14 branches, then node 23 branches which is where the goldfish is.  There are less different node splits between goldfish and shark than human and goldfish. 

vert.tree

# question 2: no, there are not branch lengths in this tree

str( vert.tree)
tree <- read.tree(text= "(((A,B), (C,D)), E) ;")
tree <- compute.brlen(tree)
plotTree (tree, offset=1)
tiplabels (frame="circle", bg='lightblue', cex=1)
nodelabels (frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge

AnolisTree <- force.ultrametric (read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col= 'black', border='white', main="", xlab=" edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0,6))
tipEdges <-which(AnolisTree$edge[, 2] <= Ntip (AnolisTree))
Lengths <-AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths) [which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply (AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex= 0.25)

# each edge has a length, and they're in order.    First value of edge length is the length of the edge defined by the first row of the edge matrix. 

?plot.phylo
# question 3:
plotTree(AnolisTree,offset=1, show.tip.label=FALSE) 

# quesiton 4:
anolis <- collapseTree(AnolisTree)

#question 5:
plotTree(AnolisTree,offset=1, show.tip.label=TRUE, tip.color="red")


# question 6, 7, 8: 
AnolisTree$edge
EdgesThatAreTips <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
TipLengths <- AnolisTree$edge.length[EdgesThatAreTips]
AnolisTree$edge.length
AnolisShortestTips <- which(TipLengths == min(TipLengths))
Anolistree_no_short <- drop.tip(tree, AnolisShortestTips[1])
plot(Anolistree_no_short)

# step 1. find only tip lengths  use R to find the vector of TIP LENGTHS x<- c(1,2,3,...)

# 2. find which is shorter use which(x==min(x))
# 3. drop it  drop.tip()

ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)

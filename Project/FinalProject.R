# Hypothesis:
# There is a positive correlation between the total begging call time measured in seconds between Vidua macroua and their host species Extrilda astrid. #

# Import data ####
rm (list = ls())
setwd("~/Desktop/Evolution/Project")
mydata <- read.csv ("Evolutionproject.csv")
mydata_PTW_CW <- subset (mydata, Species == "Vidua macroura" | Species == "Estrilda astrild")
boxplot (DeltaTime.s. ~ Species, data = Species, ylab = "Call time(s)")



setwd('~/Desktop/Evolution/Tasks/Task_02')
# read.csv() reads a file from your computer and stores the information in whatever object you tell it to
Data <- read.csv ('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)

length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]

Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds ,]

head(berenMilk)
Feeds <- which(Data[,'event'] == 'bottle')
head(Feeds)
Feeds <- which(Data$event == 'bottle')
head(Feeds)
dayID <- apply(Data, 1, function(x) paste(x[1:3],collapse='-'))
head(dayID)
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
head(dateID)
Data$age <- dateID - dateID [which(Data$event == 'birth')]
head(data)
beren2<-Data
beren3<- beren2[order(beren2$age) ,]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds] ,  beren3$age[Feeds], length)
head(numFeeds)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
head(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
head(berenANOVA)
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle" , ylab = "amount of milk consumed (oz)" )
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab=" ounces of milk")
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off() 
source("http://jonsmitchell.com/code/plotFxn02b.R")
unique(beren3$event)
beren4 <- beren3[Naps,]
startHour <-(beren4$start_hour)
startMin <- (beren4$start_minute)
stopHour <- (beren4$start_minute)
stopHour <-(beren4$end_hour)
stopMin <- (beren4$end_minute)
startHour
startMin
stopHour
beren4$sleepTime <-((stopHour - startHour)* 60) +(stopMin-startMin)
beren4
totalNap <- tapply(beren4$sleepTime, beren4$age, sum)
totalNap
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalNap)), totalNap, type= "b",pch=16, xlab="age in days", ylab='Nap time in minutes')
cor.test(beren4$start_hour,beren4$sleepTime)
# These show a negative correlation. 
# hypothesis: The amount of time that Beren naps decreased as solid food consumption increased 
# Questions1: The first two hypotheses would be inappropriate because there is not enough data to support them. 
# Question 2: This graph is difficult to read because of the small x-axis scale which makes the data scrunched together which makes it hard to determine the exact values for each point.

Amox <- which(beren3$event == "amoxicilin")
amoxData <- beren3[Amox,]
head(amoxData)
totalFeed
unique(amoxData$age)
head(amoxData)
unique(amoxData$age)
DoseDays <- unique(amoxData$age)
totalFeed
totalFeed[as.character(DoseDays)]
DoseDays
as.character(DoseDays)
doseFeedNA <- as.character(DoseDays)
na.omit(as.data.frame(x))
doesFeedsNA <-x
doseFeedsNA
na.omit(as.data.frame(doseFeedsNA))[,1] 
doseFeeds <- na.omit(as.data.frame(doseFeedsNA))[,1]
NoAmox <- setdiff(names(totalFeed), as.character(DoseDays))
NoAmox
nodoseFeeds <- totalFeed [NoAmox]
nodoseFeeds
boxplot(doseFeeds, nodoseFeeds)
t.test(doseFeeds, nodoseFeeds)

#There is not a significant difference between the amount of amoxicilin and total ounces of milk  The test did not prove my hypothesis 

# Object 1 = totalFeed on days with amoxilinc
	# find the unique days on amoxicilin-- unique(), save as DoseDays

	# use DoseDays (unique days for amox) and find feeds on only those days
	### trick here is that the NAMES of totalFeed are the days, so we havezto ask it by name, not number. Use as.character(). Call this doseFeedsNA

	# remove NAs from your data using na.omit(). 
doseFeeds <- na.omit(as.data.frame(doseFeedsNA))[,1] # Replace x with whatever you call the totalFeed on dose days


# Object 2 = totalFeed on days WITHOUT amox
NoAmox <- setdiff(names(totalFeed), as.character(DoseDays))

	# then subset totalFeeds by NoAmox to get nodoseFeeds

	# statistical test to compare them
	### t test and boxplot are best
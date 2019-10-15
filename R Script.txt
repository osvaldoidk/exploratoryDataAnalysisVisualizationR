# Libraries used
library(RColorBrewer)
library(MASS)


#Question 1#

myfulldata <- rbind(xaa, xab, xac, xad, xae, xaf, xag, xah, xai)
myfulldataColumns <- myfulldata[c(1,2,18)]
covarianceMatrix <- cov(myfulldataColumns)
corelationValue <- cor(myfulldataColumns)

#Question 2

plot(myfulldataColumns$V2, myfulldataColumns$V18, xlab = "Circularity", ylab = "Hollows Ratio", pch = 19)

#Question 3

hist(myfulldataColumns$V1, xlab = "Compactness", main = "Histogram for whole dataset")
hist(myfulldataColumns$V2, xlab = "Circularity", main = "Histogram for whole dataset")

myfulldataColumnsForHist <- myfulldata[c(1, 2, 18, 19)]
vanClass <- myfulldataColumnsForHist[myfulldataColumnsForHist$V19 == 'van',]
saabClass <- myfulldataColumnsForHist[myfulldataColumnsForHist$V19 == 'saab',]
busClass <- myfulldataColumnsForHist[myfulldataColumnsForHist$V19 == 'bus',]
opelClass <- myfulldataColumnsForHist[myfulldataColumnsForHist$V19 == 'opel',]

hist(vanClass$V1, xlab = "Compactness", main = "Histogram for van")
hist(saabClass$V1, xlab = "Compactness", main = "Histogram for saab")
hist(opelClass$V1, xlab = "Compactness", main = "Histogram for opel")
hist(busClass$V1, xlab = "Compactness", main = "Histogram for bus")

hist(vanClass$V2, xlab = "Circularity", main = "Histogram for van")
hist(saabClass$V2, xlab = "Circularity", main = "Histogram for saab")
hist(opelClass$V2, xlab = "Circularity", main = "Histogram for opel")
hist(busClass$V2, xlab = "Circularity", main = "Histogram for bus")

#Question 4

boxplot(myfulldataColumnsForHist$V1,
        myfulldataColumnsForHist$V1[myfulldataColumnsForHist$V19 == 'van'], 
        myfulldataColumnsForHist$V1[myfulldataColumnsForHist$V19 == 'saab'], 
        myfulldataColumnsForHist$V1[myfulldataColumnsForHist$V19 == 'bus'], 
        myfulldataColumnsForHist$V1[myfulldataColumnsForHist$V19 == 'opel'], 
        names = c("All classes","van", "saab", "bus", "opel"), ylab = "Compactness", main = "Boxplot")


#Question 5

plot(myfulldataColumnsForHist$V1, myfulldataColumnsForHist$V2, pch = 19, xlab = "Compactness", ylab = "Circularity",
     col=c("red","blue","green", "black")[myfulldataColumnsForHist$V19])
legend(x = "topleft", legend = levels(myfulldataColumnsForHist$V19), col=c("red","blue","green", "black"), pch=19)

plot(myfulldataColumnsForHist$V1, myfulldataColumnsForHist$V18, pch = 19, xlab = "Compactness", ylab = "Hollows Ratio",
     col=c("red","blue","green", "black")[myfulldataColumnsForHist$V19])
legend(x = "topleft", legend = levels(myfulldataColumnsForHist$V19), col=c("red","blue","green", "black"), pch=19)

plot(myfulldataColumnsForHist$V2, myfulldataColumnsForHist$V18, pch = 19, xlab = "Circularity", ylab = "Hollows Ratio",
     col=c("red","blue","green", "black")[myfulldataColumnsForHist$V19])
legend(x = "topleft", legend = levels(myfulldataColumnsForHist$V19), col=c("red","blue","green", "black"), pch=19)

#QUESTION 6

fullDataWithoutClass <- myfulldata
zscore <- fullDataWithoutClass
zscore$V1 <- (fullDataWithoutClass$V1 - mean(fullDataWithoutClass$V1)) / sd(fullDataWithoutClass$V1)
zscore$V2 <- (fullDataWithoutClass$V2 - mean(fullDataWithoutClass$V2)) / sd(fullDataWithoutClass$V2)
zscore$V3 <- (fullDataWithoutClass$V3 - mean(fullDataWithoutClass$V3)) / sd(fullDataWithoutClass$V3)
zscore$V4 <- (fullDataWithoutClass$V4 - mean(fullDataWithoutClass$V4)) / sd(fullDataWithoutClass$V4)
zscore$V5 <- (fullDataWithoutClass$V5 - mean(fullDataWithoutClass$V5)) / sd(fullDataWithoutClass$V5)
zscore$V6 <- (fullDataWithoutClass$V6 - mean(fullDataWithoutClass$V6)) / sd(fullDataWithoutClass$V6)
zscore$V7 <- (fullDataWithoutClass$V7 - mean(fullDataWithoutClass$V7)) / sd(fullDataWithoutClass$V7)
zscore$V8 <- (fullDataWithoutClass$V8 - mean(fullDataWithoutClass$V8)) / sd(fullDataWithoutClass$V8)
zscore$V9 <- (fullDataWithoutClass$V9 - mean(fullDataWithoutClass$V9)) / sd(fullDataWithoutClass$V9)
zscore$V10 <- (fullDataWithoutClass$V10 - mean(fullDataWithoutClass$V10)) / sd(fullDataWithoutClass$V10)
zscore$V11<- (fullDataWithoutClass$V11 - mean(fullDataWithoutClass$V11)) / sd(fullDataWithoutClass$V11)
zscore$V12 <- (fullDataWithoutClass$V12 - mean(fullDataWithoutClass$V12)) / sd(fullDataWithoutClass$V12)
zscore$V13 <- (fullDataWithoutClass$V13 - mean(fullDataWithoutClass$V13)) / sd(fullDataWithoutClass$V13)
zscore$V14 <- (fullDataWithoutClass$V14 - mean(fullDataWithoutClass$V14)) / sd(fullDataWithoutClass$V14)
zscore$V15 <- (fullDataWithoutClass$V15 - mean(fullDataWithoutClass$V15)) / sd(fullDataWithoutClass$V15)
zscore$V16 <- (fullDataWithoutClass$V16 - mean(fullDataWithoutClass$V16)) / sd(fullDataWithoutClass$V16)
zscore$V17 <- (fullDataWithoutClass$V17 - mean(fullDataWithoutClass$V17)) / sd(fullDataWithoutClass$V17)
zscore$V18 <- (fullDataWithoutClass$V18 - mean(fullDataWithoutClass$V18)) / sd(fullDataWithoutClass$V18)
zscore$B <- 0
zscore$B[zscore$V19 == 'bus'] <- 1
zscore$V <- 0
zscore$V[zscore$V19 == 'van'] <- 1

modelB <- lm(B ~ (V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18),data = zscore)
modelV <- lm(V ~ (V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18),data = zscore)

str(summary(modelB))
str(summary(modelV))
modelB
modelV


#QUESTION 7


# Parts of code from https://www.guru99.com/r-decision-trees.html

# split the data for B
set.seed(100)
split= sample.split(zscore$B, SplitRatio =0.8)
trainB = subset(zscore, split ==TRUE)
testB = subset(zscore, split ==FALSE)
dim(testB)
dim(trainB)
head(testB)
head(trainB)

#First model for B
treeB1 <- rpart(trainB$B~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,trainB,method="class",minsplit =100, maxdepth = 20)
rpart.plot(treeB1) 
printcp(treeB1) # display the results
#Predict Class B
predictB1<- predict(treeB1,testB, type = "class")
table <- table(testB$B,predictB1)
#accuracy for test
accuracy_Test <- sum(diag(table1)) / sum(table1)
accuracy_Test

#Second model for B
treeB2 <- rpart(trainB$B~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,trainB,method="class",minsplit =100, maxdepth = 20)
rpart.plot(treeB2) 
printcp(treeB2) # display the results
#Predict Class B
predictB2<- predict(treeB2,testB, type = "class")
tableB <- table(testB$B,predictB2)
#accuracy for test
accuracy_Test <- sum(diag(tableB)) / sum(tableB)
accuracy_Test

#Third model for B
treeB3 <- rpart(trainB$B~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,trainB,method="class",minsplit =100, maxdepth = 20)
rpart.plot(treeB3) 
printcp(treeB3) # display the results
#Predict Class B
predictB3<- predict(treeB3,testB, type = "class")
tableB <- table(testB$B,predictB3)
#accuracy for test
accuracy_Test <- sum(diag(tableB)) / sum(tableB)
accuracy_Test

# Variable V

# split the data for V
set.seed(100)
split= sample.split(zscore$V, SplitRatio =0.8)
trainV = subset(zscore, split ==TRUE)
testV = subset(zscore, split ==FALSE)
dim(testV)
dim(trainV)
head(testV)
head(trainV)

#First model for v
treeV1 <- rpart(trainV$V~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,trainV,method="class",minsplit =100, maxdepth = 20)
rpart.plot(treeV1,extra=106) #https://www.guru99.com/r-decision-trees.html
printcp(treeV1) # display the results
#Predict Class V
predictV1<- predict(treeV1,testV, type = "class")
tableV1 <- table(testV$V,predictV1)
#accuracy for test
accuracy_TestV <- sum(diag(tableV1)) / sum(tableV1)
accuracy_TestV

#Second model for v
treeV2 <- rpart(trainV$V~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,trainV,method="class",minsplit =100, maxdepth = 20)
rpart.plot(treeV2,extra=106) #https://www.guru99.com/r-decision-trees.html
printcp(treeV2) # display the results
#Predict Class V
predictV2<- predict(treeV2,testV, type = "class")
tableV2 <- table(testV$V,predictV2)
#accuracy for test
accuracy_TestV <- sum(diag(tableV2)) / sum(tableV2)
accuracy_TestV

#Third model for v
treeV3 <- rpart(trainV$V~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,trainV,method="class",minsplit =100, maxdepth = 20)
rpart.plot(treeV1,extra=106) #https://www.guru99.com/r-decision-trees.html
printcp(treeV3) # display the results
#Predict Class V
predictV3<- predict(treeV1,testV, type = "class")
tableV3 <- table(testV$V,predictV3)
#accuracy for test
accuracy_TestV <- sum(diag(tableV3)) / sum(tableV3)
accuracy_TestV







############################################################################################################################



# PART B



############################################################################################################################

Harassment05 <- read.csv(file="Harassment0-5.csv", header=TRUE, sep=",")
Harassment1217 <- read.csv(file="Harassment12-17.csv", header=TRUE, sep=",")
Harassment611 <- read.csv(file="Harassment6-11.csv", header=TRUE, sep=",")
PatitLarcency611 <- read.csv(file="PetitLarcency6-11.csv", header=TRUE, sep=",")


#Question 10
n <- 1000
k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))

smoothScatter(Harassment05$Longitude,Harassment05$Latitude, nrpoints=.3*n, colramp=colorRampPalette(my.cols), 
              pch=19, cex=.8, xlab = "Longitude", ylab = "Latitude", bandwidth = 0.005)
smoothScatter(Harassment611$Longitude,Harassment611$Latitude, nrpoints=.3*n, colramp=colorRampPalette(my.cols), 
              pch=19, cex=.8, xlab = "Longitude", ylab = "Latitude")
smoothScatter(Harassment1217$Longitude,Harassment1217$Latitude, nrpoints=.3*n, colramp=colorRampPalette(my.cols), 
              pch=19, cex=.8, xlab = "Longitude", ylab = "Latitude")
smoothScatter(PatitLarcency611$Longitude,PatitLarcency611$Latitude, nrpoints=.3*n, colramp=colorRampPalette(my.cols), 
              pch=19, cex=.8, xlab = "Longitude", ylab = "Latitude")

# Question 11

a <- with(Harassment05,MASS:::kde2d(Harassment05$Longitude,Harassment05$Latitude))
filled.contour(a)

b <- with(Harassment1217,MASS:::kde2d(Harassment1217$Longitude,Harassment1217$Latitude))
filled.contour(b)

c <- with(Harassment611,MASS:::kde2d(Harassment611$Longitude,Harassment611$Latitude))
filled.contour(c)

d <- with(PatitLarcency611,MASS:::kde2d(PatitLarcency611$Longitude,PatitLarcency611$Latitude))
filled.contour(d)

#Question 12


plot(Harassment611$Longitude, Harassment611$Latitude, col = "red" , xlab = "Longitude", ylab = "Latitude", main = "Harassments and PetitLarcency 6-11")
points(PatitLarcency611$Longitude, PatitLarcency611$Latitude, col = "green")
legend(x = "topright", legend = c("Harrassment", "PatitLarcency"), col=c("red", "green"), pch=19)

#Question 13

#1
par(mfrow=c(3,2))
hist(Harassment05$Longitude, xlab = "Longitude", main = "Harrassment 0-5")
hist(Harassment611$Longitude, xlab = "Longitude", main = "Harrassment 6-11")
hist(Harassment05$Latitude, xlab = "Latitude", main = "")
hist(Harassment611$Latitude, xlab = "Latitude", main = "")
smoothScatter(Harassment05$Longitude,Harassment05$Latitude, nrpoints=.3*n, colramp=colorRampPalette(my.cols), 
              pch=19, cex=.8, xlab = "Longitude", ylab = "Latitude")
smoothScatter(Harassment611$Longitude,Harassment611$Latitude, nrpoints=.3*n, colramp=colorRampPalette(my.cols), 
              pch=19, cex=.8, xlab = "Longitude", ylab = "Latitude")

#2

par(mfrow=c(3,2))
hist(Harassment611$Longitude, xlab = "Longitude", main = "Harrassment 6-11")
hist(Harassment1217$Longitude, xlab = "Longitude", main = "Harrassment 12-17")
hist(Harassment611$Latitude, xlab = "Latitude", main = "")
hist(Harassment1217$Latitude, xlab = "Latitude", main = "")
smoothScatter(Harassment611$Longitude,Harassment611$Latitude, nrpoints=.3*n, colramp=colorRampPalette(my.cols), 
              pch=19, cex=.8, xlab = "Longitude", ylab = "Latitude")
smoothScatter(Harassment1217$Longitude,Harassment1217$Latitude, nrpoints=.3*n, colramp=colorRampPalette(my.cols), 
              pch=19, cex=.8, xlab = "Longitude", ylab = "Latitude")











     
# =========================================================
# CZ 4073 - Data Science For Business
# Assignment 2
# Name: KAMATH SHANTANU ARUN
# Matric card: U1422577F
# =========================================================

# ---------------------------------------------------------
# Clean environment and remove all working data
rm(list = ls())

# ---------------------------------------------------------
# Importing the dataset assign2_titanicData.csv as titanicData
respData <- read.csv("responses.csv")


head(respData)
dim(respData)
str(respData)
summary(respData)

respDTM <- as.DocumentTermMatrix(respData)

M <- sapply(respData, function(x) sum(is.na(x))); 
M[M>0]

pMiss <- function(x){sum(is.na(x))/length(x)*100}
M <- apply(respData,2,pMiss)
M[M>1]
M <- apply(respData,1,pMiss)
M[M>5]

# install.packages("mice")
library(mice)
md.pattern(respData)

# install.packages("VIM")
library(VIM)
aggr_plot <- aggr(respData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7, gap=3,
                  ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(respData,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
tempData1 <- complete(tempData)
# write.csv(tempData1, file = "My50ImputedData.csv")
myPal <- c("red","blue","darkgreen","magenta","darkgrey","black")

hiercFit <- hclust(dist(tempData1, method = "euclidean"),    # choice of distance
                   method="ward.D")                         # choice of linkage
hiercFit
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)

kMin <- 1
kMax <- 20
withinSS <- double(kMax - kMin + 1)
betweenSS <- double(kMax - kMin + 1)

for (K in kMin:kMax) {
  kMeansFit <- kmeans(tempData1, centers = K, nstart = 20)
  withinSS[K] <- sum(kMeansFit$withinss)
  betweenSS[K] <- kMeansFit$betweenss
}
str(tempData1, list.len=ncol(tempData1))
sapply(tempData1, class)

plot(kMin:kMax, betweenSS, pch=19, type="b", col="red",
     xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")
points(kMin:kMax, withinSS, pch=19, type="b", col="green")


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


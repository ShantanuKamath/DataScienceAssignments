# =========================================================
# CZ 4073 - Data Science For Business
# Assignment 2
# Name: KAMATH SHANTANU ARUN
# Matric card: U1422577F
# =========================================================

# Problem 1:
# In this problem, our dataset is -- assign2_ChurnData.csv
# The target is to fit an optimal tree model with cross-
# validation and a random forest model to predict the 
# output "Churn".

# ---------------------------------------------------------
# Clean environment and remove all working data
rm(list = ls())

# ---------------------------------------------------------
# Importing the dataset assign2_ChurnData.csv as churnData
churnData <- read.csv("assign2_ChurnData.csv", header = TRUE)

# ---------------------------------------------------------
# Performing some basic exploratory data analysis
dim(churnData)      # dimension of the dataset
names(churnData)    # labels of the columns/variables
str(churnData)      # structure of the dataset
head(churnData)     # first few (6) rows of the data
tail(churnData)     # last few (6) rows of the data
summary(churnData)  # summary statistics for all variables

# Basic visualization of the label
cols <- c("green","red")
plot(churnData$Churn, col = cols[as.numeric(churnData$Churn)])

# Basic visualization for continuous variables
plot(churnData$Tenure, churnData$Churn, xlab="Tenure",
     ylab="Churn", pch = 19, col = cols[as.numeric(churnData$Churn)])

plot(churnData$MonthlyCharges, churnData$Churn, xlab="MonthlyCharges",
     ylab="Churn", pch = 19, col = cols[as.numeric(churnData$Churn)])

plot(churnData$TotalCharges, churnData$Churn, xlab="TotalCharges",
     ylab="Churn", pch = 19, col = cols[as.numeric(churnData$Churn)])

# Basic visualization for categorical variables
plot(churnData$Gender, churnData$Churn,
     xlab="Gender", ylab="Churn")
plot(churnData$SeniorCitizen, churnData$Churn,
     xlab="SeniorCitizen", ylab="Churn")
plot(churnData$Partner, churnData$Churn,
     xlab="Partner", ylab="Churn")
plot(churnData$Dependents, churnData$Churn,
     xlab="Dependents", ylab="Churn")
plot(churnData$PhoneService, churnData$Churn,
     xlab="PhoneService", ylab="Churn")
plot(churnData$MultipleLines, churnData$Churn,
     xlab="MultipleLines", ylab="Churn")
plot(churnData$InternetService, churnData$Churn,
     xlab="InternetService", ylab="Churn")
plot(churnData$OnlineSecurity, churnData$Churn,
     xlab="OnlineSecurity", ylab="Churn")
plot(churnData$OnlineBackup, churnData$Churn,
     xlab="OnlineBackup", ylab="Churn")
plot(churnData$DeviceProtection, churnData$Churn,
     xlab="DeviceProtection", ylab="Churn")
plot(churnData$TechSupport, churnData$Churn,
     xlab="TechSupport", ylab="Churn")
plot(churnData$StreamingTV, churnData$Churn,
     xlab="StreamingTV", ylab="Churn")
plot(churnData$StreamingMovies, churnData$Churn,
     xlab="StreamingMovies", ylab="Churn")
plot(churnData$Contract, churnData$Churn,
     xlab="Contract", ylab="Churn")
plot(churnData$PaperlessBilling, churnData$Churn,
     xlab="PaperlessBilling", ylab="Churn")
plot(churnData$PaymentMethod, churnData$Churn,
     xlab="PaymentMethod", ylab="Churn")

# Basic 2D plots for continuous variables
plot(churnData$Tenure, churnData$MonthlyCharges, 
     xlab="Tenure", ylab="MonthlyCharges", 
     pch = 19, col = cols[as.numeric(churnData$Churn)])

plot(churnData$Tenure, churnData$TotalCharges, 
     xlab="Tenure", ylab="TotalCharges", 
     pch = 19, col = cols[as.numeric(churnData$Churn)])

plot(churnData$MonthlyCharges, churnData$TotalCharges, 
     xlab="MonthlyCharges", ylab="TotalCharges", 
     pch = 19, col = cols[as.numeric(churnData$Churn)])

# =======================================================
# Building the Default Tree
# =======================================================

# In the summary details, It was found that the variable
# TotalCharges has some missing values marked as NA
# Removing 9 such rows from the dataset
churnData = churnData[complete.cases(churnData), ]
dim(churnData)      # dimension of the dataset

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(churnData), 0.7*nrow(churnData), replace = FALSE)
churnTrain <- churnData[train,]
churnValid <- churnData[-train,]

# Dimension of the new split datasets
dim(churnTrain)
dim(churnValid)

# install.packages("tree")
library("tree")

# Creating the default tree model 
treeFit <- tree(Churn ~ ., data = churnTrain)

# Visualize the tree
plot(treeFit)
text(treeFit, pretty = FALSE, all = TRUE)

# Print the tree
treeFit

# Print tree summary
summary(treeFit)

# Predict using the tree model
predTrainData <- predict(treeFit, churnTrain, type = "class")
predValidData <- predict(treeFit, churnValid, type = "class")

# Classification accuracy on Train data
mean(predTrainData == churnTrain$Churn)

# Classification accuracy on Validation data
mean(predValidData == churnValid$Churn)

# Predict using the tree model
predData <- predict(treeFit, churnData, type = "class")

# Confusion matrix
table(churnData$Churn, predData)

# Classification accuracy
mean(predData == churnData$Churn)

# Threshold ?

# =======================================================
# Optimal Cross-Validated Decision Tree
# =======================================================

# Building a "pruned" Classification Tree over train set
ltreeFit <- tree(Churn ~ ., data = churnTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(churnTrain),
                                        mincut = 1,
                                        minsize = 2,
                                        mindev = 0))
plot(ltreeFit)
# This tree has clearly overfit the training data

cvTree <- cv.tree(ltreeFit, FUN = prune.misclass, K = 10) # K-fold Cross-Validation
cbind(cvTree$size, cvTree$dev)                            # check cvTree output
plot(cvTree$size, cvTree$dev, type="b")                   # plot deviance vs size

bestSize <- 4  # choose this parameter carefully, based on the cvTree output
ptreeFit <- prune.misclass(ltreeFit, best = bestSize)     # prune tree to best size
plot(ptreeFit)
text(ptreeFit, pretty = FALSE)

predTrain <- predict(ptreeFit, churnTrain, type = "class")  # prediction on train set
mean(predTrain == churnTrain$Churn)                     # classification accuracy
predValid <- predict(ptreeFit, churnValid, type = "class")  # prediction on validation set
mean(predValid == churnValid$Churn)                     # classification accuracy

# Build a Random Forest Model on train set

# Each node splits with subset of features
# That is, each node checks some variables
# before making the "decision" for split.

rfFit <- randomForest(Churn ~ .,                     # formula
                      data = churnTrain,                   # data set
                      ntree = 100,                       # number of trees
                      mtry = 3,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rfFit

predTrain <- predict(rfFit, churnTrain, type = "class")    # prediction on train set
mean(predTrain == churnTrain$Churn)                    # classification accuracy
predValid <- predict(rfFit, churnValid, type = "class")    # prediction on validation set
mean(predValid == churnValid$Churn)                    # classification accuracy
# Slight drop in accuracy? Increase number of Trees.

# Random Forest also gives variable importance for free, depending on splits
importance(rfFit)         # importance of the variables in the model (values)
varImpPlot(rfFit)         # importance of the variables in the model (visual)

# Cross validated 
# Random forest


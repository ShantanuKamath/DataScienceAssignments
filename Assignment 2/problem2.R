# =========================================================
# CZ 4073 - Data Science For Business
# Assignment 2
# Name: KAMATH SHANTANU ARUN
# Matric card: U1422577F
# =========================================================

# Problem 2:
# In this problem, our dataset is -- train.csv & test.csv
# The target is to fit an optimal tree model with cross-
# validation or random forest model to predict the 
# output "Survived".

# ---------------------------------------------------------
# Clean environment and remove all working data
rm(list = ls())

# ---------------------------------------------------------
# Importing the dataset assign2_titanicData.csv as titanicData
titanicData <- read.csv("train.csv", header = TRUE, na.strings = "")

# ---------------------------------------------------------
# Performing some basic exploratory data analysis
dim(titanicData)      # dimension of the dataset
names(titanicData)    # labels of the columns/variables
str(titanicData)      # structure of the dataset
head(titanicData)    # first few (6) rows of the data
tail(titanicData)     # last few (6) rows of the data
summary(titanicData)  # summary statistics for all variables

#Fix the data type of the variable 
titanicData$Survived <- as.factor(titanicData$Survived)
titanicData$Pclass <- ordered(titanicData$Pclass, levels = c("3","2","1"))
titanicData$Name <- as.character(titanicData$Name)
titanicData$Ticket <- as.character(titanicData$Ticket)
titanicData$Cabin <- as.character(titanicData$Cabin)


########
titanicData$Fare[is.na(titanicData$Fare)] <- median(titanicData$Fare, na.rm = TRUE)
titanicData$Embarked[is.na(titanicData$Embarked)] <- "S"
titanicData$Age[is.na(titanicData$Age)] <- median(titanicData$Age, na.rm = TRUE)

str(titanicData)      # structure of the dataset

########
# Verifying correct data types
str(titanicData)

# Basic visualization of the label
cols <- c("red", "green")
plot(titanicData$Survived, col = cols[as.numeric(titanicData$Survived)])

# Basic visualization for continuous variables
plot(titanicData$Age, titanicData$Survived, xlab="Age",
     ylab="Survived", pch = 19, col = cols[as.numeric(titanicData$Survived)])

plot(titanicData$SibSp, titanicData$Survived, xlab="SibSp",
     ylab="Survived", pch = 19, col = cols[as.numeric(titanicData$Survived)])

plot(titanicData$Parch, titanicData$Survived, xlab="Parch",
     ylab="Survived", pch = 19, col = cols[as.numeric(titanicData$Survived)])

plot(titanicData$Fare, titanicData$Survived, xlab="Fare",
     ylab="Survived", pch = 19, col = cols[as.numeric(titanicData$Survived)])

# Basic visualization for categorical variables
plot(titanicData$Pclass, titanicData$Survived,
     xlab="Pclass", ylab="Survived")
plot(titanicData$Sex, titanicData$Survived,
     xlab="Sex", ylab="Survived")
plot(titanicData$Embarked, titanicData$Survived,
     xlab="Embarked", ylab="Survived")

# Basic 2D plots for continuous variables
plot(titanicData$Age, titanicData$SibSp, 
     xlab="Age", ylab="SibSp", 
     pch = 19, col = cols[as.numeric(titanicData$Survived)])

plot(titanicData$Age, titanicData$Parch, 
     xlab="Age", ylab="Parch", 
     pch = 19, col = cols[as.numeric(titanicData$Survived)])

plot(titanicData$Age, titanicData$Fare, 
     xlab="Age", ylab="Fare", 
     pch = 19, col = cols[as.numeric(titanicData$Survived)])

plot(titanicData$SibSp, titanicData$Parch, 
     xlab="SibSp", ylab="Parch", 
     pch = 19, col = cols[as.numeric(titanicData$Survived)])

plot(titanicData$SibSp, titanicData$Fare, 
     xlab="SibSp", ylab="Fare", 
     pch = 19, col = cols[as.numeric(titanicData$Survived)])

plot(titanicData$Parch, titanicData$Fare, 
     xlab="Parch", ylab="Fare", 
     pch = 19, col = cols[as.numeric(titanicData$Survived)])

# =======================================================
# Building the Default Tree
# =======================================================

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(titanicData), 0.7*nrow(titanicData), replace = FALSE)
titanicTrain <- titanicData[train,]
titanicValid <- titanicData[-train,]

# Dimension of the new split datasets
dim(titanicTrain)
dim(titanicValid)

# install.packages("tree")
library("tree")

# Creating the default tree model 
treeFit <- tree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = titanicTrain)

# Visualize the tree
plot(treeFit)
text(treeFit, pretty = FALSE, all = TRUE)

# Print the tree
treeFit

# Print tree summary
summary(treeFit)

# Predict using the tree model
predTrainData <- predict(treeFit, titanicTrain, type = "class")
predValidData <- predict(treeFit, titanicValid, type = "class")

# Classification accuracy on Train data
mean(predTrainData == titanicTrain$Survived)

# Classification accuracy on Validation data
mean(predValidData == titanicValid$Survived)

# Predict using the tree model
predData <- predict(treeFit, titanicData, type = "class")

# Confusion matrix
table(titanicData$Survived, predData)

# Classification accuracy
mean(predData == titanicData$Survived)

# Threshold ?

# =======================================================
# Optimal Cross-Validated Decision Tree
# =======================================================

# Building a "pruned" Classification Tree over train set
ltreeFit <- tree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = titanicTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(titanicTrain),
                                        mincut = 1,
                                        minsize = 2,
                                        mindev = 0))
plot(ltreeFit)
# This tree has clearly overfit the training data

cvTree <- cv.tree(ltreeFit, FUN = prune.misclass, K = 10) # K-fold Cross-Validation
cbind(cvTree$size, cvTree$dev)                            # check cvTree output
plot(cvTree$size, cvTree$dev, type="b")                   # plot deviance vs size

bestSize <- 14  # choose this parameter carefully, based on the cvTree output
ptreeFit <- prune.misclass(ltreeFit, best = bestSize)     # prune tree to best size
plot(ptreeFit)
text(ptreeFit, pretty = FALSE)

predTrain <- predict(ptreeFit, titanicTrain, type = "class")  # prediction on train set
mean(predTrain == titanicTrain$Survived)                     # classification accuracy
predValid <- predict(ptreeFit, titanicValid, type = "class")  # prediction on validation set
mean(predValid == titanicValid$Survived)                     # classification accuracy

# Build a Random Forest Model on train set

# Each node splits with subset of features
# That is, each node checks some variables
# before making the "decision" for split.
library("randomForest")

rfFit <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,                     # formula
                      data = titanicTrain,                   # data set
                      ntree = 500,                       # number of trees
                      mtry = 2,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rfFit

predTrain <- predict(rfFit, titanicTrain, type = "class")    # prediction on train set
mean(predTrain == titanicTrain$Survived)                    # classification accuracy
predValid <- predict(rfFit, titanicValid, type = "class")    # prediction on validation set
mean(predValid == titanicValid$Survived)                    # classification accuracy
# Slight drop in accuracy? Increase number of Trees.

# Random Forest also gives variable importance for free, depending on splits
importance(rfFit)         # importance of the variables in the model (values)
varImpPlot(rfFit)         # importance of the variables in the model (visual)

# Cross validated 
# Random forest


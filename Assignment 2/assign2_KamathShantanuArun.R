# =========================================================
# CZ 4073 - Data Science For Business
# Assignment 2
# Name: KAMATH SHANTANU ARUN
# Matric card: U1422577F
# =========================================================

# Problem 1:
# In this problem, our dataset is -- assign2_ChurnData.csv
# The target is to fit an optimal tree model and a random 
# forest to predict the chance of "Churn".

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

# In the summary details, It was found that the variable
# TotalCharges has some missing values marked as NA
# Removing those rows from the dataset
churnData = churnData[complete.cases(churnData), ]
dim(churnData)      # dimension of the dataset
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
# Tree Model : Building a Classification Tree
# =======================================================

####
train <- sample(nrow(churnData), 0.7*nrow(churnData), replace = FALSE)
churnTrain <- churnData[train,]
churnValid <- churnData[-train,]
summary(churnTrain)
defaultTree <- tree(Churn ~ ., data = churnTrain)
summary(defaultTree)

predData <- predict(defaultTree, churnTrain, type = "class")
mean(predData == churnTrain$Churn)

predData <- predict(defaultTree, churnValid, type = "class")
mean(predData == churnValid$Churn)

###
# install.packages("tree")
library("tree")
treeFit <- tree(Churn ~ ., data = churnData)

# Visualize the tree
plot(treeFit)
text(treeFit, pretty = FALSE, all = TRUE)

# Print the tree
treeFit

# Print tree summary
summary(treeFit)

# Predict using the tree model
predData <- predict(treeFit, churnData, type = "class")

# Confusion matrix
table(churnData$Churn, predData)

# Classification accuracy
mean(predData == churnData$Churn)



# =======================================================
# Tree Model : Performance measures for Prediction
# =======================================================

# Confusion matrix
cm <- table(churnData$Churn, predData)
TP <- cm[2,2]  # True Positive (Good predicted as Good)
TN <- cm[1,1]  # True Negative (Bad predicted as Bad)
FP <- cm[1,2]  # False Positive (Bad predicted as Good) -- Type I error
FN <- cm[2,1]  # False Negative (Good predicted as Bad) -- Type II error

# Classification Accuracy
(TN + TP) / (TN + TP + FN + FP)    # Correct Classification / Total
mean(predData == churnData$Churn)   # Classification accuracy (alt.)

# False Positive Rate (fpr) / Type I error / 1 - Specificity
FP / (TN + FP)      # False Positive / Total Negative

# True Positive Rate (tpr) / 1 - Type II error / Sensitivity
TP / (TP + FN)      # True Positive / Total Positive

# Predict probabilities using the tree model
probData <- predict(treeFit, churnData, type = "vector")

# Convert probabilities to prediction
threshold <- 0.4

predData <- rep("No", nrow(churnData))
predData[probData[,2] > threshold] = "Yes"
predData <- as.factor(predData)

table(churnData$Churn, predData)

cm <- table(churnData$Churn, predData)
cm[1,2] / (cm[1,1] + cm[1,2])      # False Positive Rate (fpr)
cm[2,2] / (cm[2,2] + cm[2,1])      # True Positive Rate (tpr)


# =======================================================
# Part 2 : Random Forests : Aggregating multiple Trees
# =======================================================

# install.packages("randomForest")
library(randomForest)

# Load the dataset and explore
churnData <- read.csv("assign2_ChurnData.csv", header = TRUE)
str(churnData)
summary(churnData)


# -------------------------------------------------------
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
completeData = churnData[complete.cases(churnData), ]

train <- sample(nrow(completeData), 0.7*nrow(completeData), replace = FALSE)
churnTrain <- completeData[train,]
churnValid <- completeData[-train,]
summary(churnTrain)
summary(churnValid)


# -------------------------------------------------------
# Start with an optimal but single Tree, just for fun
# Build a "pruned" Classification Tree over train set
ltreeFit <- tree(Churn ~ ., data = churnTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(churnTrain),
                                        mincut = 1,
                                        minsize = 2,
                                        mindev = 0))
plot(ltreeFit)

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


# -------------------------------------------------------
# Now let's aggregate multiple Trees
# Build a Bagging Model on train set

# Each node can split using all variables
# That is, each node checks all variables
# before making the "decision" for split.

bagFit <- randomForest(Churn ~ .,                    # formula
                       data = churnTrain,                  # data set
                       ntree = 1000,                      # number of trees
                       mtry = 20,                         # variables for split
                       importance = TRUE)                # importance recorded
bagFit

predTrain <- predict(bagFit, churnTrain, type = "class")   # prediction on train set
mean(predTrain == churnTrain$Churn)                    # classification accuracy
predValid <- predict(bagFit, churnValid, type = "class")   # prediction on validation set
mean(predValid == churnValid$Churn)                    # classification accuracy
# Did you see the improvement in accuracy?! :-)

# Bagging gives you variable importance for free, depending on the splits
importance(bagFit)        # importance of the variables in the model (values)
varImpPlot(bagFit)        # importance of the variables in the model (visual)


# -------------------------------------------------------
# Let's aggregate multiple Trees, randomly
# Build a Random Forest Model on train set

# Each node splits with subset of features
# That is, each node checks some variables
# before making the "decision" for split.

rfFit <- randomForest(Churn ~ .,                     # formula
                      data = churnTrain,                   # data set
                      ntree = 600,                       # number of trees
                      mtry = 4,                          # variables for split
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

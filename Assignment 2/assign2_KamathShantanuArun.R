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
plot(churnData$Churn, col = cols)

# Basic visualization for continuous variables
plot(churnData$Tenure, churnData$Churn, xlab="Tenure",
     ylab="Churn", pch = 19, col = cols[as.numeric(churnData$Churn)])

plot(churnData$MonthlyCharges, churnData$Churn, xlab="MonthlyCharges",
     ylab="Churn", pch = 19, col = cols[as.numeric(churnData$Churn)])

plot(churnData$TotalCharges, churnData$Churn, xlab="TotalCharges",
     ylab="Churn", pch = 19, col = cols[as.numeric(churnData$Churn)])
# All three variables dont seem to provide a clear path to 
# distuinguish the churn variable at the first sight

# Basic visualization for categorical variables
plot(churnData$Gender, churnData$Churn,
     xlab="Gender", ylab="Churn")
# Gender doesn't seem to make any difference in the churn variable
plot(churnData$SeniorCitizen, churnData$Churn,
     xlab="SeniorCitizen", ylab="Churn")
# Senior citizens seem more likely to churn 
plot(churnData$Partner, churnData$Churn,
     xlab="Partner", ylab="Churn")
# Instances without partners seem more likely to churn 
plot(churnData$Dependents, churnData$Churn,
     xlab="Dependents", ylab="Churn")
# Instances without dependants seem more likely to churn 
plot(churnData$PhoneService, churnData$Churn,
     xlab="PhoneService", ylab="Churn")
plot(churnData$MultipleLines, churnData$Churn,
     xlab="MultipleLines", ylab="Churn")
plot(churnData$InternetService, churnData$Churn,
     xlab="InternetService", ylab="Churn")
# Where the instances have FiberOptic they are more likely to churn 
plot(churnData$OnlineSecurity, churnData$Churn,
     xlab="OnlineSecurity", ylab="Churn")
plot(churnData$OnlineBackup, churnData$Churn,
     xlab="OnlineBackup", ylab="Churn")
plot(churnData$DeviceProtection, churnData$Churn,
     xlab="DeviceProtection", ylab="Churn")
plot(churnData$TechSupport, churnData$Churn,
     xlab="TechSupport", ylab="Churn")
# Instances not having OnlineSecurity, TechSupport, OnlineBackUp
# DeviceProtection seem more likely to churn 

plot(churnData$StreamingTV, churnData$Churn,
     xlab="StreamingTV", ylab="Churn")
plot(churnData$StreamingMovies, churnData$Churn,
     xlab="StreamingMovies", ylab="Churn")
# Where the instances without internet service
# are less likely to churn 

plot(churnData$Contract, churnData$Churn,
     xlab="Contract", ylab="Churn")
# Month-to-month contracts lead to increase in churn rates
plot(churnData$PaperlessBilling, churnData$Churn,
     xlab="PaperlessBilling", ylab="Churn")
# People opting for paperlessBilling are more likely to churn
plot(churnData$PaymentMethod, churnData$Churn,
     xlab="PaymentMethod", ylab="Churn")
# People opting for ElectronicCheck are more likely to churn

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

# Creating the default tree model with the training data
treeFit <- tree(Churn ~ ., data = churnTrain)

# Visualizing the default tree
plot(treeFit)
text(treeFit, pretty = FALSE, all = TRUE)

# Printing the default tree
treeFit

# Print tree summary
summary(treeFit)

# Predict using the tree model on training and validation data
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

# Threshold can be applied to take into consideration the company's
# aptitude. Optimist or Pessimistic in terms of predicting churn rate.
# Pessimist attitude will expect a higher churn rate.

# Building a "pruned" Classification Tree over train set
ltreeFit <- tree(Churn ~ ., data = churnTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(churnTrain),
                                        mincut = 1,
                                        minsize = 2,
                                        mindev = 0))
plot(ltreeFit)
predData <- predict(ltreeFit, churnTrain, type = "class")
mean(predData == churnTrain$Churn)
# ~1 shows that the tree has clear overfit

cvTree <- cv.tree(ltreeFit, FUN = prune.misclass, K = 20) # K-fold Cross-Validation
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

# Building a Random Forest Model on train set
# install.packages("randomForest")
library("randomForest")

rfFit <- randomForest(Churn ~ .,                     # formula
                      data = churnTrain,                   # data set
                      ntree = 1000,                       # number of trees
                      mtry = 4,                          # variables for split
                      importance = TRUE)                 # importance recorded                 

rfFit
# mtry can be 4 according to the square root of the total number of 
# variables, 20. 3 was providing a better accuracy on the validation set.
# Increasing the number of trees in the random forest allows to slightly increase accuracy

predTrain <- predict(rfFit, churnTrain, type = "class")    # prediction on train set
mean(predTrain == churnTrain$Churn)                    # classification accuracy
predValid <- predict(rfFit, churnValid, type = "class")    # prediction on validation set
mean(predValid == churnValid$Churn)                    # classification accuracy

importance(rfFit)         # importance of the variables in the model (values)
# Tenure, TotalCharges & MonthlyCharges proved top variables. Removing these variables 
# could result in a significant decrease in accuracy.
# We could try to remove Gender, Phone Service and Partner and test on the validation set to better tune the model.
# As they were the variables found to be least important.
# The importance can be visualised on the graph below
varImpPlot(rfFit)         # importance of the variables in the model (visual)

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
# Assuming a ordering in terms of class in terms of price and comfort
# Below three variables are textual data hence best to store data as characters
titanicData$Name <- as.character(titanicData$Name)
titanicData$Ticket <- as.character(titanicData$Ticket)
titanicData$Cabin <- as.character(titanicData$Cabin)

# Handling missing data of Fare by taking the median of the rest of the values
titanicData$Fare[is.na(titanicData$Fare)] <- median(titanicData$Fare, na.rm = TRUE)
# Setting the port of Embarkment of missing instances as the most common port "S"
# Doing it manually as lack of mode function
titanicData$Embarked[is.na(titanicData$Embarked)] <- "S"
# Handling missing data of Age by taking the median of the rest of the values
titanicData$Age[is.na(titanicData$Age)] <- median(titanicData$Age, na.rm = TRUE)

# Verifying missing data and correct data type of variables
str(titanicData)      # structure of the dataset

# Basic visualization of the label
cols <- c("red", "green")
plot(titanicData$Survived, col = cols)

# Basic visualization for continuous variables
plot(titanicData$Age, titanicData$Survived, xlab="Age",
     ylab="Survived", pch = 19, col = cols[as.numeric(titanicData$Survived)])
# The graph seems to suggest that younger people survived the sinking ship
plot(titanicData$SibSp, titanicData$Survived, xlab="SibSp",
     ylab="Survived", pch = 19, col = cols[as.numeric(titanicData$Survived)])
# Instances with sibling and spouse count higher than 4 seemed not to survive 
plot(titanicData$Parch, titanicData$Survived, xlab="Parch",
     ylab="Survived", pch = 19, col = cols[as.numeric(titanicData$Survived)])
# Instances with parents and children count 4 or 6 seemed not to survive 
plot(titanicData$Fare, titanicData$Survived, xlab="Fare",
     ylab="Survived", pch = 19, col = cols[as.numeric(titanicData$Survived)])
# Instances of the highest fare seem to have all survived

# Basic visualization for categorical variables
plot(titanicData$Pclass, titanicData$Survived,
     xlab="Pclass", ylab="Survived")
# People survived according to their class of ticket. 1st > 2nd > 3rd
# Suggesting some ordering
plot(titanicData$Sex, titanicData$Survived,
     xlab="Sex", ylab="Survived")
# Females were left of the ship before the men to suggest higher survival rate
plot(titanicData$Embarked, titanicData$Survived,
     xlab="Embarked", ylab="Survived")
# Those who embarked from port 'C' seem to have a higher survival rate

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

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(titanicData), 0.7*nrow(titanicData), replace = FALSE)
titanicTrain <- titanicData[train,]
titanicValid <- titanicData[-train,]

# Dimension of the new split datasets
dim(titanicTrain)
dim(titanicValid)

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

bestSize <- 9  # choose this parameter carefully, based on the cvTree output
ptreeFit <- prune.misclass(ltreeFit, best = bestSize)     # prune tree to best size
plot(ptreeFit)
text(ptreeFit, pretty = FALSE)

predTrain <- predict(ptreeFit, titanicTrain, type = "class")  # prediction on train set
mean(predTrain == titanicTrain$Survived)                     # classification accuracy
predValid <- predict(ptreeFit, titanicValid, type = "class")  # prediction on validation set
mean(predValid == titanicValid$Survived)                     # classification accuracy

# Building a Random Forest Model on train set
# install.packages("randomForest")
library("randomForest")

# Ignoring all variables containing textual character data in consideration for building randomForest
rfFit <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,                     # formula
                      data = titanicTrain,                   # data set
                      ntree = 500,                       # number of trees
                      mtry = 3,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rfFit

predTrain <- predict(rfFit, titanicTrain, type = "class")    # prediction on train set
mean(predTrain == titanicTrain$Survived)                    # classification accuracy
predValid <- predict(rfFit, titanicValid, type = "class")    # prediction on validation set
mean(predValid == titanicValid$Survived)                    # classification accuracy

importance(rfFit)         # importance of the variables in the model (values)
# Confirming the intuitions from the graph plots. Sex, PClass, Age and Fare were one of the most
# important variables in determining the variable Survived. Removing these variables 
# could result in a significant decrease in accuracy.
# We could try to remove Embarked and Parch and test on the validation set to better tune the model.
# As they were the variables found to be least important.
# The importance can be visualised on the graph below
varImpPlot(rfFit)         # importance of the variables in the model (visual)


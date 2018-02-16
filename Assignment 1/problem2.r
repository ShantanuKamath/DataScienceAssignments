# =======================================================
# CZ 4073 - Data Science For Business
# Assignment 1
# Name: KAMATH SHANTANU ARUN
# Matric Card: U1422577F
# =======================================================

# Problem 2:
# In this problem, our dataset is -- assign1_CarData.csv
rm(list = ls())

# ---------------------------------------------------------
# Importing the dataset Week04carData.csv
carData <- read.csv("assign1_CarData.csv", header = TRUE)


# ---------------------------------------------------------
# Performing some basic exploratory data analysis

# Dimension of the dataset
dim(carData)

# Labels of the variables in the dataset
names(carData)

# Structure of the dataset
str(carData)

# First few rows of the data
head(carData)

# Last few rows of the data
tail(carData)

# Summary statistics for all variables
summary(carData)

# ---------------------------------------------------------
# Viewing plots of individual variables and checking 
# correspondence with summary statistics
hist(carData$cylinders, col = "lightgreen")
hist(carData$displacement, col = "lightgreen")
hist(carData$horsepower, col = "lightgreen")
hist(carData$weight, col = "lightgreen")
hist(carData$acceleration, col = "lightgreen")
hist(carData$mpg, col = "lightgreen")

# Boxplots of all individual variables
# checking correspondence with summary statistics
boxplot(carData$cylinders , horizontal = TRUE, col = "steelblue")
boxplot(carData$displacement, horizontal = TRUE, col = "steelblue")
boxplot(carData$horsepower, horizontal = TRUE, col = "steelblue")
boxplot(carData$weight, horizontal = TRUE, col = "steelblue")
boxplot(carData$acceleration, horizontal = TRUE, col = "steelblue")
boxplot(carData$mpg, horizontal = TRUE, col = "steelblue")

# ---------------------------------------------------------
# Checking correlations between variables
cor(carData)
# install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(carData))

# ---------------------------------------------------------
# Plot 2d scatterplots of all pairs of variables
pairs(carData, pch = 19, col = "blue")

# ---------------------------------------------------------
# Perform Linear Regression (mpg vs rest of the variables)

# Linear model on mpg vs Rest of the variables
# Model 1. - Full Model
lmFit1 <- lm(mpg ~ ., data = carData)

# Display the summary of Model 1.
summary(lmFit1)

# ---------------------------------------------------------
# Building an Updated Model by removing the least significant 
# variable at a time

# Linear Model on mpg vs Rest minus acceleration
# Model 2.
lmFit2 <- update(lmFit1, ~ . - acceleration, data = carData)

# Display the summary of the updated linear model
summary(lmFit2)

# Linear Model on mpg vs Rest minus acceleration, cylinders
# Model 3.
lmFit3 <- update(lmFit2, ~ . - cylinders, data = carData)

# Display the summary of the updated linear model
summary(lmFit3)

# Linear Model on mpg vs Rest minus acceleration, cylinders
# displacement
# Model 4.
lmFit4 <- update(lmFit3, ~ . - displacement, data = carData)

# Display the summary of the updated linear model
summary(lmFit4)

# ---------------------------------------------------------
# Checking for Non-linear Relations with Variables

# plotting mpg vs all the remaining variables
plot(carData$horsepower, carData$mpg, pch = 19, col = "blue")
plot(carData$weight, carData$mpg, pch = 19, col = "blue")

# Fitting the linear model on mpg vs remaining variables but
# introducing non-linear term as per observations above
# Model 5.
lmFit5 <- update(lmFit4, ~ . + I(weight^2), data = carData)

# Display the summary of the updated linear model
summary(lmFit5)

# Model 6.
lmFit6 <- update(lmFit5, ~ . + I(horsepower^2), data = carData)

# Display the summary of the updated linear model
summary(lmFit6)

# Model 7.
lmFit7 <- update(lmFit6, ~ . + horsepower:weight, data = carData)

# Display the summary of the updated linear model
summary(lmFit7)

# Linear Model 7 minus FreeSulphurDioxide
# Model 8.
lmFit8 <- update(lmFit7, ~ . - I(weight^2), data = carData)

# Display the summary of the updated linear model
summary(lmFit8)

# Model 9.
lmFit9 <- update(lmFit8, ~ . - I(horsepower^2), data = carData)

# Display the summary of the updated linear model
summary(lmFit9)

# 13b : check the model for potential outliers
plot(lmFit9)

# 13c : remove outliers and high-leverage points
cd <- cooks.distance(lmFit9)
carData.clean <- carData[abs(cd) < 4/nrow(carData), ]
nrow(carData.clean)

# 13d : fit your best model to the clean dataset
formula(lmFit9)
lmFit <- lm(formula(lmFit9), data = carData.clean)

# 13e : did the performance of the model improve?
summary(lmFit)
plot(lmFit)

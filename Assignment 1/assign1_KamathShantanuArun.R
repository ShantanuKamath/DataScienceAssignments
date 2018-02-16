# =======================================================
# CZ 4073 - Data Science For Business
# Assignment 1
# Name: KAMATH SHANTANU ARUN
# Matric Card: U1422577F
# =======================================================

# Problem 1:
# In this problem, our dataset is -- assign1_WineData.csv


# ---------------------------------------------------------
# Importing the dataset Week04wineData.csv
wineData <- read.csv("assign1_WineData.csv", header = TRUE)


# ---------------------------------------------------------
# Performing some basic exploratory data analysis

# Dimension of the dataset
dim(wineData)

# Labels of the variables in the dataset
names(wineData)

# Structure of the dataset
str(wineData)

# First few rows of the data
head(wineData)

# Last few rows of the data
tail(wineData)

# Summary statistics for all variables
summary(wineData)

# ---------------------------------------------------------
# Viewing plots of individual variables and checking 
# correspondence with summary statistics
hist(wineData$FixedAcidity, col = "lightgreen")
hist(wineData$VolatileAcidity, col = "lightgreen")
hist(wineData$CitricAcid, col = "lightgreen")
hist(wineData$ResidualSugar, col = "lightgreen")
hist(wineData$Chlorides, col = "lightgreen")
hist(wineData$FreeSulphurDioxide, col = "lightgreen")
hist(wineData$TotalSulphurDioxide, col = "lightgreen")
hist(wineData$Density, col = "lightgreen")
hist(wineData$pH, col = "lightgreen")
hist(wineData$Sulphates, col = "lightgreen")
hist(wineData$Alcohol, col = "lightgreen")
hist(wineData$Quality, col = "lightgreen")

# Boxplots of all individual variables
# checking correspondence with summary statistics
boxplot(wineData$FixedAcidity , horizontal = TRUE, col = "steelblue")
boxplot(wineData$VolatileAcidity, horizontal = TRUE, col = "steelblue")
boxplot(wineData$CitricAcid, horizontal = TRUE, col = "steelblue")
boxplot(wineData$ResidualSugar, horizontal = TRUE, col = "steelblue")
boxplot(wineData$Chlorides, horizontal = TRUE, col = "steelblue")
boxplot(wineData$FreeSulphurDioxide, horizontal = TRUE, col = "steelblue")
boxplot(wineData$TotalSulphurDioxide, horizontal = TRUE, col = "steelblue")
boxplot(wineData$Density, horizontal = TRUE, col = "steelblue")
boxplot(wineData$pH, horizontal = TRUE, col = "steelblue")
boxplot(wineData$Sulphates, horizontal = TRUE, col = "steelblue")
boxplot(wineData$Alcohol, horizontal = TRUE, col = "steelblue")
boxplot(wineData$Quality, horizontal = TRUE, col = "steelblue")

# ---------------------------------------------------------
# Checking correlations between variables
cor(wineData)
# install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(wineData))

# ---------------------------------------------------------
# Plot 2d scatterplots of all pairs of variables
pairs(wineData, pch = 19, col = "blue")

# ---------------------------------------------------------
# Perform Linear Regression (Quality vs rest of the variables)

# Linear model on Quality vs Rest of the variables
# Model 1. - Full Model
lmFit1 <- lm(Quality ~ ., data = wineData)

# Display the summary of Model 1.
summary(lmFit1)

# ---------------------------------------------------------
# Building an Updated Model by removing the least significant 
# variable at a time

# Linear Model on Quality vs Rest minus FixedAcidity
# Model 2.
lmFit2 <- update(lmFit1, ~ . - FixedAcidity, data = wineData)

# Display the summary of the updated linear model
summary(lmFit2)

# Linear Model on Quality vs Rest minus FixedAcidity, CitricAcid
# Model 3.
lmFit3 <- update(lmFit2, ~ . - CitricAcid, data = wineData)

# Display the summary of the updated linear model
summary(lmFit3)

# Linear Model on Quality vs Rest minus FixedAcidity, CitricAcid,
# ResidualSugar
# Model 4.
lmFit4 <- update(lmFit3, ~ . - ResidualSugar, data = wineData)

# Display the summary of the updated linear model
summary(lmFit4)

# Linear Model on Quality vs Rest minus FixedAcidity, CitricAcid,
# ResidualSugar, Density
# Model 5.
lmFit5 <- update(lmFit4, ~ . - Density, data = wineData)

# Display the summary of the updated linear model
summary(lmFit5)

# ---------------------------------------------------------
# Checking for Non-linear Relations with Variables

# plotting Quality vs all the remaining variables
plot(wineData$VolatileAcidity, wineData$Quality, pch = 19, col = "blue")
plot(wineData$Chlorides, wineData$Quality, pch = 19, col = "blue")
plot(wineData$FreeSulphurDioxide, wineData$Quality, pch = 19, col = "blue")
plot(wineData$TotalSulphurDioxide, wineData$Quality, pch = 19, col = "blue")
plot(wineData$pH, wineData$Quality, pch = 19, col = "blue")
plot(wineData$Sulphates, wineData$Quality, pch = 19, col = "blue")
plot(wineData$Alcohol, wineData$Quality, pch = 19, col = "blue")


# Fitting the linear model on Quality vs remaining variables but
# introducing non-linear term as per observations above
# Model 6.
lmFit6 <- update(lmFit5, ~ . + I(Sulphates^2), data = wineData)

# Display the summary of the updated linear model
summary(lmFit6)

# Linear Model 6 minus FreeSulphurDioxide
# Model 7.
lmFit7 <- update(lmFit6, ~ . - FreeSulphurDioxide, data = wineData)

# Display the summary of the updated linear model
summary(lmFit7)

# Linear Model 6 minus FreeSulphurDioxide, TotalSulphurDioxide
# Model 8.
lmFit8 <- update(lmFit7, ~ . - TotalSulphurDioxide, data = wineData)

# Display the summary of the updated linear model
summary(lmFit8)

# 13b : check the model for potential outliers
plot(lmFit8)

# 13c : remove outliers and high-leverage points
cd <- cooks.distance(lmFit8)
wineData.clean <- wineData[abs(cd) < 4/nrow(wineData), ]
nrow(wineData.clean)

# 13d : fit your best model to the clean dataset
formula(lmFit8)
lmFit <- lm(formula(lmFit8), data = wineData.clean)

# 13e : did the performance of the model improve?
summary(lmFit)
plot(lmFit)

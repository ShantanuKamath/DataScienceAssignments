# =========================================================
# CZ 4073 - Data Science For Business
# Assignment 1
# Name: KAMATH SHANTANU ARUN
# Matric Card: U1422577F
# =========================================================

# Problem 1:
# In this problem, our dataset is -- assign1_WineData.csv
# The solution of the problem looks to fit an optimal linear
# regression model to predict the "Quality" of the wine

# ---------------------------------------------------------
# Importing the dataset Week04wineData.csv as wineData
wineData <- read.csv("assign1_WineData.csv", header = TRUE)

# ---------------------------------------------------------
# Performing some basic exploratory data analysis

# Dimensional information of the dataset
dim(wineData)
# Here we find that we have information of 1000 instances 
# of wine and there are 12 different variables that
# describe it

# Labels of the variables in the dataset
names(wineData)
# It provides us the names of each of the 12 variables

# Structure of the dataset
str(wineData)
# We find that the data is stored in a "data.frame"
# All of the variable have numeric data type

# First few rows of the data
head(wineData)
# Last few rows of the data
tail(wineData)
# heads and tails provide a general idea in terms of 
# the variables actual values and their magnitudes

# Summary statistics for all variables
summary(wineData)
# It returns a table with the results of the summary
# function applied to each column in the data frame

# For each column, it returns the minimum value, 1st quantile, 
# median, mean, 3rd quantile and the maximum value
# Providing us with the spread and distribution of the variables

# ---------------------------------------------------------
# Visulisation of the dataset

# Viewing histograms of individual variables to understand 
# its distribution shape, center, range and variation
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
# Most of the plots show a normal distribution while others
# show right skewed distribution

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
# Box plots were able to clearly point out potential outliers 
# and provide a better sense on where the median or mean lie
# hence improving our understanding of the frequency histograms
# and the entire dataset


# ---------------------------------------------------------
# Checking correlation between the variable of the dataset
cor(wineData)
# install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(wineData))
# corrplot library uses colours and shape to visual represent
# the strength of the correlations in a matrix format

# With the correlation matrix, we find that "Quality" doesnot 
# have a very strong correlation with any of the variables.
# The most correlation being with Alcohol (0.47) and 
# VolatileAcidity (-0.39)
# Low or zero correlated variable will be least significant 
# and would be eliminated early to create the best model

# The highest correlation(|0.66-0.67|) found was betwen 
# FixedAcidity & Density,
# FixedAcidity & pH,
# FixedAcidity & CitricAcid
# FreeSulphurDioxide & TotalSulphurDioxide,
# Hence these variables are likely to absorb each others
# contribution to Quality and may cause them to be futile
# in predicting Quality

# ---------------------------------------------------------
# Plotting two-dimensional scatterplots of all pairs of variables
pairs(wineData, pch = 19, col = "blue")
# Viewing the scatter plots we can understand why the correlation
# of the various variable pairs are either high or low
# It is difficult to judge the plots that involve Quality because
# its type is integer and more discrete than continuous.

# The scatter plot between sulphate and Quality seem to be less 
# spherical and hence point towards some linearity/nonlinearity
# which would be a good place to do some trial and error.

# ---------------------------------------------------------
# Perform Linear Regression (Quality vs rest of the variables)

# We will use adjusted R-squared as the metric to determine
# if the model is improving

# Linear model on Quality vs Rest of the variables
# Model 1. - Full Model
lmFit1 <- lm(Quality ~ ., data = wineData)

# Display the summary of Model 1.
summary(lmFit1)

# The "Full Model" summary statistics provide us with information
# on the formula, residuals, and coefficients.
# The coefficients provide a scale of significance on each of
# the variables that provide a contribution on predicting the Quality.

# ---------------------------------------------------------
# Building an Updated Model by removing the least significant 
# variable at a time

# To do so, we would want to eliminate the most insignificant
# variables having "." or lesser on the scale.
# With variables having the same significance, we will use 
# Probability (Pr) to decide

# The value in the Pr colum informs us of the probability of 
# coefficient of the variable to go to zero. The higher the 
# probability the lower its significance.

# Linear Model on Quality vs Rest minus FixedAcidity
# Model 2.
lmFit2 <- update(lmFit1, ~ . - FixedAcidity, data = wineData)

# Display the summary of the updated linear model
summary(lmFit2)

# In Model 2,  we removed the variable FixedAcidity (Pr = 0.76) 
# as it was the least significant
# This helped improve the R-squared to 0.3413 from 0.3407

# Linear Model on Quality vs Rest minus FixedAcidity, CitricAcid
# Model 3.
lmFit3 <- update(lmFit2, ~ . - CitricAcid, data = wineData)

# Display the summary of the updated linear model
summary(lmFit3)

# Similarly in Model 3, we removed the variable CitricAcid (Pr = 0.75) 
# This helped improve the R-squared to 0.3419 from 0.3413

# Linear Model on Quality vs Rest minus FixedAcidity, CitricAcid,
# ResidualSugar
# Model 4.
lmFit4 <- update(lmFit3, ~ . - ResidualSugar, data = wineData)

# Display the summary of the updated linear model
summary(lmFit4)

# Similarly in Model 4, we removed the variable ResidualSugar (Pr = 0.33) 
# This helped improve the R-squared to 0.342 from 0.3419

# Linear Model on Quality vs Rest minus FixedAcidity, CitricAcid,
# ResidualSugar, Density
# Model 5.
lmFit5 <- update(lmFit4, ~ . - Density, data = wineData)

# Display the summary of the updated linear model
summary(lmFit5)

# Lastly in Model 5, we removed the variable Density (Pr = 0.48) 
# This helped improve the R-squared to 0.3423 from 0.3420

# Now we have covered the linearity part of the model 
# and kept only the variables that have a significance of 
# Pr lower than 0.05 (*).

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

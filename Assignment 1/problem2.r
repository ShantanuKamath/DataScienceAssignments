# =======================================================
# CZ 4073 - Data Science For Business
# Assignment 1
# Name: KAMATH SHANTANU ARUN
# Matric Card: U1422577F
# =======================================================

# Problem 2:
# In this problem, our dataset is -- assign1_CarData.csv
# The solution of the problem looks to fit an optimal linear
# regression model to predict the mpg(Miles Per Gallon)
# of the car

# ---------------------------------------------------------
# Clean environment and remove all working data

rm(list = ls())

# ---------------------------------------------------------
# Importing the dataset Week04carData.csv

carData <- read.csv("assign1_CarData.csv", header = TRUE)

# ---------------------------------------------------------
# Performing some basic exploratory data analysis

# Dimension of the dataset
dim(carData)
# Here we find that we have information of 300 instances 
# of cars and there are 6 different variables that
# describe it

# Labels of the variables in the dataset
names(carData)
# It provides us the names of each of the 6 variables

# Structure of the dataset
str(carData)
# We find that the data is stored in a "data.frame"
# The variables vary between numeric and integer type

# First few rows of the data
head(carData)
# Last few rows of the data
tail(carData)
# heads and tails provide a general idea in terms of 
# the variables actual values and their magnitudes

# Summary statistics for all variables
summary(carData)
# It returns a table with the results of the summary
# function applied to each column in the data frame

# For each column, it returns the minimum value, 1st quantile, 
# median, mean, 3rd quantile and the maximum value
# Providing us with the spread, distribution and central 
# tendency of the variables

# ---------------------------------------------------------
# Visulisation of the dataset

# Viewing histograms of individual variables to understand 
# its distribution shape, center, range and variation
hist(carData$cylinders, col = "lightgreen")
hist(carData$displacement, col = "lightgreen")
hist(carData$horsepower, col = "lightgreen")
hist(carData$weight, col = "lightgreen")
hist(carData$acceleration, col = "lightgreen")
hist(carData$mpg, col = "lightgreen")
# Most of the plots show a right skewed distribution while others
# show normal distribution

# Boxplots of all individual variables
# checking correspondence with summary statistics
boxplot(carData$cylinders , horizontal = TRUE, col = "steelblue")
boxplot(carData$displacement, horizontal = TRUE, col = "steelblue")
boxplot(carData$horsepower, horizontal = TRUE, col = "steelblue")
boxplot(carData$weight, horizontal = TRUE, col = "steelblue")
boxplot(carData$acceleration, horizontal = TRUE, col = "steelblue")
boxplot(carData$mpg, horizontal = TRUE, col = "steelblue")
# Box plots were able to clearly point out potential outliers 
# and provide a better sense on where the median or mean lie
# hence improving our understanding of the frequency histograms
# and the entire dataset

# ---------------------------------------------------------
# Checking correlations between variables
cor(carData)
# install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(carData))
# corrplot library uses colours and shape to visual represent
# the strength of the correlations in a matrix format

# With the correlation matrix, we find that mpg has strong 
# correlation with all the variables.
# The strongest correlations being with weight (-0.84) and 
# displacement (-0.81)
# Low or zero correlated variable will be least significant 
# and would be eliminated early to create the best model

# The highest correlation(0.90-0.95) found was betwen 
# cylinder & displacement,
# displacement & weight,
# cylinder & weight,
# Hence these variables are likely to absorb each others
# contribution to mpg and may cause them to be futile
# in predicting mpg

# ---------------------------------------------------------
# Plot 2d scatterplots of all pairs of variables
pairs(carData, pch = 19, col = "blue")
# Viewing the scatter plots we can understand why the correlation
# of the various variable pairs are either high or low
# We can quickly understand how exactly the linearly related
# mpg is to the other variables

# The scatter plot between acceleration and mpg is more linear
# less spherical indicating a good correlation between them
# For displacement, horsepower and weight; we find that mpg
# has some what of a non-linear relation.
# These variables also have a non-linear relation amongst
# themselves, hence we can find the possibility of an 
# interactions between them too.

# ---------------------------------------------------------
# Perform Linear Regression (mpg vs rest of the variables)

# We will use adjusted R-squared as the metric to determine
# if the model has improved

# Linear model on mpg vs Rest of the variables
# Model 1. - Full Model
lmFit1 <- lm(mpg ~ ., data = carData)

# Display the summary of Model 1.
summary(lmFit1)

# The "Full Model" summary statistics provide us with information
# on the formula, residuals, and coefficients.
# The coefficients provide a scale of significance on each of
# the variables that provide a contribution on predicting the mpg.

# We see that only weight shows high significance while horsepower
# display low significance. This is probably due the fact that
# weight is proved to be independant of other variables and most 
# other variables have their contribution absorbed by either 
# horsepower or weight.

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

# Linear Model on mpg vs Rest minus acceleration
# Model 2.

lmFit2 <- update(lmFit1, ~ . - acceleration, data = carData)

# Display the summary of the updated linear model
summary(lmFit2)
# In Model 2,  we removed the variable acceleration (Pr = 0.82) 
# as it was the least significant and has the highest probability
# of the coefficient going to zero, amongst other variables.
# This helped improve the R-squared to 0.7118 from 0.7127 and also
# increased horsepowers significance as its contribution increased.

# Linear Model on mpg vs Rest minus acceleration, cylinders
# Model 3.
lmFit3 <- update(lmFit2, ~ . - cylinders, data = carData)

# Display the summary of the updated linear model
summary(lmFit3)

# Similarly in Model 3, we removed the variable cylinders 
# (Pr = 0.64) This helped improve the R-squared to
# 0.7127 from 0.7134. cylinder proved not have any correlation
# with weight at all, also didnt effect significance of any other
# variable. Positively helping the degree of freedom and F-statistic

# Linear Model on mpg vs Rest minus acceleration, cylinders
# displacement
# Model 4.
lmFit4 <- update(lmFit3, ~ . - displacement, data = carData)

# Display the summary of the updated linear model
summary(lmFit4)

# Similarly in Model 4, we removed the variable displacement
# (Pr = 0.21). Here we see a negligible loss in R-squared.
# But helped increase horsepower's significance to most 
# significant. Also increased degree of freedom and F-statistic

# Now we have covered the linearity part of the model 
# and kept only the variables that have a significance of 
# Pr lower than 0.05 (*). Although, all our variables are
# already highly significant

# ---------------------------------------------------------
# Checking for Non-linear Relations with Variables

# Plotting mpg vs all the remaining variables 
# individually to try and better understand and detect 
# non-linearity
plot(carData$horsepower, carData$mpg, pch = 19, col = "blue")
plot(carData$weight, carData$mpg, pch = 19, col = "blue")
# Both variables show a prominent non-linearity with the
# mpg variable and can be used as independant variables.


# Fitting the linear model on mpg vs remaining variables but
# introducing non-linear term weight^2
# Model 5.
lmFit5 <- update(lmFit4, ~ . + I(weight^2), data = carData)

# Display the summary of the updated linear model
summary(lmFit5)

# With addition of an independant variable (weight)^2 in
# Model 5, we can see that the R-squared has improved from
# 0.7129 to 0.746. The new variable has high significance and 
# confirmed its non-linear relation as seen in the plots before.
# No variable lost its significant showing that this provided 
# a new contribution.

# Updating the model 5 by introducing non-linear term horsepower^
# Model 6.
lmFit6 <- update(lmFit5, ~ . + I(horsepower^2), data = carData)

# Display the summary of the updated linear model
summary(lmFit6)

# With addition of an independant variable (horsepower)^2 in
# Model 6, we can see that the R-squared has improved from
# 0.746 to 0.7546. The new variable has high significance and 
# confirmed its non-linear relation as seen in the plots before.
# No variable lost its significant showing that this provided 
# a new contribution.

# Updating the model 6 by introducing interaction term 
# horsepower:weight
# Model 7.
lmFit7 <- update(lmFit6, ~ . + horsepower:weight, data = carData)

# Display the summary of the updated linear model
summary(lmFit7)

# Seeing that both these variables have a non-linear relation
# with mpg. It make sense to check if the two variables themselves
# can provide a useful contribution through interaction

# With addition of the interaction horsepower:weight in
# Model 7, we can see that the R-squared has improved from
# 0.7546 yo 0.7553. Both independant variables lost their 
# significance along with the low significance of this interaction.
# Hence now we seem to realise that the interaction actually 
# absorbed the contribution of both variables and the probability 
# of the coefficient going to zero increased significantly.

# Linear Model 7 minus weight^2
# Model 8.
lmFit8 <- update(lmFit7, ~ . - I(weight^2), data = carData)

# Display the summary of the updated linear model
summary(lmFit8)

# As mention above, weight^2 (Pr = 0.72) lost its importance
# as the interaction absorbed its contribution.
# As it was the least significant and has the highest probability
# of the coefficient going to zero, amongst other variables.
# This helped improve the R-squared to 0.7561 from 0.7553
# As seen, the significance of the interaction variable increased 
# to the highest significance category

# Linear Model 8 minus horsepower^2
# Model 9.
lmFit9 <- update(lmFit8, ~ . - I(horsepower^2), data = carData)

# Display the summary of the updated linear model
summary(lmFit9)

# As mention above, horsepower^2 (Pr = 0.39) lost its importance
# as the interaction absorbed its contribution.
# As it was the least significant and has the highest probability
# of the coefficient going to zero, amongst other variables.
# This helped improve the R-squared to 0.7563 from 0.7561

# ---------------------------------------------------------
# Finding and removing outliers

# Plotting to check the model for potential outliers
plot(lmFit9)

# Calculating cooks distance to find and remove outliers
# as well as high-leverage points
cd <- cooks.distance(lmFit9)
carData.clean <- carData[abs(cd) < 4/nrow(carData), ]
nrow(carData.clean)

# Viewing and confirming the parameters in the final model
formula(lmFit9)
# Fitting the "Best Model" with the cleaned data
lmFit <- lm(formula(lmFit9), data = carData.clean)

# Evaluating to find increase in performance
summary(lmFit)
plot(lmFit)


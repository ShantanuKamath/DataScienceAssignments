# =========================================================
# CZ 4073 - Data Science For Business
# Assignment 2
# Name: KAMATH SHANTANU ARUN
# Matric Card: U1422577F
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

# Dimensional information of the dataset
dim(churnData)
# Here we find that we have information of 5000 instances 
# and there are 20 different variables that describe it

# Labels of the variables in the dataset
names(churnData)
# It provides us the names of each of the 12 variables

# Structure of the dataset
str(churnData)
# We find that the data is stored in a "data.frame"
# Quality has integer data type while,
# Rest of the variables have numeric data type

# First few rows of the data
head(churnData)
# Last few rows of the data
tail(churnData)
# heads and tails provide a general idea in terms of 
# the variables actual values and their magnitudes

# Summary statistics for all variables
summary(churnData)
# It returns a table with the results of the summary
# function applied to each column in the data frame

# For each column, it returns the minimum value, 1st quantile, 
# median, mean, 3rd quantile and the maximum value
# Providing us with the spread, distribution and central 
# tendency of the variables

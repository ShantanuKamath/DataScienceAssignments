# =========================================================
# CZ 4073 - Data Science For Business
# Assignment 3
# Name: KAMATH SHANTANU ARUN
# Matric card: U1422577F
# =========================================================

# ---------------------------------------------------------
# Clean environment and remove all working data
rm(list = ls())

# Problem 1:
# In this problem, our dataset is -- assign3_CusineData.json
# The target is to perform clustering with appropriate choice
# of algorithm and distance notion to identify the optimal 
# number of clusters in the dataset.
# =======================================================
# Starter Code : JSON to Document-Term Matrix
# =======================================================

# Install essential packages
# install.packages("jsonlite")    # read JSON file in R
# install.packages("tm")          # text mining in R

# Load essential packages
library(jsonlite)
library(tm)


# -------------------------------------------------------
# Cuisine Data : Loading and Pre-Processing

# Load the dataset from JSON format
cuisData <- read_json("assign3_CuisineData.json", simplifyVector = TRUE)

# Inspect the first few elements
head(cuisData)

# Remove the c() wrapper from the list of ingredients
cuisData$ingredients <- substring(cuisData$ingredients, 3, nchar(cuisData$ingredients)-1)

# Remove anything present in brackets (mostly explanations and amounts) 
cuisData$ingredients <- gsub("\\s*\\([^\\)]+\\)","",cuisData$ingredients)

# Remove the space following the comma between ingredients
cuisData$ingredients <- gsub(', ',',',cuisData$ingredients)

# Remove the space between two words of the same ingredient
cuisData$ingredients <- gsub(' ','',cuisData$ingredients)

# Remove any hyphen present within the same ingredient
cuisData$ingredients <- gsub('-','',cuisData$ingredients)

# Inspect the first few elements
head(cuisData)


# -------------------------------------------------------
# Cuisine Data : Converting to Document-Term Matrix
# Ref : https://www.youtube.com/watch?v=dE10fBCDWQc

# Create the corpus of terms (dictionary) for ingredients
cuisCorpus <- Corpus(VectorSource(cuisData$ingredients))

# Inspect the first few elements
inspect(cuisCorpus[1:5])

# Translate all letters to lower case
cuisCorpus <- tm_map(cuisCorpus, tolower)

# Further cleaning and preprocessing guide:
# https://www.youtube.com/watch?v=3putwMZpt1E

# Create the DTM from the Corpus
cuisDTM <- DocumentTermMatrix(cuisCorpus)

# Removing sparse terms from the DTM
# Ref: https://stackoverflow.com/questions/28763389/how-does-the-removesparseterms-in-r-work
cuisDTM <- removeSparseTerms(cuisDTM, .95)

# Inspect the first few elements
inspect(cuisDTM[1:5,])

# Create DataFrame from the DTM
cuisDF <- as.data.frame(as.matrix(cuisDTM))

# Remove data entries where the rows correspond to only zeros
rowTotals <- apply(cuisDF , 1, sum) #Find the sum of words in each Document
cuisDF   <- cuisDF[rowTotals> 0, ]           #remove all docs without words

# Inspect the DataFrame
names(cuisDF)
str(cuisDF)

# Check dimensions of the DataFrame
dim(cuisDF)



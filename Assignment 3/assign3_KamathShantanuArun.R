# =========================================================
# CZ 4073 - Data Science For Business
# Assignment 3
# Name: KAMATH SHANTANU ARUN
# Matric card: U1422577F
# =========================================================
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

# Inspect the first few elements
inspect(cuisDTM[1:5,])

# Check dimensions of the DTM
dim(cuisDTM)

# We evaluate the relative document frequency
# and look to remove colums where the frequency
# of the occurence is less than 5%.
# Rather the sparsity of the column is > 95%
# Removing sparse terms from the DTM
# Ref: https://stackoverflow.com/questions/28763389/how-does-the-removesparseterms-in-r-work
cuisDTM <- removeSparseTerms(cuisDTM, .95)

# Inspect the first few elements
inspect(cuisDTM[1:5,])

# Check dimensions of the DTM
dim(cuisDTM)

# We can see that removing columns that are
# More than 95% sparse resulted in having only
# columns remaining in the dataset.
# Create DataFrame from the DTM
cuisDF <- as.data.frame(as.matrix(cuisDTM))

# Having removed 6708 colums due to sparsity, 
# We now have to check for rows which could have 
# become 100% sparse (All Zero) due to the 
# elimination of the columns
rowTotals <- apply(cuisDF , 1, sum) #Find the sum of words in each Document
cuisDF   <- cuisDF[rowTotals> 0, ]           #remove all docs without words

# In eliminating empty rows, we were able
# to successfully eliminate 2901 rows. 
# Hence, eliminating columns having more
# than 95% sparsity only removed 7.2% of 
# the records in the dataset.
# Allowing us to still have good information 
# about the data.
# Check dimensions of the DataFrame
dim(cuisDF)

# Inspect the DataFrame
names(cuisDF)
str(cuisDF)

# -------------------------------------------------------
# Hierarchical Clustering : Using this clustering method 
# to build the dendogram and analyse how many clusters or 
# K is required. Using method ward to check for compact 
# sperical clusters.
hiercFit <- hclust(dist(cuisDF, method = "euclidean"),
                   method="ward.D")
hiercFit
# Visualize Dendogram : Through the dendogram it is clear 
# that there is a good possibility of having atleast two 
# clusters. There are also cases between two and seven 
# clusters that could prove optimal.
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)

# Assuming that there two clusters are optimal, 
# Visualise the seperation
K <- 2 # 3, 5, 7
rect.hclust(hiercFit, k = K)

# -------------------------------------------------------
# Perform K-Means Clustering with K determined using above 
# Dendogram. K means tends to form similar shaped clusters.
# Hence even if there were actually 2 clusters but the shapes, 
# differ in size, it is possible that K-means was not able
# to optimally cluster the dataset. They also could be faulting
# because they look to perform Hard Spherical Clustering, while,
# the data itself may not be organised like that.
K <- 2
kMeansFit <- kmeans(cuisDF, centers = K, nstart = 20)
kMeansFit

# -------------------------------------------------------
# Finding the optimal K value for clustering via K-Means.
# Runing through different values of K, and visualize where 
# the optimal value could lie. 
kMin <- 1
kMax <- 20
withinSS <- double(kMax - kMin + 1)
betweenSS <- double(kMax - kMin + 1)

for (K in kMin:kMax) {
  kMeansFit <- kmeans(cuisDF, centers = K, nstart = 20)
  withinSS[K] <- sum(kMeansFit$withinss)
  betweenSS[K] <- kMeansFit$betweenss
}

# -------------------------------------------------------
# Visualizing the Between and Within Sum of Square
# to look for an elbow point, the optimal value of K
plot(kMin:kMax, withinSS, pch=19, type="b", col="red",
     xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")

plot(kMin:kMax, betweenSS, pch=19, type="b", col="green",
       xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")

# The only elbow point that could be visualized from the
# two plots is at K = 2.

# -------------------------------------------------------
# Perform EM Clustering on the dataset
# install.packages("mclust")
library(mclust)
emFit <- Mclust(cuisDF)
summary(emFit)
# EM Clustering does not support the assumption that the 
# optimal number of clusters is 2. 
# This maybe due to the shape of the clusters being formed.
# EM tends to find elliptical or gaussian clusters and 
# hence could divide  long oddly shaped or chain clusters
# into multiple smaller clusters.

# -------------------------------------------------------
# Upon performing K-Means and EM Clustering we could probably
# conclude that the structure of the data is arbitrary.
# Hierarchical clustering works well in such situations. 

# -------------------------------------------------------
# Optimizing through variations in Hierarchical Clustering

# -------------------------------------------------------
# Single 
hiercFit <- hclust(dist(cuisDF, method = "euclidean"),method="single")
hiercFit

# Visualize Dendogram
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)
# Single method of clustering tends to produce long 
# thin cluster chains.With the dendogram we can see
# that there is no easy distinction to form or clearly
# gain the optimal number of clusters. This type of 
# method doesn't seem to suit the structure of the data.

# -------------------------------------------------------
# Complete 
hiercFit <- hclust(dist(cuisDF, method = "euclidean"),
                   method="complete")
hiercFit

# Visualize Dendogram
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)
# Complete method excels in identifying compact clusters 
# of similar diameters. However the dendogram doesnt show
# clear seperation of clusters between each other. The data
# is not suited for complete linkage. We can even see that 
# some single points are added to the clusters at the end.
# Signalling bad clustering done before hand.

# -------------------------------------------------------
# Average 
hiercFit <- hclust(dist(cuisDF, method = "euclidean"), method="average")
hiercFit
# Visualize Dendogram
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)
# Average, basically averages the pairwise distances dist(x,y).
# Averages seems to work differently than other methods in the ways it
# is combining the clusters in the dendogram. 
# We find that there is no proper straight cut that could be used to determine
# optimal clusters.

# -------------------------------------------------------
# The initial model built turned out to be the best suited
# for the dataset. It seems to provide the arbitrary shape of the 
# the two cluster with clearly defined seperation.

# Ward 
hiercFit <- hclust(dist(cuisDF, method = "euclidean"),method="ward.D")
hiercFit

# Visualize Dendogram
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)

# Number of clusters : 2 
K <- 2 
rect.hclust(hiercFit, k = K)

# =========================================================
# Problem 2:
# In this problem, our dataset is -- responses.csv. It 
# documents the responses from a survey, connecting individual 
# preferences to the demography. The target is to perform 
# clustering with appropriate choice of algorithm and distance
# notion to identify the optimal number of clusters in the dataset.
# Also mention the strongest parameters and answer if you think
# Gender plays a major role in clustering

# =======================================================

# ---------------------------------------------------------
# Clean environment and remove all working data
rm(list = ls())

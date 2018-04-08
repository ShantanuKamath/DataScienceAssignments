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

# ---------------------------------------------------------
# Final  
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

# ---------------------------------------------------------
# Importing the dataset
respData <- read.csv("responses.csv", header=TRUE)

# Inspect the DataFrame
head(respData)
str(respData)

# Summary Statistics - Identified NA's
summary(respData)

# Check the dimensions
dim(respData)

# ---------------------------------------------------------
# Checking for missing values
M <- sapply(respData, function(x) sum(is.na(x))); 

# Generating list of columns and respective number of missing values.
M[M>0]

# Calculating sparsity percent
pMiss <- function(x){sum(is.na(x))/length(x)}
M <- apply(respData,2,pMiss)
M[M>0.01]
M <- apply(respData,1,pMiss)
M[M>0.05]

# ---------------------------------------------------------
# Using mice to fill missing value
# Ref: https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
# install.packages("mice")
library(mice)

# It allows us to understand how many types of rows
# having what missing.
# From the data we can see we have, 686 complete rows.
# Hence it doesnt make sense to remove rows with NA's 
# as this would result in loss of atleast 31% of data
md.pattern(respData)

# ---------------------------------------------------------
# Using VIM to visualise how much data is missing 
# and what can be done
# install.packages("VIM")
library(VIM)
aggr_plot <- aggr(respData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(respData), cex.axis=.7, gap=3,
                  ylab=c("Histogram of missing data","Pattern"))

# We now know that the variable Height and Weight
# Have the most missing data. The most itself is quite low 
# ~2%. Hence we can easily use mice to predict these values and
# fill it in.

# ---------------------------------------------------------
tempData <- mice(respData,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
tempData1 <- complete(tempData)
# The above took very long to run so I ran it once and saved the file
# for reuse in future.
# write.csv(tempData1, file = "My50ImputedData.csv")
# tempData1 <- read.csv("My50ImputedData.csv", header = TRUE)
# tempData1 <- tempData1[-1]

# ---------------------------------------------------------
# Converting factors into level numbers of numerica type
# to allow for use of Hierarchical Clustering
# Ref: https://stackoverflow.com/questions/14984989/how-to-avoid-warning-when-introducing-nas-by-coercion
indx <- sapply(tempData1, is.factor)
tempData1[indx] <- lapply(tempData1[indx], function(x) as.numeric(as.factor(x)))

# -------------------------------------------------------
# Hierarchical Clustering : Using this clustering method 
# to build the dendogram and analyse how many clusters or 
# K is required. Using method ward to check for compact 
# sperical clusters.
hiercFit <- hclust(dist(tempData1, method = "euclidean"), method="ward.D")
hiercFit
# Visualize Dendogram : Through the dendogram it is clear 
# that there is a good possibility of having atleast two 
# clusters. It seems that 4 can be optimal number of 
# clusters too.
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)

# Assuming that there two clusters are optimal, 
# Visualise the seperation
K <- 2 # 4
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
kMeansFit <- kmeans(tempData1, centers = K, nstart = 20)
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
  kMeansFit <- kmeans(tempData1, centers = K, nstart = 20)
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
# two plots is at K =2 very clearly in both visualizations.
# Ruling out the K = 4 option

# -------------------------------------------------------
# Perform EM Clustering on the dataset
# install.packages("mclust")
library(mclust)
emFit <- Mclust(tempData1)
summary(emFit)
# EM Clustering resulted in only one cluster. This is probably
# due to the shape of the dataset not adhering to the gaussian
# distribution. Hence the clustering algorithm encompasses the
# entire dataset into one gaussian structure. Though the dataset
# isnt of the same form.

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
hiercFit <- hclust(dist(tempData1, method = "euclidean"),method="single")
hiercFit

# Visualize Dendogram
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)
# Single method of clustering tends to produce long 
# thin cluster chains. With the dendogram we can see
# Most of the datapoints are clustered at the same level of
# similarity and into almost one cluster, very early on. 
# This points that the data could be chained or a long string
# like structure.

# At this point we can probaly imagine the data to be a ring or
# hair like stricture, where the points are all continuous with 
# similar distance between them in a chained manner.
# If we remove some outliers it could be just one cluster.

# -------------------------------------------------------
# Complete 
hiercFit <- hclust(dist(tempData1, method = "euclidean"),
                   method="complete")
hiercFit

# Visualize Dendogram
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)
# Complete method excels in identifying compact clusters 
# of similar diameters. The dendogram shows that the data is 
# most probably having this 2 or 3 clusters. 

# -------------------------------------------------------
# Average 
hiercFit <- hclust(dist(tempData1, method = "euclidean"), method="average")
hiercFit
# Visualize Dendogram
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)
# Average, basically averages the pairwise distances dist(x,y).
# Upon viewing of so many dendograms, It has become clear that
# a couple of points are serving as outliers to the all clusters
# and are being cluster only at the end. 
# We should take care of this as this changes the number of cluster
# immediately. This dendogram too seems to suggest that there are two
# clusters in the most optimal clustering 

# -------------------------------------------------------
# As seen earlier ward too supports the ideology of having two clusters.
# It seems that average, complete and ward are looking for similar structures.
# Probably differentiating based on minor features or attributes.

# Ward 
hiercFit <- hclust(dist(tempData1, method = "euclidean"),method="ward.D")
hiercFit

# Visualize Dendogram
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)

# -------------------------------------------------------
# Final 
# Number of clusters : 2 
K <- 2 
rect.hclust(hiercFit, k = K)

# -------------------------------------------------------
# Calculating Gender distribution to understand importance
K <- 2
kMeansFit <- kmeans(tempData1, centers = K, nstart = 20)
kMeansFit
out <- cbind(tempData1, clusterNum=kMeansFit$cluster)


# -------------------------------------------------------
# Gender segementation in Cluster 1
# Total population = 619
nrow(out[out$clusterNum == 1,])
# Female = 543 and Male = 76
nrow(out[out$Gender == 2 & out$clusterNum == 1,])
nrow(out[out$Gender == 3 & out$clusterNum == 1,])

# -------------------------------------------------------
# Gender segementation in Cluster 2
# Total population = 391
nrow(out[out$clusterNum == 2,])
# Female = 50 and Male = 341
nrow(out[out$Gender == 2 & out$clusterNum == 2,])
nrow(out[out$Gender == 3 & out$clusterNum == 2,])

# -------------------------------------------------------
# Yes we can see that Gender plays a major role.
# Cluster 1 is 87% dominated by Females, where as
# Cluster 2 is 87% dominated by Males.
# Hence gender help to easily identify which
# cluster the data point is likely to be clustered
# We can perform similar metrics to determine which variable
# can dominate which cluster and hence prove to be a strong 
# variable to seperate clusters.

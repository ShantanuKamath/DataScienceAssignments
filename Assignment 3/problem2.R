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
# Using mice to fill missing valyes
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

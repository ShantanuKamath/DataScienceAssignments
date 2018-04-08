# =======================================================
# Problem 1 : Starter Code : JSON to Document-Term Matrix
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

cuisDTM <- removeSparseTerms(cuisDTM, .95)
# Ref: https://stackoverflow.com/questions/28763389/how-does-the-removesparseterms-in-r-work
# Inspect the first few elements
inspect(cuisDTM[1:5,])

# Create DataFrame from the DTM
cuisDF <- as.data.frame(as.matrix(cuisDTM))

rowTotals <- apply(cuisDF , 1, sum) #Find the sum of words in each Document
cuisDF   <- cuisDF[rowTotals> 0, ]           #remove all docs without words

# Inspect the DataFrame
names(cuisDF)
str(cuisDF)
dim(cuisDF)


myPal <- c("red","blue","darkgreen","magenta","darkgrey","black")

hiercFit <- hclust(dist(cuisDF, method = "euclidean"),    # choice of distance
                   method="ward.D")                         # choice of linkage
hiercFit
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)

K <- 7
rect.hclust(hiercFit, k = K)

kMin <- 1
kMax <- 20
withinSS <- double(kMax - kMin + 1)
betweenSS <- double(kMax - kMin + 1)

for (K in kMin:kMax) {
  kMeansFit <- kmeans(cuisDF, centers = K, nstart = 20)
  withinSS[K] <- sum(kMeansFit$withinss)
  betweenSS[K] <- kMeansFit$betweenss
}

plot(kMin:kMax, betweenSS, pch=19, type="b", col="red",
     xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")
points(kMin:kMax, withinSS, pch=19, type="b", col="green")
plot(kMin:kMax, withinSS, pch=19, type="b", col="green",
     xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")

library(mclust)
emFit <- Mclust(cuisDF)
summary(emFit)

plot(emFit, what = "classification")
emFit$parameters

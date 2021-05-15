load("idList-co-100.Rdata")

id <- do.call(rbind, idList[1:10]) #change to 10...
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

set.seed(423)
id_shuffle <- id[sample(nrow(id)),]

id_pca <- prcomp(id_shuffle[,-1], 
                 center = TRUE, scale. = TRUE)

load("id100.Rda")
testData <- id[,-1]
colnames(testData) <- colnames(id_shuffle[,-1])
testX <- predict(id_pca,testData)


################################################################# Exercise 2.1.1 #################################################################

library(class)
library(caret)
library(ggplot2)
library(ggfortify)

load("id100.Rda")

set.seed(423)
id_shuffle <- id[sample(nrow(id)),]

id_pca <- prcomp(id_shuffle[,-1], 
                 center = TRUE, scale. = TRUE)

eigs <- id_pca$sdev^2
Proportion = eigs/sum(eigs)
Cumulative = cumsum(eigs)/sum(eigs)

plot(Proportion,  type="o", col="blue")
plot(Cumulative, type="o", col="red")


######################################################## Exercise 2.1.2 & 2.1.3 #################################################################

load("idList-co-100.Rdata")

id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

set.seed(423)
id_shuffle <- id[sample(nrow(id)),]

percent <- c(0.8, 0.9, 0.95, 0.99)

## Method 1 (lazy way, not rigorous): we train the whole data set including training set and test set
id_pca <- prcomp(id_shuffle[,-1], 
                 center = TRUE, scale. = TRUE)

id_train_labels <- id_shuffle[1:(nrow(id)/2),1]
id_test_labels <- id_shuffle[(nrow(id)/2 + 1):nrow(id),1]
  
for (p in 1:4) {
  id_train <- id_pca$x[1:(nrow(id)/2),cumsum(Proportion) < percent[p]] 
  id_test <- id_pca$x[((nrow(id)/2)+1):nrow(id),cumsum(Proportion) < percent[p]]
  
  ptm <- proc.time();
  id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=1)
  time <- proc.time() - ptm
  cf <- confusionMatrix(id_test_labels, id_test_pred)
  print( time )
  print( sum(diag(cf$table))/sum(cf$table) )
  
}

## Method 2 (more rigorous): PCA is trained on only the training data set 

data_train <- id_shuffle[1:(nrow(id)/2),-1]
data_test <- id_shuffle[(nrow(id)/2 + 1):nrow(id),-1]

for (p in 1:4) {
  id_pca <- prcomp(data_train, center = TRUE, scale. = TRUE)
  eigs <- id_pca$sdev^2
  Proportion = eigs/sum(eigs)
  
  test_pca <- predict(id_pca,data_test)
  
  id_train <- id_pca$x[,cumsum(Proportion) < percent[p]] 
  id_test <- test_pca[,cumsum(Proportion) < percent[p]]
  
  ptm <- proc.time();
  id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=3)
  time <- proc.time() - ptm
  cf <- confusionMatrix(id_test_labels, id_test_pred)
  print( time )
  print( sum(diag(cf$table))/sum(cf$table) )
}


################################################################### Exercise 2.2 #################################################################

## define the function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


## Proc 1: perform the normalization before PCA
id_sn <- as.data.frame(lapply(id_shuffle[-1], normalize))
id_pca <- prcomp(id_sn, center = TRUE, scale. = TRUE) 
# then you can do the rest of processing (refer to line 145)

## Proc 2: perform the normalization after PCA
id_pca <- prcomp(id_shuffle[,-1], center = TRUE, scale. = TRUE) 
id_pca_x <- id_pca$x[, cumsum(Proportion) < 0.80]
id_pca_sn <- as.data.frame(lapply(as.data.frame(id_pca_x[,]), normalize))
# then you can do the rest of processing (refer to line 145)


################################################################### Exercise 2.3 #################################################################

library(spatstat)

## define the function
smoothImage <- function(grayImg){
  smoothed <- as.matrix(blur(as.im(grayImg), sigma = 0.7, normalise=FALSE, bleed = TRUE, varcov=NULL))
  return(smoothed)
}

id_mat <- data.matrix(id, rownames.force = NA)
imageSize <- sqrt(ncol(id_mat) - 1)

# Smooth all images
for(i in 1:nrow(id_mat))
{
  image <- c(id_mat[i,2:ncol(id)]) #Convert pixels to matrix type
  image <- matrix(image,nrow = imageSize,ncol = imageSize, byrow = FALSE) # Resize to nxn matrix instead of 1x(n*n)
  image <- smoothImage(image) # Perform smoothing
  id_mat[i,2:ncol(id_mat)] <- matrix(image,nrow = 1,ncol = ncol(id_mat) - 1, byrow = FALSE) #Reszie and insert
}

id <- as.data.frame(id_mat) # Convert back to data.frame
id[,1] <- factor(id[,1])


## 10 folds cross validation
set.seed(123)
folds <- createFolds(id[,1], k = 10)
listOfFolders <- c(1:10)
for(i in 1:10)
{
  id_train <- id[-folds[[i]],-1]
  id_test <- id[folds[[i]],-1]
  
  id_train_labels <- id[-folds[[i]],1]
  id_test_labels <- id[folds[[i]],1]
  
  # Here I used raw data for example, but you can apply PCA before KNN
  id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=7)
  
  cf <- confusionMatrix(id_test_labels, id_test_pred)
  listOfFolders[i] <- sum(diag(cf$table))/sum(cf$table)
  
}

print(listOfFolders)
mean(listOfFolders)
var(listOfFolders)

boxplot(listOfFolders,data=listOfFolders, main="10 Cross Test (Smoothed)", xlab="", ylab="Recognition") 


################################################################# Exercise 2.4.1 #################################################################

load("id100.Rda")
id_mat <- data.matrix(id, rownames.force = NA)
imageSize <- sqrt(ncol(id_mat) - 1)

rotate <- function(x) t(apply(x, 2, rev))

# Show first 10 images
for(i in 1:10)
{
  rotated <- c(id_mat[-400+i*400+1,2:ncol(id_mat)]) # Choose the pixel
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated))) # Normalize
  eigenVector <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE) # Resize to image size nxn instead of 1x(n*n)
  eigenVector <- rotate(eigenVector) # Rotate to correct orientation
  image( eigenVector,  zlim=c(0,1), col=gray(0:100/100) ) # Visualize image
}


################################################################# Exercise 2.4.2 #################################################################

id_pca <- prcomp(id_mat[,-1], center = TRUE, scale. = TRUE) 

eigs <- id_pca$sdev^2
Proportion = eigs/sum(eigs)

### Show first 10 Principal Components ###
for(i in 1:10)
{
  rotated <- c(id_pca$rotation[,i]) # Rotation and eigen vector is the same
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated))) # Normalize for visualization
  eigenVector <- matrix(rotated,nrow = imageSize,ncol = imageSize,byrow = FALSE) # convert to matrix 
  eigenVector <- rotate(eigenVector) # Rotate 90 degree for display
  image( eigenVector,  zlim=c(0,1), col=gray(0:100/100) ) # Show
}


################################################################# Exercise 2.4.3 #################################################################

cipherNumber <- 1 # We select a cipher for visualization

# Combine all scores with eigen vectors to recreate image
trunc <- id_pca$x[cipherNumber,1:324] %*%  t(id_pca$rotation[,1:324])
 
trunc <- scale(trunc, center = -1 * id_pca$center, scale=FALSE) 
imageM <- matrix( trunc,nrow = imageSize,ncol = imageSize,byrow = FALSE)
imageM <- rotate(imageM) # rotate is a function to rotate the image
image( imageM, col=gray(0:100/100))


################################################################# Exercise 2.4.4 #################################################################

eigs <- id_pca$sdev^2
Proportion = eigs/sum(eigs)

for(cipherNumber in c(1,401,801,1201,1601,2001,2401,2801,3201,3601))
{
  # recreate using 90% of the data 
  trunc <- id_pca$x[cipherNumber,cumsum(Proportion) < 0.90] %*% t(id_pca$rotation[,cumsum(Proportion) < 0.90])
  
  trunc <- scale(trunc, center = -1 * id_pca$center, scale=FALSE)
  imageM <- matrix( trunc,nrow = imageSize,ncol = imageSize,byrow = FALSE)
  imageM <- rotate(imageM) # rotate is a function to rotate the image
  image( imageM, col=gray(0:100/100))
}


################################################################# Exercise 2.4.5 #################################################################

for(cipherNumber in c(1,401,801,1201,1601,2001,2401,2801,3201,3601))
{
  print( id_pca$x[cipherNumber,1:10] )
}


# define the function for normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# prepare data
load("idList-co-100.Rdata")

id <- do.call(rbind, idList[1:2])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

id_sn <- as.data.frame(lapply(id[-1], normalize))


################################################################# Exercise 4.1.1 ################################################################# 

# define the function to compute entropy
entropy <- function(S) { # Function to calculate entropy as in the slides
  fullSum <- 0
  for( i in (0:9) ) { 
    if( nrow(S) > 0 ) # Make sure that there is something in the list
      { pi <- nrow(S[ S[,1] == i , ]) / nrow(S) }else{pi <- 0} 
    if(pi > 0 ){
    fullSum <- fullSum - pi * log2(pi)
    }
  }
  return(fullSum)
}

id_pca <- prcomp(id_sn, center = TRUE, scale. = TRUE) 

id_pca_first_5 <- id_pca$x[, 1:5] # Take first five scores
id_pca_first_5 <- data.frame(id_pca_first_5) # Make it a data frame

# Calculate starting entropy
entBefore <- entropy(id) 

for( img in (1:5)){
  entList <- c() 
  xList <- c()  
  
  Pts <- seq(min(id_pca_first_5[ ,img]), max(id_pca_first_5[ ,img]), length.out=200)
  for( splitP in (1:200) ){
    S1 <- id[ id_pca_first_5[ ,img] < Pts[splitP], ]   # Perform splits
    S2 <- id[ id_pca_first_5[ ,img] >= Pts[splitP], ]
    s1 <- nrow(S1) 
    s2 <- nrow(S2)
    xList[splitP] <- Pts[splitP]
    ent <- ( s1 * entropy(S1) )/(s1 + s2) + ( s2 * entropy(S2) )/(s1 + s2)  # Calculate entropy
    entList[splitP] <-  entBefore - ent 	# Information gain is calculated 
  }
  plot(Pts, entList)
}


################################################################# Exercise 4.1.2 #################################################################

# if you use the rpart lib
library(rpart)
library(rpart.plot)

datanew <- cbind(id_pca_first_5, id[,1])
datanew$States <-factor(datanew[,6])
treeNew <- rpart(States ~ PC1 + PC2 + PC3 + PC4 + PC5, data = datanew, method = "class")
summary(treeNew)
# Plot the tree
rpart.plot(treeNew) 

# if you use the C50 lib
library(C50) 
treeModel <- C5.0(x = id[, -1], y = id[,1]) 
plot(treeModel)


################################################################# Exercise 4.1.3 #################################################################

library(class)
library(caret)

# Train the tree without PCA
treeNew <- rpart(V1 ~ ., data = id, method = "class")

# new data for evaluation
id2 <- do.call(rbind, idList[6:10])
id2 <- as.data.frame(id2)
id2[,1] <- factor(id2[,1])

predictions <- predict(treeNew, id2[,-1], type = "class") # Test, remember to set output as "class"

cf <- confusionMatrix(predictions, id2[,1])
print( sum(diag(cf$table))/sum(cf$table) )



################################################################# 4.2 Random Forests #################################################################

library(party)
library(randomForest)
library(rfUtilities)

load("idList-co-100.Rdata")

id <- do.call(rbind, idList[1:5])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

id2 <- do.call(rbind, idList[6:10])
id2 <- as.data.frame(id2)
id2[,1] <- factor(id2[,1])

model.randomforest <- randomForest(V1 ~ ., data = id, ntree=100)
p <- predict(model.randomforest,id2)

cf <- confusionMatrix(p, id2[,1])
print( sum(diag(cf$table))/sum(cf$table) )

plot(model.randomforest) # Error as a function of trees

# 10-fold cross validation
model.cv <- rf.crossValidation(x = model.randomforest, xdata = id, p = 0.1, n = 10) 

# Plot cross validation verses model producers accuracy
par(mfrow=c(1,2)) 
plot(model.cv, type = "cv", main = "CV producers accuracy")
plot(model.cv, type = "model", main = "Model producers accuracy")

# Plot cross validation verses model oob
par(mfrow=c(1,2)) 
plot(model.cv, type = "cv", stat = "oob", main = "CV oob error")
plot(model.cv, type = "model", stat = "oob", main = "Model oob error")


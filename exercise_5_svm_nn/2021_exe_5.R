normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


################################################################### Exercise 5.1 ################################################################# 

#install.packages("kernlab")

library(kernlab)
library(class)
library(caret)

load("idList-co-100.Rdata")

id <- do.call(rbind, idList[1:2])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

classifier_rbf <- ksvm(V1~ ., data = id, kernel = "rbfdot", kpar=list(sigma=0.05), C = 3)

id2 <- do.call(rbind, idList[3])
id2 <- as.data.frame(id2)
id2[,1] <- factor(id2[,1])

predictions_rbf <- predict(classifier_rbf, id2[,-1]) #, type = "class")

agreement_rbf <- predictions_rbf == id2[,1]
confusionMatrix(predictions_rbf, id2[,1])

table(agreement_rbf)

prop.table(table(agreement_rbf))




################################################################# Exercise 5.2.1 ################################################################# 

load("idList-co-100.Rdata")

id <- do.call(rbind, idList[1:5])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

lev <- levels(id[,1]) # Get number of output classes

nnTrainingClass <- matrix(nrow = length(id[,1]), ncol = 10, data =  0) # Create training matrix

for(i in 1:length(id[,1])) { # For all rows
  matchList <- match(lev,toString(id[i,1])) # Find which position matches the Class
  matchList[is.na(matchList)] <- 0 # Insert 0 everywhere else
  nnTrainingClass[i,] <- matchList # insert into our training matrix
}

trainingClass <- as.data.frame(nnTrainingClass)

################################################################# Exercise 5.2.2 ################################################################# 

library("RSNNS")
library("devtools")
library("neuralnet")

# Training network
network <- mlp(id[,-1], trainingClass, size = c(120,120,120), maxit = 200, hiddenActFunc = "Act_TanH", learnFunc="Std_Backpropagation", learnFuncParams = c(0.01,0)) 
plot(network)
plotIterativeError(network) # Plot the training error


################################################################# Exercise 5.2.3 ################################################################# 

id2 <- do.call(rbind, idList[6:10])
id2 <- as.data.frame(id2)
id2[,1] <- factor(id2[,1])

predictions_nn <- predict(network, id2[,-1]) # Predict the probabilities

responselist <- matrix(nrow = length(id2[,1]), ncol = 1, data = "Na") # Create an empty list

for(i in 1:nrow(predictions_nn)) {
  responselist[i,] <- toString( which(predictions_nn[i,]==max(predictions_nn[i,])) - 1 ) # Insert max in each row into response list
}
responselist <- data.frame(responselist)
responselist[,1] <- as.factor(responselist[,1]) # Factorize

levels(responselist[,1]) <- levels(id2[,1]) # Make sure number of levels is equal

cf <- confusionMatrix(responselist[,1], id2[,1])
print( sum(diag(cf))/sum(cf) )



################################################################# Exercise 5.2.4 ################################################################# 

id_pca <- prcomp(id[,-1], center = TRUE, scale. = TRUE)
test_pca <- predict(id_pca,id2)

# Training network
network <- mlp(id_pca$x, trainingClass, size = c(150,100,50), maxit = 100, hiddenActFunc = "Act_TanH", learnFunc="Std_Backpropagation", learnFuncParams = c(0.01,0)) 

predictions_nn <- predict(network, test_pca) # Predict the probabilities

responselist <- matrix(nrow = nrow(id2), ncol = 1, data = "Na") # Create an empty list

for(i in 1:nrow(predictions_nn)) {
  responselist[i,] <- toString( which(predictions_nn[i,]==max(predictions_nn[i,])) - 1 ) # Insert max in each row into response list
}
responselist <- data.frame(responselist)
responselist[,1] <- as.factor(responselist[,1]) # Factorize

levels(responselist[,1]) <- levels(id2[,1]) # Make sure number of levels is equal

cf <- confusionMatrix(responselist[,1], id2[,1])
print( sum(diag(cf))/sum(cf) )

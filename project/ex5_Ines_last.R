library(rpart)
library(rpart.plot)
library(stats)
library(randomForest)
library(caret)
library(kernlab)
library(RSNNS)
library(neuralnet)

################################
# This version: 
# - plots and computes computational code 
# - changed to 3 hidden layers with 120 nodes like Zhuoqi and (150,100,50) for PCA part

# TODO: add pre-processing from Karol's 
# TODO: Cross-validation 
# TODO: mean and variance
# TODO: a function for computing + plotting run-time maybe instead of copying same code everywhere

# current issue: takes too long to run
################################

#Load the dataset
load("idList-cornered-100-2021.Rdata")

id <- do.call(rbind, idList[])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

items_per_person = nrow(id) / length(idList)
id_train <- id[1:(items_per_person*9),]
id_test <- id[(items_per_person*9 + 1):(items_per_person*13),]

####################################
####### 5.2: Neural Networks ####### 
####################################

#######################################################################################################
## 5.2.1
## Create a matrix (as below in grey background) for the training classes. It has N rows (the same as 
## the number of training data) and 10 columns (binary). The column with '1' marks the 
## corresponding class as the example shown below (eg. cyper '0' is represented by '1000000000').
#######################################################################################################

lev <- levels(id_train$V1) # Number of classes

# Create a list probabilities, for all labels
nnTrainingClass <- matrix(nrow = length(id_train$V1), ncol = 10, data = 0) 

for(i in 1:length(id_train$V1)) { # Set probabilities to one for matching class
  matchList <- match(lev,toString(id_train$V1[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i,] <- matchList
}
trainingClass <- as.data.frame(nnTrainingClass)


############################################################################################
## 5.2.2
## Train a neural network with N inputs and 10 outputs, based on the modified training data.
############################################################################################


# train_mlp <- function(training_data, training_classes, hidden_layers) {
#   
#   network <- mlp(id_train[,-1], training_classes, size = hidden_layers, maxit = 200, hiddenActFunc = "Act_TanH", 
#                  learnFunc="Std_Backpropagation", learnFuncParams = c(0.01,0))
#   
#   plot(network)
#   
#   plotIterativeError(network)
#   
#   network$IterativeFitError[200]
#   
#   return(network)
# }
# 
# network <- train_mlp(id_train, trainingClass, c(120,120,120))

# network <- mlp(id_train[,-1], trainingClass, size = c(120,120,120), maxit = 300, hiddenActFunc = "Act_TanH", learnFunc="Std_Backpropagation", learnFuncParams = c(0.01,0)) 
# plot(network)
# plotIterativeError(network) # Plot the training error

train_mlp <- function(training_data, training_classes, size) {
  
  #network <- mlp(id[,-1], trainingClass, size = c(2,2,2), maxit = 2, hiddenActFunc = "Act_TanH", learnFunc="Std_Backpropagation", learnFuncParams = c(0.01,0))
  
  network <- mlp(id_train[,-1], training_classes, size = size, maxit = 300, hiddenActFunc = "Act_TanH", learnFunc="Std_Backpropagation", learnFuncParams = c(0.01,0))
  
  plotIterativeError(network)
  
  network$IterativeFitError[300]
  
  return(network)
}

network <- train_mlp(id_train, trainingClass, c(5, 5))


############################################################################################
## 5.2.3
## Evaluate the neural network with the test data.
############################################################################################
evaluate_nn <- function(network, test_data) { 
  
  predictions <- predict(network, test_data[,-1])
  
  #You can use the following code to convert the mlp output into class labels (0 - 9)
  responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")
  for(i in 1:nrow(predictions)) {
    responselist[i,] <- toString( which(predictions[i,]==max(predictions[i,])) - 1 )
    
  }
  responselist <- data.frame(responselist)
  responselist[,1] <- as.factor(responselist[,1])
  
  # Calculating the accuracy
  cf <- confusionMatrix(responselist[,1], test_data[,1])
  return( sum(diag(cf))/sum(cf) )
  
}

######################## One hidden layer #########################################################
####################################################################################################
accuracyTestList <- c()
accuracyTrainingList <- c()
runtimeList <- c()

#neurons <- c(2,4,6,8,10,12,14,16,18,20, 22, 24, 26, 28, 30)

neurons <- c(38,40,42,44,46,48,50,52)

for (i in seq(38,52, by = 2)) {
  start_time <- Sys.time()
  network <- train_mlp(id_train, trainingClass, (i+1))
  end_time <- Sys.time()
  runtimeList[[i+1]] <- difftime(Sys.time(), start_time, units = "secs")
  
  accuracyTestList[[i+1]] <- evaluate_nn(network, id_test)
  accuracyTrainingList[[i+1]] <- evaluate_nn(network, id_train)
  
}

plot(neurons, unlist(accuracyTrainingList), type="b", col=1, lwd=1, pch=15, xlab="Number of Neurons in Hidden Layer", ylab="Accuracy")
plot_labels <- c("Training Data")

lines(neurons, unlist(accuracyTestList), type="b", col=2, lwd=1, pch=16)
plot_labels[2] <- paste("Test Data")


title("Evaluation of an ANN with One Hidden Layer")
legend("bottomright",plot_labels, lwd=c(1), col=c(1,2), pch=c(15,16), y.intersp=1)

plot(neurons, unlist(runtimeList), type="b", col=1, lwd=1, pch=15, xlab="Number of Neurons in Hidden Layer", ylab="Runtime (s)")
title("NN Training Runtime -- one hidden layer")

######################## Two hidden layers #########################################################
####################################################################################################
accuracyTestList <- c()
accuracyTrainingList <- c()
runtimeList <- c()

#neurons <- c(2,4,6,8,10,12,14,16,18,20, 22, 24, 26, 28, 30)

neurons <- c(38,40,42,44,46,48,50,52)

for (i in seq(38,52, by = 2)) {  
  start_time <- Sys.time()
  
  size <- (i + 1)
  network <- train_mlp(id_train, trainingClass, c(150,size))
  
  end_time <- Sys.time()
  runtimeList[[i+1]] <- difftime(Sys.time(), start_time, units = "secs")
  
  accuracyTestList[[i+1]] <- evaluate_nn(network, id_test)
  accuracyTrainingList[[i+1]] <- evaluate_nn(network, id_train)
}

plot(neurons, unlist(accuracyTrainingList), type="b", col=2, lwd=0.8, pch=16, xlab="Number of Neurons in Second Hidden Layer", ylab="Accuracy")
plot_labels <- c("Training Data")

# For some reason R doesn't show this second line
lines(neurons, unlist(accuracyTestList), type="b", col=1, lwd=0.8, pch=15)
plot_labels[2] <- paste("Test Data")

title("Evaluation of an ANN with Two Hidden Layers")
legend("bottomright",plot_labels, lwd=c(1), col=c(1,2), pch=c(16,15), y.intersp=1)

plot(neurons, unlist(runtimeList), type="b", col=1, lwd=1, pch=15, xlab="Number of Neurons in Second Hidden Layer", ylab="Runtime (s)")
title("NN Training Runtime -- two hidden layers")


######################## Three hidden layers #########################################################
####################################################################################################
accuracyTestList <- c()
accuracyTrainingList <- c()
runtimeList <- c()

#neurons <- c(2,4,6,8,10,12,14,16,18,20, 22, 24, 26, 28, 30)

neurons <- c(38,40,42,44,46,48,50,52)

for (i in seq(38,52, by = 2)) {
  
  start_time <- Sys.time()
  
  size <- (i + 1)
  network <- train_mlp(id_train, trainingClass, c(150, 100,size)) #(150,100,50),
  
  end_time <- Sys.time()
  runtimeList[[i+1]] <- difftime(Sys.time(), start_time, units = "secs")
  
  accuracyTestList[[i+1]] <- evaluate_nn(network, id_test)
  accuracyTrainingList[[i+1]] <- evaluate_nn(network, id_train)
}

plot(neurons, unlist(accuracyTrainingList), type="b", col=2, lwd=0.8, pch=16, xlab="Number of Neurons in Third Hidden Layer", ylab="Accuracy")
plot_labels <- c("Training Data")

# For some reason R doesn't show this second line
lines(neurons, unlist(accuracyTestList), type="b", col=1, lwd=0.8, pch=15)
plot_labels[2] <- paste("Test Data")

title("Evaluation of an ANN with Three Hidden Layers")
legend("bottomright",plot_labels, lwd=c(1), col=c(1,2), pch=c(16,15), y.intersp=1)

plot(neurons, unlist(runtimeList), type="b", col=1, lwd=1, pch=15, xlab="Number of Neurons in Second Hidden Layer", ylab="Runtime (s)")
title("NN Training Runtime -- three hidden layers")

############################################################################################
## 5.2.4
## Training of NN with pre-processed data
############################################################################################

performPCA <- function(dataset, pcaCount){
  pca <- prcomp(dataset,scale=FALSE, rank. = pcaCount) # rank = a number specifying the maximal rank, i.e., maximal number of principal components to be used. 
  return (pca)
}

pca.10 <- performPCA(id[,-1], 10)

accuracyTestList.pca <- c()
accuracyTrainingList.pca <- c()
runtimeList.pca <- c()

neurons <- c(38,40,42,44,46,48,50,52)

for (i in seq(38,52, by = 2)) {
  
  start_time <- Sys.time()
  size <- (i + 1)
  network <- train_mlp(pca.10$x, trainingClass, c(150,100,size)) #c(150,100,50) <- From Zhuoqi
  end_time <- Sys.time()
  
  runtimeList.pca[[i+1]] <- difftime(Sys.time(), start_time, units = "secs")
  
  accuracyTestList.pca[[i+1]] <- evaluate_nn(network, id_test)
  accuracyTrainingList.pca[[i+1]] <- evaluate_nn(network, id_train)
}

plot(neurons, unlist(accuracyTrainingList.pca), type="b", col=1, lwd=1, pch=15, xlab="Number of Neurons in Hidden Layer", ylab="Accuracy")
plot_labels <- c("Training Data")

lines(neurons, unlist(accuracyTestList.pca), type="b", col=2, lwd=1, pch=16)
plot_labels[2] <- paste("Test Data")


title("Evaluation of an ANN w/ PCA Preprocessing and One Hidden Layer")
legend("bottomright",plot_labels, lwd=c(1), col=c(1,2), pch=c(15,16), y.intersp=1)


plot(neurons, unlist(runtimeList), type="b", col=1, lwd=1, pch=15, xlab="Number of Neurons in Second Hidden Layer", ylab="Runtime (s)")
title("NN w/ PCA Preprocessing Training Runtime -- two hidden layers")



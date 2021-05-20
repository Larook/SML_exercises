
get_training_class <- function(training_data){
  # Create a list probabilities, for all labels
  nnTrainingClass <- matrix(nrow = length(training_data$V1), ncol = 10, data = 0) 
  
  for(i in 1:length(training_data$V1)) { # Set probabilities to one for matching class
    matchList <- match(levels(training_data$V1), toString(training_data$V1[i]))
    matchList[is.na(matchList)] <- 0
    nnTrainingClass[i,] <- matchList
  }
  trainingClass <- as.data.frame(nnTrainingClass)
  return(trainingClass)
}



train_mlp <- function(training_data, training_classes, size) {
  
  network <- mlp(training_data[,-1], training_classes, size = size, maxit = 300, hiddenActFunc = "Act_TanH", learnFunc="Std_Backpropagation", learnFuncParams = c(0.01,0))
  
  plotIterativeError(network)
  
  network$IterativeFitError[300]
  
  return(network)
}


evaluate_nn <- function(network, test_data) { 
  # check if this is working!
  
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

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



create_folds_disjunct <- function(data, k = 10){
  # we want to have 10 lists with 2k of labels in each - easy manual work
  
  list_ids <- list( 1:2000, 2001:4000, 4001:6000, 6001:8000, 8001:10000, 10001:12000, 12001:14000, 14001:16000, 16001:18000, 18001:20000 )
  return( list_ids )
}




generate_cross_validation_mlp <- function(data, nn_sizes_check, csv_name, do_all_in){
  # Return the dataframe with information from MLP
  # each person has 200 ciphers per digit - resulting in 2000 rows in total, so disjunct should take those 2000 rows
  library(dplyr)
  library(gmodels)
  library(class)
  library(caret)
  library(swirl)
  library(ggplot2)
  library(spatstat)

  library(rpart)
  library(rpart.plot)
  library(stats)
  library(randomForest)
  library(caret)
  library(kernlab)
  library(RSNNS)
  library(neuralnet)
  
  
  print(paste("data in dimensions = ", dim(data)))
  
  if (do_all_in == TRUE){
    csv_name <- paste(csv_name,"_allin")
    print("data all in cross validation")
  }
  else {
    csv_name <- paste(csv_name,"_disjunct")
    print("data disjunct cross validation")
  }
  
  
  
  cv.error <- NULL
  k <- 10
  library(plyr) 
  pbar <- create_progress_bar('text')
  pbar$init(k)
  
  # dataframe saving information about all the trained and evaluated models with CV
  df_mlp_full_information <- data.frame()
  
  for (nn_size in nn_sizes_check){
    
    # get the correct architecture of network
    layer_1 <- nn_size[1]
    layer_2 <- nn_size[2]
    layer_3 <- nn_size[3]
    # print(nn_size)
    layers_now <- c(layer_1, layer_2, layer_3)[!is.na(c(layer_1, layer_2, layer_3))]
    
    if (is.na(layer_1)){
      layer_1 = 0
    }
    if (is.na(layer_2)){
      layer_2 = 0
    }
    if (is.na(layer_3)){
      layer_3 = 0
    }
    # print(layer_1)
    # print(layer_2)
    # print(layer_3)
    
    print(paste("layers_now", layers_now))
    
    # do the cross validation
    if (do_all_in == TRUE){
      # ALL IN - shuffle the dataset  
      data_shuffled <- data[sample(nrow(data)),]
      folds <- createFolds(data_shuffled[,1], k = 10)
    }
    # DISJUNCT - dont shuffle the data yet
    else {
      folds <- create_folds_disjunct(data, k = 10)
    }
    # save this information
    nn_size_l1 <- c()
    nn_size_l2 <- c()
    nn_size_l3 <- c()
    accuracy_test_list <- c()
    accuracy_training_list <- c()
    time_training_list <- c()
    time_eval_list <- c()
    
    for(i in 1:10){
      
      # CV splitting from Zouchi knn
      if (do_all_in == TRUE){
        # folds have already shuffeled data
        train.cv <- data_shuffled[-folds[[i]], ]
        test.cv <- data_shuffled[folds[[i]], ]
      }
      else {
        # DISJUNCT - shuffle separately
        train.cv <- data[-folds[[i]], ]
        train.cv <- train.cv[sample(nrow(train.cv)),]
        
        # SHUFFLE TEST DATASET
        test.cv <- data[folds[[i]], ]
        test.cv <- test.cv[sample(nrow(test.cv)),]
      }
      
      print(paste("test.cv dimensions = ", dim(test.cv)))
      print(paste("train.cv dimensions = ", dim(train.cv)))
      
      # train the MLP and save the time
      start_time <- Sys.time()
      training_class <- get_training_class(training_data = train.cv)
      nn <- train_mlp(training_data=train.cv, training_classes=training_class, size=layers_now)
      time_training_list[i] <- difftime(Sys.time(), start_time, units = "secs")
      
      # evaluate MLP and save the time and accuracy
      start_time <- Sys.time()
      accuracy_test_list[i] <- evaluate_nn(nn, test.cv)
      time_eval_list[i] <- difftime(Sys.time(), start_time, units = "secs")
      accuracy_training_list[i] <- evaluate_nn(nn, train.cv)
      
      # save the current architecture
      nn_size_l1[i] <- layer_1
      nn_size_l2[i] <- layer_2
      nn_size_l3[i] <- layer_3
      
      pbar$step()
    }
    
    # save results to DF of 1 mlp configuration
    df_mlp_this_architecture <-data.frame(list(
      'nn_size_l1' = nn_size_l1, 
      'nn_size_l2' = nn_size_l2, 
      'nn_size_l3' = nn_size_l3, 
      'accuraccy_test' = accuracy_test_list, 
      'accuracy_training' = accuracy_training_list,
      'time_training' = time_training_list,
      'time_eval' = time_eval_list
    ))
    
    # add current architecture information to full df
    df_mlp_full_information <- rbind(df_mlp_full_information, df_mlp_this_architecture)
    write.csv(df_mlp_full_information, paste("nn_results/df_mlp_full_information_", csv_name, ".csv"), row.names = FALSE)
    
  }
  
  
  return(df_mlp_full_information)
}

get_training_test_data_disjunct <- function(data){
  # get half of people as training and half as test
  id_train <- data[1:(nrow(data)/2), ]
  id_test <- data[((nrow(data)/2)+1):nrow(data), ]
  
  # shuffle the datasets after splitting
  set.seed(423)
  id_train_shuffled <- id_train[sample(nrow(id_train)),]
  id_test_shuffled <- id_test[sample(nrow(id_test)),]
  
  return(list(id_train_shuffled, id_test_shuffled))
}




get_training_test_data_allin <- function(data, training_percent){
  # shuffle the datasets before splitting
  set.seed(423)
  data_shuffled <- data[sample(nrow(data)),]
  
  # get half of people as training and half as test
  last_training_row = floor(training_percent / 100 * nrow(data_shuffled))
  # train_data_limit <- floor(nrow(id_dataset)/2)
  
  id_train_shuffled <- data_shuffled[1:(last_training_row), ]
  id_test_shuffled <- data_shuffled[(last_training_row+1):nrow(data_shuffled), ]
  
  
  return(list(id_train_shuffled, id_test_shuffled))
}

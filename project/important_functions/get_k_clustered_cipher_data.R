get_k_clustered_cipher_data <- function(data, n_cluster){
  # for each digit 0-9 find all the rows and cluster them
  # what we get is the dataset with reduced amount of rows
  
  data <- id
  id_train_labels <- data[,1]
  cipher_cluster <- c()
  label_cluster <- c()
  for( i in 0:9) {
    data_cipher_i <- data[ id_train_labels == i, ]
    clusterData <- kmeans(data_cipher_i, n_cluster)
    cipher_cluster[[i + 1]] <- clusterData$centers
    label_cluster[[i + 1]] <- replicate(n_cluster, i)
  }
  
  train_lab <- factor(unlist(label_cluster))
  train_dat <- do.call(rbind, cipher_cluster)
  
  df = as.data.frame(train_dat)
  df <- df %>% select(V1, everything())
  return(df)
}
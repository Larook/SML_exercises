# get the maximum needed number for getting the searched variance 
get_pcs_for_accum_variance <-function(data, searched_variance){
  pca <- prcomp(data[,(2:ncol(data))], center = TRUE,scale. = TRUE)
  pca_sum <- summary(pca)
  
  i <- 1 # keep track of the principle component number
  cumul_prop = pca_sum$importance[3, i] # get cumulative proportion of current PC
  #print(paste0("searched_accum_var=",searched_accum_var))
  #print(paste0("i=",i, " Cumulative Proportion=",cumul_prop))
  while(cumul_prop*100 <  searched_variance){
    i <- i + 1
    cumul_prop = pca_sum$importance[3, i] # get cumulative proportion of current PC
    #print(paste0("i=",i, " Cumulative Proportion=",cumul_prop))
  }
  print(paste0("To get the ", searched_variance, "% variance, wee need first ", i, " principal components"))
  return(i)
}



get_reconstructed_pca <- function(dataset){
  # get reconstruction of pca
  restr <- dataset$x %*% t(dataset$rotation)
  # unscale and uncenter the data
  if(dataset$scale != FALSE){
    restr <- scale(restr, center = FALSE , scale=1/dataset$scale)
  }
  if(all(dataset$center != FALSE)){
    restr <- scale(restr, center = -1 * dataset$center, scale=FALSE)
  }
  restr
}




get_pca_obj <- function(data){
  data_pca <- prcomp(data[,(2:ncol(data))], center = TRUE, scale. = TRUE)
  return(data_pca)
}

show_pca_reconstruction <- function(pca_obj, cipher){
  # wanted_pcs <- prcomp(dataset[,(2:ncol(dataset))], center = TRUE,scale. = TRUE, rank. = limit_pca_rank)
  # print(wanted_pcs)
  print(paste0("function returns data with dimensions", dim(pca_obj)))
  
  restr <- get_reconstructed_pca(pca_obj)
  
  #cipher_index <- 3*400 + 3 # get any index of a cipher - ideally show all 10 digits
  # find indexes of digits and print them
  #for (digit in c(0,9)){
  #  cipher_index <- 1
  #  while (dataset[cipher_index, ]$V1 != digit){
  #    cipher_index <- cipher_index + 1
  #  } 
  #  image(get_img_rot(restr, cipher_index), col = gray(0:100/100) )
  #}
  image(get_img_rot(restr, cipher), col = gray(0:100/100) )
}


get_PCA_reduced_data <- function(labels, pca_obj, searched_accum_var){
  # # how to get the components that give us the % of variance?
  # limit_pca_rank <- get_pcs_for_accum_variance(data, searched_accum_var)
  # 
  # # get pcs
  # wanted_pcs <- prcomp(data[,(2:ncol(data))], center = TRUE,scale. = TRUE, rank. = limit_pca_rank)
  # 
  # # reduced_data <- as.data.frame(wanted_pcs$x[data[,1]])
  # reduced_data <- as.data.frame(wanted_pcs$x)
  #   # test_df <- 
  #   # train_df <- as.data.frame(wanted_pcs$x[train_rows_id,])
  # id_labels <- data[,1]
  # reduced_data$V1 <- id_labels
  # reduced_data <- reduced_data %>% select(V1, everything())

  
  
  
  
  # data_pca <- prcomp(data[,(2:ncol(data))], center = TRUE, scale. = TRUE)
  eigs <- pca_obj$sdev^2
  Proportion = eigs/sum(eigs)
  
  reduced_data <- pca_obj$x[,cumsum(Proportion) < searched_accum_var/100] 
  
  # id_labels <- labels
  reduced_data <- as.data.frame(reduced_data)
  reduced_data$V1 <- labels
  reduced_data <- reduced_data %>% select(V1, everything())
  
  
  #----------------------
  
  # # wanted_pcs <- prcomp(dataset[,(2:ncol(dataset))], center = TRUE,scale. = TRUE, rank. = limit_pca_rank)
  # # print(wanted_pcs)
  # print(paste0("function returns data with dimensions", dim(data_pca)))
  # 
  # restr <- get_reconstructed_pca(data_pca)
  # 
  # #cipher_index <- 3*400 + 3 # get any index of a cipher - ideally show all 10 digits
  # # find indexes of digits and print them
  # #for (digit in c(0,9)){
  # #  cipher_index <- 1
  # #  while (dataset[cipher_index, ]$V1 != digit){
  # #    cipher_index <- cipher_index + 1
  # #  } 
  # #  image(get_img_rot(restr, cipher_index), col = gray(0:100/100) )
  # #}
  # image(get_img_rot(restr, 3), col = gray(0:100/100) )
  
  
  return(reduced_data)
  
}
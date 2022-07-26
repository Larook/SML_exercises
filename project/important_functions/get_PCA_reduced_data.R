

get_pca_obj <- function(data){
  # returns the object of pca - made with max size of the pcs
  data_pca <- prcomp(data[,(2:ncol(data))], center = TRUE, scale. = TRUE)
  return(data_pca)
}


show_pca_reconstruction <- function(pca_obj, cipher, n_pcs){
  # wanted_pcs <- prcomp(dataset[,(2:ncol(dataset))], center = TRUE,scale. = TRUE, rank. = limit_pca_rank)
  # print(wanted_pcs)
  print(paste0("function returns data with dimensions", dim(pca_obj)))
  
  restr <- get_reconstructed_pca(pca_obj, n_pcs)
  print(restr)
  
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
  # how to get the components that give us the % of variance? Proportion from Zouchi
  library(dplyr)
  
  eigs <- pca_obj$sdev^2
  Proportion = eigs/sum(eigs)
  
  reduced_data <- pca_obj$x[,cumsum(Proportion) < searched_accum_var/100] 
  
  # id_labels <- labels
  reduced_data <- as.data.frame(reduced_data)
  reduced_data$V1 <- labels
  reduced_data <- reduced_data %>% select(V1, everything())
  
  return(reduced_data)
  
}



get_reconstructed_pca <- function(dataset, n_pcs){
  library(dplyr)
  
  # get reconstruction of pca
  restr <- dataset$x[1:n_pcs] %*% t(dataset$rotation[1:n_pcs])
  # unscale and uncenter the data
  if(dataset$scale[1:n_pcs] != FALSE){
    restr <- scale(restr, center = FALSE , scale=1/dataset$scale[1:n_pcs])
  }
  if(all(dataset$center[1:n_pcs] != FALSE)){
    restr <- scale(restr, center = -1 * dataset$center[1:n_pcs], scale=FALSE)
  }
  return(restr)
}


# get the maximum needed number for getting the searched variance - our 
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

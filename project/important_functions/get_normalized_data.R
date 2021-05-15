get_normalized_data <- function(data){
  # normalize the image, but save the position of the label!
  # tmp_normalized_dataset <- get_normalized_data(id)
  
  library(dplyr)
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  id_sn <- as.data.frame(lapply(data[-1], normalize))
  
  id_labels <- data[,1]
  
  
  id_sn$V1 <- id_labels
  # id_sn[moveme(names(id_sn), "V1 first")]
  id_sn <- id_sn %>% select(V1, everything())
  return(id_sn)
}

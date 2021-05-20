smoothImage <- function(grayImg, sigma){
  smoothed <- as.matrix(blur(as.im(grayImg), sigma = sigma, normalise=FALSE, bleed = TRUE, varcov=NULL))
  return(smoothed)
}

rotate <- function(x) t(apply(x, 2, rev))

get_gaussian_smoothed_data <- function(dataset, smooth_sigma){

  id_mat <- data.matrix(dataset, rownames.force = NA)
  imageSize <- sqrt(ncol(id_mat) -1)
  
  for(i in 1:nrow(id_mat)){
    rotated <- c(id_mat[i,2:ncol(id)])
    image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
    image <- smoothImage(grayImg=image, sigma=smooth_sigma)
    id_mat[i,2:ncol(id_mat)] <-matrix(image,nrow = 1,ncol = ncol(id_mat) -1, byrow = FALSE)
  }
  
  data_smoothed <- as.data.frame(id_mat)
  data_smoothed[,1] <- factor(dataset[,1])
  return(data_smoothed)
  }

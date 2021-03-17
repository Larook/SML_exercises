library(spatstat)

id_mat <- data.matrix(id, rownames.force = NA)
imageSize <- sqrt(ncol(id_mat) -1)
rotate <- function(x) t(apply(x, 2, rev))

smoothImage <- function(grayImg, sigma){
  smoothed <- as.matrix(blur(as.im(grayImg), sigma = sigma, normalise=FALSE, bleed = TRUE, varcov=NULL))
  return(smoothed)
}
  
#smoothedImages <- smoothImage(id_mat[,c(2:ncol(id_mat))], 0.9)

for(i in 1:nrow(id_mat)){
  rotated <- c(id_mat[i,2:ncol(id)])
  image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <- smoothImage(image,0.9)
  id_mat[i,2:ncol(id_mat)] <-matrix(image,nrow = 1,ncol = ncol(id_mat) -1, byrow = FALSE)
}

id <- as.data.frame(id_mat)
id[,1] <- factor(id[,1])


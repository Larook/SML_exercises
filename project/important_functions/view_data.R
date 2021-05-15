
#id <- do.call(rbind, idList[1:13])
# id <- idList[[5]]

get_img_rot <- function(dataset, id_no){
  # modified code that we got in exercise, put only dataset and id of number
  id_mat <- data.matrix(dataset, rownames.force = NA)
  rotate <- function(x) t(apply(x, 2, rev))
  
  # wants to print only 0-9 digits
  #rotated <- c(id_mat[-400+id_no*400+1, 2:ncol(id_mat)])
  
  # prints digit from id
  rotated <- c(id_mat[id_no, 2:ncol(id_mat)])
  
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated,nrow = 18,ncol = 18, byrow = FALSE)
  image <- rotate(image)
}

view_data <- function(data){

  return( image(get_img_rot(data, cipherNumber), col = gray(0:100/100) ) )
  
  
  
# Zouchi's code not working
# data <- id
# 
#   for (num in 0:9) {
#     img <- matrix(,180,360)
#     idx <- 0
#     for (x in 1:10){
#       for (y in 1:20) {
#         idx <- idx+1
#         tmpM <- matrix(data[(num*200+idx),2:325],18,18)
#         for (xM in 1:18) {
#           for (yM in 1:18) {
#             img[(x-1)*18+xM, (y-1)*18+yM] <- tmpM[xM,yM]
#           }
#           
#         }
#         
#       }
#       
#     }
#     
#     image(img,col=gray(0:100/100) )
#   }
}






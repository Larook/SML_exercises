
#id <- do.call(rbind, idList[1:13])
# id <- idList[[5]]

vied_data <- function(data){
  for (num in 0:9) {
    img <- matrix(,180,360)
    idx <- 0
    for (x in 1:10){
      for (y in 1:20) {
        idx <- idx+1
        tmpM <- matrix(data[(num*200+idx),2:325],18,18)
        for (xM in 1:18) {
          for (yM in 1:18) {
            img[(x-1)*18+xM, (y-1)*18+yM] <- tmpM[xM,yM]
          }
          
        }
        
      }
      
    }
    
    image(img,col=gray(0:100/100) )
  }
}



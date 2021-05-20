load_data_id <- function(load_full){
  # loads the final report dataset
  load("data/idList-FinalExam.Rdata") 
  
  if(load_full) {
    id <- do.call(rbind, idList[1:38]) # all the people
  } else {
    id <- do.call(rbind, idList[1:4]) # 4 persons
  }
  
  id <- as.data.frame(id)
  id[,1] <- factor(id[,1])
  
  return(id)
}

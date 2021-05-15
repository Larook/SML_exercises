load_data_id <- function(){
  # loads the final report dataset
  load("data/idList-FinalExam.Rdata") 
  
  id <- do.call(rbind, idList[1:38]) # 5 person
  id <- as.data.frame(id)
  id[,1] <- factor(id[,1])
  
  return(id)
}

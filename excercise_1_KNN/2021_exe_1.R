########################################################################## Exercise 1.4.1 ######################################################################################

library(class)
library(caret)

load("id100.Rda")

set.seed(423)
id_shuffle <- id[sample(nrow(id)),]

id_train <- id_shuffle[1:2000,-1] 
id_test <- id_shuffle[2001:4000,-1]

id_train_labels <- id_shuffle[1:2000,1]
id_test_labels <- id_shuffle[2001:4000,1]


ptm <- proc.time();

id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=3)

time <- proc.time() - ptm

cf <- confusionMatrix(id_test_labels, id_test_pred)
cf$table

sum(diag(cf$table))/sum(cf$table)
print( time )

####################################################################################### Exercise 1.4.2 ######################################################################################

listOfFolders <- c(1:14)
listOfTime <- c(1:14)
listOfKmes <- c(1:14)


kList <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)

for(i in 1:14)
{
  set.seed(4213)
  id_shuffle <- id[sample(nrow(id)),]
  
  id_train <- id_shuffle[1:2000,-1]
  id_test <- id_shuffle[2001:4000,-1]
  
  id_train_labels <- id_shuffle[1:2000,1]
  id_test_labels <- id_shuffle[2001:4000,1]
  ptm <- proc.time();
  id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=kList[i])
  time <- proc.time() - ptm
  
  cf <- confusionMatrix(id_test_labels, id_test_pred)
  
  listOfFolders[i] <- sum(diag(cf$table))/sum(cf$table)
  
  listOfTime[i] <- time
  listOfKmes[i] <- kList[i]
}

print(listOfFolders)
print(listOfTime)
print(listOfKmes)

plot(listOfFolders, type="o", col="blue", ylab="Acc")
par(new = T)
plot(listOfTime, type="o", col="blue", pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2)
axis(side = 4)
mtext(side = 4, line = 3, 'Time')

####################################################################################### Exercise 1.4.3 ####################################################################################### 

load("id100.Rda")

listOfFolders <- c(1:10)

set.seed(423)

folds <- createFolds(id[,1], k = 10)

for(i in 1:10)
{

  id_train <- id[-folds[[i]],-1]
  id_test <- id[folds[[i]],-1]
  
  id_train_labels <- id[-folds[[i]],1]
  id_test_labels <- id[folds[[i]],1]
  
  id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=7)
  
  cf <- confusionMatrix(id_test_labels, id_test_pred)
  listOfFolders[i] <- sum(diag(cf$table))/sum(cf$table)
  
}

print(listOfFolders)
mean(listOfFolders)
var(listOfFolders)

boxplot(listOfFolders,data=listOfFolders, main="10 Cross Test", xlab="", ylab="Recognition") 

################################################################################### Exercise 1.4.4 + 1.4.5 #######################################################################################

load("idList-co-100.Rdata")

# All persons in, SHUFFLE

id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])
# round(prop.table(table(id[,1])) * 100, digits = 1) 

id_shuffle <- id[sample(nrow(id)),]

id_train <- id_shuffle[1:20000,2:325]
id_test <- id_shuffle[20001:40000,2:325]

id_train_labels <- id_shuffle[1:20000,1]
id_test_labels <- id_shuffle[20001:40000,1]

id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=7)

cf <- confusionMatrix(id_test_labels, id_test_pred)
sum(diag(cf$table))/sum(cf$table)

# Disjunct , DONT SHUFFLE

id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

id_train <- id[1:20000,2:325]
id_test <- id[20001:40000,2:325]

id_train_labels <- id[1:20000,1]
id_test_labels <- id[20001:40000,1]

id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=7)

cf <- confusionMatrix(id_test_labels, id_test_pred)
sum(diag(cf$table))/sum(cf$table)


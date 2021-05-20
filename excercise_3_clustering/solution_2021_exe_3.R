library(class)
library(caret)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

################################################################# Exercise 3.1.1 #################################################################

load("../data/idList-co-100.Rdata")
# load("idList-cornered-100-2021.Rdata")

id <- do.call(rbind, idList[1:2]) # 2 person 
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

id_sn <- as.data.frame(lapply(id[-1], normalize))

id_train_labels <- id[1:(nrow(id)/2),1]
id_test_labels <- id[(nrow(id)/2 + 1):nrow(id),1]

id_train <- id_sn[1:(nrow(id)/2), ]
id_test <- id_sn[((nrow(id)/2)+1):nrow(id), ]

set.seed(2345)

cipher_cluster <- c()
label_cluster <- c()

accuracy_v <- c()
n_clusters <- c(1,100)#,200)#,400,600,1000,1300,1600,2000)

for (n_cluster in n_clusters){
  
# n_cluster <- 100 # you can change the number of clusters

  for( i in 0:9) {
    clusterData <- kmeans(id_train[ id_train_labels == i, ], n_cluster)
    cipher_cluster[[i + 1]] <- clusterData$centers
    label_cluster[[i + 1]] <- replicate(n_cluster, i)
  }
  
  train_lab <- factor(unlist(label_cluster))
  train_dat <- do.call(rbind, cipher_cluster)
  
  ptm <- proc.time();
  id_test_pred <- knn(train = train_dat, test = id_test, cl = train_lab, k=5)
  time <- proc.time() - ptm
  cf <- confusionMatrix(id_test_labels, id_test_pred)
  print( time )
  accuracy <- print( sum(diag(cf$table))/sum(cf$table) )
  accuracy_v <- c(accuracy_v, accuracy)
}

# 
# 
# ################################################################# Exercise 3.1.2 #################################################################
# ## The results of cross validation in terms of raw data has been done in the previous exercise
# 
# ## 10 folds cross validation
# set.seed(123)
# folds <- createFolds(id[,1], k = 10)
# listOfFolders <- c(1:10)
# 
# ptm <- proc.time();  
# for(i in 1:10)
# {
#   id_train <- id[-folds[[i]],-1]
#   id_test <- id[folds[[i]],-1]
#   
#   id_train_labels <- id[-folds[[i]],1]
#   id_test_labels <- id[folds[[i]],1]
#   
#   # apply K-Menas before KNN
#   n_cluster <- 50 # you can change the number of clusters
#   
#   for( j in 0:9) {
#     clusterData <- kmeans(id_train[ id_train_labels == j, ], n_cluster)
#     cipher_cluster[[j + 1]] <- clusterData$centers
#     label_cluster[[j + 1]] <- replicate(n_cluster, j)
#   }
#   
#   train_lab <- factor(unlist(label_cluster))
#   train_dat <- do.call(rbind, cipher_cluster)
#   
#   id_test_pred <- knn(train = train_dat, test = id_test, cl = train_lab, k=7)
#   
#   cf <- confusionMatrix(id_test_labels, id_test_pred)
#   listOfFolders[i] <- sum(diag(cf$table))/sum(cf$table)
# }
# 
# time <- proc.time() - ptm
# print( time )
# print(listOfFolders)
# mean(listOfFolders)
# var(listOfFolders)
# 
# 
# ################################################################# Exercise 3.1.3 #################################################################
# 
# load("idList-co-100.Rdata")
# id <- do.call(rbind, idList[1:10])
# load("idList-cornered-100-2021.Rdata")
# idnew <- do.call(rbind, idList[1:13])
# id <- rbind(id, idnew)
# id <- as.data.frame(id)
# id[,1] <- factor(id[,1])
# 
# # Then you can repeat the above processing. 
# 
# 
# ################################################################# Exercise 3.2.1 #################################################################
# 
# load("idList-co-100.Rdata")
# 
# id <- do.call(rbind, idList[2])
# id <- as.data.frame(id)
# id[,1] <- factor(id[,1])
# 
# cipher_cluster <- c()
# label_cluster <- c()
# 
# for( i in 1:10) {
#   cipher_cluster[[i]] <- id[ (i-1)*400 + 1:5, -1 ]
#   label_cluster[[i]] <- replicate( n_cluster, i-1 )
# }
# 
# train_lab <- factor(unlist(label_cluster))
# train_dat <- do.call(rbind,cipher_cluster)
# 
# hc.complete = hclust ( dist ( train_dat ) , method ="complete")
# hc.average = hclust ( dist ( train_dat ) , method ="average")
# hc.single = hclust ( dist ( train_dat ) , method ="single")
# 
# plot ( hc.average , main ="5 samples each" , xlab ="" , sub ="" ,  cex =.9, labels = train_lab)
# 
# 
# ################################################################# Exercise 3.2.2 #################################################################
# 
# id_sn <- as.data.frame(lapply(id[-1], normalize))
# 
# id_train_labels <- id[1:(nrow(id)/2),1]
# id_test_labels <- id[(nrow(id)/2 + 1):nrow(id),1]
# 
# id_train <- id_sn[1:(nrow(id)/2), ]
# id_test <- id_sn[((nrow(id)/2)+1):nrow(id), ]
# 
# cipher_cluster <- c()
# label_cluster <- c()
# 
# for( i in 0:9) {
#   clusterData <- kmeans(id_train[ id_train_labels == i, ], 5)
#   cipher_cluster[[i + 1]] <- clusterData$centers
#   label_cluster[[i + 1]] <- replicate(5, i)
# }
# 
# train_lab <- factor(unlist(label_cluster))
# train_dat <- do.call(rbind,cipher_cluster)
# 
# hc.complete = hclust ( dist ( train_dat ) , method ="complete")
# hc.average = hclust ( dist ( train_dat ) , method ="average")
# hc.single = hclust ( dist ( train_dat ) , method ="single")
# 
# plot ( hc.average , main =" Complete Linkage K-means first" , xlab ="" , sub ="" ,  cex =.9, labels = train_lab)
# 
# 
# 
# ################################################################# Exercise 3.3 #################################################################
# 
# ## Exercise 3.3.1
# 
# ## function to calculate true positive and false positive
# truP_falP <- function(x, y) {
#   falP <- 0
#   truP <- 0
#   for(i in 1:length(x))
#   {
#     if( !is.na(x[i]) ) {
#       if( x[i] == y[i] )
#       { truP <- truP + 1; }
#       else
#       { falP <- falP + 1; }
#     }
#   }
#   res <- c(truP, falP)
#   return(res)
# }
# 
# set.seed(423)
# id_shuffle <- id[sample(nrow(id)),]
# id_sn <- as.data.frame(lapply(id_shuffle[-1], normalize))
# 
# id_train <- id_sn[1:4000,]
# id_test <- id_sn[4001:8000,]
# 
# id_train_labels <- id_shuffle[1:4000,1]
# id_test_labels <- id_shuffle[4001:8000,1]
# 
# maxF1 <- list(0:6)
# maxF1x <- list(0:6*2 + 1)
# 
# plot(NULL,NULL, xlim=c(0.75, 1), ylim=c(0.75, 1), xlab="Rec", ylab="Pre",main="all" )
# 
# for(j in 0:6) {
#   
#   k <- j*2 + 1
#   
#   trupL <- c(1:k)
#   falpL <- c(1:k)
#   
#   for(i in 1:k)
#   {
#     id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=k, l=i)
#     cf <- confusionMatrix(id_test_labels, id_test_pred)
#     print( sum(diag(cf$table))/sum(cf$table) )
#     
#     res = truP_falP(id_test_pred,id_test_labels)
#     
#     trupL[i] <- res[1]
#     falpL[i] <- res[2]
#   }
#   
#   tab <- table(id_test_pred,id_test_labels)
#   rec <-trupL/length(id_test_pred)
#   pre <- trupL/(trupL + falpL)
#   
#   f1 <- 2*(rec*pre)/(rec+pre) 
#   maxF1[j + 1] <- max(f1)
#   
#   lines( trupL/length(id_test_pred), trupL/(trupL + falpL),"b", pch=k)
# }  
# 
# 
# ## Exercise 3.3.2
# 
# plot(unlist(maxF1x), unlist(maxF1), ylim=c(0.4, 1), "l", xlab="k", ylab="maxF1",main="max F1" )

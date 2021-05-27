knitr::opts_chunk$set(echo = TRUE)
library("kernlab")
library(readr)
library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(plotly)


# # LOAD THE DATA 
# load("idList-cornered-100-2021.Rdata")
# 
# 
# # NORMALIZE FUNCTION 
# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# # DATA SPLITTING INTO TRAIN / TEST PARTS
# id <- do.call(rbind, idList[1:2])
# id <- as.data.frame(id)
# id[,1] <- factor(id[,1])
# id_sn <- as.data.frame(lapply(id[-1], normalize))
# ## 50/50 split of dataset
# smp_size <- floor(0.5 * nrow(id))
# ## set the seed to make your partition reproducible
# set.seed(123)
# train_ind <- sample(seq_len(nrow(id)), size = smp_size)
# train_data<- id[train_ind, ]
# test <- id[-train_ind, ]


source("important_functions/load_data_id.R")
source("important_functions/get_training_test_data.R")

source("important_functions/get_normalized_data.R")
source("important_functions/get_gaussian_smoothed_data.R")
source("important_functions/view_data.R")

source("important_functions/get_k_clustered_cipher_data.R") 
source("important_functions/get_PCA_reduced_data.R") 


# load dataset all_in
id <- load_data_id(load_full=FALSE)

# preprocessing options
# id_smooth <- get_gaussian_smoothed_data(dataset=id, smooth_sigma=0.05)
id_norm <- get_normalized_data(id)

# which of the preprocessing to use
# id_use <- id_smooth
id_use <- id_norm
# id_use <- id

# split to train and test dataset
data_train_test_allin <- get_training_test_data_allin(data=id_use, training_percent=50)
train_data_allin <- data_train_test_allin[[1]]
test_data_allin <- data_train_test_allin[[2]]


# splitting data disjunct
data_train_test_disjunct <- get_training_test_data_disjunct(data=id_use)
train_data_disjunct <- data_train_test_disjunct[[1]]
test_data_disjunct <- data_train_test_disjunct[[2]]


# decide if doing the all_in or disjunct
# id_train <-train_data_allin
# id_test <- test_data_allin

id_train <-train_data_disjunct
id_test <- test_data_disjunct



# ----------- map from data obtainin to used variables ----------------
train_data<- id_train
test <- id_test



# ...................... TRY DIFFERENT C VALUES ................................................

#classifier_rbf <-ksvm(V1~ ., data = train, kernel = "vanilladot", C = 1)
#classifier_rbf <-ksvm(V1~., data = train, kernel = "rbfdot", kpar=list(sigma=0.05), C = 1)
classifier_rbf <-ksvm(V1~ ., data = train_data, kernel = "polydot", C = 5)
datasetTest <- predict(classifier_rbf,test)
test_confusion <- confusionMatrix(datasetTest, test$V1)




#.......................... CROSS VALIDATION ............................................................
Model_linear <- ksvm(V1~ ., data = train_data, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)
test_confusion <- confusionMatrix(Eval_linear, test$V1)
paste("confusionMatrix(datasetTest,test$V1) = ", test_confusion$overall[1])
# Accuracy    :  0.972
#______________We use train_datafunction from caret package to perform crossvalidation______________#
trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"
set.seed(100)
Model_linear



# RADIAL KERNEL 
#grid <- expand.grid(C=c(0.01, 0.1, 0.2, 0.3, 0.4), sigma = 0.1)
#fit.svm <- train(V1~ ., data=train_data, method="svmRadial", metric=metric,
#                   tuneGrid=grid, trControl=trainControl)



#LINEAR KERNEL
# grid <- expand.grid(C=c(1, 10, 100, 500, 1000))
# Performing 5-fold cross validation
#fit.svm <- train(V1~ ., data=train_data, method="svmLinear", metric=metric,
#                 tuneGrid=grid, trControl=trainControl)



# ............................... BEST ONE TO USE FOR DIGITS ..............................
# POLYNOMIAL KERNEL
grid <- expand.grid(C=c(0.01, 0.1, 0.2, 0.3, 0.4), degree=2, scale=0.5)
fit.svm <- train(V1~ ., data=train_data, method="svmPoly", metric=metric,
                 tuneGrid=grid, trControl=trainControl)



# Printing cross validation result
print(fit.svm, zero.print = ".")
# Best tune at C = 0.1,
# Accuracy = 0.9725012
# Plotting "fit.svm" results
plot(fit.svm, ylim = c(0.97, 1.05))
# plot(fit.svm)




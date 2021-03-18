#3.2

#loading libraries and dataset
library(data.table) # working with ranges 
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
load('id100.Rda') # dataset


#3.2.1

#creating helping vector and ranges for filtering dataset
library(data.table)
values <- data.table(value = c(1:4000))
range <-  data.table(start = c(1, 401, 801, 1201, 1601, 2001, 2401, 2801, 3201, 3601), end = c(5, 405, 805, 1205, 1605, 2005, 2405, 2805, 3205, 3605))
values[range, on = .(value >= start, value <= end), .(results = x.value)]


results <- data.table(NULL)

for (i in 1:NROW(range)){ 
  results <- rbind(results, 
                   data.table(result = values[value >= range[i, start] & 
                                                value <= range[i, end], value]))}

#creating new dataset with 5 instances for each number
for (i in results){
  new_ds <- id[i,(1:ncol(id))]
}


# prepare hierarchical cluster
hc = hclust(dist(new_ds))
# simple dendrogram plot
plot(hc)


#3.2.2
# CREATING NEW DATASET AND REMOVING FIRST COLUMN
df <- new_ds[,-1]
rownames(df) <- df[,1]
#distance <- get_dist(df)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# K-MEANS CLUSTERING - CREATING 5 CLUSTERS
k <- kmeans(df, centers = 5, nstart = 25)
#VIZUALIZATION OF CLUSTERS
fviz_cluster(k, data = df)



# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)









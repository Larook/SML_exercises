# Exercise 3 - Clustering

Zouchi's advices


## advices for 3.1

get data from 2 people - disjunct

for each cipher do k-means for training set

we then get new training set with the number of cluster centroids

do the training of the k-NN using the centroids (new data)

function kmeans()

x - data

nstart - how many times does it run?

```
 i <- 8
 
 data <- id_train[ id_train_labels == i, ]
 set.seed(2345)
 clusterData <- kmeans(data, 100)  # use 100 clusters
```

ClusterData - each row represent the center of the claster

### basically in pdf we have a snippet of code which divides to cluster labels and cluster data


3.1.2. Do this with cross validation to measure accuracy and execution time.

3.1.3. K-means on each cipher individually with disjunct. Compare performance with k-means and without k-means. Try different cluster size (second argument) ~ Norbert says that 100 should be good, maybe more will not be better. It should be faster with nearly same accuracy

## advices for 3.2

use function hclust(dist(train_dat), method="complete")

other methods = "average" or "single

Cross validation tables - gmodels:CrossTable() - how many data with classify correct information and compare it to hierarchy.
can plot table as:

https://prnt.sc/10i0cg2  or just summaraize the table with words


## advices for 3.3

3.3.1 precision-recall curves

3.3.2 F1 score is calculated from precision and recall - find in slides

3.3.3 discuss

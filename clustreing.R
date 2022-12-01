library(dendextend)
library(cluster)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(pheatmap)


customers = read.csv("C:/r_files/segmentation_data.csv")


a = max(customers$Age)
b = min(customers$Age)
c = max(customers$Education)
d = min(customers$Education)
e = max(customers$Income)
f = min(customers$Income)


table(is.na(customers))
set.seed(12)

View(customers)

customers = customers[-c(1)]
  
customers = customers[3:5]



normalize = function(x) { # we have to normalize data too
  return ((x - min(x)) / (max(x) - min(x))) 
}


inds = c() # column indices 
for (i in 1:length(customers)){
  print(i)
  inds = append(inds, i)
}

back = function(num, maximum, minimum){ # with this we can restore normalized values, if needed for intepretation
  down = maximum - minimum
  down = down * num
  return (down+minimum)
}


customers = as.data.frame(lapply(customers[,inds], normalize))

summary(customers)

View(customers)

vec = vector()
for (i in 1:10){
  vec[i] = sum(kmeans(customers ,i)$withinss)
}



plot(1:10, vec, type = "b", main = paste("clusters of clients"), xlab = "Number of clients", ylab = "vector of sums") 


kmeans_algo = kmeans(customers, 4,iter.max = 100, nstart = 4) # 3 or 4

kmeans_algo$cluster
kmeans_algo$centers
kmeans_algo$size



clusplot(customers, kmeans_algo$cluster, # ?
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'))

cluster = c(1: 4)
center_df = data.frame(cluster, kmeans_algo$centers)
center_reshape <- gather(center_df, features, values, Age:Income)

hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) + # heap
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()

#back(0.3, e, f) example of how we can convert normalized data to original

#######, 4 clusters from k means

#### hierarchical

d = dist(customers, method = "euclidean")

hc1 = hclust(d, method = "complete" )

plot(hc1)


hc1_dend= as.dendrogram(hc1)


labels_colors(hc1_dend) = rainbow(4)

cutree(hc1_dend, k = 4)
hc1_dend = color_branches(hc1_dend, k = 4)
plot(hc1_dend)


#heatmap(as.matrix(customers),scale = "row")

pheatmap(as.matrix(customers), cutree_rows = 4)

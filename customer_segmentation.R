movie.data = read.csv("D:\\FDS project\\movie.csv")
head(movie.data)

set.seed(6)

(kmeans = kmeans(scale(movie.data),3))

kmeans$centers


# Analyse clusters
colMeans(movie.data[kmeans$cluster == 1, 1:5])
colMeans(movie.data[kmeans$cluster == 2, 1:5])
colMeans(movie.data[kmeans$cluster == 3, 1:5])


plot(movie.data[, 1:5], col = kmeans$cluster, main = "Average scores for 5 movie genres coloured by k-means cluster")


tot.within.ss = numeric(10)
for (i in 1:10){
  set.seed(6)
  tot.within.ss[i] = kmeans(scale(movie.data[, 1:5]), i)$tot.withinss
}

# Plot total within-groups sum of squares
plot(1:10, tot.within.ss,
     main = "Total within-group sum of squares against cluster count",
     xlab = "Cluster count", ylab = "Total within-group sum of squares", type = "b")
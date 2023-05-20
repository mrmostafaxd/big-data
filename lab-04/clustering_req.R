# 1- cleaning the workspace and setting the working directory
rm(list=ls())
setwd("D:/College/SPRING 2023/CMPN451/labs/Lab 6 - Clustering/")
list.files()
cat("\014")  # clear console (CTRL+L)
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE) # clear plots and suppress errors

# 2- Import the dataset clustering_data.csv into a data frame and plot the points.
data = read.csv("clustering_data.csv")
head(data, 20)
plot(data$X, data$Y,   xlab = "X", ylab = "Y", col="blue", pch=20)
str(data)

# 3- Perform a k-means clustering on the data with 10 clusters and 15 iterations
library(NbClust)
library(cluster)
library(HSAUR)

#set seed to avoid random initiation for every run
set.seed(1234)

# apply kmeans clustering 
km <- kmeans(data,10, 15)
km

# 4- Print the cluster centroids
km$centers
# 5- Plot data such that each point is colored according to its cluster
plot(data$X, data$Y, col=km$cluster, pch=20, xlab="X", ylab="Y")

#6- Overlay the cluster centroids on the above plot.
#   Plot them as solid filled triangles
plot(data$X, data$Y, col = km$cluster, pch = 20, xlab = "X", ylab = "Y")
points(km$centers, col = "red", pch = 17)

#8- Now, determine the best number of clusters by two different ways
wss <-list()
for (i in 1:15) wss[i] <- sum(kmeans(data, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# another way to examine the best number of clusters 
nc <- NbClust(data, min.nc=2, max.nc=15, method="kmeans")
#see the voting results 
table(nc$Best.n[1,])

# cluster = 3
# Perform a k-means clustering on the data with the best number of clusters chosen in step 3
km <- kmeans(data, 3, nstart = 15)
km

km$centers

plot(data$X, data$Y, col = km$cluster, pch = 20, xlab = "X", ylab = "Y")
plot(data$X, data$Y, col = kmeans$cluster, pch = 20, xlab = "X", ylab = "Y")
points(kmeans$centers, col = "red", pch = 17)



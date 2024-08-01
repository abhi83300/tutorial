library(ISLR)
library(factoextra)

fix(USArrests)
names(USArrests)
scale(USArrests)
help("USArrests")
set.seed(2)

data<-scale(USArrests)
km.out=kmeans(data,2,nstart=20)
km.out
plot(data, col=(km.out$cluster+1), main="K-Means Clustering
 Results with K=2", xlab="", ylab="", pch=20, cex=2)

km.out=kmeans(data,3,nstart=20)
km.out

plot(data, col=(km.out$cluster+1), main="K-Means Clustering
 Results with K=3", xlab="", ylab="", pch=20, cex=2)

km.out=kmeans(data,4,nstart=20)
km.out

plot(data, col=(km.out$cluster+1), main="K-Means Clustering
 Results with K=4", xlab="", ylab="", pch=20, cex=2)


data<-scale(USArrests)
fviz_nbclust(data,kmeans,method = "wss")
















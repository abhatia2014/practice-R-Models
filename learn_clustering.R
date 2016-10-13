#finding patterns is often unsupervised learning
# to find groups we therefore need to first create a set of distance 
#measures to compare samples and second create a rule to cluster or group samples based on these distance measures
data("mtcars")
head(mtcars)
#lets first scale the data
mtcars.sc=scale(mtcars)
head(mtcars.sc)

#calculate the euclidean distance 

d=dist(mtcars.sc)
head(d)
class(d)

#create a clustering object

hc=hclust(d,method="complete")
hc
plot(hc,hang = -1)
#as a guide to whether our clustering is meaningful we can calculate the cophenetic distance, the dissimilarity in distances between our input distance table and the distances that are shown in the dendrogram. 
cop.d=cophenetic(hc)
cor.test(d,cop.d)

#plotting high dimensional data

#the dendrogram does not reveal the relationship between the variables

library(gplots)
heatmap.2(as.matrix(mtcars),trace="n",scale = "col",margins = c(4,8),Colv = F)
heatmap.2(as.matrix(mtcars,trace="n",scale="col",margins=c(4,8)))

library(stats)
set.seed(101)
data("iris")
head(iris)
km=kmeans(iris[,1:4],3)
head(km)

plot(iris[,1],iris[,2],col=km$cluster)
points(km$centers[,c(1,2)],col=1:3,pch=19,cex=2)
table(km$cluster,iris$Species)

#another round
set.seed(500)
km=kmeans(iris[,1:4],3)
plot(iris[,1],iris[,2],col=km$cluster)
points(km$centers[,c(1,2)],col=1:3,pch=19,cex=2)
table(predicted=km$cluster,true=iris$Species)

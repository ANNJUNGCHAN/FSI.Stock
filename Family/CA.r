### Single & ward Linkage Clustering with euclidean distance
n<-nrow(X)
xbar<-t(X)%*%matrix(1,n,1)/n 
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J                 
Y<-H%*%as.matrix(X)         
S<-t(Y)%*%Y/(n-1)          
D<-diag(1/sqrt(diag(S)))     
Z<-Y%*%D                
colnames(Z)<-colnames(X)
rownames(Z)<-rownames(X)
gov<-rownames(X)
ds <- dist(Z, method="euclidean")
round(ds, 3)
sinle=hclust(ds, method="single")
plot(sinle, hang=-1, main="Single Linkage 
     with euclidean")
ward=hclust(ds, method="ward")
plot(ward, hang=-1, main=" Ward Linkage 
     with euclidean")

### Single & ward Linkage Clustering with mahalanobis distance
library(biotools)
dm<-D2.dist(X, S)
round(sqrt(dm), 3)
sinlem=hclust(dm, method="single")
plot(sinlem, hang=-1, main="Single Linkage 
     with mahalanobis Distance")
wardm=hclust(ds, method="ward")
wardm=hclust(ds, method="ward.D")
plot(wardm, labels=gov, 
     main="Ward Linkage with Mahalanobis Distance")

### CCC & Dindex
library(NbClust)
Z<-scale(X)
ccc<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8,
             method = "kmeans", index = "ccc")
ccc
plot(2:8, type="b", ccc$All.index, xlab="Number of Clusters",
     ylab="CCC")
dindex<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8,
                method = "kmeans", index = "dindex")
dindex

### Consider ALL index
allindex<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8, 
                  method = "kmeans", index = "all", )
allindex

### K-means clustering
Z<-scale(X)
ds <- dist(Z, method="euclidean")
round(ds, 3)
people=rownames(X)
kmeans <- kmeans(Z, 7) # 4 cluster solution
cluster=data.frame(people,cluster=kmeans$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C5=cluster[(cluster[,2]==5),]
C6=cluster[(cluster[,2]==6),]
C7=cluster[(cluster[,2]==7),]
C1;C2;C3;C4;C5;C6;C7

### K-means Aggregate
aggregate(X, by=list(kmeans$cluster),FUN=mean)
### Data Loading
setwd("C:/Users/sec/Desktop")
X = read.csv("final.csv",header=T)
rownames(X)<-c( 201:216, 301:316, 401:416, 
                501:516)
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

### Cluster numbers with ALL indexs
install.packages("NbClust")
library(NbClust)
Z<-scale(X)
ccc<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8,
             method = "kmeans", index = "ccc")
ccc

allindex<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8, 
                  method = "kmeans", index = "all", )
allindex

### K-means
Z<-scale(X)
ds <- dist(Z, method="euclidean")
people=rownames(X)
kmeans <- kmeans(Z, 6) # 4 cluster solution
cluster=data.frame(people,cluster=kmeans$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C5=cluster[(cluster[,2]==5),]
C6=cluster[(cluster[,2]==6),]
C1;C2;C3;C4;C5;C6;
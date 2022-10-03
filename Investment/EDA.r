### Data Loading
setwd("C:/Users/sec/Desktop/data")
X = read.csv("stock.csv",header=T)
rownames(X)<-c( 201:216, 301:316, 401:416, 
                501:516)setwd("C:/Users/sec/Desktop/data")
X = read.csv("stock.csv",header=T)
rownames(X)<-c( 201:216, 301:316, 401:416, 
                501:516)

### Data Transformation
X=log(X+1)
class(X)
X<-as.matrix(X)			
n<-nrow(X)			
xbar<-t(X)%*%matrix(1,n,1)/n 	
I<-diag(n)			
J<-matrix(1,n,n)
H<-I-1/n*J			
Y<-H%*%X			
S<-t(Y)%*%Y/(n-1)		
D<-diag(1/sqrt(diag(S)))		
Z<-H%*%X%*%D		
colnames(Z)<-colnames(X)
R<-t(Z)%*%Z/(n-1)

### Summary Data
summary(X)

### Correlation Analysis
plot(X)

### Correlation Matrix
cor(X)

### Multivariate Normality Test
install.packages("MVN")
library("MVN") # for mardia test
result = mvn(X, 
             mvnTest = "mardia", 
             multivariatePlot =  "qq")
result
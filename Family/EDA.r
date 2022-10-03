### Import DataSet
setwd("C:/Users/sec/Desktop/family/data")
X = read.csv("family.csv",header=T)
colnames(X)<-c("utility bills", "livingfund", "tax"
               , "annualpension", "savings", "loan")
rownames(X)<-c(101:117, 201:217, 301:317, 401:417, 
               501:517, 601:617, 701:717)

### Data Preprocessing
X
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

### Summary
summary(X)

### Correlation
plot(X)

### Correlation Matrix
cor(X)

### Multivariate Normality
library("MVN") # for mardia test
result = mvn(X, 
            mvnTest = "mardia", 
            multivariatePlot =  "qq")
result

### Normality Test and QQ plot
library("MVT")
rownames<-rownames(X) 
R=round(cor(X),3)
R
eigen.R=eigen(R)
round(eigen.R$values, 2) # Eigenvalues
V=round(eigen.R$vectors, 2) # Eigenvectors
V6=V[,1:6]
V6
Z=scale(X, scale=T) # Standardized Data Matrix
Z
P=Z%*%V6 # PCs Scores
round(P, 3)
round(Z, 3)
result = mvn(P, 
             mvnTest = "mardia", 
             multivariatePlot =  "qq")

result
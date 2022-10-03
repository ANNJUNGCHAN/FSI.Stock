### Explain Variance and Scree Plot for Common Factor
p=ncol(X)
eigen.R=eigen(R)
round(eigen.R$values, 2) # Eigenvalues
V=round(eigen.R$vectors, 2) # Eigenvectors
V

gof=eigen.R$values/p*100 # Goodness-of fit
round(gof, 3) # contribution rate

plot(eigen.R$values, type="b", main="Scree Graph",
     xlab="Factor Number", ylab="Eigenvalue")

### PCFA
install.packages("MVT")
library("MVT")
library(psych)
pcfa<-principal(Z, nfactors=3, rotate="none")
pcfa

### Factor Loadings Plot
lim<-range(pretty(L))
plot(L[,1], L[,2],main="PC Factor Loadings : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(L[,1], L[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1)
plot(L[,1], L[,3],main="PC Factor Loadings : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(L[,1], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 3], col=2, code=2, length=0.1)
plot(L[,2], L[,3],main="PC Factor Loadings : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(L[,2], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,2], L[, 3], col=2, code=2, length=0.1)

### Factor Loading Score
fpc=pcfa$scores
round(fpc, 3)
par(pty="s")
rownames(fpc)<-c( 201:216, 301:316, 401:416, 
                    501:516)
lim<-range(pretty(fpc))
plot(fpc[,1], fpc[,2],main="Factor Scores : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(fpc[,1], fpc[,2], labels=rownames(fpc), cex=0.6, col="blue")
abline(v=0, h=0)
plot(fpc[,1], fpc[,3],main="Factor Scores : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(fpc[,1], fpc[,3], labels=rownames(fpc), cex=0.6, col="blue")
abline(v=0, h=0)
plot(fpc[,2], fpc[,3],main="Factor Scores : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(fpc[,2], fpc[,3], labels=rownames(fpc), cex=0.6, col="blue")
abline(v=0, h=0)

### Varimax Rotated Biplot
svd.Z <- svd(Z) 
U <- svd.Z$u    
V <- svd.Z$v 
D <- diag(svd.Z$d)
F <- (sqrt(n-1)*U)[,1:2]  # Factor Scores Matrix : F
L <- (sqrt(1/(n-1))*V%*%D)[,1:2] # Factor Loadings Matrix : Lambda
C<- rbind(F, L)
rownames(F)<-rownames(X)
rownames(L)<-colnames(X)
varimax<-varimax(L)
Lt = varimax$loadings 
T=varimax$rotmat
T
Ft= F%*%T
biplot(Ft,Lt, xlab="f1",ylab="f2", main="Varimax Rotated Biplot",
       xlim=c(-4,4),ylim=c(-4.2,2),cex=0.5,pch=16)
abline(v=0,h=0)

### MLFA
L=pcfa$loading[, 1:3]
round(L, 3)
Psi=pcfa$uniquenesses
Psi
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm, 3)
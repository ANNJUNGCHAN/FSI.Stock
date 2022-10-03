### Covariance Matrix
cov(X)

### Explained Variance and Scree Plot
eigen.R=eigen(R)
round(eigen.R$values,2)
V=round(eigen.R$vectors,2)
gof=eigen.R$values/sum(eigen.R$values)*100 # Goodness-of fit
round(gof, 2)

plot(eigen.R$values, type="b", 
     main="Scree Graph", 
     xlab="Component Number", 
     ylab="Eigenvalue")

### Interpretation of the Principal Components
V3=V[,1:3]
V3
P=Z%*%V3

### Principal Component Score plot PC1 and PC2
plot(P[,1], P[, 2],
     main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2], labels=rownames(X), cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

### Principal Component Score plot PC1 and PC3
plot(P[,1], P[, 3],
     main="Plot of PCs Scores", xlab="1st PC", ylab="3rd PC")
text(P[,1], P[, 3], labels=rownames(X), cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

### Principal Component Score plot PC2 and PC3
plot(P[,2], P[, 3],
     main="Plot of PCs Scores", xlab="2nd PC", ylab="3rd PC")
text(P[,2], P[, 3], labels=rownames(X), cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

### Biplot
pcasvd.Z<-prcomp(X, scale=T) 
summary(pcasvd.Z)
round(pcasvd.Z$rotation, 3)
pcasvd.Z$scale
biplot(pcasvd.Z, scale=0,  xlab="1st PC",ylab="2nd PC",
       main="PC Biplot")
abline(v=0, h=0)
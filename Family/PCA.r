### Choose Covariance Matrix or Correlation Matrix
cov(X)

### Elbow plot
gof=eigen.R$values/sum(eigen.R$values)*100 # Goodness-of fit
round(gof, 2)

plot(eigen.R$values, type="b", 
     main="Scree Graph", 
     xlab="Component Number", 
     ylab="Eigenvalue")

### PCA interpretation
V3=V[,1:3]
V3

### PC Score PC1 and PC2
plot(P[,1], P[, 2],
     main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2], labels=rownames, cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

### PC Score PC1 and PC3
plot(P[,1], P[, 3],
     main="Plot of PCs Scores", xlab="1st PC", ylab="3rd PC")
text(P[,1], P[, 3], labels=rownames, cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

### PC Score PC2 and PC3
plot(P[,2], P[, 3],
     main="Plot of PCs Scores", xlab="2nd PC", ylab="3rd PC")
text(P[,2], P[, 3], labels=rownames, cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

### Biplot
pcasvd.Z<-prcomp(X, scale=T) 
summary(pcasvd.Z)
round(pcasvd.Z$rotation, 3)
pcasvd.Z$scale
biplot(pcasvd.Z, scale=0,  xlab="1st PC",ylab="2nd PC",
       main="PC Biplot")
abline(v=0, h=0)
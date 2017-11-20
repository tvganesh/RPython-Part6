states=row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
biplot(pr.out, scale=0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')


digits= read.csv("digits.csv")
# Digits target
digitClasses <- factor(digits$X0.000000000000000000e.00.29)
digitsPCA=prcomp(digits[,1:64])
digitsPCA
plot(digitsPCA$x[,1:2], col = digitClasses)

df <- data.frame(digitsPCA$x)
df1 <- cbind(df,digitClasses)
ggplot(df1,aes(x=PC1,y=PC2,col=digitClasses)) + geom_point() +
  ggtitle("Top 2 Principal Components") 
    
    geom_point(aes(x=k$centers[,1],y=k$centers[,2]))


digitsPCA$sdev
digitsVar=digitsPCA$sdev^2

percentVarExp=digitsVar/sum(digitsVar)
percentVarExp
plot(percentVarExp, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(percentVarExp), xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained", 
     main="Principal Components vs % Variance explained",ylim=c(0,1),type='l',lwd=2,
       col="blue")


plot(digitsPCA$x[,1:2], col = digitClasses,pch=19,alpha=0.3)
points(k$centers,pch=8,cex=2)



###############
digits= read.csv("digits.csv")
# Digits target
digitClasses <- factor(digits$X0.000000000000000000e.00.29)
digitsPCA=prcomp(digits[,1:64])

df <- data.frame(digitsPCA$x)
df1 <- cbind(df,digitClasses)

# Pick only the first 2 principal components
a<- df[,1:2]
# Compute K Means of 10 clusters
k<-kmeans(a,10,1000)

# Create a dataframe of the centroids of the clusters
df2<-data.frame(k$centers)

#Plot the first 2 principal components with the K Means
ggplot(df1,aes(x=PC1,y=PC2,col=digitClasses)) + geom_point() +
    geom_point(data=df2,aes(x=PC1,y=PC2),col="black",size = 4) + 
    ggtitle("Top 2 Principal Components with KMeans clustering") 

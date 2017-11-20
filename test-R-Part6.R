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


tendulkar <- read.csv("tendulkar1.csv",stringsAsFactors = FALSE)
df <- tendulkar %>% select(Mins,BF,Runs)
a <- complete.cases(df)
df <- df[a,]
hc <- hclust(df, method = "euclidean")
plot(average)

d <- dist(as.matrix(mtcars))
hc <- hclust(d,method = "average") 
plot(hc)

iris <- datasets::iris
iris2 <- iris[,-5]
species_labels <- iris[,5]
d_iris <- dist(iris2) # method="man" # is a bit better
hc_iris <- hclust(d_iris, method = "average")
plot(hc_iris)


x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4


hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")



df <- na.omit(USArrests)
df <- scale(df)
d <- dist(df, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
plot(hc1)

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

iris <- datasets::iris
iris2 <- iris[,-5]
species <- iris[,5]
d_iris <- dist(iris2) # method="man" # is a bit better

d_iris <- dist(iris)
hc_iris <- hclust(d_iris, method = "average")
plot(hc_iris)
# Cut tree into 4 groups
sub_grp <- cutree(hc_iris, k = 3)

# Number of members in each cluster
table(sub_grp)

rect.hclust(hc_iris, k = 3, border = 2:5)

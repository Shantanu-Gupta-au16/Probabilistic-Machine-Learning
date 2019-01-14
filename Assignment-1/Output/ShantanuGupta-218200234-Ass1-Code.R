#1)

#Dataset
the.data<-as.matrix(read.table("BikeShareTabSep.txt"))
my.data<-the.data[sample(1:727,400),c(1:9)]
write.table(my.data,"ShantanuGupta-218200234-BikeShareMyData.txt")

#Data Preprocessing
#bike.data[,3] <- (3-bike.data[,3])/2
#bike.data[,4] <- 1-abs(bike.data[,4]-30)/41
#bike.data[,6] <- 1-abs(bike.data[,5]-60)/44
#bike.data[,7] <- 1-bike.data[,7]/41
#bike.data[,8] <- (bike.data[,8]-1)/267
#bike.data[,9] <- (bike.data[,9]-9)/437

#1.1)
install.packages("ggplot2")
library(ggplot2)
change<-as.data.frame(my.data)
pl<-ggplot(change,aes(x=V9))+geom_histogram(color='red',fill='pink')+xlab('Registered Users')+ylab('Count')
print(pl)
pl2<-ggplot(change,aes(x=V4))+geom_histogram(color='black',fill='pink')+xlab('Temperature')+ylab('Count')
print(pl2)

#1.2)
#Five Number Summary for Casual Users
df=as.data.frame(my.data)
min(df$V8)
max(df$V8)
median(df$V8)
quantile(df$V8)
fivenum(df$V8)
summary(df$V8)
mean(df$V8)

#Five Number Summary for Registered Users
min(df$V9)
max(df$V9)
median(df$V9)
quantile(df$V9)
fivenum(df$V9)
summary(df$V9)
mean(df$V9)

#1.3)
#Parallel Box plot for Casual Users
pl1<-ggplot(change,aes(x=factor(V1),y=V8))+geom_boxplot(aes(fill=factor(V1)))+xlab('Season')+ylab('Casual Users')
print(pl1)

pl3<-ggplot(change,aes(x=factor(V2),y=V8))+geom_boxplot(aes(fill=factor(V2)))+xlab('Working Day')+ylab('Casual Users')
print(pl3)

pl4<-ggplot(change,aes(x=factor(V3),y=V8))+geom_boxplot(aes(fill=factor(V3)))+xlab('Weather')+ylab('Casual Users')
print(pl4)

#Parallel Box plot for Registered Users
pl5<-ggplot(change,aes(x=factor(V1),y=V9))+geom_boxplot(aes(fill=factor(V1)))+xlab('Season')+ylab('Registered Users')
print(pl5)

pl6<-ggplot(change,aes(x=factor(V2),y=V9))+geom_boxplot(aes(fill=factor(V2)))+xlab('Working Day')+ylab('Registered Users')
print(pl6)

pl7<-ggplot(change,aes(x=factor(V3),y=V9))+geom_boxplot(aes(fill=factor(V3)))+xlab('Weather')+ylab('Registered Users')
print(pl7)

#1.4)
scatter.data<-the.data[sample(1:727,200),c(1:9)]
change2<-as.data.frame(scatter.data)
pl8<-ggplot(change2,aes(x=V4,y=V8))+geom_point(aes(color="red",alpha=0.5))+xlab('Temperature')+ylab('Casual Users')
print(pl8)

#1.5)
install.packages("corrgram")
library(corrgram)
install.packages("corrplot")
library(corrplot)

num.cols<-sapply(change2, is.numeric)
cor.data<-cor(change[,num.cols])
print(cor.data)
print(corrplot(cor.data,method='color'))
print(corrgram(change2,order=TRUE, lower.panel=panel.shade,
               upper.panel=panel.pie, text.panel=panel.txt))

#Linear Regression Model
model<-lm(V8~V4,change2)
summary(model)


# Grab residuals
res <- residuals(model)

# Convert to DataFrame for gglpot
res <- as.data.frame(res)

#Head of the residuals
head(res)

# Histogram of residuals
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(model)

#Correlation Coefficient
cor.test(~V8+V4,data=change2,method="pearson")

#6)Dimensionality Reduction

#Dataset
the.data<-as.matrix(read.table("BikeShareTabSep.txt"))
selData<-the.data[sample(1:727,200),c(4:9)]
write.table(selData,"ShantanuGupta-218200234-PCASelData.txt")

#6.1)
#Conduct Principal Component Analysis
pZ<-prcomp(selData,tol=0.01,scale=TRUE)
pZ
summary(pZ)

#plot the resultant principal components        
biplot(pZ)

#6.2)
#Conduct Principal Component Analysis
names(pZ)

#outputs theprincipal component loading
pZ$rotation
#outputs the mean of variables
pZ$center
#outputs the standard deviation of variables
pZ$scale
#matrix x has the principal component score vectors in a 200x6 dimension
pZ$x

#compute standard deviation of each principal component
std_dev<-pZ$sdev
#compute variance
pr_variance<-std_dev^2
pr_variance

#proportion of variance explained
prop_varex<-pr_variance/sum(pr_variance)
prop_varex

#Proportion of Variance Explained
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative screen plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#6.3)
#Conduct Classical MultiDimensional Scaling
selData.dist = dist(selData) 
selData.dist 
mds <-cmdscale(selData.dist)
mds
plot(mds,main="CLASSICAL MULTIDIMENSIONAL SCALING")

#6.4)
#Conduct Non-metric MultiDimensional Scaling
library(MASS)
fit = isoMDS(selData.dist, k = 2)
plot(fit$points,xlab="X",ylab="Y",xlim=c(-4,4),ylim=c(-4,4),main="NON-METRIC MULTIDIMENSIONAL SCALING",type="n")
text(fit$points,col="blue")

#6.5)
#Plotting Shepard plot
fit<-Shepard(selData.dist,fit$points)
plot(fit,main="Shepard Plot for k=2",col="red",xlab="Original Distance",ylab="Dimensions according to n-MDS")
lines(fit$x,fit$yf,type="S")

#6.6)
#Conduct Non-metric MultiDimensional Scaling
library(MASS)
fit = isoMDS(selData.dist, k = 4)
plot(fit$points,xlab="X",ylab="Y",xlim=c(-4,4),ylim=c(-4,4),main="NON-METRIC MULTIDIMENSIONAL SCALING",type="n")
text(fit$points,col="blue")

#Plotting Shepard plot for K=4
fit<-Shepard(selData.dist,fit$points)
plot(fit,main="Shepard Plot for k=4",col="red",xlab="Original Distance",ylab="Dimensions according to n-MDS")
lines(fit$x,fit$yf,type="S")

#7.1)K-Means Clustering

#Dataset
zz<-read.table("SITdata2018.txt")
zz<-as.matrix(zz)

#7.1)a
install.packages("ggplot2")
library(ggplot2)
zz<-as.data.frame(zz)
pl1<-ggplot(zz,aes(x=V1,y=V2))
print(pl1+geom_point(color='red',fill='red',alpha=0.5))

#7.1)c
# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = zz, centers = 5)
print(kmeans)
y_kmeans = kmeans$cluster
#Scatter Plot
plot(zz, col=kmeans$cluster,main="Scatter Plot using K-means") 

# Visualising the clusters
install.packages("cluster")
library(cluster)
clusplot(zz,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of SITdata2018'),
         xlab = 'V1',
         ylab = 'V2')


#7.1)d
# Using the elbow method to find the optimal number of clusters
set.seed(29)
wcss = vector()
xy<-as.vector(zz)
for (i in 1:20) wcss[i] = sum(kmeans(xy, i)$withinss)
install.packages("graphics")
library(graphics)
plot(1:20,wcss,type = 'b',main = paste('The Elbow Method'),xlab = 'Number of clusters(K)',ylab = 'TOTWSS')

#Optimal k=7
# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = zz, centers = 7)
print(kmeans)
y_kmeans = kmeans$cluster
#Scatter Plot
plot(zz, col=kmeans$cluster,main="Scatter Plot using K-means") 

# Visualising the clusters
install.packages("cluster")
library(cluster)
clusplot(zz,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of SITdata2018'),
         xlab = 'V1',
         ylab = 'V2')

#7.2)Spectral Clustering

#Dataset

zz<-read.table("SITdata2018.txt")
zz<-as.matrix(zz)

# max min transform to make [0,1]

maxMinScale <- function(x){(x-min(x))/(max(x)-min(x))} 
zz<-maxMinScale(zz) 
plot(zz)

#Compute Simalarity matrix

dXX<-as.matrix(dist(zz)) # compute Euclidean distance between data points 
cParam =1 # parameter of similarity function 
S<-exp(-dXX/cParam)  #compute similarity matrix
S

#Compute Affinity Matrix
#mutual k-nearest neighbor graphs.  
#modified from: nng() method (https://rdrr.io/cran/cccd/src/R/nng.R)

AffMat<-function(S,k) #S-distance matrix and k-no of neighbours
{ 
  AM <- matrix(0,nrow=nrow(S),ncol=ncol(S)) 
  for(i in 1:nrow(S)){ 
    d <- sort(S[i,],decreasing=TRUE) 
    for (t in 1:ncol(S)) 
    { 
      if (S[i,t] < d[k]) 
      { 
        AM[i,t]<-0 
        AM[t,i]<-0 
      } 
      else 
      { 
        AM[i,t] <- S[i,t] 
        AM[t,i] <- AM[i,t] 
      } 
    } 
  } 
  AM 
} 
A<-AffMat(S,3) 
A   

#Plot the Graph

install.packages("shape")
library(shape)
install.packages("diagram")
library(diagram) 
#names <- c("1", "2", "3", "4", "5", "6", "7", "8") 
B<-A 
diag(B) <- 0  # to avoid self loop in the graph. 
B 
pp <- plotmat(B, curve = 0, 
              lwd = 1, box.lwd = 2, cex.txt = 0.8, 
              box.type = "circle", box.prop = 0.1, arr.width=0,
              arr.pos = 0.5, shadow.size = 0, 
              main = "Graph: connented components") 
D <- diag(apply(A, 1, sum)) # sum rows 
D 

#Laplacian Matrix

L <- D - A 
L 

#Eigen Values & Eigen Vectors

eigL<-eigen(L) 
eigL 
plot(eigL$values)

#Smallest eigenvalues of L

k<-4 
Z<- eigL$vectors[,(ncol(eigL$vectors)-k+1):ncol(eigL$vectors)] 

#Plot data using the two eigenvectors

plot(Z)

#Perform K-means Clustering
#install.packages("stats")

library(stats) 
km <- kmeans(Z, centers=k, nstart=5) 
plot(Z, col=km$cluster) 
plot(zz, col=km$cluster) 






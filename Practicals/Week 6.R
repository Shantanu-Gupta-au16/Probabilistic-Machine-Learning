xx<-  matrix(c(1.1408900,0.4516467,0.8383141,0.9684895,1.0183068,2.4605491
           ,1.9738521,1.7066738, 3.933012, 3.670620, 3.635292, 4.101677, 3.525617, 
           1.448882,1.581013, 1.826728), 8,2) 
# max min transform to make [0,1] 
maxMinScale <- function(x){(x-min(x))/(max(x)-min(x))} 
xx<-maxMinScale(xx) 
plot(xx)            

dXX<-as.matrix(dist(xx)) # compute Euclidean distance between data points
cParam =1   # parameter of similarity function 
S<-exp(-dXX/cParam)  #compute similarity matrix 
S

#mutual k-nearest neighbor graphs.  
#modified from: nng() method (https://rdrr.io/cran/cccd/src/R/nng.R
 
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
install.packages("shape")
library(shape)
install.packages("diagram")
library(diagram) 
names <- c("1", "2", "3", "4", "5", "6", "7", "8") 
B<-A 
diag(B) <- 0  # to avoid self loop in the graph. 
B 
pp <- plotmat(B, curve = 0, name = names, 
              lwd = 1, box.lwd = 2, cex.txt = 0.8, 
              box.type = "circle", box.prop = 0.1, arr.width=0,
              arr.pos = 0.5, shadow.size = 0, 
              main = "Graph: connented components") 
D <- diag(apply(A, 1, sum)) # sum rows 
D 

L <- D - A 
L 
eigL<-eigen(L) 
eigL 
plot(eigL$values)
k<-2 
Z<- eigL$vectors[,(ncol(eigL$vectors)-k+1):ncol(eigL$vectors)] 
#plot data using the two eigenvectors 
plot(Z)
library(stats) 
km <- kmeans(Z, centers=k, nstart=5) 
plot(Z, col=km$cluster) 
plot(xx, col=km$cluster) 

set.seed (2) 
x=matrix (rnorm (50*2) , ncol =2) 
x[1:25 ,1]=x[1:25 ,1]+3 
x[1:25 ,2]=x[1:25 ,2]-4 
#We now perform K-means clustering 
km.out =kmeans (x,2, nstart =20) 
km.out$cluster 
plot(x, col =(km.out$cluster +1) , main="K-Means Clustering 
+ Results with K=2", xlab ="", ylab="", pch =20, cex =2) 


set.seed (1) 
x=matrix (rnorm (60*2) , ncol =2) 
x[1:20 ,1]=x[1:20,1]+15 
x[1:20 ,2]=x[1:20,2] 
x[1:20 ,3]=x[1:20,3]-15 
km.out =kmeans (x,3, nstart =20) 
km.out$cluster 
plot(x, col =(km.out$cluster +1) , main="K-Means Clustering 
+ Results with K=2", xlab ="", ylab="", pch =20, cex =2)

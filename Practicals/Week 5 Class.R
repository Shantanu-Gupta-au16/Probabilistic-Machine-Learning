# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd= 0.3), ncol= 2),matrix(rnorm(100, mean = 1, sd= 0.3), ncol= 2))
colnames(x) <- c("x", "y")
## random starts do help here with too many clusters
## (and are often recommended anyway!):
(cl <- kmeans(x, 5, nstart= 25))
plot(x, col = cl$cluster)points(cl$centers, col = 1:5, pch= 8)

#run for several k value and save totalwithin sum of squares 
totwss= array(,c(20,1))
for (i in 2:20)
{  
  print(i)
  totwss[i,1]=(kmeans(x,centers=i))$tot.withinss
  print(totwss[i]) 
}
plot(totwss, main="total within sum of squres(totWSS) with diiferent k value")

totwss

install.packages("mclust")
library(mclust) 
data(banknote)
banknote
head(banknote) 
bn<-banknote[,-1] 
head(bn) 
#Classical MDS 
selData.dist = dist(bn) 
selData.dist 
mds <-cmdscale(selData.dist)
mds
plot(mds)

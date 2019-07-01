graphBinormDist <- function(n,p){   
  x <-dbinom(0:n,size=n,prob=p)   
  barplot(x,names.arg=0:n, main=sprintf(paste('bin. dist. ',n,p,sep=':')))
    } 
par(mfcol=c(1,3))   # to display plots side by side in a matrix. 
graphBinormDist(40,0.15)
graphBinormDist(40,0.50)
graphBinormDist(40,0.85)

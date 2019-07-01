#clear variables and close 
rm(list=ls(all=TRUE)) 
graphics.off() 
install.packages("MASS")
library(MASS)  
#   load the MASS library
load("uscrime.rda")
UScrime.dist <- dist(uscrime)
UScrime.dist
UScrime.mds <- isoMDS(UScrime.dist)
UScrime.mds
plot(UScrime.mds$points, main="US crime", type = "n", ylab="Y", xlab="X") 
title("US crime")
text(UScrime.mds$points,col=1+as.numeric(uscrime$reg) ,labels= row.names(uscrime))

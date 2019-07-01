#clear variables and close 
rm(list=ls(all=TRUE)) 
graphics.off() 
install.packages("MASS")
library(MASS)  
#dataload
load("athletic.rda") 
# compute 
distanceathletic.dist = dist(athletic) 
# nonmetric 
MDSathletic.mds = isoMDS(athletic.dist)
plotplot(athletic.mds$points, main="Athletic records", type = "n", ylab="Y", xlab="X")
text(athletic.mds$points, labels=row.names(athletic))
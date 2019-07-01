require(graphics)
## the variances of the variables in the
## USArrestsdata vary by orders of magnitude, so scaling is appropriate
prcomp(USArrests)  
prcomp(USArrests, scale = TRUE)
par(mfrow=c(2,1))
plot(prcomp(USArrests))
plot(prcomp(USArrests,scale= TRUE))
summary(prcomp(USArrests, scale = TRUE))
biplot(prcomp(USArrests, scale = TRUE))


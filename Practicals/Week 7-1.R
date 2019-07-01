dataSynth<-c(-0.39,0.12,0.94, 1.67, 1.76, 2.44, 3.72,4.28, 4.92, 5.53, 0.06, 0.48, 1.01, 1.68,1.80,3.25, 4.12, 4.60, 5.28, 6.22) 
#Histogram 
hist(dataSynth) 
#Gaussian mixture
install.packages("mixtools")
library(mixtools)
mixmdl = normalmixEM(dataSynth)  # default k=2 components
mixmdl 
summary(mixmdl) 
plot(mixmdl,which=2) 
lines(density(dataSynth), lty=2, lwd=2) 
mixmdl$lambda 
mixmdl$mu 
mixmdl$sigma 
plot(mixmdl$all.loglik)

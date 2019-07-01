colors <-c("black", "blue", "red")
labels <- c("prior (mean=0, var=12)", "liklihood (x1=6, var=4)", "posterior") 

#prior
mean=0; 
sd=sqrt(12) 
x <- seq(-10,10,length=200)*sd + mean 
hx <- dnorm(x,mean,sd) 
plot(x, hx, type="n", xlab="", ylab="", ylim=c(0, 0.4),main="Bayesian estimation", axes=TRUE) 
lines(x, hx, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors[1])

#liklihood 
mean1=6;
sd1=2 
hx <- dnorm(x,mean1,sd1) 
lines(x, hx,lwd=2, col=colors[2])  
legend("topleft", inset=.005,labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

#posterior
mean2=4.5; 
sd2=3 
hx <- dnorm(x,mean2,sd2) 
lines(x, hx,lwd=2, col=colors[3])  
legend("topleft", inset=.005, labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
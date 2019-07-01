the.data<-as.matrix(read.table("allimageDigit.txt"))
str(the.data)  
selDat<-the.data 
par(mfrow=c(1,3)) 
image((matrix(the.data[,1], nrow=28,  ncol=28)))
image((matrix(the.data[,201], nrow=28,  ncol=28)))
image((matrix(the.data[,401], nrow=28,  ncol=28)))

#perform multinomial mixing... 
multiRes<-multmixEM(t(selDat), k=3) 
#summary(multiRes) 
str(multiRes) 
par(mfrow=c(1,1)) 
plot(multiRes) 

multiRes$lambda 
#multiRes$theta 
#multiRes$theta[1,] 
#plot the  
par(mfrow=c(1,3)) 
image((matrix(multiRes$theta[1,], nrow=28,  ncol=28)))
image((matrix(multiRes$theta[2,], nrow=28,  ncol=28)))
image((matrix(multiRes$theta[3,], nrow=28,  ncol=28)))
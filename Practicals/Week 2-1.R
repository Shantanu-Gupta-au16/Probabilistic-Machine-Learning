b<- c(22,17,40,5,37,19,23,6,7,53,34) 
d<- c(4,3,21,1,16,8,14,3,3,31,24)
e<-cbind(b,d)
plot(e[,1],e[,2])
cor(e[,1],e[,2])
lm(e[,2]~e[,1]) # note hat y should be before x in the brackets 
abline(lm(e[,2]~e[,1])
       
       
s<-seq(0.01,1.5,0.0001)
L = 1/((sqrt(2*pi)*s)^8)*exp(-0.93195/s^2)
plot(s,L,axes=TRUE)
lines(s,L)
#Max likelihood from graphs
s[which.max(L)]^2

#theoritical MLE
data<-c(-0.32, 0.5, 0.45, -0.55, -0.76, 0.44, -0.48, -0.07)
mML = mean(data)
N=length(data)
varML=1/N*sum((data-mML)^2)
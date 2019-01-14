#1
AIMSDataAirPres<-as.matrix(read.csv("AIMSNingalooReefAirPressure.csv", header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")) 
View(AIMSDataAirPres)

#1.1)
plot(AIMSDataAirPres, col = "red",main="Time series plot of the Data")
summary(AIMSDataAirPres)
mean(AIMSDataAirPres)
sd(AIMSDataAirPres)

#1.2)
install.packages("ggplot2")
library(ggplot2)
change<-as.data.frame(AIMSDataAirPres)
pl<-ggplot(change,aes(x=V1))+geom_histogram(binwidth=0.5,color='red',fill='pink')+xlab('Air Pressure')+ylab('Count')+ggtitle(" Ningaloo Reef Air Pressure Measurement")
print(pl)

#1.3)
install.packages("MASS")
library(MASS)
set.seed(101)
mu=0
sd=1
my_data <- rnorm(AIMSDataAirPres,mean=mu, sd=sd)# unkonwn distribution parameters
fit1<-fitdistr(AIMSDataAirPres,"normal") 
fit1
hist(my_data, pch=20, breaks=25, prob=TRUE, main="")
#Maximum Likelihood Estimate
install.packages("bbmle")
library(bbmle)
logLik(fit1)
LL <- function(mu, sigma) {
     R = dnorm(my_data, mu, sigma)
      #
         -sum(log(R))
  }
install.packages("stats4")
library(stats4)
mle(LL, start = list(mu = 1, sigma=1))

#1.4)
install.packages("mixtools")
library(mixtools)
mixmdl = normalmixEM(AIMSDataAirPres)  # default k=2 components 
plot(mixmdl,which=2)
lines(density(AIMSDataAirPres), lty=2, lwd=2)
plot(mixmdl$all.loglik,col="red")
mixmdl=summary(mixmdl) 

#1.5)
plot(mixmdl,which=2)
lines(density(AIMSDataAirPres), lty=2, lwd=2)

#1.6)
plot(mixmdl$all.loglik,col="red")

#3
#3.2)
install.packages("gRain")
library("gRain")
source("https://bioconductor.org/biocLite.R")
biocLite("RBGL")
library(RBGL)
library(gRbase)
library(gRain)
biocLite("Rgraphviz")

hl<-c("High","Low")
hnl<-c("High","Normal","Low")
eh<-cptable(~ecohealth, values = c(2,8),levels = hl)
oil.eh<-cptable(~oil|ecohealth, values = c(9,1,5,95),levels = hl)
inf.oileh<-cptable(~inflation|oil:ecohealth, values = c(9,1,1,9,2,8,2,98),levels = hl)
bp.oil<-cptable(~britishptl|oil, values = c(8,1.5,0.5,1,4,5),levels = hnl)
rt.infeh<-cptable(~retailStock|inflation:ecohealth, values = c(6,3,1,1,2,7,2,2,6,5,10,85),levels = hnl)

plist_hl<-compileCPT(list(eh,oil.eh,inf.oileh,bp.oil,rt.infeh))
plist_hl
plist_hl$oil
plist_hl$retailStock
net1<-grain(plist_hl)
net1
summary(net1)
install.packages("mixtools")
library(grid)
plot(net1)

net12<-setEvidence(net1,evidence = list(britishpt="High",retailStock="Normal"))
net12<-setEvidence(net1,nodes = c("britishpt","retailStock"), states = c("High","Normal"))
pEvidence(net12)

#3.3)
querygrain(net12,nodes=c("inflation"))

#4
#4.1)
install.packages("igraph")
library(igraph)
install.packages("ggm")
library(ggm)
dag <-DAG(c~a,d~a+b,f~c+d+e,h~e+f,g~c)
drawGraph(dag,adjust = FALSE)
dSep(dag, first="c", second="g", cond=NULL)
dSep(dag, first="c", second="h", cond="e")
dSep(dag, first="g", second="e", cond="d")
dSep(dag, first="c", second="h", cond="f")
dSep(dag, first="b", second="g", cond="f")
dSep(dag, first="b", second="g", cond=c("d", "c","e"))
dSep(dag, first="a", second="h", cond=c("d","f"))



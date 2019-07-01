source("https://bioconductor.org/biocLite.R") 
biocLite("RBGL")
install.packages("RBGL")
library(RBGL)
install.packages("gRbase")
library(gRbase) 
install.packages("gRain")
library(gRain)

#Specify conditional probability tables (with values as given in Lauritzen and Spiegelhalter (1988)): 

yn <- c("yes","no") 
a <- cptable(~asia, values=c(1,99),levels=yn) 
t.a <- cptable(~tub|asia, values=c(5,95,1,99),levels=yn) 
s <- cptable(~smoke, values=c(5,5), levels=yn) 
l.s <- cptable(~lung|smoke, values=c(1,9,1,99), levels=yn) 
b.s <- cptable(~bronc|smoke, values=c(6,4,3,7), levels=yn) 
e.lt <- cptable(~either|lung:tub,values=c(1,0,1,0,1,0,0,1),levels=yn) 
x.e <- cptable(~xray|either, values=c(98,2,5,95), levels=yn) 
d.be <- cptable(~dysp|bronc:either, values=c(9,1,7,3,8,2,1,9), levels=yn) 

#Compile list of conditional probability tables and create the network: 
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be)) 
plist 
plist$tub 
plist$either ## Notice: a logical node 
net1 <- grain(plist) 
summary(net1) 
#source("https://bioconductor.org/biocLite.R")
install.packages("Rgraphviz")
library(Rgraphviz)
install.packages("grid")
library(grid)
biocLite("Rgraphviz")
plot(net1)
plist

#The network can be queried to give marginal probabilities: 
querygrain(net1, nodes=c("lung","bronc"), type="marginal")

#Likewise, a joint distribution can be obtained: 
querygrain(net1,nodes=c("lung","bronc"), type="joint")

#Evidence can be entered in one of these two equivalent forms: 
net12 <- setEvidence(net1, evidence=list(asia="yes", dysp="yes")) 
net12 <- setEvidence(net1,nodes=c("asia", "dysp"), states=c("yes", "yes"))

#The probability of observing this evidence under the model is 
pEvidence( net12 )

#The network can be queried again: 
querygrain( net12, nodes=c("lung","bronc") )
querygrain( net12, nodes=c("lung","bronc"),type="joint" )

library(bnlearn)
library(gRbase)
data(marks)
bn.hc = hc(marks, debug = TRUE)
bn.hc  
graphviz.plot(bn.hc)
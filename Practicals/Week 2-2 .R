data <- c(0, 0, 0,0.45, 0.92, 0.86,0,0,0, 0.08, 0.8,0.83, 0, 0.02,0.19, 0)
dataMatrix <- matrix(data, nrow=4, byrow=TRUE)
eigen(dataMatrix)
t(dataMatrix)
eigen(t(dataMatrix))

invDataMatrix<-solve(dataMatrix)
invDataMatrix
eigvalues<-eigen(invDataMatrix)$values
eigvalues
recipEigenValues<-1/eigvalues
recipEigenValues

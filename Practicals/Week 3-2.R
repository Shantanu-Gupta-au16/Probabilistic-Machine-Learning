install.packages("DirichletReg")
library(DirichletReg)

x <- rdirichlet(1000, c(1,1,1))
plot(DR_data(x), a2d = list(colored = TRUE), main="c(1,1,1)")

x <- rdirichlet(1000, c(0.1,1,1) )
plot(DR_data(x), a2d = list(colored = TRUE))

x <- rdirichlet(1000, c(1,1,0.1) )
plot(DR_data(x), a2d = list(colored = TRUE))

x <- rdirichlet(1000, c(1,0.1,1) )
plot(DR_data(x), a2d = list(colored = TRUE))

x <- rdirichlet(1000, c(0.1,0.1,0.1) )
plot(DR_data(x), a2d = list(colored = TRUE))
#Definindo g=(1-xˆ2)ˆ(3/2)

g <- function(x){
  return((1-x**2)**(3/2))
}

#Estimando a integral
a <- 0
for (i in 1:10000){
  a[i] <- g(runif(1))
}
mean(a)

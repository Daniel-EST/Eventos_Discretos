#Definindo g=exp(y**2+x**2)

g <- function(x,y){
  return(exp(y**2+x**2))
}

#Estimando a integral
a <- 0
for (i in 1:100000){
  a[i] <- g(runif(1),runif(1))
}
mean(a)

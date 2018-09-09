#Definindo g=exp(-x**2), note que G assim definida é PAR

g <- function(x){
  return(exp(-x**2))
}

h <- function(x){
  return(g(1/x-1)*(x**(-2)))
}

#Estimando a integral
a <- 0
for (i in 1:100000){
  a[i] <- 2*h(runif(1))
}
mean(a)

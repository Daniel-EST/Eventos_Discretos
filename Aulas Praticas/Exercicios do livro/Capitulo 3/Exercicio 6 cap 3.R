#Definindo g=x*(1+x**2)*(-2)

g <- function(x){
  return(x*(1+x**2)**(-2))
}

h <- function(x){
  return(g(1/x-1)*(x**(-2)))
}

#Estimando a integral
a <- 0
for (i in 1:100000){
  a[i] <- h(runif(1))
}
mean(a)

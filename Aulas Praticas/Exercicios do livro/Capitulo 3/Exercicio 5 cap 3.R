#Definindo g=exp(x+x**2)

g <- function(x){
  return(exp(x+x**2))
}

h <- function(x,a,b){
  return(g(a+(b-a)*x)*(b-a))
}

#Estimando a integral
a <- 0
for (i in 1:100000){
  a[i] <- h(runif(1),-2,2)
}
mean(a)

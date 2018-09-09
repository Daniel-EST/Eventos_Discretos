#Definindo g=exp(x)

g <- function(x){
  return(exp(x))
}

#Estimando a integral

a <- 0
for (i in 1:10000){
  a[i] <- g(runif(1))
}
mean(a)

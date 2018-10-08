#Vamos usar o metodo da função inversa.

f <- function(a,b){
  u <- runif(1)
  x <- (log(1-u)/(-a))**(1/b)
  return(x)
}

#Verificando se esta certo
a <- 0
for (i in 1:10000){
  a[i] <- f(1,1)
}
mean(a)

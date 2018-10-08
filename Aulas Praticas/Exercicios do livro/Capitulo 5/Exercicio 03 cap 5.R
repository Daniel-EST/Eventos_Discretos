#Vamos usar o metodo da função inversa.

f <- function( ){
  u <- runif(1)
  x <- (-1+sqrt(1+8*u))/2
  return(x)
}

#Verificando se esta certo
a <- 0
for (i in 1:10000){
  a[i] <- f( )
}
mean(a)

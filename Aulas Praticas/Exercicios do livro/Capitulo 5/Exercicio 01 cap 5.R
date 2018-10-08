#Vamos usar o metodo da função inversa.
f <- function( ){
  u <- runif(1)
  x <- log(u*(exp(1)-1)+1)
  return(x)
}

#Verificando se esta certo
a <- 0
for (i in 1:10000){
  a[i] <- f( )
}
mean(a)

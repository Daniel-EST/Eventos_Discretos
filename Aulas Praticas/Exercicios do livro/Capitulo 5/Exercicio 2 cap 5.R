#Vamos usar o metodo da função inversa.

f <- function( ){
  u <- runif(1)
  if (u<=1/4){
    x <- 2+2*sqrt(u)
  }
  if (u>1/4){
    x <- 6-2*sqrt(3-3*u)
  }
  return(x)
}

#Verificando se esta certo
a <- 0
for (i in 1:10000){
  a[i] <- f( )
}
mean(a)

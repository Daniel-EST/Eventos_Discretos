#Vamos usar o metodo da função inversa.

f <- function( ){
  u <- runif(1)
  if (u<1/2){
    x <- log(2*u)/2
  }
  if (u>=1/2){
    #nesse passo poderiamos trocar 1-u por u
    x <- -log(2*(1-u))/2
  }
  return(x)
}

#Verificando se esta certo
a <- 0
for (i in 1:10000){
  a[i] <- f( )
}
mean(a)

#Estou usando o metodo da rejeição y~U(0,1) assim c=exp(1)/(exp(1)-1)
#h=exp(x-1)

f1 <- function( ){
  y <- runif(1)
  u1 <- runif(1)
  while (u1>exp(y-1)){
    y <- runif(1)
    u1 <- runif(1)
  }
  x <- y
  return(x)
}

#Verificando 
a <- 0
for (i in 1:10000){
  a[i] <- f1( )
}
mean(a)

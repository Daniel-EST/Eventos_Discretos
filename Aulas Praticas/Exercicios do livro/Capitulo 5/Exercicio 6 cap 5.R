#Vamos usar o metodo da funcao inversa

f <- function( ){
  u <- runif(1)
  x <- -log(u*(1-exp(-.05))+1)
  return(x)
}

#Estimando a esperança

a <- 0
for (i in 1:100000){
  a[i] <- f( )
}
mean(a)
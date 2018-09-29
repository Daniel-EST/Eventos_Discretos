#Exercicio 3 capitulo 4

p <- c(.3,.2,.35,.15)

f <- function( ){
  x <- 0
  u <- runif(1)
  if (u<.3){
    x <- 1
    return(x)
  }
  if(u<.5){
    x <- 2
    return(x)
  }
  if(u<.85){
    x <- 3
    return(x)
  }
  if (u<1){
    x <- 4
    return(x)
  }
}

#testando o algoritmo

e <- 0 
for (i in 1:100000){
  e[i] <- f( )
}
mean(e)

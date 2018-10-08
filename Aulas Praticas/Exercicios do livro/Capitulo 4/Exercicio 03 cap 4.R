#Exercicio 3 capitulo 4
p <- c(.3,.2,.35,.15)
f <- function(){
  u <- runif(1)
  if(u < 0.3) return(1)
  if(u < 0.5) return(2)
  if(u < 0.85) return(3)
  return(4)
}

# Pelo método da rejeição
g <- function(){
  prob <- c(.3, .2, .35, .15)
  y <- as.integer(4*runif(1)) + 1
  while(runif(1) > prob[y]/.35){
    prob <- c(.3, .2, .35, .15)
    y <- as.integer(4*runif(1)) + 1
  }
  return(y)
}

# testando o algoritmo
# Método da transformação inversa
e <- 0 
for (i in 1:100000){
  e[i] <- f( )
}
mean(e)

# Método da rejeição.
h <- 0 
for (i in 1:100000){
  h[i] <- g()
}
mean(h)

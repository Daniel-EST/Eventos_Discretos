exp <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

gama <- function(n,lambda){
  #sabemos que um gama pode ser escrito com o a soma de exponencias
  #n tem q ser um número inteiro
  y <- 0
  for (i in 1:n){
    y[i] <- exp(lambda)
  }
  x <- sum(y)
  return(x)
}
gama(2,1)

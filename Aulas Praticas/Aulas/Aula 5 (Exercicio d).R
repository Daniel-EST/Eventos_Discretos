#Gerando Exponenciais

expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

#Gerando Binomiais

ber <- function(p){
  # p é a probabilidade de sucesso
  u <- runif(1)
  if (u<1-p){
    x <- 0
    return(x)
  }
  else {
    x <- 1
    return(x)
  }
}

binomial <- function(n,p){
  y <- 0
  for (i in 1:n){
    y[i] <- ber(p)
  }
  x <- sum(y)
  return(x)
}

#Defina Xi uma Bernoulli com p=0.05, ou seja, a Xi=1 o i esimo cliente usou o seguro

custo <- function(lambda,n,p){
  N <- binomial(n,p)
  x <- 0
  for (i in 1:N){
    x[i] <- expo(lambda)
  }
  custo <- sum(x)
  return(custo)
}

custo(1/800,1000,.05)

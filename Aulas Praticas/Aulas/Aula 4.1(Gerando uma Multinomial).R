#Gerando um vetor (X1,X2,X3)~Multinomial(n,p) esse é o caso particular dps vamos gerar o um caso 
#mais generico

#Primeiro vamos gerar um binomial
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

#Gerando a multinomial

multi <- function(n,p){
  x[1] <- binomial(n,p[1])
  p2 <- p[2]/(1-p[1])
  x[2] <- binomial(n-x[1],p2)
  x[3] <- n-x[2]-x[1]
  return(x)
}

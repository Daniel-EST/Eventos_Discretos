#Sabemos que se X tem NM entao X=AZ+mu, onde At*A=Matriz das covariancias

#Vamos gerar um exponencial

expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

#Vamos gerar normal padrão e dps com transformacoes teremos uma normal qualquer
#Vamos usar o metodo da rejeicao com c=exp{2*exp{1/2}/sqrt(2*pi)} e h=exp{-(y-1)**2/2} 

normal <- function(mu,sigma2){
  y <- expo(1); y
  u1 <- runif(1)
  while (u1>=exp(-(y-1)**2/2)){
    y <- expo(1)
    u1 <- runif(1)
  }
  u2 <- runif(1)
  if (u2<=.5){
    x <- (-1)*y
  }
  else {
    x <- y
  }
  return(x)
}

#Gerando X
#Tome cuidado na hora de colocar os valores de mu e Sigma

normal_multi <- function(mu,Sigma){
  A <- chol(Sigma)
  X <- A%*%Sigma+mu
  return(X)
}


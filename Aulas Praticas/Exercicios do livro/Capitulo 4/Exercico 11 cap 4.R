#Vamos gerar um exponencial 1º

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
  #Gerando uma normal sem ser padrão
  x <- sigma2*x + mu
  return(x)
}

#Gerando |Z| onde Z~N(0,1)

Z <- function ( ){
  z <- abs(normal(0,1))
  return(z)
}

#Calculando E[|Z|]

e <- 0
for (i in 1:100000){
  e[i] <- Z( )
}
mean(e)
hist(e,freq=FALSE)

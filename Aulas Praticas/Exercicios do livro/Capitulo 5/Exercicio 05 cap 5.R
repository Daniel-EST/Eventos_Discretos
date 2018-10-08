#Gerando Exponenciais

expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

#Vamos usar o Metodo da Composição
#F1=... ==> exp(2)
#F2=exp(2a)/2-1 ==> ???
#Defiindo T uma v.a assumindo valores {1,2} ==> P(T=1)=1/2 e P(T=2)=1/2

#Primeiro vamos gerar F2 pelo metodo da inversa
F2 <- function( ){
  u <- runif(1)
  x <- log(u)/2
  return(x)
}

#Gerando X

f <- function( ){
  u <- runif(1)
  t <- as.integer(2*u)+1
  if (t==1){
    x <- expo(2)
    return(x)
  }
  if (t==2){
    x <- F2( )
    return(x)
  }
}

#Testando o algoritmo

e <- 0
for (i in 1:100000){
  e[i] <- f( )
}
mean(e)

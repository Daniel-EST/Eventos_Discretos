#Vamos gerar uma v.a X com distribuição Exponencial Dupla ou Distribuição de Laplace
#Com b=1 e mu=0 ==> exp(-|x|)/2, tente escrever usando a função indicadora
#Vamos usar o metodo da composição e metodo da inversa

#Criando T com P(T=1)=P(T=2)=1/2

t <- function( ){
  u <- runif(1)
  if (u<.5){
    t <- 1
    return(t)
  }
  else {
    t <- 2
    return(t)
  }
}

#Gerando Exponenciais
expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

#Gerando X
#Temos F1=exp(-x) ==> F1~Exp(1) e F2=exp(x) ===> F2~???.
#Vamos usar o metodo da inversa para gerar F2

laplace <- function( ){
  t <- t( )
  if (t==1){
    x <- expo(1)
    return(x)
  }
  if (t==2){
    u <- runif(1)
    x <- log(u)
    return(x)
  }
}

#Testando para ver se esta certo
e <- 0
for (i in 1:100000){
  e[i] <- laplace ( )
}
mean(e)

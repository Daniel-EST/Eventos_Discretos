#Exercicio b

#Gerando Exponenciais

expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

#F1=... ==> exp(2)
#F2=y ==> uniforme 
#Defiindo T uma v.a assumindo valores {1,2} ==> P(T=1)=1/3 e P(T=2)=2/3

#Gerando T
t <- function( ){
  u <- runif(1)
  if (u<1/3){
    x <- 1
    return(x)
  }
  else {
    x <- 2
    return(x)
  }
}

f <- function( ){
  t <- t( )
  if (t==1){
    x <- expo(2)
    return(x)
  }
  if (t==2){
    x <- runif(1)
    return(x)
  }
}

e <- 0
for (i in 1:100000){
  e[i] <- f( )
}
mean(e)


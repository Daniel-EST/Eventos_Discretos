#Exercicio a

#Gerando Exponenciais

expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

#F1=... ==> Exp(2)
#F2=2x==> uniforme(0,1/2) 
#F3=......=> ??? (USAR METODO DA INVERSA)
#Defiindo T uma v.a assumindo valores {1,2,3} ==> P(T=i)=1/3

#Gerando F3
F3 <- function( ){
   u <- runif(1)
   x <- log(3-u)/2
   return(x)
}
F3( )
#Gerando X
f <- function( ){
  u <- runif(1)
  t <- as.integer(3*u)+1
  if (t==1){
    x <- expo(2)
    return(x)
  }
  if (t==2){
    x <- runif(1,0,.5)
    return(x)
  }
  if (t==3){
    x <- F3( )
    return(x)
  }
}

#Testando o algoritmo
e <- 0
for (i in 1:100000){
  e[i] <- runif(1,0,.5)
  
}
max(e)

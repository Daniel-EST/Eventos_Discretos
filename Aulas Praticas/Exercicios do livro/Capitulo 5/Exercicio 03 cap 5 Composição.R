#Vamos usar o metodo da coposição para gerar X

#F(x)=.5*F1(x)+.5*F2(x)
#Onde F1=x^2 ==> Vamos usar o Metodo da Inversa para gerar esse valores, note q 0<x<1
# e F2=x ==> tem distribuiçao uniforme

#Gernado F1

F1 <- function( ){
  u <- runif(1)
  x <- sqrt(u)
  return(x)
}

f <- function( ){
  u <- runif(1)
  t <- as.integer(2*u)+1
  if (t==1){
    x <- F1()
    return(x)
  }
  if (t==2){
    x <- runif(1)
    return(x)
  }
}

#Testando 

e <- 0
for (i in 1:10000){
  e[i] <- f( )
}
mean(e)
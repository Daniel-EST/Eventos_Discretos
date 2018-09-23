#Exercicio a

#F1=y ==> uniforme
#F2=y**3 ==> ???; F3=y**5==> ??? (USAR METODO DA INVERSA)
#Defiindo T uma v.a assumindo valores {1,2,3} ==> P(T=i)=1/3

#Gerando X2 e X3

f <- function( ){
  u <- runif(1)
  t <- as.integer(3*u)+1
  if (t==1){
    x1 <- runif(1)
    return(x1)
  }
  if (t==2){
    x2 <- runif(1)**(1/3)
    return(x2)
  }
  if (t==3){
    x3 <- runif(1)**(1/5)
    return(x3)
  }
}

#Verificando se esta correto
e <- 0
for (i in 1:100000){
  e[i] <- f( )
}
mean(e)

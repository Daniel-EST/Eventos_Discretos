#Vamos usar o metodo da rejeição comarando com uma Y~U(.8,1)
#temos que c=95.2381 ==> h(y)=156.25*x*(1-x)^3, veja que esse maximo foi achando testando os valores extermos

uniforme <- function(a,b){
  u <- runif(1)
  x <- u*(b-a)+a
  return(x)
}

 f <- function( ){
   y <- uniforme(.8,1)
   u <- runif(1)
   t <- 156.25*y*(1-y)^3
   while (u>t){
     y <- uniforme(.8,1)
     u <- runif(1)
     t <- 156.25*y*(1-y)^3
   }
   x <- y
   return(x)
 }
 
 #testando
 
 e <- 0
 for (i in 1:100000){
   e[i] <- f( )
 }
 mean(e)
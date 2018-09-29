#Vamos usar o metodo da rejeição comarando com uma Y~U(0,1)
#temos que c=15/8 ==> h(y)=16(x^2-2x^3+x^4)

f <- function( ){
  y <- runif(1)
  u1 <- runif(1)
  t <- 16*(y^2-2*y^3+y^4)
  while (u1>t){
    y <- runif(1)
    u1 <- runif(1)
    t <- 16*(y^2-2*y^3+y^4)
  }
  x <- y
  return(x)
}

#Testando

e <- 0
for (i in 1:1000){
  e[i] <- f( )
}
mean(e)
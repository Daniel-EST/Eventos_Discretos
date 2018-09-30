#Vamos usar o metodo da rejeição para gera X para isso vamos usar Y~U(0,1)
#Temos que c=k*((a-1)/d)^(a-1)*((b-1)/d)^(b-1), onde k=Gamma(a+b)/(Gamma(a)*Gamma(b)) e d=a+b-1
#Assim temos tambem que h=(a+b-2)^(a+b-2)*y^(a-1)*((1-y)^(b-1))/(((a-1)^(a-1))*((b-1)^(b-1)))

beta <- function(a,b){
  y <- runif(1)
  u <- runif(1)
  h <- (a+b-2)^(a+b-2)*y^(a-1)*((1-y)^(b-1))/(((a-1)^(a-1))*((b-1)^(b-1)))
  while (u>h){
    y <- runif(1)
    u <- runif(1)
    h <- (a+b-2)^(a+b-2)*y^(a-1)*((1-y)^(b-1))/(((a-1)^(a-1))*((b-1)^(b-1)))
  }
  x <- y
  return(x)
}

#Testando 

e <- 0
for (i in 1:10000){
  e[i] <- beta(5,50)
}
mean(e)

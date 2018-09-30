#Usando o metodo da rejeição temos que o ponto q maximiza f(x)/g(x) onde g(x)=lambda*exp(-lambda*x)
#x=2/(1-lambda). Pela teoria sabemos que E[X]=c, queremos minimar E[X], ou seja queremos achar 
#valor de c(lambda) min tal que c(lambda)=2*exp(-2)/(((1-lambda)^2)*lambda) ==> lambda = 1/3

#Gerando Exponencial
expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

#Gerando os numeros

f <- function(lambda){
  y <- expo(lambda)
  u1 <- runif(1)
  t <- y^2*exp(-y*(1-lambda))*(1-lambda)^2/(4*exp(-2))
  while (u1>=t){
    u1 <- runif(1)
    y <- expo(lambda)
    t <- y^2*exp(-y+lambda*y+2)*(1-lambda)^2/4
  }
  x <- y
  return(x)
}

#Testando 

b <- 0
for (i in 1:50000){
  b[i] <- f(1/3)
}
mean(b)

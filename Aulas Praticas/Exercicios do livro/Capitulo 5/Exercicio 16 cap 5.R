#Usando o metodo da rejeição temos que o ponto q maximiza f(x)/g(x) onde g(x)=lambda*exp(-lambda*x)
#x=2/(1-lambda). Pela teoria sabemos que E[X]=c, queremos minimar E[X], ou seja queremos achar 
#valor de c(lambda)=2*exp(-2)/((1-lambda)**2*lambda) ==> lambda = 1/3

#Gerando Exponencial
expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

#Gerando os numeros

f <- function(lambda){
  y <- expo(lambda)
  u <- runif(1)
  print(u)
  while (u>=y*(lambda-1)**2*exp(-y*(lambda-1)+2)/4){
    y <- expo(lambda)
    u <- runif(1)
  }
  x <- y
  return(x)
}

#Testando 

b <- 0
for (i in 1:100){
  b[i] <- f(1/3)
}
mean(a)
mean(b)

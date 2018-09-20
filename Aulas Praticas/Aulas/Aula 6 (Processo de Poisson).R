#Metodo para gerar os tempos dos n primeiros eventos de um process de poisson
#Note que nesse metodo temos que definir a quantidade de eventos que ocorreram 

pois <- function(n,lambda){
  u <- runif(n); y <- 0; t <- 0
  for (i in 1:n){
    y[i] <- -log(u[i])/lambda
  }
  for (j in 1:n){
    t[j] <- sum(y[1:j])
  }
  return(t)
}

#Verificando se esta correto
e <- 0
for (i in 1:10000){
  e[i] <- pois(1,1)
}
mean(e)

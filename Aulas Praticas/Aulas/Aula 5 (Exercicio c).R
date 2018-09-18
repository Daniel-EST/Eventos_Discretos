#Exercicio c

#No exercicio 12 do cap 5 fazer x~exp(1)

#Gerando Exponenciais

expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

y <- function(lambda,n){
  y <- 0
  for (i in 1:n){
    y[i] <- expo(lambda)
  }
  return(max(y))
}

e <- 0
for (i in 1:1000){
  e[i] <- y(2,100)
}
mean(e)

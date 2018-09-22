#Temos que X="Número de pessoas dentro do onibus"~U{20,21,...,41}
#e Y="Quantidade de onibus que chegam"~Pois(5) ate um t<1

#Simulando Y
pois <- function(lambda){
  u <- runif(1)
  t <- -log(1-u)/lambda; N <- 0
  while (t<1) {
    N <- N+1; u <- runif(1)
    t <- t+(-log(1-u)/lambda)
  }
  return(N)
}

#Simulando X
unif <- function(n){
  u <- runif(1)
  x <- as.integer(n*u)+1
  x <- x+20
  return(x)
}

#Simulando a quantidade de pessoas
pessoas <- function(lambda){
  onibus <- pois(lambda); x <- 0
  for (i in 1:0){
    x[i] <- unif(21)
  }
  return(sum(x))
}
pessoas(5)

#Testando
e <- 0
for (i in 1:1000){
  e[i] <- pessoas(5)
}
mean(e)

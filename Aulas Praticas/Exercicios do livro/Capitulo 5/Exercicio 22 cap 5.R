#Gerando X~Pois(lambda) ate um tmepo t<Tempo

pois <- function(lambda,Tempo){
  u <- runif(1)
  t <- -log(1-u)/lambda; N <- 0
  while (t<Tempo) {
    N <- N+1; u <- runif(1)
    t <- t+(-log(1-u)/lambda)
  }
  return(N)
}

#Testando o algoritmo E[x]=lambda*Tempo
e <- 0
for (i in 1:10000){
  e[i] <- pois(5,2)
}
mean(e)
#Metodo para gerar os tempos de um processo de Poisson ate um tempo T

pois <- function(Tempo,lambda){
  t <- 0; n <- 0; s <- 0; u <- runif(1)
  t <- -log(u)/lambda
  while (t<Tempo){
    n <- n+1; s[n] <- t
    u <- runif(1); t <- t+(-log(u)/lambda)
  }
  return(n)
}

#Verificando se esta certo
e <- 0
for (i in 1:10000){
  e[i] <- pois(1,3)
}
mean(e)

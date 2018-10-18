#Gerando uma uniforme discreta num intervalo {m,m+1,...,n-1,n}

unif <- function(n,m){
  u <- runif(1)
  x <- as.integer(u*(n-m+1))+m
  return(x)
}

#Testando 

n <- 10; m <- 2
e <- 0
for (i in 1:1000){
  e[i] <- unif(n,m)
}
mean(e)


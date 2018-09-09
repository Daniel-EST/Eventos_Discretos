#Usando a funcao 2 do exercicio 2

X2 <- function(p,x,n){
  a <- 0
  for (j in 1:n){
    u <- runif(1); s <- 0
    for (i in 1:length(p)){
      s <- s+p[i]
      if (u<s){
        a[j] <- x[i]
        break
      }
    }
  }
  if (n<10){
    cat("Os numeros gerados são:",a,"\n") 
  }
  mean(a)
}

x <- c(5:14)
p <- rep(c(0.11,.09),times=5)
X2(p,x,10000)

#Esperança verdadeira

e <- sum(x*p); e

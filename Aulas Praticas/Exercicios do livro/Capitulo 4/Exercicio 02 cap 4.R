#Primeira ideia

X <- function(p,x){
  u <- runif(1); s <- 0
  for (i in 1:length(p)){
    s <- s+p[i]
    if (u<s){
      return(x[i])
    }
    
  }
}

#Tentando
e <- 0
for (i in 1:10000){
  e[i] <- X(c(1/3,2/3),c(1,2))
}
mean(e)

#Segunda ideia 

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
  mean(a)
}
X2(c(1/3,2/3),c(1,2),100000)

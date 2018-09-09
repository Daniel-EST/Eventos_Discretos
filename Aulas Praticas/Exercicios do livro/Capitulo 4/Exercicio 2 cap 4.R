#Primeira ideia nao da para verificar se esta certo a funcao

X <- function(p,x){
  u <- runif(1); s <- 0
  for (i in 1:length(p)){
    s <- s+p[i]
    if (u<s){
      return(x[i])
    }
    
  }
}
X(c(1/3,2/3),c(1,2))

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

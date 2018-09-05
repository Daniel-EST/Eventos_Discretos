ber <- function(p){
  # p é a probabilidade de sucesso
  u <- runif(1)
  if (u<1-p){
    x <- 0
    return(x)
  }
  else {
    x <- 1
    return(x)
  }
}

binomial <- function(n,p){
  y <- 0
  for (i in 1:n){
    y[i] <- ber(p)
  }
  x <- sum(y)
  return(x)
}
x <- 0
for (i in 1:100000){
  x[i] <- binomial(10,0.4)
}
mean(x); mean(rbinom(100000,10,.4))

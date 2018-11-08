expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}
expo_n <- function(n,lambda){
  x <- NULL
  for (i in 1:n){
    x[i] <- expo(lambda)
  }
  return(x)
}
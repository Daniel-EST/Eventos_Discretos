expo_n <- function(n,lambda){
  x <- NULL
  for (i in 1:n){
    x[i] <- expo(lambda)
  }
  return(x)
}
# Item c ------------------------------------------------------------------
f <- function(n){
  prob <- NULL
  Y <- 0; Z <- 0
  for (i in 1:n){
    Y[i] <- I(5,1)
    Z[i] <- sum(1:5*expo_n(5,1))
  }
  cov <- cov(Y,Z)
  prob <- c(prob,mean(Y)-(cov/55)*(mean(Z)-15))
  return(prob)
}
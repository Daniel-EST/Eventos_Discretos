poisson <- function(lambda){
  u <- runif(1);I <- as.integer(lambda); f <- ppois(I,lambda)
  if (u<f){
    i <- I; p <- dpois(i,lambda); f <- f-p
    while (u<f){
      i <- i-1; p <- dpois(i,lambda); f <- f-p
    }
    x <- i
    return(x)
  }
  else{
    i <- I+1; p <- dpois(i,lambda); f <- f+p
    while (u>=f){
      i <- i+1; p <- p*lambda/i; f <- f+p
    }
    x <- i+1
    return(x)
  }
}
poisson(1000)

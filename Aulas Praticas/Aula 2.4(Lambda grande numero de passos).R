poisson <- function(lambda){
  u <- runif(1);I <- as.integer(lambda); f <- ppois(I,lambda); passos <- 1
  if (u<f){
    i <- I; p <- dpois(i,lambda); f <- f-p
    while (u<f){
      i <- i-1; p <- dpois(i,lambda); f <- f-p
      passos <- passos+1
    }
    return(passos)
  }
  else{
    i <- I+1; p <- dpois(i,lambda); f <- f+p
    while (u>=f){
      i <- i+1; p <- p*lambda/i; f <- f+p
      passos <- passos+1
    }
    return(passos)
  }
}


r_poisson <- function(lambda){
  u <- runif(1)
  i <- 0 ; p <- exp(-lambda) ; f <- p
  while(u >= f){
    i <- i + 1
    p <- p * lambda/i
    f <- f + p 
  }
  return(i)
}
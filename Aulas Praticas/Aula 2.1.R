poisson=function(lambda){
  u <- runif(1);i <- 0;p <- exp(-1*lambda);f <-p
  while(u>=f){
    i <- i+1;p <- p*lambda/i;f <- f+p
    print(f)
  }
  x <- i
  return(x)
}
poisson(500)

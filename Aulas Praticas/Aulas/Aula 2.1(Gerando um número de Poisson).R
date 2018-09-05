poisson=function(lambda,n){
  x <- 0
  u <- runif(1)
  i <- 0;p <- exp(-1*lambda);f <-p
  while(u>=f){
      i <- i+1;p <- p*lambda/i;f <- f+p
    }
    x <- i
  cat("Número gerado é;",x)
}
poisson(10)

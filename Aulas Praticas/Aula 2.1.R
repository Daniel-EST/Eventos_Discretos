poisson=function(lambda,n){
  x <- 0
  y <- 1:n
  u <- runif(n)
  i <- 0;p <- exp(-1*lambda);f <-p
  for (j in y) {
    while(u[j]>=f){
      i <- i+1;p <- p*lambda/i;f <- f+p
    }
    x[j] <- i
    i <- 0;p <- exp(-1*lambda);f <-p
  }
  print(x)
  mean(x)
}
poisson(10,400)
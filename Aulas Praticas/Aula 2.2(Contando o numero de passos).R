poisson=function(lambda,n){
  x <- 0
  u <- runif(n)
  passos <- 1:n
  for (j in 1:n){
    i <- 0;p <- exp(-1*lambda);f <-p;cont <- 1
    while(u[j]>=f){
      i <- i+1;p <- p*lambda/i;f <- f+p
      cont<- cont+1
    }
    x[j] <- i
    passos[j] <- cont
  }
  cat('Media do nuumero de passos para gerar X;',mean(passos))
}
poisson(10,400)


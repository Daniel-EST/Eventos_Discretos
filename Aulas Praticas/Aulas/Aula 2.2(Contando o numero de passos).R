poisson=function(lambda,n){
  u <- runif(1)
  i <- 0;p <- exp(-1*lambda);f <-p;cont <- 1
  while(u>=f){
    i <- i+1;p <- p*lambda/i;f <- f+p
    cont<- cont+1
  }
  passos <- cont
  cat('Media do numero de passos para gerar X;',passos)
}
poisson(10)


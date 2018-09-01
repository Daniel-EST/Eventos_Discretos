r_poisson <- function(lambda){
  u <- runif(1)
  i <- 0 ; p <- exp(-lambda) ; f <- p
  while(u >= f){
    i <- i + 1
    p <- p * lambda/i
    f <- f + p 
  }
  return(c(i,i +1))
}
r_poisson(100)

r_poissonG <- function(lambda){
  u <- runif(1) ; I <- as.integer(lambda) ; f <- ppois(I,lambda)
  if(u < f){
    i <- I ; p <- dpois(i, lambda) ; f <- f - p
    while(u < f){
      i <- i - 1 ; p <- dpois(i, lambda) ; f <- f - p
    }
    x <- i
    int <- I - i + 1
  } else {
   i <- I +1 ; p <-  dpois(i, lambda) ; f <- f + p
   while(u >= f){
     i <- i + 1 ; p <- p * lambda/i ; f <- f + p
   }
   x <- i + 1
   int <- i - I + 1
  }
  return(c(x, int))
}
r_poissonG(1000)

interacoes <- function(n, lambda){
  x <- 0
  for(i in 1:n){
    x[i] <- r_poisson(lambda)[2]
  }
  return(mean(x))
}
interacoes(400,10)

interacoesG <- function(n, lambda){
  x <- 0
  for(i in 1:n){
    x[i] <- r_poissonG(lambda)[2]
  }
  return(mean(x))
}
interacoesG(1000, 400)

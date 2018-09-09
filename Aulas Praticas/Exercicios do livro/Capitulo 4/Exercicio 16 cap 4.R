#Tentei fazer algo parecido com a distribuição de poisson

funcao=function(){
  x <- 0
  u <- runif(1)
  i <- 1; p <- 5/12;f <-p
  while(u>=f){
    i <- i+1; p <- p*(2**(-i-1)+2**(i-2)/3**i)/(2**(-i-2)+2**(i-1)/3**(i+1)); f <- f+p
  }
  x <- i
  return(x)
}
funcao()

x <- 0
for (i in 1:100000){
  x[i] <- funcao()
}
mean(x)

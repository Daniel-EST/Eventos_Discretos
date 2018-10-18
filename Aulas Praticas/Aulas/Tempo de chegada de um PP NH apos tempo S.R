#Tempo de chegada de um processo poisson não homogeneo apos S

#Temos que ter lambda(t)

t <- function(lambda,s){
  t <- s
  u1 <- runif(1)
  t <- -log(u1)/lambda
  u2 <- runif(1)
  while (u2>lambda(t)/lambda){
    u1 <- runif(1)
    t <- -log(u1)/lambda
    u2 <- runif(1)
  }
  ts <- t
  return(ts)
}
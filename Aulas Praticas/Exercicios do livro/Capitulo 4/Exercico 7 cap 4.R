#Vamos estimar a esperança X Px=c(1,2,3,4,5,6,5,4,3,2,1)/36
#Temos q E[X]=7
#Vamos usar o método da rejeição e c=66/36 => h=6*Px

p <- c(1,2,3,4,5,6,5,4,3,2,1)/36; n <- c(2:12); e <- sum(p*n); e

f <- function(p){
  u1 <- runif(1); u2 <- runif(1)
  y <- as.integer(11*u1)+1
  h <- 6*p[y]
  while (u2>=h){
    u1 <- runif(1)
    y <- as.integer(11*u1)+1
    h <- 6*p[y]
    u2 <- runif(1)
  }
  x <- y
  return(x)
}
f(p)
x <- 0
for (i in 1:1000){
  x[i] <- f(p)
}
mean(x)

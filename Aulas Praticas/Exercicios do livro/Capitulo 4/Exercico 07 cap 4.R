#Vamos primeiro gerar $X$ com função de probabidade dada por: $P_{x}=c(1,2,3,4,5,6,5,4,3,2,1)/36$
#, e em seguida vamos estimar o q se pede no enunciado.   
#Vamos usar o método da rejeição e $c=\frac{66}{36}$ $\rightarrow$ $h=6P_{x}$.

p <- c(1,2,3,4,5,6,5,4,3,2,1)/36; n <- c(2:12); e <- sum(p*n); e

f <- function(p){
  n <- c(2:12); x <- 0
  while(sum(n)>0){
    u1 <- runif(1); u2 <- runif(1)
    y <- as.integer(11*u1)+1
    h <- 6*p[y]
    while (u2>=h){
      u1 <- runif(1)
      y <- as.integer(11*u1)+1
      h <- 6*p[y]
      u2 <- runif(1)
    }
    #Podeira substituir y <- n[y]
    n <- setdiff(n,y+1)
    x <- x+1
  }
  return(x)
}
f(p)

#Testando 

x <- 0
for (i in 1:1000){
  x[i] <- f(p)
}
mean(x)

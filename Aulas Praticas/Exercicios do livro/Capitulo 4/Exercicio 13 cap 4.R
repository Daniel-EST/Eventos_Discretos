#ITEM a

#Note que P(Y=i)=P(X=i)/a para n>=i>=k, veja tambem P(Y=i+1)=(n-i)/(i+1)*p/(1-p)*P(Y=i)

f <- function(n,p,k,a){
  u <- runif(1)
  u1 <- runif(1)
  i <- k; p_inicial <- p
  p <- (choose(n,k)*(p**k)*(1-p)**(n-k))/a; f <- p
  while (u>=f){
    p <- (n-i)*p_inicial*p/((i+1)*(1-p_inicial)); i <- i+1; f <- f+p
  }
  x <- i
  return(x)
}
f(3,.5,1,.5)

#ITEM b

#COMPARAR COM QUAL DSITRIBUIÇÃO



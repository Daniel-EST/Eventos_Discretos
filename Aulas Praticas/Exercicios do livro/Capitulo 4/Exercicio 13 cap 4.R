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

#Testando 

e <- 0
for (i in 1:1000){
 e[i] <- f(3,.5,1,.5) 
}
mean(e)

#ITEM b

#Vamos usar o Metodo da Rejeição comparando com uma binomial
#Temos que c=1/a ==> h=a*pz/qz???????

#Gerando um binomial
ber <- function(p){
  # p é a probabilidade de sucesso
  u <- runif(1)
  if (u<1-p){
    x <- 0
    return(x)
  }
  else {
    x <- 1
    return(x)
  }
}
binomial <- function(n,p){
  y <- 0
  for (i in 1:n){
    y[i] <- ber(p)
  }
  x <- sum(y)
  return(x)
}

py <- function(i,n,p,a,k){
  if (i>=k & n>i){
    p <- dbinom(i,n,p)/a
    return(p)
  }
  else{
    p <- 0
    return(p)
  }
} 

f2 <- function(n,p,k,a){
  y <- binomial(n,p)
  qy <- dbinom(y,n,p) 
  py <- py(y,n,p,a,k)
  u <- runif(1)
  h <- a*py/qy
  while(u>h){
    y <- binomial(n,p)
    qy <- dbinom(y,n,p) 
    py <- py(y,n,p,a,k)
    u <- runif(1)
    h <- a*py/qy
  }
  x <- y
  return(x)
}

#Testando

e1 <- 0
for (i in 1:1000){
  e1[i] <- f2(3,.5,1,.5) 
}
mean(e1)

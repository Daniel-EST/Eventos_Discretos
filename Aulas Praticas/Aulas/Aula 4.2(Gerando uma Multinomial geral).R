#Gerando um vetor (X1,...,Xn)~Multinomial(n,p) esse é o caso generico

#Primeiro vamos gerar um binomial
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

#Criando duas função para ter o Qi sera util na hora de gerar a multinominal

qi <- function(p,i){
  q <- p[i]/(1-sum(p[1:i-1]))
  return(q)
}

#Gerando a multinomial

multig <- function(n,m,p){
  x <- 0
  x[1] <- binomial(m,p[1])
  for (i in 2:n){
    m1 <- m-sum(x[1:i-1])

    x[i] <- binomial(m1,qi(p,i)); m1 <- 0
  }
  x[n] <- m-sum(x[1:n-1])
  return(x)
}
p <- c(.2,.2,.5,.1)
multig(4,5,p)

#Verificando se esta correto (peguei a 1ª "coordenada" pq sabemos X1~bin(m,p[1]))
e <- 0
for (i in 1:1000){
  e[i] <- multig(4,5,p)[1]
}
mean(e)

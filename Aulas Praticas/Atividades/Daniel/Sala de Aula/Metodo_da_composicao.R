# Exercício aula 18/09/18
# Gere Y v.a.
qa <- function(x){
  t <- as.integer(3*runif(1)) + 1
  if(t == 1) return(runif(1)**(1))
  if(t == 2) return(runif(1)**(1/3))
  return(runif(1)**(1/5))
}

qb <- function(x){
  t <- as.integer(3*runif(1)) + 1
  if(t == 1) return(-log(1-runif(1))/2)
  return(runif(1))
}

# Exercício 12 (Cap 5)
# Considere que x_i ~ Exp(1)
q12 <- function(lambda,n){
  x <- NULL
  for(i in 1:n){
    x[i] <- -log(1-runif(1))/lambda
  }
  return(max(x))
}
q12(1, 100)

# Exercício empresa

bern <- function(p){
  u <- runif(1)
  if(u >= 1-p) return(1)
  return(0)
}

bin <- function(n, p){
  x <- NULL
  for(i in 1:n){
    x[i] <- bern(p)
  }
  return(x)
}

exp <- function(lambda) return(-log(runif(1))/lambda)

custo <- function(n, p, lambda){
  return(sum(bin(n, p)) * exp(lambda))
}

custo(1000, 0.05, 1/800)
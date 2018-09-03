# Questão I
# a)
# Para \lambda pequeno
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
r_poisson(100) # Primeira posição do vetor é o número gerado, a segunda o número
# de interações.

# Para \lambda grande
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
r_poissonG(1000) # Primeira posição do vetor é o número gerado, a segunda o nú-
# mero de interações.

# b)
# Número médio de interações
interacoes <- function(n, lambda){
  x <- 0
  for(i in 1:n){
    x[i] <- r_poisson(lambda)[2]
  }
  return(mean(x))
}
interacoes(400,10) # Note que o valor esperado para esse algorítimo é: 
# 1 + lambda,
# (ver notas de aula), no caso estudado é 1 + 10 = 11, o número médio de intera-
# ções foi parecido com o valor esperado!

interacoesG <- function(n, lambda){
  x <- 0
  for(i in 1:n){
    x[i] <- r_poissonG(lambda)[2]
  }
  return(mean(x))
}
interacoesG(1000, 400) # Note que o valor esperado para esse algorítimo é:
# 1 + 0.79*sqrt(lambda),
# (ver notas de aula), no caso estudado é 1 + 0.79*sqrt(400) = 16.8, o número 
# médio de interações foi parecido com o valor esperado!

# Questão II
# a) Caso discreto exercício 4f do livro do Ross.
example4f <- function(q, p){
  if(length(q) != length(p)) stop("Both vectors must have same length")
  if(sum(p) != 1) stop("Probabilities vector doesn't sum 1")
  c <- max(p/q)
  u1 <- runif(1)
  y <- as.integer(10*u1) + 1
  u2 <- runif(1)
  while(u2 > p[y]/c*q[y]){
    u1 <- runif(1)
    y <- as.integer(10*u1) + 1
    u2 <- runif(1)
  }
  return(y)
}
q <- 1:10
p <- c(.11, .12, .09, .08, .12, .1, .09, .09, .1, .1)

# Vou calcular a esperança pra saber se a função faz sentido
sum(q * p) # 5.39

x <- c()
for(i in 1:100){
  x[i] <- example4f(q, p)
}
mean(x) # um dos valores foi 5.66, é faz sentido, é bem próximo do valor espera-
# do.

# b) Caso contínuo
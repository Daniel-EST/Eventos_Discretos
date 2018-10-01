# Questão 03 ####
# Método da rejeição
q3rej <- function(){
  prob <- c(.3, .2, .35, .15)
  y <- as.integer(4*runif(1)) + 1
  while(runif(1) > prob[y]/.35){
    prob <- c(.3, .2, .35, .15)
    y <- as.integer(4*runif(1)) + 1
  }
  return(y)
}

# Método da transformação inversa
q3inv <- function(){
  u <- runif(1)
  if(u < 0.3) return(1)
  if(u < 0.5) return(2)
  if(u < 0.85) return(3)
  return(4)
}

# Questão 04 ####
# Criando uma função auxiliar para gerar bernoulli.
bern <- function(){
  if(runif(1) < 1/100) return(1)
  return(0)
}
q4 <- function(){
  hits <- 0
  for(i in 1:100){
    hits <- hits + bern()
  }
  return(hits)
}

# Momentos
E_x <- function(){
  value <- 0
  for(i in 1:1000){
    value[i] <- q4()
  }
  return(mean(value))
}
E_x2 <- function(){
  value <- 0
  for(i in 1:1000){
    value[i] <- q4()^2
  }
  return(mean(value))
}

# Variância
var_x <- function() return(E_x2() - E_x()**2)

# Valores aproximados.
E_x() # Aproximação do valor esperado.
var_x() # Aproximação da variância esperada.

# Questão 07 ####
# Usei o método da rejeição
aux <- function(){
  y <- as.integer(11*runif(1)) + 2
  p <- c(1,2,3,4,5,6,5,4,3,2,1)/36
  while(runif(1) > p[y-1]/(1/6)) y <- as.integer(11*runif(1)) + 2
  return(y)
}
aux()
q07 <- function(){
  n <- 0
  vector <- 2:12
  while(!all(vector == 0)){
    vector[aux()-1] <- 0
    n <- n + 1
  }
  return(n)
}
q07()

# Valor esperado
{
  x <- 0
  for(i in 1:100){
    x[i] <- q07()
  }
  cat("Valor esperado de tentativas é =", mean(x))
  rm(list = c("x","i"))
}

# Questão 10 ####

# Questão 11 ####

# Questão 13 ####

# Questão 17 ####

# Questão 18 ####

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
# Questão 07 ####
# Questão 10 ####
# Questão 11 ####
# Questão 13 ####
# Questão 17 ####
# Questão 18 ####

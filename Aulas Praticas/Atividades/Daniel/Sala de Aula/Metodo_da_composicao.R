# Exercício aula 18/09/18
# Gere Y v.a.

# gerando unif.disc~{1,2,3}
as.integer(3*runif(1))+1

qa <- function(x){
  t <- as.integer(3*runif(1)) + 1
  if(t == 1) return(runif(1)**(1))
  if(t == 2) return(runif(1)**(1/3))
  else return(runif(1)**(1/5))
}

qb <- function(x){
  t <- as.integer(3*runif(1)) + 1
  if(t == 1) return(-log(1-runif(1))/2)
  return(runif(1))
}

# Exercício 12 (Cap 5)


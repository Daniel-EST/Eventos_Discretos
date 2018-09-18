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

ber(0.2)
#Verificando se esta certo
e <- 0
for (i in 1:10000){
  e[i] <- ber(0.2)
}
mean(e)


# Funções auxiliares
# Gerar uma exponencial
expRandom <- 
  function(lambda) return(-log(runif(1))/lambda)

# Gerar uma gama
gammaRandom  <-
  function(alpha, beta)
{
  temp <- NULL
  for (i in 1:alpha){
    temp[i] <- expRandom(beta)
  }
  return(sum(temp))
}

# Resolvendo o problema

algorithm <- 
  function()
{
  y <- runif(1, .02, .1)
  w1 <- expRandom(y)
  w2 <- expRandom(y)
  return(c(w1, w2, y))
}

algorithm2 <- 
  function(n)
{
  temp <- algorithm()
  w1 <- temp[1] ; w2 <- temp[2] ; y <- temp[3]
  aux <- list()
  for(i in 1:n)
  {
    while(y < .02 | y > 0.1){
      y <- gammaRandom(3, w1[i] + w2[i])
    }
    w1[i] <- gammaRandom(26, y + 1/2)
    w2[i] <- gammaRandom(19, y + 1/2)
  }
  return(list(w1 = w1, 
              w2 = w2))
}

simulation <- algorithm2(10200)

JogadorA <- mean(25 + .5*algorithm2(10200)$w1[201:10200]) ; JogadorA
JogadorB <- mean(18 + .5*algorithm2(10200)$w2[201:10200]) ; JogadorB

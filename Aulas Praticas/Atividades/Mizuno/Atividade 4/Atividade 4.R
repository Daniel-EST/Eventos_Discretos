# Atividade 3 ---------------------------------------------------------------
expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

expo_n <- function(n,lambda){
  x <- NULL
  for (i in 1:n){
    x[i] <- expo(lambda)
  }
  return(x)
}


# Item a ------------------------------------------------------------------

#Definindo I a v.a indicadora de  soma(iXi)>21.6 sabemos que E[I]=P(soma(iXi)>21.6)

I <- function(n,lambda){
  s <- sum(1:n*expo_n(n,1))
  if (s>=21.6){
    return(1)
  }
  else{
    return(0)
  }
}

#Vamos achar E[I] e assim estimar a probabilidade pedida

e <- 0
for (i in 1:1000){
  e[i] <- I(5,1)
}
mean(e)
p <- plot_ly(x = e,
             type = "histogram",
             histnorm = "probability")
p

# Item b ------------------------------------------------------------------
ant <- function(n){
  z1 <- z2 <- m <- prob <- NULL
  for (j in 1:n){
    for (i in 1:100){
      u <- runif(5)
      z1[i] <- sum(1:5*(-log(u)))
      z2[i] <- sum(1:5*(-log(1-u)))
      
      if(z1[i]>21.6){z1[i]=1}
      else{z1[i]=0}
      if(z2[i]>21.6){z2[i]=1}
      else{z2[i]=0}
      m[i] <- mean(c(z1[i],z2[i]))
    }
    prob[j] <- mean(m)
  }
  return(prob)
}

mean(ant(100))
# Item c ------------------------------------------------------------------

#Definindo a variavel controle Z=soma(iXi) ==> E[Z]=15 e Var[Z]=55
#Simulando cov(Y,Z), onde Y=I

f <- function(n){
  prob <- NULL
  Y <- 0; Z <- 0
  for (i in 1:n){
    Y[i] <- I(5,1)
    Z[i] <- sum(1:5*expo_n(5,1))
  }
  cov <- cov(Y,Z)
  prob <- c(prob,mean(Y)-(cov/55)*(mean(Z)-15))
  return(prob)
}
f(100)

# Item d ------------------------------------------------------------------

e1 <- 0
for (i in 1:1000){
  e1[i] <- f(100)
}

e <- ant(1000)

hist(e1, col="red", ylab=" ", 
     xlab=expression(theta), main="Analisando a variancia dos dois metodos" )
hist(e, col="blue", add=T)
legend("topright", legend=c("Controle","Antagonicas"), col=c("red", 
                                                      "blue"), pt.cex=2, pch=15 )

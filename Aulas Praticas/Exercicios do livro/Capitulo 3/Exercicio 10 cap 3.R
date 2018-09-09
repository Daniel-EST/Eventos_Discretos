#Temos que Cov(X,exp(X))=E[X*exp(X)]-E[X]*E[exp(X)], vamos estimar essas 3 esperanças

g1 <- function(x){
  return(x*exp(x))
}

g2 <- function(x){
  return(x)
}

g3 <- function(x){
  return(exp(x))
}

e1 <- 0
for (i in 1:100000){
  e1[i] <- g1(runif(1))
}

e2 <- 0
for (i in 1:100000){
  e2[i] <- g2(runif(1))
}

e3 <- 0
for (i in 1:100000){
  e3[i] <- g3(runif(1))
}
cov <- mean(e1)-mean(e2)*mean(e3); cov
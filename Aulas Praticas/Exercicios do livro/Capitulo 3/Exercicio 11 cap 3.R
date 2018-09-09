#ITEM a

#Temos que Cov(X,sqrt(1-X**2))=E[X*sqrt(1-X**2)]-E[X]*E[sqrt(1-X**2)], 
#vamos estimar essas 3 esperanças

g1 <- function(x){
  return(x*sqrt(1-x**2))
}
g2 <- function(x){
  return(x)
}
g3 <- function(x){
  return(sqrt(1-x**2))
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

#ITEM b

#Temos que Cov(X**@,sqrt(1-X**2))=E[X*sqrt(1-X**2)]-E[X]*E[sqrt(1-X**2)], 
#vamos estimar essas 3 esperanças

g4 <- function(x){
  return(x**2)
}

e4 <- 0
for (i in 1:100000){
  e4[i] <- g4(runif(1))
}
cov <- mean(e1)-mean(e4)*mean(e3); cov

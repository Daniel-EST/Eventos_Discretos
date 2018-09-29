#Gernado Uniformes num intervalo (a,b)

uniforme <- function(a,b){
  u <- runif(1)
  x <- u*(b-a)+a
  return(x)
}

#Testando

e <- 0
for (i in 1:1000000){
  e[i] <- uniforme(0,1)
}
mean(e)

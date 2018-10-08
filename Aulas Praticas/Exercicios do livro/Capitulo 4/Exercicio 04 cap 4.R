#Sabemos que a probabiliade da i-esima carta sair na i-esima posição é 1/100
#Temos que a esperança que queremos estimar é:E[X]=1, onde X=I1+I2+...+In
#Vamos estimar E[I1+I2+...+I100], onde sabemos que I1~Ber(1/100)

#Gerando Ii~Ber(1/100)
ber <- function(p){
  # p é a probabilidade de sucesso
  u <- runif(1)
  if (u<1-p){
    y <- 0
    return(y)
  }
  else {
    y <- 1
    return(y)
  }
}

#Achando I1+I2+...+I100
a <- 0;  x <- 0
for (j in 1:1000){
  for (i in 1:100){
    a[i] <- ber(1/100)
  }
  x[j] <- sum(a)
}
mean(x)



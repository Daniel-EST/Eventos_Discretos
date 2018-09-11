a <- 0
for (i in 1:20){
  c <- 0
  for (j in 1:100000){
    c[j] <- poisson_c1(i)
  }
  a[i] <- mean(c)
}
b <- 0
for (i in 1:20){
  c <- 0
  for (j in 1:100000){
    c[j] <- poisson_c2(i)
  }
  b[i] <- mean(c)
}

plot(1:20,a,type='l',xlab=expression(lambda),ylab="Número de interações",col="blue")
lines(1:20,b,type="l",col='red')

x <- c(23,66)
for (i in 3:100){
  x[i] <- (3*x[i-1]+5*x[i-2])%%100
}
u <- x/100
for (i in 1:14){
  print(u[i])
}
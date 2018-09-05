geometrica <- function(p){
  u1 <- runif(1)
  x <- 1+as.integer(log(1-u1)/log(1-p))
  return(x)
}
a <- 0
for (i in 1:10000){
  a[i] <- geometrica(0.1)
}
mean(a-1)
mean(rgeom(10000,.1))

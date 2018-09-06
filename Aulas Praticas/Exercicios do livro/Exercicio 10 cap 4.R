#Item A#######

#Sabemos que uma binomial negativa é a soma de Geometricas com parâmetro p
geometrica <- function(p){
  u1 <- runif(1)
  x <- 1+as.integer(log(1-u1)/log(1-p))
  return(x)
}

bio_neg <- function(p,r){
  y <- 0
  for (i in 1:r){
    y[i] <- geometrica(p)
  }
  return(sum(y))
}
bio_neg(0.1,4)

a <- 0
for (i in 1:10000){
  a[i] <- bio_neg(0.1,4)
}
mean(a)
#Item C##########

bio_neg2 <- function(p,r){
  u1 <- runif(1)
  i <- r; p <- p**r; f <- p
  while (u1>=f){
    i <- i+1; p <- i*(1-p)*p/(i+1-r); f <- p+f
  }
  x <- i
  return(x)
}
bio_neg2(0.1,4)

a <- 0
for (i in 1:10000){
  a[i] <- bio_neg2(0.1,4)
}
mean(a)

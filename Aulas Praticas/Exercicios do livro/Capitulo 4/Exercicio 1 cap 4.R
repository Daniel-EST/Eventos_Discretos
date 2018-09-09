#Gerando os X's

f <- function( ){
  u <- runif(1)
  if (u<1/3){
    x <- 1
  }
  else {x <- 2}
  return(x)
}

#Achando a quantidade de 1

N <- function(n){
  x <- 0; cont <- 0
  for (i in 1:n){
    x[i] <- f( )
    if (x[i]==1){
      cont <- cont+1
    }
  }
  cont <- cont/n
  return(cont)
}

#ITEM a
N(100)

#ITEM b
N(1000)

#ITEM c
N(10000)
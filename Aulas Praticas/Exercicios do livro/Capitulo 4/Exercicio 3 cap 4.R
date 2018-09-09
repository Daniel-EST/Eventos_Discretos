#Gerando os X's

f <- function( ){
  u <- runif(1)
  if (u<.3){
    x <- 1
    return(x)
  }
  if (u<0.5){
    x <- 2
    return(x)
  }
  if (u<0.85){
    x <- 3
    return(x)
  }
  if (u<1){
    x <- 4
    return(x)
  }
}

#Verificando se esta correto
a <- 0
for (i in 1:100000){
  a[i] <- f( )
}
mean(a)

#Usando a funcao 1 do execicio anterior

X <- function(p,x){
  u <- runif(1); s <- p[1]
  for (i in 1:length(p)){
    if (u<s){
      return(x[i])
    }
    s <- s+p[i]
  }
}

X(c(.3,.2,.35,.15),c(1:4))

#Usando a funcao 2 do execicio anterior

X2 <- function(p,x,n){
  a <- 0
  for (j in 1:n){
    u <- runif(1); s <- 0
    for (i in 1:length(p)){
      s <- s+p[i]
      if (u<s){
        a[j] <- x[i]
        break
      }
    }
  }
  cat("Os numeros gerados são:",a,"\n")
  mean(a)
}
X2(c(.3,.2,.35,.15),c(1:4),100000)

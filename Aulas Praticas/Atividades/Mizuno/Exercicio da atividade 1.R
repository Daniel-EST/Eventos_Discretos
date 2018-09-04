#Criando a funcao de maneira generica para o caso discreto
gerando_X <- function(p,q){
  #Achando a constante C
  c <- max(p/q)
  
  #Estou gerando Y
  u1 <- runif(1)
  Y <- as.integer(10*u1)+1
  u2 <- runif(1);u2
  
  #Gerando X
  while(u2>=p[Y]/(c*q[Y])){
    u1 <- runif(1)
    Y <- as.integer(10*u1)+1
    u2 <- runif(1)
  }
  X <- Y
  print(X)
} 
for ( i in 1:10){
  gerando_X(p,q)
}






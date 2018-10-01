#Usando a função do exercicio 2 desse capitulo
#Não estou usando o metodo mandado pelo livro

X <- function(p,x){
  u <- runif(1); s <- 0
  for (i in 1:length(p)){
    s <- s+p[i]
    if (u<s){
      return(x[i])
    }
    
  }
}
x <- c(1:10)
p <- c(rep(.06,time=5),.15,.13,.14,.15,.13); sum(p)

#Testando

e <- 0
for (i in 1:10000){
  e[i] <- X(p,x)
}
mean(e)


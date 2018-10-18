#Criadno um modelo de filas 


# Variaveis ---------------------------------------------------------------

#t <- tempo
#SS <- c(x,y), x=Total no estoque e y=Total pedido
#r=preço do produto
#L=tempo ate a entrega do pedido
#C=custo do pedido ate o tempo t
#H=custo do estoque ate o tmepo t 
#R=Ganho com vendas ate o tmepo t 
#t0=chegada do cliente
#t1=chegada do pedido
#dados r,h,s,S,L e c(y)

#Custo 
c <- function(x){
  c <- y
  return(c)
}

modelo <- function(lambda){
  t <- 0; t1 <- Inf; t0 <- -log(runif(1))/lambda; ss <- c(x,0)
  if (t0<t1){
    H <- H+(t0-t)*xh
    #Gere D~G
    w=min(D,x)
    R <- R+w*r
    x <- x-w
    if (x<s & y=0){
      y <- s-x; t1 <- t+L
      u <- runif(1); t0 <- t-log(u)/lambda
    }
    if (t1<t0){
      H <- H+(t1-t)*xh
      t <- t1
      #Temos que definir o custo
      c <- c+c(y)
      x <- x+y
      y <- 0; t1 <- Inf
      L <- (R-H-C)/T
    }
  }
}

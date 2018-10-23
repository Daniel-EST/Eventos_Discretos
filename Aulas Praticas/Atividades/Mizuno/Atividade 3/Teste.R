expo <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

t_s <- function(lambda,s){
  t <- s
  u1 <- runif(1)
  t <- t-log(u1)/lambda
  u2 <- runif(1)
  lambda_t <- 3+4/(t+1)
  while (u2>lambda_t/lambda){
    u1 <- runif(1)
    t <- t-log(u1)/lambda
    u2 <- runif(1)
  }
  t_s <- t
  return(t_s)
}

series_queue <- function(Time){
  t <- 0; Na <- 0; Nd <- 0; n1 <- 0; n2 <- 0; A1 <- 0; A2 <- 0; D <- 0
  ta <- t_s(7, 0) ; t1 <- Inf ; t2 <- Inf
  #browser()
  while(ta <= Time){
    # CASO 1
    if(ta == min(ta, t1, t2)){
      t <- ta
      Na <- Na + 1
      n1 <- n1 + 1
      ta <- t_s(7, t) 
      if(n1 == 1) {
        t1 <- t + expo(1)
      }
      A1[Na] <- t
    }
    # CASO 2
    if(t1 < ta & t1 <= t2){
      t <-  t1
      n1 <- n1 - 1 ; n2 <- n2 + 1
      if(n1 == 0){
        t1 <- Inf
      } 
      else {
        t1 <- t + expo(1)
      }
      if(n2 == 1) {
        t2 <- t + expo(3)
        }
      A2[Na - n1] <- t
    }
    #CASO3
    if(t2 < ta & t2 < t1){
      t <- t2
      Nd <- Nd + 1
      n2 <- n2 - 1
      if(n2 == 0) {
        t2 <- Inf
      }
      if(n2 > 0) {
        t2 <- t + expo(3)
      }
      D[Nd] <- t
    }
  }
  #Depois que fecha estou tirando as pessas da fila
  while (n1!=0 | n2!=0){
    if (t1 <= t2){
      t <-  t1
      n1 <- n1 - 1 ; n2 <- n2 + 1
      if(n1 == 0){
        t1 <- Inf
      } 
      else {
        t1 <- t + expo(1)
      }
      if(n2 == 1) {
        t2 <- t + expo(3)
      }
      A2[Na - n1] <- t
    }
    else{
      t <- t2
      Nd <- Nd + 1
      n2 <- n2 - 1
      if(n2 == 0) {
        t2 <- Inf
      }
      if(n2 > 0) {
        t2 <- t + expo(3)
      }
      D[Nd] <- t
    }
  }
  return(list(Chegada = A1, `Saída` = D,perm = (D - A1)))
  }
series_queue(100)

e <- 0
for(i in 1:100){
  e[i] <- series_queue(100)$perm
}
mean(e)


# Número medio de chegadas ------------------------------------------------

cont <- 0; e <- 0
for (i in 1:1000){
  t <- t_s(7,0); cont <- 0
  while(t<100){
    cont <- cont+1
    t <- t_s(7,t)
  }
  e[i] <- cont
}

mean(e)

# Variavel usado para analise ---------------------------------------------

dados <- series_queue(100); dados

# Fazendo grafico da permanencia---------------------------------------------------------

library(plotly)
y <- dados$perm; x <- 1:length(dados$perm)
eixo_x <- list(
  title = "Permanencia do i-esimo cliente",
  titlefont = t
)
eixo_y <- list(
  title = "Tempo de permancia",
  titlefont = f)

 p1<-plot_ly(y=y, x=x , type="scatter", mode="makers+lines",line = list(color = 'rgb(255, 0, 0)', width = 3)) %>% 
  layout(xaxis = eixo_x, yaxis = eixo_y); p1

# Analisando a quantidade de chegadas --------------------------------------------

quant <- runif(20)

for (i in 1:20){
  c <- 0
  for (k in 1:100){
    dado <- series_queue(100)
    cont <- 0
    for (j in 1:length(dado$Chegada)){
      if (dado$Chegada[[j]]>5*(i-1) & dado$Chegada[[j]]<5*i){
        cont <- cont+1
      }
    }
    c[k] <- cont
  }
  quant[i] <-  mean(c)
}

library(plotly)
y <- quant; x <- seq(5,100,by=5)
eixo_x <- list(
  title = "Intervalo",
  titlefont = t,
  range=c(0,105)
)
eixo_y <- list(
  title = "Número medio de chegadas",
  range=c(10,25))

p2<-plot_ly(y=y, x=x , type="scatter", mode="makers+lines",line = list(color = 'rgb(255, 0, 0)')) %>% 
  layout(xaxis = eixo_x, yaxis = eixo_y); p2


